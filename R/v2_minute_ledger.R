# ============================================================
# TBI V2 Phase 2A minute-ledger contract
# ============================================================

v2_minute_policy <- function() {
  list(
    policy_type = "tbi-v2-minute-policy",
    policy_version = "1.0.0",
    total_minutes = 240L,
    rotation_sizes = c(10L, 11L),
    starter_minimum = 26L,
    starter_maximum = 40L,
    bench_minimum = 8L,
    bench_maximum = 28L
  )
}


validate_v2_minute_policy <- function(policy) {
  required <- c("total_minutes", "rotation_sizes", "starter_minimum", "starter_maximum", "bench_minimum", "bench_maximum")
  if (!is.list(policy) || !all(required %in% names(policy))) {
    stop("policy is missing required V2 minute fields.", call. = FALSE)
  }
  numeric_fields <- unlist(policy[setdiff(required, "rotation_sizes")], use.names = FALSE)
  if (!identical(as.integer(policy$rotation_sizes), c(10L, 11L)) ||
      any(!is.finite(numeric_fields)) || any(numeric_fields < 0) ||
      policy$starter_minimum > policy$starter_maximum ||
      policy$bench_minimum > policy$bench_maximum ||
      as.integer(policy$total_minutes) != 240L) {
    stop("policy changes fixed Phase 2A minute constraints.", call. = FALSE)
  }
  policy
}


v2_minute_override_records <- function(manual_overrides, rotation_ids) {
  if (is.null(manual_overrides)) return(data.frame())
  if (!is.data.frame(manual_overrides)) stop("manual overrides must be a data frame.", call. = FALSE)
  required <- c("player_id", "minutes", "source", "reason", "provenance", "validation")
  missing <- setdiff(required, names(manual_overrides))
  if (length(missing)) stop("manual overrides are malformed; missing: ", paste(missing, collapse = ", "), call. = FALSE)
  if (!nrow(manual_overrides)) return(manual_overrides)
  value <- manual_overrides
  value$player_id <- suppressWarnings(as.integer(value$player_id))
  value$minutes <- suppressWarnings(as.numeric(value$minutes))
  if (any(is.na(value$player_id)) || any(!is.finite(value$minutes))) stop("manual overrides are malformed.", call. = FALSE)
  if (any(!value$player_id %in% rotation_ids)) stop("manual override references a player outside the rotation.", call. = FALSE)
  if (any(value$minutes < 0)) stop("manual override minutes cannot be negative.", call. = FALSE)
  if (any(!nzchar(trimws(as.character(value$source)))) ||
      any(!nzchar(trimws(as.character(value$reason)))) ||
      any(!nzchar(trimws(as.character(value$provenance)))) ||
      any(toupper(trimws(as.character(value$validation))) != "VERIFIED")) {
    stop("manual overrides require source, reason, provenance, and VERIFIED validation.", call. = FALSE)
  }
  duplicate_ids <- unique(value$player_id[duplicated(value$player_id) | duplicated(value$player_id, fromLast = TRUE)])
  if (length(duplicate_ids)) {
    conflicting <- vapply(duplicate_ids, function(id) length(unique(value$minutes[value$player_id == id])) > 1L, logical(1))
    if (any(conflicting)) stop("manual overrides contain conflicting values.", call. = FALSE)
    stop("manual overrides contain duplicate records.", call. = FALSE)
  }
  value[order(value$player_id, method = "radix"), , drop = FALSE]
}


v2_minute_availability_records <- function(availability_evidence, rotation_ids) {
  if (!is.list(availability_evidence) ||
      !identical(availability_evidence$contract_type, "tbi-v2-availability-evidence-ledger") ||
      !is.list(availability_evidence$records)) {
    stop("availability_evidence must be a Phase 1E availability ledger.", call. = FALSE)
  }
  records <- availability_evidence$records
  ids <- vapply(records, function(x) suppressWarnings(as.integer(x$player_id)), integer(1))
  if (any(is.na(ids)) || anyDuplicated(ids) || !all(rotation_ids %in% ids)) {
    stop("availability ledger must contain one unique record per rotation player.", call. = FALSE)
  }
  records[match(rotation_ids, ids)]
}


allocate_v2_minutes <- function(rotation_state,
                                availability_evidence,
                                policy = v2_minute_policy(),
                                manual_overrides = NULL) {
  policy <- validate_v2_minute_policy(policy)
  if (is.list(rotation_state) &&
      identical(rotation_state$contract_type, "tbi-v2-rotation-state") &&
      (isTRUE(rotation_state$is_blocked) || identical(rotation_state$status, "FAIL"))) {
    stop("rotation_state must be a structurally valid non-blocked V2 rotation.",
      call. = FALSE)
  }
  members <- if (is.list(rotation_state) && is.data.frame(rotation_state$members)) rotation_state$members else rotation_state
  if (!is.data.frame(members)) stop("rotation_state must provide rotation members.", call. = FALSE)
  required <- c("player_id", "player_name", "is_starter", "rotation_role")
  missing <- setdiff(required, names(members))
  if (length(missing)) stop("rotation members are missing required fields.", call. = FALSE)
  members$player_id <- suppressWarnings(as.integer(members$player_id))
  if (any(is.na(members$player_id)) || anyDuplicated(members$player_id)) stop("rotation players must have unique known IDs.", call. = FALSE)
  if (!nrow(members) %in% policy$rotation_sizes) stop("V2 minute allocation supports only 10- and 11-player rotations.", call. = FALSE)
  starters <- vapply(seq_len(nrow(members)), function(i) isTRUE(members$is_starter[[i]]), logical(1))
  if (sum(starters) != 5L) stop("rotation must contain exactly five starters.", call. = FALSE)

  availability <- v2_minute_availability_records(availability_evidence, members$player_id)
  overrides <- v2_minute_override_records(manual_overrides, members$player_id)
  status <- vapply(availability, function(x) toupper(trimws(as.character(x$availability_status %||% "UNKNOWN"))), character(1))
  if (any(!status %in% c("AVAILABLE", "LIMITED", "OUT", "UNKNOWN"))) stop("availability ledger contains an unsupported status.", call. = FALSE)
  cap <- vapply(availability, function(x) {
    value <- suppressWarnings(as.numeric(x$minute_restriction %||% NA_real_))
    if (length(value) != 1L || !is.finite(value)) NA_real_ else value
  }, numeric(1))

  minimum <- ifelse(starters, policy$starter_minimum, policy$bench_minimum)
  maximum <- ifelse(starters, policy$starter_maximum, policy$bench_maximum)
  minimum[status == "OUT"] <- 0L
  maximum[status == "OUT"] <- 0L
  limited_without_cap <- status == "LIMITED" & is.na(cap)
  maximum[status == "LIMITED" & !is.na(cap)] <- pmin(maximum[status == "LIMITED" & !is.na(cap)], floor(cap[status == "LIMITED" & !is.na(cap)]))
  minimum <- pmin(minimum, maximum)
  assigned <- as.integer(minimum)
  fixed <- rep(FALSE, nrow(members))
  override_source <- rep(NA_character_, nrow(members))
  findings <- list()
  add_finding <- function(...) findings[[length(findings) + 1L]] <<- new_v2_validation_finding(...)

  if (any(limited_without_cap)) add_finding("LIMITED_MINUTE_CEILING_UNKNOWN", "REVIEW", "LIMITED availability requires an explicit verified minute ceiling.", missing_fields = "verified_minute_ceiling")
  if (any(status == "UNKNOWN")) add_finding("AVAILABILITY_UNKNOWN", "REVIEW", "Decision-relevant availability remains unknown.", missing_fields = "verified_availability_status")
  locked <- if ("lock_source" %in% names(members)) !is.na(members$lock_source) & nzchar(trimws(as.character(members$lock_source))) else starters
  if (any(status == "OUT" & starters & locked)) add_finding("LOCKED_STARTER_OUT", "FAIL", "An authoritative starter is OUT; the minute plan cannot pass.", is_blocking = TRUE)
  if ("ranking_evidence_status" %in% names(members) && any(toupper(as.character(members$ranking_evidence_status)) != "COMPLETE")) {
    add_finding("RANKING_EVIDENCE_INCOMPLETE", "REVIEW", "Selected-player ranking evidence is incomplete.", missing_fields = "complete_ranking_evidence")
  }

  if (nrow(overrides)) {
    for (i in seq_len(nrow(overrides))) {
      index <- match(overrides$player_id[[i]], members$player_id)
      value <- overrides$minutes[[i]]
      if (value != as.integer(value) || value < minimum[[index]] || value > maximum[[index]]) {
        add_finding("MANUAL_OVERRIDE_OUTSIDE_BOUNDS", "FAIL", "A manual minute override violates player bounds.", is_blocking = TRUE, path = paste0("player:", members$player_id[[index]]))
      } else {
        assigned[[index]] <- as.integer(value)
        fixed[[index]] <- TRUE
        override_source[[index]] <- as.character(overrides$source[[i]])
      }
    }
  }

  if (sum(maximum) < policy$total_minutes) {
    add_finding("MINUTE_CAPACITY_INSUFFICIENT", "FAIL", "Verified player ceilings cannot supply 240 minutes.", is_blocking = TRUE)
  } else if (sum(minimum) > policy$total_minutes || sum(assigned[fixed]) + sum(minimum[!fixed]) > policy$total_minutes) {
    add_finding("MINUTE_MINIMUMS_EXCEED_TOTAL", "FAIL", "Player minimums or overrides exceed 240 minutes.", is_blocking = TRUE)
  }

  if (!any(vapply(findings, function(x) identical(x$status, "FAIL"), logical(1)))) {
    remaining <- policy$total_minutes - sum(assigned)
    order_index <- order(!starters, ifelse(is.na(members$bench_order), 0L, members$bench_order), members$player_id, method = "radix")
    while (remaining > 0L) {
      eligible <- order_index[!fixed[order_index] & assigned[order_index] < maximum[order_index]]
      if (!length(eligible)) break
      for (index in eligible) {
        if (remaining <= 0L) break
        assigned[[index]] <- assigned[[index]] + 1L
        remaining <- remaining - 1L
      }
    }
    if (remaining != 0L) add_finding("EXACT_240_INFEASIBLE", "FAIL", "Exact 240-minute allocation is infeasible under current constraints.", is_blocking = TRUE)
  }

  reason_codes <- lapply(seq_len(nrow(members)), function(i) unique(c(
    if (status[[i]] == "UNKNOWN") "AVAILABILITY_UNKNOWN" else paste0("AVAILABILITY_", status[[i]]),
    if ("ranking_evidence_status" %in% names(members) && toupper(as.character(members$ranking_evidence_status[[i]])) != "COMPLETE") "RANKING_EVIDENCE_INCOMPLETE",
    if (fixed[[i]]) "MANUAL_OVERRIDE_APPLIED"
  )))
  evidence_fields <- lapply(availability, function(x) unique(c("rotation_membership", x$evidence_fields %||% character())))
  missing_fields <- lapply(seq_len(nrow(members)), function(i) unique(c(
    availability[[i]]$missing_fields %||% character(),
    if (limited_without_cap[[i]]) "verified_minute_ceiling",
    if ("ranking_evidence_status" %in% names(members) && toupper(as.character(members$ranking_evidence_status[[i]])) != "COMPLETE") "complete_ranking_evidence"
  )))
  ledger <- data.frame(
    player_id = members$player_id,
    player_name = as.character(members$player_name),
    is_starter = starters,
    rotation_role = as.character(members$rotation_role),
    availability_status = status,
    minimum_minutes = as.integer(minimum),
    maximum_minutes = as.integer(maximum),
    target_minutes = as.integer(assigned),
    assigned_minutes = as.integer(assigned),
    allocation_source = ifelse(fixed, "MANUAL_OVERRIDE", "V2_POLICY"),
    override_source = override_source,
    explanation = vapply(seq_len(nrow(members)), function(i) paste0(members$player_name[[i]], " receives ", assigned[[i]], " minutes within governed bounds."), character(1)),
    stringsAsFactors = FALSE
  )
  ledger$evidence_fields <- I(evidence_fields)
  ledger$missing_fields <- I(missing_fields)
  ledger$reason_codes <- I(reason_codes)
  validation <- aggregate_v2_validation(findings)
  identity <- list(model_version = "2.0.0-phase2a", members = members, availability_signature = availability_evidence$input_signature, policy = policy, overrides = overrides, ledger = ledger)
  list(
    contract_type = "tbi-v2-minute-ledger",
    contract_version = "1.0.0",
    model_name = "tbi-minutes",
    model_version = "2.0.0-phase2a",
    ledger = ledger,
    total_assigned_minutes = as.integer(sum(ledger$assigned_minutes)),
    status = validation$status,
    is_blocked = validation$is_blocked,
    validation = validation,
    input_signature = v2_input_signature(identity)
  )
}
