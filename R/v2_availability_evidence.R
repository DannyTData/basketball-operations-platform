# ============================================================
# TBI V2 Phase 1E governed availability evidence
# ============================================================

v2_availability_policy <- function() {
  list(
    policy_type = "tbi-v2-availability-evidence-policy",
    contract_version = "1.0.0",
    statuses = c("AVAILABLE", "LIMITED", "OUT", "UNKNOWN"),
    verification_statuses = c("VERIFIED", "DERIVED_VERIFIED", "UNVERIFIED", "MISSING"),
    evidence_classes = c(
      "AUTHORITATIVE_FACT", "DETERMINISTIC_DERIVATION", "MODEL_EVIDENCE", "UNKNOWN"
    ),
    precedence = c(
      "MANUAL_VERIFIED", "AUTHORITATIVE_FACT", "DETERMINISTIC_DERIVATION",
      "MODEL_EVIDENCE", "UNKNOWN"
    ),
    infer_from_minutes = FALSE,
    infer_from_games = FALSE,
    infer_from_injury_history = FALSE,
    infer_from_roster_status = FALSE,
    infer_from_player_name = FALSE,
    infer_from_bie = FALSE,
    infer_from_lineup_absence = FALSE
  )
}


v2_availability_evidence_sources <- function() {
  data.frame(
    source = c(
      "MANUAL_VERIFIED", "AUTHORITATIVE_AVAILABILITY_FEED",
      "ROSTER_STATUS", "MINUTES_OR_RECENT_GAMES", "INJURY_HISTORY",
      "LINEUP_ABSENCE", "PLAYER_NAME_OR_BIE"
    ),
    evidence_class = c(
      "AUTHORITATIVE_FACT", "AUTHORITATIVE_FACT", rep("UNKNOWN", 5L)
    ),
    can_establish_status = c(TRUE, TRUE, rep(FALSE, 5L)),
    audit_finding = c(
      "Allowed only as validated session-scoped evidence with full provenance.",
      "No such source is currently present in the production database.",
      "Roster membership is not current medical or participation availability.",
      "Historical participation cannot establish current availability.",
      "Past injury evidence cannot establish present status, clearance, or return.",
      "Absence from a lineup is not availability evidence.",
      "Names and basketball evaluation outputs are never availability evidence."
    ),
    stringsAsFactors = FALSE
  )
}


validate_v2_availability_policy <- function(policy) {
  expected <- v2_availability_policy()
  if (!is.list(policy) || !all(names(expected) %in% names(policy)) ||
      any(vapply(names(expected), function(name) {
        !identical(policy[[name]], expected[[name]])
      }, logical(1)))) {
    stop("policy weakens the approved Phase 1E availability contract.", call. = FALSE)
  }
  policy
}


v2_availability_date <- function(value, field, required = FALSE) {
  v2_iso_date(value, field, required)
}


new_v2_availability_evidence <- function(player_id,
                                         team_id,
                                         season,
                                         availability_status = "UNKNOWN",
                                         verification_status = "MISSING",
                                         evidence_class = "UNKNOWN",
                                         source = NULL,
                                         source_version = NULL,
                                         effective_date = NULL,
                                         expiration_date = NULL,
                                         verified_by = NULL,
                                         reason = NULL,
                                         minute_restriction = NULL,
                                         missing_fields = character(),
                                         reason_codes = character(),
                                         evidence_version = "1.0.0",
                                         policy = v2_availability_policy()) {
  policy <- validate_v2_availability_policy(policy)
  player_id <- suppressWarnings(as.integer(player_id))
  if (length(player_id) != 1L || is.na(player_id)) {
    stop("player_id must be one known integer ID.", call. = FALSE)
  }
  team_id <- v2_role_scalar_text(team_id, "team_id")
  season <- v2_role_scalar_text(season, "season")
  availability_status <- toupper(trimws(as.character(availability_status %||% "UNKNOWN")))
  verification_status <- toupper(trimws(as.character(verification_status %||% "MISSING")))
  evidence_class <- toupper(trimws(as.character(evidence_class %||% "UNKNOWN")))
  if (length(availability_status) != 1L || is.na(availability_status) ||
      !availability_status %in% policy$statuses) {
    stop("availability_status is outside the Phase 1E vocabulary.", call. = FALSE)
  }
  if (length(verification_status) != 1L || is.na(verification_status) ||
      !verification_status %in% policy$verification_statuses) {
    stop("verification_status is outside the Phase 1E vocabulary.", call. = FALSE)
  }
  if (length(evidence_class) != 1L || is.na(evidence_class) ||
      !evidence_class %in% policy$evidence_classes) {
    stop("evidence_class is outside the Phase 1E vocabulary.", call. = FALSE)
  }

  known <- availability_status != "UNKNOWN"
  if (known && (!verification_status %in% c("VERIFIED", "DERIVED_VERIFIED") ||
                evidence_class %in% c("MODEL_EVIDENCE", "UNKNOWN"))) {
    stop("Model evidence cannot establish known availability; verified provenance is required.",
      call. = FALSE)
  }
  source <- trimws(as.character(source %||% ""))
  source_version <- trimws(as.character(source_version %||% ""))
  verified_by <- trimws(as.character(verified_by %||% ""))
  reason <- trimws(as.character(reason %||% ""))
  effective_date <- v2_availability_date(effective_date, "effective_date", required = known)
  expiration_date <- v2_availability_date(expiration_date, "expiration_date")
  if (!is.null(effective_date) && !is.null(expiration_date) &&
      expiration_date < effective_date) {
    stop("availability effective window is invalid.", call. = FALSE)
  }
  if (known && (!nzchar(source) || !nzchar(source_version) ||
                !nzchar(verified_by) || !nzchar(reason))) {
    stop("Known availability requires source, source_version, verified_by, and reason.",
      call. = FALSE)
  }
  approved <- v2_availability_evidence_sources()
  approved_index <- match(source, approved$source)
  if (known && (is.na(approved_index) || !approved$can_establish_status[[approved_index]] ||
                evidence_class != approved$evidence_class[[approved_index]] ||
                verification_status != "VERIFIED")) {
    stop("Known availability requires an approved source, evidence class, and VERIFIED status.",
      call. = FALSE)
  }

  restriction <- suppressWarnings(as.numeric(minute_restriction))
  if (is.null(minute_restriction) || !length(minute_restriction) ||
      is.na(restriction)) {
    restriction <- NA_real_
  } else if (length(restriction) != 1L || !is.finite(restriction) ||
             restriction < 0 || restriction > 48) {
    stop("minute_restriction must be between zero and 48.", call. = FALSE)
  }
  if (!is.na(restriction) && availability_status != "LIMITED") {
    stop("minute_restriction may only be supplied for LIMITED availability.", call. = FALSE)
  }

  if (!known) {
    verification_status <- if (verification_status %in% c("VERIFIED", "DERIVED_VERIFIED")) {
      "MISSING"
    } else {
      verification_status
    }
    evidence_class <- if (evidence_class == "MODEL_EVIDENCE") "MODEL_EVIDENCE" else "UNKNOWN"
  }
  validation <- if (known) {
    aggregate_v2_validation()
  } else {
    aggregate_v2_validation(list(new_v2_validation_finding(
      code = "AVAILABILITY_EVIDENCE_UNKNOWN",
      status = "REVIEW",
      message = "Authoritative availability evidence is unknown.",
      missing_fields = unique(c(missing_fields, "verified_availability_status"))
    )))
  }
  contract <- list(
    contract_type = "tbi-v2-availability-evidence",
    contract_version = "1.0.0",
    evidence_version = v2_role_scalar_text(evidence_version, "evidence_version"),
    player_id = player_id[[1]],
    team_id = team_id,
    season = season,
    availability_status = availability_status,
    verification_status = verification_status,
    evidence_class = evidence_class,
    source = if (nzchar(source)) source else NULL,
    source_version = if (nzchar(source_version)) source_version else NULL,
    effective_date = effective_date,
    expiration_date = expiration_date,
    verified_by = if (nzchar(verified_by)) verified_by else NULL,
    reason = if (nzchar(reason)) reason else if (known) NULL else
      "No authoritative current availability evidence was supplied.",
    minute_restriction = restriction,
    missing_fields = unique(as.character(if (known) missing_fields else
      c(missing_fields, "verified_availability_status"))),
    reason_codes = unique(as.character(if (known) {
      c(reason_codes, paste0("VERIFIED_AVAILABILITY_", availability_status))
    } else {
      c(reason_codes, "AVAILABILITY_EVIDENCE_UNKNOWN")
    })),
    status = validation$status,
    is_blocked = validation$is_blocked,
    validation = validation
  )
  contract$input_signature <- v2_input_signature(contract[setdiff(
    names(contract), c("input_signature", "status", "is_blocked", "validation")
  )])
  contract
}


v2_availability_manual_records <- function(manual_evidence,
                                           roster_ids,
                                           team_id,
                                           season,
                                           policy = v2_availability_policy()) {
  if (is.null(manual_evidence)) return(list())
  if (!is.data.frame(manual_evidence)) {
    stop("manual availability evidence must be a data frame.", call. = FALSE)
  }
  required <- c(
    "player_id", "team_id", "season", "availability_status", "evidence_class",
    "source", "source_version", "verification_status", "verified_by", "reason",
    "effective_date", "evidence_version"
  )
  missing <- setdiff(required, names(manual_evidence))
  if (length(missing)) {
    stop("manual availability evidence is missing: ", paste(missing, collapse = ", "),
      call. = FALSE)
  }
  if (!nrow(manual_evidence)) return(list())
  value <- manual_evidence
  value$player_id <- suppressWarnings(as.integer(value$player_id))
  value$team_id <- trimws(as.character(value$team_id))
  value$season <- trimws(as.character(value$season))
  value$availability_status <- toupper(trimws(as.character(value$availability_status)))
  value$source <- toupper(trimws(as.character(value$source)))
  value$verified_by <- trimws(as.character(value$verified_by))
  if (any(is.na(value$player_id))) stop("manual availability player_id is malformed.", call. = FALSE)
  if (any(!value$player_id %in% roster_ids)) {
    stop("manual availability evidence references a player outside roster.", call. = FALSE)
  }
  if (any(value$team_id != team_id)) stop("manual availability evidence has the wrong team.", call. = FALSE)
  if (any(value$season != season)) stop("manual availability evidence has the wrong season.", call. = FALSE)
  if (any(value$source != "MANUAL_VERIFIED")) {
    stop("manual availability source must be MANUAL_VERIFIED.", call. = FALSE)
  }
  if (any(!nzchar(value$verified_by))) {
    stop("manual availability evidence requires verified_by.", call. = FALSE)
  }
  keys <- as.character(value$player_id)
  duplicate_keys <- unique(keys[duplicated(keys) | duplicated(keys, fromLast = TRUE)])
  if (length(duplicate_keys)) {
    conflicting <- vapply(duplicate_keys, function(key) {
      length(unique(value$availability_status[keys == key])) > 1L
    }, logical(1))
    if (any(conflicting)) stop("manual availability evidence contains conflicting records.", call. = FALSE)
    stop("manual availability evidence contains duplicate records.", call. = FALSE)
  }
  value <- value[order(value$player_id, method = "radix"), , drop = FALSE]
  lapply(seq_len(nrow(value)), function(i) {
    row <- value[i, , drop = FALSE]
    expiration <- if ("expiration_date" %in% names(row)) row$expiration_date[[1]] else NULL
    restriction <- if ("minute_restriction" %in% names(row)) row$minute_restriction[[1]] else NULL
    new_v2_availability_evidence(
      player_id = row$player_id[[1]], team_id = team_id, season = season,
      availability_status = row$availability_status[[1]],
      verification_status = row$verification_status[[1]],
      evidence_class = row$evidence_class[[1]], source = row$source[[1]],
      source_version = row$source_version[[1]], effective_date = row$effective_date[[1]],
      expiration_date = expiration, verified_by = row$verified_by[[1]],
      reason = row$reason[[1]], minute_restriction = restriction,
      evidence_version = row$evidence_version[[1]], policy = policy
    )
  })
}


build_v2_availability_evidence_ledger <- function(roster,
                                                  team_id,
                                                  season,
                                                  manual_evidence = NULL,
                                                  as_of_date = NULL,
                                                  policy = v2_availability_policy()) {
  policy <- validate_v2_availability_policy(policy)
  if (!is.data.frame(roster) || !all(c("player_id", "player_name") %in% names(roster))) {
    stop("roster must contain player_id and player_name.", call. = FALSE)
  }
  team_id <- v2_role_scalar_text(team_id, "team_id")
  season <- v2_role_scalar_text(season, "season")
  value <- roster
  value$player_id <- suppressWarnings(as.integer(value$player_id))
  if (any(is.na(value$player_id)) || anyDuplicated(value$player_id)) {
    stop("roster player_id values must be known and unique.", call. = FALSE)
  }
  if ("team_id" %in% names(value)) {
    teams <- unique(trimws(as.character(value$team_id)))
    teams <- teams[!is.na(teams) & nzchar(teams)]
    if (length(teams) && !identical(teams, team_id)) {
      stop("roster has the wrong team for the availability ledger.", call. = FALSE)
    }
  }
  if ("season" %in% names(value)) {
    seasons <- unique(trimws(as.character(value$season)))
    seasons <- seasons[!is.na(seasons) & nzchar(seasons)]
    if (length(seasons) && !identical(seasons, season)) {
      stop("roster has the wrong season for the availability ledger.", call. = FALSE)
    }
  }
  value <- value[order(value$player_id, method = "radix"), , drop = FALSE]
  if (!is.null(manual_evidence) && nrow(manual_evidence) && is.null(as_of_date)) {
    stop("as_of_date is required when manual availability evidence is supplied.", call. = FALSE)
  }
  as_of_date <- v2_availability_date(as_of_date, "as_of_date")
  manual <- v2_availability_manual_records(
    manual_evidence, value$player_id, team_id, season, policy
  )
  manual <- lapply(manual, function(record) {
    outside_window <- !is.null(as_of_date) &&
      (as_of_date < record$effective_date ||
       (!is.null(record$expiration_date) && as_of_date > record$expiration_date))
    if (!outside_window) return(record)
    new_v2_availability_evidence(
      record$player_id, team_id, season,
      source = record$source, source_version = record$source_version,
      effective_date = record$effective_date, expiration_date = record$expiration_date,
      verified_by = record$verified_by,
      reason = "Evidence is outside its effective window and was not applied.",
      reason_codes = "AVAILABILITY_EVIDENCE_OUTSIDE_EFFECTIVE_WINDOW",
      evidence_version = record$evidence_version, policy = policy
    )
  })
  manual_ids <- vapply(manual, `[[`, integer(1), "player_id")
  records <- lapply(value$player_id, function(player_id) {
    index <- match(player_id, manual_ids)
    if (!is.na(index)) return(manual[[index]])
    new_v2_availability_evidence(
      player_id, team_id, season,
      missing_fields = "authoritative_current_availability",
      reason_codes = "NO_AUTHORITATIVE_AVAILABILITY_SOURCE",
      policy = policy
    )
  })
  identity <- list(
    contract_version = policy$contract_version,
    team_id = team_id,
    season = season,
    as_of_date = as_of_date,
    policy_signature = v2_input_signature(policy),
    records = records
  )
  list(
    contract_type = "tbi-v2-availability-evidence-ledger",
    contract_version = "1.0.0",
    team_id = team_id,
    season = season,
    as_of_date = as_of_date,
    evidence_precedence = policy$precedence,
    records = records,
    input_signature = v2_input_signature(identity),
    status = if (all(vapply(records, function(x) {
      x$availability_status != "UNKNOWN"
    }, logical(1)))) "PASS" else "REVIEW",
    is_blocked = FALSE,
    conflicting_count = 0L,
    malformed_count = 0L
  )
}


v2_availability_record <- function(ledger, player_id) {
  if (!is.list(ledger) ||
      !identical(ledger$contract_type, "tbi-v2-availability-evidence-ledger")) {
    stop("ledger must be a Phase 1E availability-evidence ledger.", call. = FALSE)
  }
  player_id <- suppressWarnings(as.integer(player_id))
  matches <- Filter(function(x) identical(x$player_id, player_id), ledger$records)
  if (length(matches) != 1L) {
    stop("ledger does not contain exactly one requested availability record.", call. = FALSE)
  }
  matches[[1]]
}


v2_availability_for_rotation <- function(ledger) {
  data.frame(
    player_id = vapply(ledger$records, `[[`, integer(1), "player_id"),
    availability_status = vapply(
      ledger$records, `[[`, character(1), "availability_status"
    ),
    stringsAsFactors = FALSE
  )
}


summarize_v2_availability_completeness <- function(ledger) {
  records <- ledger$records
  verified <- sum(vapply(records, function(x) {
    x$availability_status != "UNKNOWN" &&
      x$verification_status %in% c("VERIFIED", "DERIVED_VERIFIED")
  }, logical(1)))
  unknown <- sum(vapply(records, function(x) {
    identical(x$availability_status, "UNKNOWN")
  }, logical(1)))
  list(
    team_id = ledger$team_id,
    season = ledger$season,
    roster_players = length(records),
    verified_count = as.integer(verified),
    unknown_count = as.integer(unknown),
    conflicting_count = as.integer(ledger$conflicting_count %||% 0L),
    malformed_count = as.integer(ledger$malformed_count %||% 0L),
    coverage_status = if (verified == length(records) && !unknown) "PASS" else "REVIEW",
    input_signature = v2_input_signature(ledger$input_signature)
  )
}
