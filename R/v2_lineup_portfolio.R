# ============================================================
# TBI V2 Phase 2C lineup-portfolio contract
# ============================================================

v2_lineup_portfolio_policy <- function() {
  list(
    policy_type = "tbi-v2-lineup-portfolio-policy",
    policy_version = "1.0.0",
    lineup_types = c("BASE", "BENCH_BRIDGE", "OFFENSE", "DEFENSE", "SMALL_BALL", "CLOSING"),
    positions = c("PG", "SG", "SF", "PF", "C"),
    players_per_lineup = 5L,
    regulation_player_minutes = 240L
  )
}


v2_lineup_portfolio_inputs <- function(rotation_state, minute_ledger,
                                       stagger_plan, role_ledger) {
  if (!is.list(rotation_state) ||
      !identical(rotation_state$contract_type, "tbi-v2-rotation-state") ||
      !is.data.frame(rotation_state$members)) {
    stop("rotation_state must be a V2 rotation state.", call. = FALSE)
  }
  if (isTRUE(rotation_state$is_blocked) || identical(rotation_state$status, "FAIL")) {
    stop("rotation_state must be non-blocked.", call. = FALSE)
  }
  members <- rotation_state$members
  required_members <- c("player_id", "player_name", "is_starter")
  if (!all(required_members %in% names(members)) ||
      !nrow(members) %in% c(10L, 11L)) {
    stop("rotation_state has malformed members.", call. = FALSE)
  }
  members$player_id <- suppressWarnings(as.integer(members$player_id))
  if (any(is.na(members$player_id)) || anyDuplicated(members$player_id) ||
      sum(vapply(seq_len(nrow(members)), function(i) isTRUE(members$is_starter[[i]]), logical(1))) != 5L) {
    stop("rotation must contain unique players and exactly five starters.", call. = FALSE)
  }

  rows <- v2_stagger_minute_rows(minute_ledger)
  if (!setequal(rows$player_id, members$player_id)) {
    stop("minute ledger must match rotation membership.", call. = FALSE)
  }
  if (!is.list(stagger_plan) ||
      !identical(stagger_plan$contract_type, "tbi-v2-stagger-plan") ||
      !is.data.frame(stagger_plan$segments)) {
    stop("stagger_plan must be a Phase 2B stagger plan.", call. = FALSE)
  }
  if (isTRUE(stagger_plan$is_blocked) || identical(stagger_plan$status, "FAIL") ||
      as.integer(stagger_plan$total_player_minutes %||% NA_integer_) != 240L) {
    stop("stagger plan must be non-blocked and reconcile to 240 player-minutes.", call. = FALSE)
  }
  segments <- stagger_plan$segments
  required_segments <- c("duration", "player_ids", "lineup_id")
  if (!all(required_segments %in% names(segments)) || !nrow(segments)) {
    stop("stagger plan segments are malformed.", call. = FALSE)
  }
  segment_valid <- vapply(segments$player_ids, function(ids) {
    ids <- suppressWarnings(as.integer(ids))
    length(ids) == 5L && !any(is.na(ids)) && !anyDuplicated(ids) && all(ids %in% members$player_id)
  }, logical(1))
  if (!all(segment_valid)) {
    stop("every stagger segment must contain five unique rotation players.", call. = FALSE)
  }
  durations <- suppressWarnings(as.integer(segments$duration))
  if (any(is.na(durations)) || any(durations <= 0L) || sum(durations) * 5L != 240L) {
    stop("stagger segments must reconcile to exactly 240 player-minutes.", call. = FALSE)
  }
  exposure <- setNames(integer(nrow(members)), members$player_id)
  for (i in seq_len(nrow(segments))) {
    ids <- as.character(segments$player_ids[[i]])
    exposure[ids] <- exposure[ids] + durations[[i]]
  }
  assigned <- rows$assigned_minutes[match(members$player_id, rows$player_id)]
  if (!identical(as.integer(exposure[as.character(members$player_id)]), as.integer(assigned))) {
    stop("stagger exposure does not reconcile to the minute ledger.", call. = FALSE)
  }
  if (!is.list(role_ledger) ||
      !identical(role_ledger$contract_type, "tbi-v2-role-eligibility-ledger") ||
      !is.list(role_ledger$records)) {
    stop("role_ledger must be a Phase 1 role-eligibility ledger.", call. = FALSE)
  }
  list(members = members, minute_rows = rows, segments = segments)
}


v2_lineup_verified_positions <- function(role_ledger, player_id,
                                         positions = v2_lineup_portfolio_policy()$positions) {
  Filter(function(position) {
    record <- tryCatch(
      v2_role_record(role_ledger, player_id, paste0("POSITION_", position)),
      error = function(e) NULL
    )
    !is.null(record) && identical(record$eligibility, "ELIGIBLE") &&
      record$verification_status %in% c("VERIFIED", "DERIVED_VERIFIED") &&
      record$evidence_class %in% c("AUTHORITATIVE_FACT", "DETERMINISTIC_DERIVATION")
  }, positions)
}


v2_lineup_position_index <- function(role_ledger, player_ids,
                                     policy = v2_lineup_portfolio_policy()) {
  ids <- sort(unique(suppressWarnings(as.integer(player_ids))))
  index <- setNames(rep(list(character()), length(ids)), as.character(ids))
  for (record in role_ledger$records) {
    id <- suppressWarnings(as.integer(record$player_id))
    if (length(id) != 1L || is.na(id) || !id %in% ids ||
        !startsWith(as.character(record$role), "POSITION_") ||
        !identical(record$eligibility, "ELIGIBLE") ||
        !record$verification_status %in% c("VERIFIED", "DERIVED_VERIFIED") ||
        !record$evidence_class %in% c("AUTHORITATIVE_FACT", "DETERMINISTIC_DERIVATION")) {
      next
    }
    position <- sub("^POSITION_", "", as.character(record$role))
    if (position %in% policy$positions) {
      index[[as.character(id)]] <- unique(c(index[[as.character(id)]], position))
    }
  }
  index
}


v2_lineup_legal_assignment <- function(player_ids, role_ledger,
                                       policy = v2_lineup_portfolio_policy(),
                                       position_index = NULL) {
  ids <- suppressWarnings(as.integer(player_ids))
  if (length(ids) != policy$players_per_lineup || any(is.na(ids)) || anyDuplicated(ids)) {
    return(NULL)
  }
  if (is.null(position_index)) {
    position_index <- v2_lineup_position_index(role_ledger, ids, policy)
  }
  v2_role_position_assignment(ids, position_index, policy$positions)
}


v2_lineup_individual_evidence <- function(player_evidence, rotation_ids) {
  fields <- c("bie_rating", "offensive_impact", "defensive_impact")
  if (is.null(player_evidence)) {
    value <- data.frame(player_id = rotation_ids, stringsAsFactors = FALSE)
    for (field in fields) value[[field]] <- NA_real_
    value$evidence_source <- "UNKNOWN"
    value$reliability <- "UNKNOWN"
    return(value)
  }
  if (!is.data.frame(player_evidence) || !"player_id" %in% names(player_evidence)) {
    stop("player_evidence must be a data frame keyed by player_id.", call. = FALSE)
  }
  value <- player_evidence
  value$player_id <- suppressWarnings(as.integer(value$player_id))
  if (any(is.na(value$player_id)) || anyDuplicated(value$player_id) ||
      any(!value$player_id %in% rotation_ids)) {
    stop("player evidence must contain unique rotation player IDs.", call. = FALSE)
  }
  if (!"evidence_source" %in% names(value) ||
      any(!toupper(as.character(value$evidence_source)) %in% c("FROZEN_BIE", "FROZEN_PLAYER_EVIDENCE"))) {
    stop("Phase 2C accepts only frozen BIE/player evidence.", call. = FALSE)
  }
  for (field in fields) {
    if (!field %in% names(value)) value[[field]] <- NA_real_
    value[[field]] <- suppressWarnings(as.numeric(value[[field]]))
  }
  value[match(rotation_ids, value$player_id), , drop = FALSE]
}


v2_lineup_objective_score <- function(ids, lineup_type, evidence, minute_rows) {
  rows <- evidence[match(ids, evidence$player_id), , drop = FALSE]
  safe_mean <- function(field) {
    values <- rows[[field]]
    if (!any(is.finite(values))) return(0)
    mean(values[is.finite(values)])
  }
  bie <- safe_mean("bie_rating")
  offense <- safe_mean("offensive_impact")
  defense <- safe_mean("defensive_impact")
  minutes <- mean(minute_rows$assigned_minutes[match(ids, minute_rows$player_id)])
  switch(lineup_type,
    OFFENSE = 0.55 * offense + 0.45 * bie,
    DEFENSE = 0.60 * defense + 0.40 * bie,
    CLOSING = 0.35 * offense + 0.35 * defense + 0.20 * bie + 0.10 * minutes,
    SMALL_BALL = 0.40 * offense + 0.30 * defense + 0.30 * bie,
    bie
  )
}


v2_lineup_candidates <- function(active_ids, assignment_for) {
  combinations <- combn(sort(active_ids), 5L, simplify = FALSE)
  Filter(function(ids) !is.null(assignment_for(ids)), combinations)
}


v2_lineup_choose <- function(candidates, lineup_type, evidence, minute_rows,
                             exclude_ids = integer()) {
  candidates <- Filter(function(ids) !length(intersect(ids, exclude_ids)), candidates)
  if (!length(candidates)) return(NULL)
  scores <- vapply(candidates, v2_lineup_objective_score, numeric(1),
    lineup_type = lineup_type, evidence = evidence, minute_rows = minute_rows)
  keys <- vapply(candidates, function(ids) {
    paste(sprintf("%010d", sort(ids)), collapse = "-")
  }, character(1))
  candidates[[order(-scores, keys, method = "radix")[[1]]]]
}


build_v2_lineup_portfolio <- function(rotation_state, minute_ledger, stagger_plan,
                                      role_ledger, player_evidence = NULL,
                                      synergy_evidence = NULL,
                                      clutch_evidence = NULL,
                                      policy = v2_lineup_portfolio_policy()) {
  expected <- v2_lineup_portfolio_policy()
  if (!identical(policy, expected)) {
    stop("policy changes fixed Phase 2C lineup constraints.", call. = FALSE)
  }
  inputs <- v2_lineup_portfolio_inputs(rotation_state, minute_ledger, stagger_plan, role_ledger)
  members <- inputs$members
  rows <- inputs$minute_rows
  segments <- inputs$segments
  evidence <- v2_lineup_individual_evidence(player_evidence, members$player_id)
  if (!is.null(synergy_evidence) || !is.null(clutch_evidence)) {
    stop("Phase 2C has no approved governed synergy or clutch evidence contract.", call. = FALSE)
  }
  out_player <- toupper(trimws(as.character(rows$availability_status))) == "OUT"
  active_ids <- rows$player_id[rows$assigned_minutes > 0L & !out_player]
  position_index <- v2_lineup_position_index(role_ledger, active_ids, policy)
  assignment_cache <- new.env(parent = emptyenv())
  assignment_for <- function(ids) {
    key <- paste(sprintf("%010d", sort(as.integer(ids))), collapse = "-")
    if (!exists(key, envir = assignment_cache, inherits = FALSE)) {
      assign(key, v2_lineup_legal_assignment(
        ids, role_ledger, policy, position_index
      ), envir = assignment_cache)
    }
    get(key, envir = assignment_cache, inherits = FALSE)
  }
  legal_candidates <- v2_lineup_candidates(active_ids, assignment_for)
  findings <- list()
  add_finding <- function(...) findings[[length(findings) + 1L]] <<- new_v2_validation_finding(...)
  records <- list()

  add_lineup <- function(type, ids, objective) {
    if (is.null(ids)) return(invisible(FALSE))
    ids <- as.integer(ids)
    assignment <- assignment_for(ids)
    if (is.null(assignment) || any(!ids %in% active_ids)) return(invisible(FALSE))
    lineup_id <- v2_state_id("lineup", sort(ids))
    actual <- as.integer(sum(segments$duration[segments$lineup_id == lineup_id]))
    target <- as.integer(max(actual, min(rows$assigned_minutes[match(ids, rows$player_id)])))
    evidence_rows <- evidence[match(ids, evidence$player_id), , drop = FALSE]
    missing_individual <- Filter(function(field) {
      field %in% names(evidence_rows) && any(!is.finite(evidence_rows[[field]]))
    }, c("bie_rating", "offensive_impact", "defensive_impact"))
    evidence_complete <- !length(missing_individual)
    reliability <- if (evidence_complete) {
      "INDIVIDUAL_EVIDENCE_ONLY"
    } else {
      "INDIVIDUAL_EVIDENCE_INCOMPLETE"
    }
    records[[length(records) + 1L]] <<- list(
      lineup_id = lineup_id,
      lineup_type = type,
      player_ids = ids,
      assigned_positions = unname(assignment),
      legality_status = "PASS",
      objective = objective,
      target_exposure = target,
      actual_exposure = actual,
      bie_components = lapply(c("bie_rating", "offensive_impact", "defensive_impact"), function(field) {
        setNames(evidence_rows[[field]], evidence_rows$player_id)
      }),
      role_coverage = setNames(unname(assignment), ids),
      reliability = reliability,
      evidence_fields = unique(c("verified_position_eligibility", "rotation_membership", "minute_ledger", "stagger_plan", if (!length(missing_individual)) "frozen_individual_evidence")),
      missing_fields = unique(c("verified_lineup_synergy", "verified_clutch_performance", missing_individual)),
      reason_codes = unique(c(
        "LEGAL_VERIFIED_POSITION_ASSIGNMENT", reliability,
        "LINEUP_SYNERGY_UNKNOWN", "CLUTCH_EVIDENCE_UNKNOWN"
      )),
      explanation = paste(type, "is a legal five-player rotation unit selected from governed position eligibility and frozen individual evidence only.")
    )
    invisible(TRUE)
  }

  starter_ids <- members$player_id[vapply(seq_len(nrow(members)), function(i) isTRUE(members$is_starter[[i]]), logical(1))]
  if (!add_lineup("BASE", starter_ids, "Approved V2 starter state")) {
    add_finding("BASE_LINEUP_ILLEGAL", "REVIEW", "The approved starter group lacks a verified legal five-position assignment.", missing_fields = "verified_starter_position_eligibility")
  }

  bridge_order <- order(-segments$bench_count, segments$period,
    -as.integer(sub(":00$", "", segments$start_clock)), segments$lineup_id, method = "radix")
  bridge_ids <- NULL
  for (index in bridge_order) {
    candidate <- as.integer(segments$player_ids[[index]])
    if (segments$bench_count[[index]] > 0L && !is.null(assignment_for(candidate))) {
      bridge_ids <- candidate
      break
    }
  }
  if (!add_lineup("BENCH_BRIDGE", bridge_ids, "Highest-bench legal unit observed in the stagger plan")) {
    add_finding("BENCH_BRIDGE_UNAVAILABLE", "REVIEW", "No stagger segment supplies a verified legal bench bridge.")
  }

  offense_ids <- v2_lineup_choose(legal_candidates, "OFFENSE", evidence, rows)
  defense_ids <- v2_lineup_choose(legal_candidates, "DEFENSE", evidence, rows)
  closing_ids <- v2_lineup_choose(legal_candidates, "CLOSING", evidence, rows)
  if (!add_lineup("OFFENSE", offense_ids, "Frozen individual offensive and BIE evidence")) add_finding("OFFENSE_LINEUP_UNAVAILABLE", "REVIEW", "No verified legal offense unit is available.")
  if (!add_lineup("DEFENSE", defense_ids, "Frozen individual defensive and BIE evidence")) add_finding("DEFENSE_LINEUP_UNAVAILABLE", "REVIEW", "No verified legal defense unit is available.")

  base_record <- Filter(function(x) identical(x$lineup_type, "BASE"), records)
  base_center <- if (length(base_record)) {
    base_record[[1]]$player_ids[match("C", base_record[[1]]$assigned_positions)]
  } else integer()
  small_candidates <- Filter(function(ids) {
    assignment <- assignment_for(ids)
    center_id <- ids[match("C", unname(assignment))]
    length(center_id) == 1L && any(v2_lineup_verified_positions(role_ledger, center_id) != "C")
  }, legal_candidates)
  small_ids <- if (length(base_center)) {
    v2_lineup_choose(small_candidates, "SMALL_BALL", evidence, rows, exclude_ids = base_center)
  } else NULL
  if (!add_lineup("SMALL_BALL", small_ids, "Legal alternate-center unit using verified position eligibility")) {
    add_finding("SMALL_BALL_ELIGIBILITY_UNKNOWN", "REVIEW", "No legal unit can omit the base center using verified alternate center eligibility.", missing_fields = "verified_alternate_center_eligibility")
  }
  if (!add_lineup("CLOSING", closing_ids, "Balanced frozen individual evidence with minute feasibility")) {
    add_finding("CLOSING_LINEUP_UNAVAILABLE", "FAIL", "No verified legal closing lineup can be constructed.", is_blocking = TRUE)
  }
  add_finding("LINEUP_SYNERGY_UNKNOWN", "REVIEW", "No governed pair or lineup synergy evidence is available.", missing_fields = "verified_lineup_synergy")
  add_finding("CLUTCH_EVIDENCE_UNKNOWN", "REVIEW", "No governed clutch-performance evidence is available and none was inferred.", missing_fields = "verified_clutch_performance")
  if (identical(minute_ledger$status, "REVIEW") || identical(stagger_plan$status, "REVIEW")) {
    add_finding("UPSTREAM_PHASE2_REVIEW", "REVIEW", "Minute or stagger evidence contains unresolved review findings.")
  }

  if (!length(records)) {
    lineups <- data.frame(
      lineup_id = character(), lineup_type = character(), legality_status = character(),
      objective = character(), target_exposure = integer(), actual_exposure = integer(),
      reliability = character(), explanation = character(), stringsAsFactors = FALSE
    )
    for (field in c("player_ids", "assigned_positions", "bie_components", "role_coverage", "evidence_fields", "missing_fields", "reason_codes")) lineups[[field]] <- I(list())
  } else {
    lineups <- data.frame(
      lineup_id = vapply(records, `[[`, character(1), "lineup_id"),
      lineup_type = vapply(records, `[[`, character(1), "lineup_type"),
      legality_status = vapply(records, `[[`, character(1), "legality_status"),
      objective = vapply(records, `[[`, character(1), "objective"),
      target_exposure = vapply(records, `[[`, integer(1), "target_exposure"),
      actual_exposure = vapply(records, `[[`, integer(1), "actual_exposure"),
      reliability = vapply(records, `[[`, character(1), "reliability"),
      explanation = vapply(records, `[[`, character(1), "explanation"),
      stringsAsFactors = FALSE
    )
    for (field in c("player_ids", "assigned_positions", "bie_components", "role_coverage", "evidence_fields", "missing_fields", "reason_codes")) {
      lineups[[field]] <- I(lapply(records, `[[`, field))
    }
    lineups <- lineups[order(match(lineups$lineup_type, policy$lineup_types), lineups$lineup_id, method = "radix"), , drop = FALSE]
    rownames(lineups) <- NULL
  }
  validation <- aggregate_v2_validation(findings)
  identity <- list(
    model_version = "2.0.0-phase2c",
    rotation_signature = rotation_state$input_signature,
    minute_signature = minute_ledger$input_signature,
    stagger_signature = stagger_plan$input_signature,
    role_signature = role_ledger$input_signature,
    player_evidence = evidence,
    policy = policy,
    lineups = lineups
  )
  list(
    contract_type = "tbi-v2-lineup-portfolio",
    contract_version = "1.0.0",
    model_name = "tbi-lineups",
    model_version = "2.0.0-phase2c",
    lineups = lineups,
    status = validation$status,
    is_blocked = validation$is_blocked,
    validation = validation,
    input_signature = v2_input_signature(identity)
  )
}
