# ============================================================
# TBI V2 starter-state contract
# ============================================================

v2_starter_findings <- function(slots, policy = v2_rotation_policy()) {
  policy <- validate_v2_rotation_policy(policy)
  findings <- list()
  add <- function(...) {
    findings[[length(findings) + 1L]] <<- new_v2_validation_finding(...)
  }

  required <- c(
    "position", "player_id", "player_name", "lock_status", "lock_source",
    "availability_status", "position_eligibility"
  )
  missing_columns <- setdiff(required, names(slots))
  if (length(missing_columns)) {
    add(
      code = "STARTER_COLUMNS_MISSING",
      status = "FAIL",
      message = "Starter slots are missing required contract fields.",
      is_blocking = TRUE,
      missing_fields = missing_columns
    )
    return(findings)
  }

  positions <- toupper(trimws(as.character(slots$position)))
  player_ids <- suppressWarnings(as.integer(slots$player_id))
  lock_sources <- toupper(trimws(as.character(slots$lock_source)))
  lock_statuses <- toupper(trimws(as.character(slots$lock_status)))
  availability <- toupper(trimws(as.character(slots$availability_status)))
  eligibility <- toupper(trimws(as.character(slots$position_eligibility)))

  if (nrow(slots) != 5L || !setequal(positions, policy$starter_positions) ||
      anyDuplicated(positions)) {
    add(
      code = "STARTER_POSITION_SET_INVALID",
      status = "FAIL",
      message = "Starter state must contain one PG, SG, SF, PF, and C slot.",
      is_blocking = TRUE
    )
  }
  if (any(is.na(player_ids)) || anyDuplicated(player_ids)) {
    add(
      code = "STARTER_PLAYER_SET_INVALID",
      status = "FAIL",
      message = "Starter slots require five unique known player IDs.",
      is_blocking = TRUE,
      missing_fields = if (any(is.na(player_ids))) "player_id" else character()
    )
  }
  if (any(!lock_statuses %in% c("LOCKED", "UNLOCKED", "PROPOSED"))) {
    add(
      code = "STARTER_LOCK_STATUS_INVALID",
      status = "FAIL",
      message = "Starter lock status must be LOCKED, UNLOCKED, or PROPOSED.",
      is_blocking = TRUE
    )
  }
  allowed_lock_sources <- c(
    "APPROVED", "MANUAL", "V1_BASELINE", "MODEL_PROPOSAL", "UNKNOWN"
  )
  if (any(!lock_sources %in% allowed_lock_sources)) {
    add(
      code = "STARTER_LOCK_SOURCE_INVALID",
      status = "FAIL",
      message = "Starter lock source is outside the V2 contract vocabulary.",
      is_blocking = TRUE
    )
  }
  unknown_lock_source <- is.na(lock_sources) | lock_sources == ""
  if (any(unknown_lock_source)) {
    add(
      code = "STARTER_LOCK_SOURCE_UNKNOWN",
      status = "REVIEW",
      message = "One or more starter lock sources are unknown.",
      missing_fields = "lock_source"
    )
  }
  allowed_availability <- c("AVAILABLE", "LIMITED", "OUT", "UNAVAILABLE", "UNKNOWN")
  if (any(!availability %in% allowed_availability)) {
    add(
      code = "STARTER_AVAILABILITY_INVALID",
      status = "FAIL",
      message = "Starter availability is outside the V2 contract vocabulary.",
      is_blocking = TRUE
    )
  }
  invalid_eligibility <- !eligibility %in% c("PASS", "REVIEW", "FAIL")
  if (any(invalid_eligibility)) {
    add(
      code = "STARTER_ELIGIBILITY_STATUS_INVALID",
      status = "FAIL",
      message = "Starter position eligibility must use PASS, REVIEW, or FAIL.",
      is_blocking = TRUE
    )
  }
  if (any(eligibility == "REVIEW", na.rm = TRUE)) {
    add(
      code = "STARTER_POSITION_ELIGIBILITY_UNKNOWN",
      status = "REVIEW",
      message = "One or more starter position assignments require eligibility review."
    )
  }
  if (any(eligibility == "FAIL", na.rm = TRUE)) {
    add(
      code = "STARTER_POSITION_INELIGIBLE",
      status = "FAIL",
      message = "One or more starter assignments are position-ineligible.",
      is_blocking = TRUE
    )
  }

  authoritative_lock <- lock_statuses == "LOCKED"
  locked_unavailable <- authoritative_lock & availability %in% c("OUT", "UNAVAILABLE")
  if (any(locked_unavailable, na.rm = TRUE)) {
    add(
      code = "LOCKED_STARTER_UNAVAILABLE",
      status = "FAIL",
      message = "An approved starter is unavailable; the lock remains authoritative.",
      is_blocking = TRUE,
      evidence_fields = c("lock_source", "availability_status")
    )
  }
  unknown_availability <- is.na(availability) | availability %in% c("", "UNKNOWN")
  if (any(unknown_availability)) {
    add(
      code = "STARTER_AVAILABILITY_UNKNOWN",
      status = "REVIEW",
      message = "One or more starter availability values are unknown.",
      missing_fields = "availability_status"
    )
  }

  findings
}


new_v2_starter_state <- function(team_id,
                                 season,
                                 slots,
                                 roster_signature,
                                 scenario_id = NULL,
                                 replacement_proposals = list(),
                                 explanation_log = list(),
                                 created_at = NA_character_,
                                 policy = v2_rotation_policy()) {
  if (!is.data.frame(slots)) {
    stop("slots must be a data frame.", call. = FALSE)
  }

  metadata <- v2_rotation_metadata()
  findings <- v2_starter_findings(slots, policy)
  validation <- aggregate_v2_validation(findings)
  identity_input <- list(
    contract_version = "1.0.0",
    model_version = metadata$model_version,
    team_id = team_id,
    season = season,
    scenario_id = scenario_id,
    roster_signature = roster_signature,
    slots = slots,
    policy_signature = v2_input_signature(policy)
  )

  list(
    contract_type = "tbi-v2-starter-state",
    contract_version = "1.0.0",
    model_name = metadata$model_name,
    model_version = metadata$model_version,
    starter_state_id = v2_state_id("starter", identity_input),
    input_signature = v2_input_signature(identity_input),
    team_id = team_id,
    season = season,
    scenario_id = scenario_id,
    created_at = created_at,
    roster_signature = roster_signature,
    slots = slots,
    replacement_proposals = replacement_proposals,
    explanation_log = explanation_log,
    status = validation$status,
    is_blocked = validation$is_blocked,
    validation = validation
  )
}
