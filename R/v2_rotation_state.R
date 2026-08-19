# ============================================================
# TBI V2 rotation-state contract
# ============================================================

v2_rotation_state_findings <- function(requested_rotation_size,
                                       members,
                                       role_eligibility,
                                       policy = v2_rotation_policy()) {
  policy <- validate_v2_rotation_policy(policy)
  findings <- list()
  add <- function(...) {
    findings[[length(findings) + 1L]] <<- new_v2_validation_finding(...)
  }
  requested_rotation_size <- suppressWarnings(as.integer(requested_rotation_size))

  if (length(requested_rotation_size) != 1L || is.na(requested_rotation_size) ||
      !requested_rotation_size %in% policy$rotation_sizes) {
    add(
      code = "ROTATION_SIZE_INVALID",
      status = "FAIL",
      message = "Phase 1 rotation size must be exactly 10 or 11.",
      is_blocking = TRUE
    )
  }
  required <- c(
    "player_id", "player_name", "is_starter", "starter_position",
    "bench_order", "rotation_role"
  )
  missing_columns <- setdiff(required, names(members))
  if (length(missing_columns)) {
    add(
      code = "ROTATION_COLUMNS_MISSING",
      status = "FAIL",
      message = "Rotation members are missing required contract fields.",
      is_blocking = TRUE,
      missing_fields = missing_columns
    )
    return(findings)
  }

  player_ids <- suppressWarnings(as.integer(members$player_id))
  if (!is.na(requested_rotation_size) && nrow(members) != requested_rotation_size) {
    add(
      code = "ROTATION_SIZE_UNMET",
      status = "FAIL",
      message = "Rotation member count does not match the requested size.",
      is_blocking = TRUE
    )
  }
  if (any(is.na(player_ids)) || anyDuplicated(player_ids)) {
    add(
      code = "ROTATION_PLAYER_SET_INVALID",
      status = "FAIL",
      message = "Rotation members require unique known player IDs.",
      is_blocking = TRUE
    )
  }
  starters <- vapply(seq_len(nrow(members)), function(i) isTRUE(members$is_starter[[i]]), logical(1))
  if (sum(starters) != 5L) {
    add(
      code = "ROTATION_STARTER_COUNT_INVALID",
      status = "FAIL",
      message = "Rotation state must contain exactly five starters.",
      is_blocking = TRUE
    )
  }
  roles <- toupper(trimws(as.character(members$rotation_role)))
  starter_positions <- toupper(trimws(as.character(members$starter_position[starters])))
  if (sum(starters) == 5L &&
      (!setequal(starter_positions, policy$starter_positions) ||
       anyDuplicated(starter_positions))) {
    add(
      code = "ROTATION_STARTER_POSITIONS_INVALID",
      status = "FAIL",
      message = "Rotation starters must map uniquely to PG, SG, SF, PF, and C.",
      is_blocking = TRUE
    )
  }
  if (any(roles[starters] != "STARTER", na.rm = TRUE) ||
      any(roles[!starters] == "STARTER", na.rm = TRUE)) {
    add(
      code = "ROTATION_ROLE_STARTER_MISMATCH",
      status = "FAIL",
      message = "Starter flags and rotation roles are inconsistent.",
      is_blocking = TRUE
    )
  }
  if (sum(roles == "SIXTH_MAN", na.rm = TRUE) != 1L) {
    add(
      code = "SIXTH_MAN_INVALID",
      status = "FAIL",
      message = "Rotation state must identify exactly one sixth man.",
      is_blocking = TRUE
    )
  }
  bench_order <- suppressWarnings(as.integer(members$bench_order[!starters]))
  if (length(bench_order) &&
      (!identical(sort(bench_order), seq_along(bench_order)) || any(is.na(bench_order)))) {
    add(
      code = "ROTATION_BENCH_ORDER_INVALID",
      status = "FAIL",
      message = "Bench order must be a unique contiguous sequence beginning at one.",
      is_blocking = TRUE
    )
  }

  if (!is.list(role_eligibility)) {
    add(
      code = "ROLE_ELIGIBILITY_CONTRACT_INVALID",
      status = "FAIL",
      message = "Role eligibility must be supplied as V2 role contracts.",
      is_blocking = TRUE
    )
  } else {
    for (contract in role_eligibility) {
      if (!is.list(contract) ||
          !identical(contract$contract_type, "tbi-v2-role-eligibility")) {
        add(
          code = "ROLE_ELIGIBILITY_CONTRACT_INVALID",
          status = "FAIL",
          message = "Role eligibility contains an invalid contract.",
          is_blocking = TRUE
        )
      }
    }

    valid_contracts <- Filter(
      function(x) is.list(x) && identical(x$contract_type, "tbi-v2-role-eligibility"),
      role_eligibility
    )
    member_ids <- suppressWarnings(as.integer(members$player_id))
    outside_contracts <- Filter(
      function(x) !x$player_id %in% member_ids,
      valid_contracts
    )
    if (length(outside_contracts)) {
      add(
        code = "ROLE_ELIGIBILITY_PLAYER_OUTSIDE_ROTATION",
        status = "FAIL",
        message = "Role eligibility may only support players in the rotation.",
        is_blocking = TRUE
      )
    }
    valid_contracts <- Filter(
      function(x) x$player_id %in% member_ids,
      valid_contracts
    )
    for (role in policy$role_types) {
      role_contracts <- Filter(function(x) identical(x$role, role), valid_contracts)
      has_eligible <- any(vapply(
        role_contracts,
        function(x) identical(x$eligibility, "ELIGIBLE"),
        logical(1)
      ))
      has_unknown <- !length(role_contracts) || any(vapply(
        role_contracts,
        function(x) identical(x$eligibility, "UNKNOWN"),
        logical(1)
      ))

      if (!has_eligible && has_unknown) {
        add(
          code = paste0(role, "_COVERAGE_UNKNOWN"),
          status = "REVIEW",
          message = paste(role, "coverage is unknown and requires review."),
          missing_fields = paste0(tolower(role), "_eligibility")
        )
      } else if (!has_eligible) {
        add(
          code = paste0(role, "_COVERAGE_UNMET"),
          status = "FAIL",
          message = paste(role, "coverage is verified as unmet."),
          is_blocking = TRUE
        )
      }
    }
  }

  findings
}


new_v2_rotation_state <- function(team_id,
                                  season,
                                  starter_state_id,
                                  requested_rotation_size,
                                  members,
                                  roster_signature,
                                  policy_signature,
                                  role_eligibility = list(),
                                  scenario_id = NULL,
                                  excluded_players = data.frame(),
                                  override_ledger = list(),
                                  explanation_log = list(),
                                  created_at = NA_character_,
                                  policy = v2_rotation_policy()) {
  if (!is.data.frame(members)) {
    stop("members must be a data frame.", call. = FALSE)
  }

  metadata <- v2_rotation_metadata()
  findings <- v2_rotation_state_findings(
    requested_rotation_size = requested_rotation_size,
    members = members,
    role_eligibility = role_eligibility,
    policy = policy
  )
  validation <- aggregate_v2_validation(findings)
  identity_input <- list(
    contract_version = "1.0.0",
    model_version = metadata$model_version,
    team_id = team_id,
    season = season,
    scenario_id = scenario_id,
    starter_state_id = starter_state_id,
    requested_rotation_size = suppressWarnings(as.integer(requested_rotation_size)),
    members = members,
    roster_signature = roster_signature,
    policy_signature = policy_signature,
    role_eligibility = role_eligibility
  )

  list(
    contract_type = "tbi-v2-rotation-state",
    contract_version = "1.0.0",
    model_name = metadata$model_name,
    model_version = metadata$model_version,
    rotation_state_id = v2_state_id("rotation", identity_input),
    input_signature = v2_input_signature(identity_input),
    starter_state_id = starter_state_id,
    team_id = team_id,
    season = season,
    scenario_id = scenario_id,
    created_at = created_at,
    roster_signature = roster_signature,
    policy_signature = policy_signature,
    requested_rotation_size = suppressWarnings(as.integer(requested_rotation_size))[[1]],
    actual_rotation_size = nrow(members),
    members = members,
    role_eligibility = role_eligibility,
    excluded_players = excluded_players,
    override_ledger = override_ledger,
    explanation_log = explanation_log,
    status = validation$status,
    is_blocked = validation$is_blocked,
    validation = validation
  )
}
