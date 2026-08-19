# ============================================================
# TBI V2 Phase 1B rotation engine
# ============================================================

v2_rotation_availability <- function(x) {
  value <- toupper(trimws(as.character(x %||% "UNKNOWN")))
  if (length(value) != 1L || is.na(value) || !nzchar(value)) return("UNKNOWN")
  if (value %in% c("AVAILABLE", "ACTIVE", "HEALTHY", "PROBABLE")) return("AVAILABLE")
  if (value %in% c("LIMITED", "QUESTIONABLE", "RESTRICTED", "MINUTES RESTRICTION")) return("LIMITED")
  if (value %in% c("OUT", "INACTIVE", "INJURED", "SUSPENDED", "UNAVAILABLE")) return("OUT")
  "UNKNOWN"
}


v2_rotation_roster <- function(roster, availability = NULL) {
  if (!is.data.frame(roster)) stop("roster must be a data frame.", call. = FALSE)
  if (!all(c("player_id", "player_name") %in% names(roster))) {
    stop("roster must contain player_id and player_name.", call. = FALSE)
  }

  value <- roster
  n <- nrow(value)
  add_default <- function(name, default) {
    if (!name %in% names(value)) value[[name]] <<- rep(default, n)
  }
  add_default("position", "")
  add_default("depth_order", NA_integer_)
  add_default("availability_status", "UNKNOWN")
  add_default("bie_rating", NA_real_)
  add_default("projected_bie_rating", NA_real_)
  add_default("impact_score", NA_real_)
  add_default("primary_role", "")
  add_default("archetype", "")
  add_default("impact_tier", "")
  add_default("is_preseason_rookie", NA)
  add_default("tbi_performance_available", NA)

  if (!is.null(availability)) {
    if (is.data.frame(availability)) {
      if (!all(c("player_id", "availability_status") %in% names(availability))) {
        stop("availability must contain player_id and availability_status.", call. = FALSE)
      }
      match_index <- match(value$player_id, availability$player_id)
      present <- !is.na(match_index)
      value$availability_status[present] <-
        availability$availability_status[match_index[present]]
    } else if (!is.null(names(availability)) && all(nzchar(names(availability)))) {
      match_index <- match(as.character(value$player_id), names(availability))
      present <- !is.na(match_index)
      value$availability_status[present] <- availability[match_index[present]]
    } else {
      stop("availability must be a data frame or named vector.", call. = FALSE)
    }
  }

  value$player_id <- suppressWarnings(as.integer(value$player_id))
  value$player_name <- as.character(value$player_name)
  value$depth_order <- suppressWarnings(as.integer(value$depth_order))
  value$availability_status <- vapply(
    value$availability_status,
    v2_rotation_availability,
    character(1)
  )
  value
}


v2_rotation_rank_components <- function(player, is_starter) {
  performance_available <- isTRUE(as.logical(player$tbi_performance_available[[1]]))
  known_fields <- c("bie_rating", "projected_bie_rating", "impact_score", "depth_order")
  numeric_values <- suppressWarnings(as.numeric(unlist(player[known_fields], use.names = FALSE)))
  complete <- performance_available && all(is.finite(numeric_values)) &&
    player$availability_status[[1]] != "UNKNOWN"
  priority <- if (complete) {
    calculate_player_minute_priority(list(
      bie_rating = player$bie_rating[[1]],
      projected_bie_rating = player$projected_bie_rating[[1]],
      impact_score = player$impact_score[[1]],
      primary_role = player$primary_role[[1]],
      archetype = player$archetype[[1]],
      impact_tier = player$impact_tier[[1]],
      depth_order = player$depth_order[[1]],
      is_starter = is_starter,
      availability_status = player$availability_status[[1]]
    ))
  } else {
    NA_real_
  }

  list(
    minute_priority = priority,
    depth_order = player$depth_order[[1]],
    bie_rating = if (performance_available) player$bie_rating[[1]] else NA_real_,
    projected_bie_rating = if (performance_available) player$projected_bie_rating[[1]] else NA_real_,
    impact_score = if (performance_available) player$impact_score[[1]] else NA_real_,
    availability_status = player$availability_status[[1]],
    performance_available = performance_available
  )
}


v2_rotation_excluded_row <- function(player_id, player_name, exclusion_type,
                                     reason_codes, explanation) {
  data.frame(
    player_id = suppressWarnings(as.integer(player_id)),
    player_name = as.character(player_name),
    exclusion_type = as.character(exclusion_type),
    reason_codes = paste(unique(as.character(reason_codes)), collapse = "|"),
    explanation = as.character(explanation),
    stringsAsFactors = FALSE
  )
}


v2_rotation_member_row <- function(player, is_starter, starter_position = NA_character_) {
  components <- v2_rotation_rank_components(player, is_starter)
  evidence <- c("player_id", "player_name", "depth_order", "availability_status")
  missing <- character()
  reasons <- if (is_starter) "APPROVED_STARTER_INCLUDED" else "V1_RANKING_ADAPTER_SELECTED"

  if (isTRUE(components$performance_available)) {
    evidence <- c(evidence, "bie_rating", "projected_bie_rating", "impact_score")
  } else {
    missing <- c(missing, "performance_evidence")
    reasons <- c(reasons, "PERFORMANCE_EVIDENCE_MISSING")
  }
  if (player$availability_status[[1]] == "UNKNOWN") {
    missing <- c(missing, "availability_status")
    reasons <- c(reasons, "AVAILABILITY_UNKNOWN")
  }

  result <- data.frame(
    player_id = suppressWarnings(as.integer(player$player_id[[1]])),
    player_name = as.character(player$player_name[[1]]),
    is_starter = isTRUE(is_starter),
    starter_position = as.character(starter_position),
    bench_order = NA_integer_,
    rotation_role = if (is_starter) "STARTER" else "BENCH",
    availability_status = player$availability_status[[1]],
    selection_source = if (is_starter) "APPROVED_STARTER_STATE" else "FROZEN_V1_RANKING_ADAPTER",
    explanation = if (is_starter) {
      "Included because the approved V2 starter state is authoritative."
    } else {
      "Selected by the deterministic frozen V1 ranking adapter."
    },
    stringsAsFactors = FALSE
  )
  result$rank_components <- I(list(components))
  result$evidence_fields <- I(list(unique(evidence)))
  result$missing_fields <- I(list(unique(missing)))
  result$reason_codes <- I(list(unique(reasons)))
  result
}


build_v2_rotation <- function(roster,
                              starter_state,
                              requested_rotation_size,
                              role_eligibility,
                              availability = NULL,
                              policy = v2_rotation_policy()) {
  policy <- validate_v2_rotation_policy(policy)
  prepared <- v2_rotation_roster(roster, availability)
  requested <- suppressWarnings(as.integer(requested_rotation_size))
  findings <- list()
  add_finding <- function(...) {
    findings[[length(findings) + 1L]] <<- new_v2_validation_finding(...)
  }

  if (length(requested) != 1L || is.na(requested) || !requested %in% policy$rotation_sizes) {
    add_finding("ROTATION_SIZE_INVALID", "FAIL",
      "Phase 1B supports only 10-player and 11-player rotations.", TRUE)
    requested <- if (length(requested) && !is.na(requested[[1]])) requested[[1]] else 0L
  }
  if (!is.list(starter_state) ||
      !identical(starter_state$contract_type, "tbi-v2-starter-state")) {
    stop("starter_state must be a V2 starter-state contract.", call. = FALSE)
  }
  if (!is.list(role_eligibility)) {
    stop("role_eligibility must be a list of V2 role contracts.", call. = FALSE)
  }
  if (identical(starter_state$status, "FAIL") || isTRUE(starter_state$is_blocked)) {
    add_finding("STARTER_STATE_INVALID", "FAIL",
      "The approved starter state is not valid for rotation construction.", TRUE)
  } else if (identical(starter_state$status, "REVIEW")) {
    findings <- c(findings, starter_state$validation$findings)
  }
  excluded <- data.frame()
  invalid_id_rows <- which(is.na(prepared$player_id) | duplicated(prepared$player_id))
  if (length(invalid_id_rows)) {
    add_finding("ROSTER_PLAYER_IDS_INVALID", "FAIL",
      "Roster player IDs must be known and unique.", TRUE)
    for (i in invalid_id_rows) {
      excluded <- rbind(excluded, v2_rotation_excluded_row(
        prepared$player_id[[i]], prepared$player_name[[i]], "INVALID_ROSTER_RECORD",
        "ROSTER_PLAYER_ID_INVALID",
        "Excluded because the roster record has a missing or duplicate player ID."
      ))
    }
    prepared <- prepared[!is.na(prepared$player_id) & !duplicated(prepared$player_id), , drop = FALSE]
  }

  starter_slots <- starter_state$slots
  starter_ids <- suppressWarnings(as.integer(starter_slots$player_id))
  starter_positions <- as.character(starter_slots$position)
  starter_match <- match(starter_ids, prepared$player_id)
  if (any(is.na(starter_match))) {
    add_finding("STARTER_NOT_ON_ROSTER", "FAIL",
      "One or more approved starters are absent from the roster.", TRUE)
  }

  members <- data.frame()
  for (i in seq_along(starter_ids)) {
    if (is.na(starter_match[[i]])) next
    player <- prepared[starter_match[[i]], , drop = FALSE]
    members <- rbind(members, v2_rotation_member_row(player, TRUE, starter_positions[[i]]))
    if (player$availability_status[[1]] == "OUT") {
      locked <- identical(toupper(as.character(starter_slots$lock_status[[i]])), "LOCKED")
      add_finding(
        if (locked) "LOCKED_STARTER_UNAVAILABLE" else "STARTER_UNAVAILABLE",
        "FAIL",
        if (locked) {
          "An approved locked starter is unavailable and remains authoritative."
        } else {
          "A starter-state player is unavailable and requires a replacement decision."
        },
        TRUE
      )
    } else if (player$availability_status[[1]] == "UNKNOWN") {
      existing_codes <- vapply(findings, `[[`, character(1), "code")
      if (!"STARTER_AVAILABILITY_UNKNOWN" %in% existing_codes) {
        add_finding("STARTER_AVAILABILITY_UNKNOWN", "REVIEW",
          "An approved starter has unknown availability.", FALSE,
          missing_fields = "availability_status")
      }
    }
  }

  candidates <- prepared[!prepared$player_id %in% starter_ids, , drop = FALSE]
  candidate_components <- lapply(seq_len(nrow(candidates)), function(i) {
    v2_rotation_rank_components(candidates[i, , drop = FALSE], FALSE)
  })
  candidate_priority <- vapply(candidate_components, `[[`, numeric(1), "minute_priority")
  rookie <- vapply(seq_len(nrow(candidates)), function(i) {
    isTRUE(as.logical(candidates$is_preseason_rookie[[i]]))
  }, logical(1))
  unavailable <- candidates$availability_status == "OUT"
  rookie_blocked <- isTRUE(policy$preseason_rookie_gate_active) & rookie

  for (i in which(unavailable | rookie_blocked)) {
    type <- if (unavailable[[i]]) "UNAVAILABLE" else "ROOKIE_GATE"
    code <- if (unavailable[[i]]) "PLAYER_UNAVAILABLE" else "PRESEASON_ROOKIE_GATE"
    text <- if (unavailable[[i]]) {
      "Excluded because explicit availability marks the player unavailable."
    } else {
      "Excluded by the preserved preseason rookie eligibility gate."
    }
    excluded <- rbind(excluded, v2_rotation_excluded_row(
      candidates$player_id[[i]], candidates$player_name[[i]], type, code, text
    ))
  }

  eligible <- which(!unavailable & !rookie_blocked)
  ordering <- order(
    candidates$depth_order[eligible],
    -candidate_priority[eligible],
    candidates$player_name[eligible],
    na.last = TRUE,
    method = "radix"
  )
  ranked <- eligible[ordering]
  needed <- max(0L, requested - nrow(members))
  valid_role_eligibility <- Filter(function(x) {
    is.list(x) && identical(x$contract_type, "tbi-v2-role-eligibility") &&
      length(x$player_id) == 1L && !is.na(x$player_id)
  }, role_eligibility)
  if (any(vapply(
    valid_role_eligibility,
    function(x) !x$player_id %in% prepared$player_id,
    logical(1)
  ))) {
    add_finding("ROLE_ELIGIBILITY_PLAYER_NOT_ON_ROSTER", "FAIL",
      "Role eligibility references a player who is not on the input roster.", TRUE)
  }
  selected_index <- integer()
  for (role in policy$role_types) {
    eligible_ids <- vapply(
      Filter(function(x) identical(x$role, role) &&
               identical(x$eligibility, "ELIGIBLE"), valid_role_eligibility),
      `[[`,
      integer(1),
      "player_id"
    )
    ranked_role <- ranked[candidates$player_id[ranked] %in% eligible_ids]
    if (length(ranked_role)) selected_index <- unique(c(selected_index, ranked_role[[1]]))
  }
  selected_index <- head(selected_index, needed)
  selected_index <- c(
    selected_index,
    head(setdiff(ranked, selected_index), needed - length(selected_index))
  )

  for (i in selected_index) {
    members <- rbind(members, v2_rotation_member_row(candidates[i, , drop = FALSE], FALSE))
  }
  outside <- setdiff(eligible, selected_index)
  for (i in outside) {
    missing_rank <- !is.finite(candidate_priority[[i]])
    excluded <- rbind(excluded, v2_rotation_excluded_row(
      candidates$player_id[[i]], candidates$player_name[[i]],
      if (missing_rank) "INSUFFICIENT_EVIDENCE" else "OUTSIDE_ROTATION",
      if (missing_rank) "RANKING_EVIDENCE_INCOMPLETE" else "LOWER_DETERMINISTIC_RANK",
      if (missing_rank) {
        "Excluded at the deterministic rotation boundary with incomplete ranking evidence."
      } else {
        "Excluded because the player ranked below the requested rotation boundary."
      }
    ))
  }

  if (nrow(members) != requested) {
    add_finding("ROTATION_SIZE_UNSATISFIABLE", "FAIL",
      "The requested rotation size cannot be satisfied by legitimate eligible players.", TRUE)
  }

  selected_unknown <- which(members$availability_status == "UNKNOWN")
  if (length(selected_unknown)) {
    add_finding("SELECTED_AVAILABILITY_UNKNOWN", "REVIEW",
      "One or more selected players have unknown availability.", FALSE,
      missing_fields = "availability_status")
  }
  selected_priority <- vapply(
    members$rank_components,
    function(x) x$minute_priority,
    numeric(1)
  )
  selected_incomplete <- which(!members$is_starter & !is.finite(selected_priority))
  if (length(selected_incomplete)) {
    add_finding("SELECTED_RANKING_EVIDENCE_INCOMPLETE", "REVIEW",
      "One or more selected bench players have incomplete ranking evidence.", FALSE,
      missing_fields = "ranking_evidence")
  }
  selected_rookie_unknown <- which(
    !members$is_starter &
      is.na(as.logical(prepared$is_preseason_rookie[match(members$player_id, prepared$player_id)]))
  )
  if (length(selected_rookie_unknown)) {
    add_finding("SELECTED_ROOKIE_ELIGIBILITY_UNKNOWN", "REVIEW",
      "One or more selected bench players have unknown preseason rookie eligibility.", FALSE,
      missing_fields = "is_preseason_rookie")
  }

  role_by_type <- lapply(policy$role_types, function(role) {
    Filter(function(x) identical(x$role, role), valid_role_eligibility)
  })
  names(role_by_type) <- policy$role_types
  structural_invalid <- any(vapply(findings, function(x) isTRUE(x$is_blocking), logical(1)))
  role_blocked <- FALSE
  for (role in policy$role_types) {
    contracts <- role_by_type[[role]]
    bench_ids <- members$player_id[!members$is_starter]
    selected_contracts <- Filter(function(x) x$player_id %in% bench_ids, contracts)
    eligible_present <- any(vapply(
      selected_contracts, function(x) identical(x$eligibility, "ELIGIBLE"), logical(1)
    ))
    unknown_present <- !length(selected_contracts) || any(vapply(
      selected_contracts, function(x) identical(x$eligibility, "UNKNOWN"), logical(1)
    ))
    verified_none <- length(selected_contracts) && !unknown_present && !eligible_present
    if (verified_none) role_blocked <- TRUE
  }

  can_designate_sixth <- nrow(members) == requested && nrow(members) >= 6L &&
    !structural_invalid && !role_blocked
  bench_rows <- which(!members$is_starter)
  if (length(bench_rows)) {
    members$bench_order[bench_rows] <- seq_along(bench_rows)
  }
  if (can_designate_sixth) {
    sixth <- bench_rows[[1]]
    members$rotation_role[[sixth]] <- "SIXTH_MAN"
    members$reason_codes[[sixth]] <- unique(c(
      members$reason_codes[[sixth]], "HIGHEST_RANKED_SELECTED_BENCH"
    ))
    members$explanation[[sixth]] <- paste(
      members$explanation[[sixth]],
      "Designated sixth man as the highest-ranked selected bench player."
    )
  }

  new_v2_rotation_state(
    team_id = starter_state$team_id,
    season = starter_state$season,
    starter_state_id = starter_state$starter_state_id,
    requested_rotation_size = requested,
    members = members,
    roster_signature = v2_input_signature(prepared),
    policy_signature = v2_input_signature(policy),
    role_eligibility = Filter(
      function(x) is.list(x) && length(x$player_id) == 1L &&
        !is.na(x$player_id) && x$player_id %in% members$player_id,
      role_eligibility
    ),
    scenario_id = starter_state$scenario_id,
    excluded_players = excluded,
    explanation_log = list(
      new_v2_explanation(
        "V1_RANKING_ADAPTER",
        "Bench candidates use frozen V1 minute-priority evidence and ordering without allocating minutes."
      )
    ),
    additional_findings = findings,
    created_at = NA_character_,
    policy = policy
  )
}
