# ============================================================
# TBI V2 Phase 2B regulation staggering-plan contract
# ============================================================

v2_stagger_policy <- function() {
  list(
    policy_type = "tbi-v2-stagger-policy",
    policy_version = "1.0.0",
    periods = 4L,
    minutes_per_period = 12L,
    players_on_court = 5L,
    avoid_all_bench = TRUE,
    role_coverage_requires_verified_evidence = TRUE
  )
}


validate_v2_stagger_policy <- function(policy) {
  required <- c(
    "periods", "minutes_per_period", "players_on_court",
    "avoid_all_bench", "role_coverage_requires_verified_evidence"
  )
  if (!is.list(policy) || !all(required %in% names(policy))) {
    stop("policy is missing required V2 staggering fields.", call. = FALSE)
  }
  if (as.integer(policy$periods) != 4L ||
      as.integer(policy$minutes_per_period) != 12L ||
      as.integer(policy$players_on_court) != 5L ||
      !isTRUE(policy$avoid_all_bench) ||
      !isTRUE(policy$role_coverage_requires_verified_evidence)) {
    stop("policy changes fixed Phase 2B regulation or safety constraints.", call. = FALSE)
  }
  policy
}


v2_stagger_minute_rows <- function(minute_ledger, policy = v2_stagger_policy()) {
  policy <- validate_v2_stagger_policy(policy)
  if (!is.list(minute_ledger) ||
      !identical(minute_ledger$contract_type, "tbi-v2-minute-ledger") ||
      !is.data.frame(minute_ledger$ledger)) {
    stop("minute_ledger must be a Phase 2A minute ledger.", call. = FALSE)
  }
  if (isTRUE(minute_ledger$is_blocked) || identical(minute_ledger$status, "FAIL")) {
    stop("minute_ledger must be a non-blocked Phase 2A plan.", call. = FALSE)
  }
  rows <- minute_ledger$ledger
  required <- c(
    "player_id", "player_name", "is_starter", "assigned_minutes",
    "availability_status", "reason_codes"
  )
  missing <- setdiff(required, names(rows))
  if (length(missing)) stop("minute ledger is malformed; missing: ", paste(missing, collapse = ", "), call. = FALSE)
  rows$player_id <- suppressWarnings(as.integer(rows$player_id))
  rows$assigned_minutes <- suppressWarnings(as.integer(rows$assigned_minutes))
  if (!nrow(rows) %in% c(10L, 11L)) stop("staggering supports only 10- and 11-player rotations.", call. = FALSE)
  if (any(is.na(rows$player_id)) || anyDuplicated(rows$player_id)) stop("minute-ledger player IDs must be unique.", call. = FALSE)
  if (any(is.na(rows$assigned_minutes)) || any(rows$assigned_minutes < 0L)) stop("assigned minutes must be non-negative integers.", call. = FALSE)
  regulation_minutes <- as.integer(policy$periods * policy$minutes_per_period)
  regulation_exposure <- as.integer(regulation_minutes * policy$players_on_court)
  if (any(rows$assigned_minutes > regulation_minutes)) {
    stop("no player can exceed regulation minutes.", call. = FALSE)
  }
  if (sum(rows$assigned_minutes) != regulation_exposure ||
      as.integer(minute_ledger$total_assigned_minutes %||% regulation_exposure) !=
        regulation_exposure) {
    stop("minute ledger must reconcile to exactly 240 minutes.", call. = FALSE)
  }
  availability_status <- toupper(trimws(as.character(rows$availability_status)))
  if (any(!availability_status %in% c("AVAILABLE", "LIMITED", "OUT", "UNKNOWN"))) {
    stop("minute ledger contains an unsupported availability status.", call. = FALSE)
  }
  out_flag <- availability_status == "OUT"
  if (any(out_flag & rows$assigned_minutes > 0L)) stop("an OUT player cannot receive stagger-plan exposure.", call. = FALSE)
  if ("maximum_minutes" %in% names(rows) && any(rows$assigned_minutes > suppressWarnings(as.integer(rows$maximum_minutes)), na.rm = TRUE)) {
    stop("assigned minutes exceed an encoded availability or policy ceiling.", call. = FALSE)
  }
  rows
}


v2_stagger_role_sets <- function(role_ledger, rotation_ids) {
  role_names <- c("PRIMARY_CREATOR", "BALL_HANDLER", "POSITION_C", "BACKUP_C")
  empty <- setNames(rep(list(integer()), length(role_names)), role_names)
  unknown <- setNames(rep(list(rotation_ids), length(role_names)), role_names)
  if (is.null(role_ledger)) return(list(eligible = empty, unknown = unknown, signature = NULL))
  if (!is.list(role_ledger) ||
      !identical(role_ledger$contract_type, "tbi-v2-role-eligibility-ledger") ||
      !is.list(role_ledger$records)) {
    stop("role_ledger must be a Phase 1D role-eligibility ledger.", call. = FALSE)
  }
  eligible <- empty
  unknown <- empty
  for (role in role_names) {
    records <- Filter(function(x) identical(as.character(x$role), role) && suppressWarnings(as.integer(x$player_id)) %in% rotation_ids, role_ledger$records)
    eligible[[role]] <- sort(unique(vapply(Filter(function(x) {
      identical(x$eligibility, "ELIGIBLE") && x$verification_status %in% c("VERIFIED", "DERIVED_VERIFIED")
    }, records), function(x) as.integer(x$player_id), integer(1))))
    unknown[[role]] <- sort(unique(vapply(Filter(function(x) identical(x$eligibility, "UNKNOWN"), records), function(x) as.integer(x$player_id), integer(1))))
  }
  list(eligible = eligible, unknown = unknown, signature = role_ledger$input_signature %||% v2_input_signature(role_ledger$records))
}


v2_stagger_select_lineups <- function(rows, role_sets,
                                      policy = v2_stagger_policy()) {
  remaining <- rows$assigned_minutes
  ids <- rows$player_id
  starters <- as.logical(rows$is_starter)
  creator_ids <- union(role_sets$eligible$PRIMARY_CREATOR, role_sets$eligible$BALL_HANDLER)
  handler_ids <- role_sets$eligible$BALL_HANDLER
  big_ids <- union(role_sets$eligible$POSITION_C, role_sets$eligible$BACKUP_C)
  total_ticks <- as.integer(policy$periods * policy$minutes_per_period)
  lineup_size <- as.integer(policy$players_on_court)
  lineups <- vector("list", total_ticks)

  for (tick in seq_len(total_ticks)) {
    future_slots <- total_ticks - tick
    selected <- which(remaining > future_slots)
    if (length(selected) > lineup_size) stop("minute ledger cannot be reconciled into legal five-player segments.", call. = FALSE)
    add_best <- function(candidates) {
      candidates <- setdiff(candidates[remaining[candidates] > 0L], selected)
      if (!length(candidates) || length(selected) >= lineup_size) return(invisible(NULL))
      ordering <- order(-remaining[candidates], !starters[candidates], ids[candidates], method = "radix")
      selected <<- c(selected, candidates[ordering[[1]]])
      invisible(NULL)
    }
    add_coverage <- function(player_ids) {
      candidates <- match(player_ids, ids, nomatch = 0L)
      candidates <- candidates[candidates > 0L]
      if (!length(candidates) || any(selected %in% candidates)) return(invisible(NULL))
      add_best(candidates)
    }
    add_best(which(starters))
    add_coverage(creator_ids)
    add_coverage(handler_ids)
    add_coverage(big_ids)
    while (length(selected) < lineup_size) {
      candidates <- setdiff(which(remaining > 0L), selected)
      if (!length(candidates)) stop("minute ledger cannot fill every regulation segment.", call. = FALSE)
      redundant_coverage <- vapply(candidates, function(candidate) {
        candidate_id <- ids[[candidate]]
        as.integer(candidate_id %in% creator_ids && any(ids[selected] %in% creator_ids)) +
          as.integer(candidate_id %in% handler_ids && any(ids[selected] %in% handler_ids)) +
          as.integer(candidate_id %in% big_ids && any(ids[selected] %in% big_ids)) +
          as.integer(starters[[candidate]] && any(starters[selected]))
      }, integer(1))
      ordering <- order(redundant_coverage, -remaining[candidates], !starters[candidates], ids[candidates], method = "radix")
      selected <- c(selected, candidates[ordering[[1]]])
    }
    selected <- sort(unique(selected))
    if (length(selected) != lineup_size) stop("stagger planner produced a duplicate lineup member.", call. = FALSE)
    remaining[selected] <- remaining[selected] - 1L
    if (any(remaining < 0L)) stop("stagger planner exceeded a player's assigned minutes.", call. = FALSE)
    lineups[[tick]] <- ids[selected]
  }
  if (any(remaining != 0L)) stop("minute ledger did not reconcile exactly into regulation segments.", call. = FALSE)
  lineups
}


v2_stagger_coverage <- function(lineup, eligible_ids, unknown_ids) {
  if (any(lineup %in% eligible_ids)) return("PASS")
  if (any(lineup %in% unknown_ids) || (!length(eligible_ids) && !length(unknown_ids))) return("REVIEW")
  "FAIL"
}


v2_stagger_position_state_index <- function(
    role_ledger,
    player_ids,
    positions = v2_role_policy()$canonical_positions) {
  ids <- unique(suppressWarnings(as.integer(player_ids)))
  ids <- ids[!is.na(ids)]
  positions <- as.character(positions)

  eligibility_state <- function(player_id, position) {
    record <- tryCatch(
      v2_role_record(role_ledger, player_id, paste0("POSITION_", position)),
      error = function(e) NULL
    )
    if (is.null(record)) return("UNKNOWN")
    eligibility <- toupper(trimws(as.character(record$eligibility %||% "UNKNOWN")))
    verification <- toupper(trimws(as.character(record$verification_status %||% "MISSING")))
    evidence_class <- toupper(trimws(as.character(record$evidence_class %||% "UNKNOWN")))
    is_verified <- verification %in% c("VERIFIED", "DERIVED_VERIFIED") &&
      evidence_class %in% c("AUTHORITATIVE_FACT", "DETERMINISTIC_DERIVATION")
    if (is_verified && identical(eligibility, "ELIGIBLE")) return("ELIGIBLE")
    if (is_verified && identical(eligibility, "NOT_ELIGIBLE")) return("NOT_ELIGIBLE")
    "UNKNOWN"
  }

  setNames(lapply(ids, function(id) {
    setNames(vapply(positions, function(position) {
      eligibility_state(id, position)
    }, character(1)), positions)
  }), as.character(ids))
}


v2_stagger_position_legality <- function(
    player_ids,
    role_ledger,
    positions = v2_role_policy()$canonical_positions,
    position_state_index = NULL) {
  ids <- suppressWarnings(as.integer(player_ids))
  positions <- as.character(positions)
  if (length(ids) != length(positions) ||
      any(is.na(ids)) || anyDuplicated(ids) ||
      !identical(positions, c("PG", "SG", "SF", "PF", "C"))) {
    return(list(
      status = "FAIL",
      assigned_positions = setNames(rep(NA_character_, length(ids)), as.character(ids)),
      reason_code = "SEGMENT_POSITION_ASSIGNMENT_IMPOSSIBLE"
    ))
  }

  if (is.null(position_state_index)) {
    position_state_index <- v2_stagger_position_state_index(
      role_ledger,
      ids,
      positions
    )
  }

  states <- setNames(lapply(ids, function(id) {
    player_states <- position_state_index[[as.character(id)]]
    setNames(vapply(positions, function(position) {
      value <- player_states[[position]] %||% "UNKNOWN"
      value <- toupper(trimws(as.character(value)))
      if (length(value) != 1L || is.na(value) ||
          !value %in% c("ELIGIBLE", "NOT_ELIGIBLE", "UNKNOWN")) {
        return("UNKNOWN")
      }
      value
    }, character(1)), positions)
  }), as.character(ids))
  verified_index <- lapply(states, function(player_states) {
    names(player_states)[player_states == "ELIGIBLE"]
  })
  verified_assignment <- v2_role_position_assignment(ids, verified_index, positions)
  if (!is.null(verified_assignment)) {
    return(list(
      status = "PASS",
      assigned_positions = verified_assignment,
      reason_code = "SEGMENT_POSITION_ASSIGNMENT_VERIFIED"
    ))
  }

  possible_index <- lapply(states, function(player_states) {
    names(player_states)[player_states != "NOT_ELIGIBLE"]
  })
  possible_assignment <- v2_role_position_assignment(ids, possible_index, positions)
  if (!is.null(possible_assignment)) {
    return(list(
      status = "REVIEW",
      assigned_positions = setNames(rep(NA_character_, length(ids)), as.character(ids)),
      reason_code = "SEGMENT_POSITION_ASSIGNMENT_UNKNOWN"
    ))
  }
  list(
    status = "FAIL",
    assigned_positions = setNames(rep(NA_character_, length(ids)), as.character(ids)),
    reason_code = "SEGMENT_POSITION_ASSIGNMENT_IMPOSSIBLE"
  )
}


v2_stagger_clock <- function(minutes_remaining) paste0(as.integer(minutes_remaining), ":00")


v2_stagger_segments <- function(lineups, rows, role_sets,
                                policy = v2_stagger_policy(),
                                role_ledger = NULL) {
  total_ticks <- as.integer(policy$periods * policy$minutes_per_period)
  period_minutes <- as.integer(policy$minutes_per_period)
  starts <- c(1L, which(vapply(seq.int(2L, total_ticks), function(i) {
    period_change <- ((i - 1L) %/% period_minutes) !=
      ((i - 2L) %/% period_minutes)
    period_change || !identical(lineups[[i]], lineups[[i - 1L]])
  }, logical(1))) + 1L)
  ends <- c(starts[-1L] - 1L, total_ticks)
  position_state_index <- v2_stagger_position_state_index(
    role_ledger,
    rows$player_id
  )
  position_cache <- new.env(parent = emptyenv())
  position_legality <- function(lineup) {
    key <- paste(as.integer(lineup), collapse = ",")
    if (exists(key, envir = position_cache, inherits = FALSE)) {
      return(get(key, envir = position_cache, inherits = FALSE))
    }
    value <- v2_stagger_position_legality(
      lineup,
      role_ledger,
      position_state_index = position_state_index
    )
    assign(key, value, envir = position_cache)
    value
  }
  records <- lapply(seq_along(starts), function(i) {
    start <- starts[[i]]
    end <- ends[[i]]
    period <- ((start - 1L) %/% period_minutes) + 1L
    period_start <- (period - 1L) * period_minutes
    duration <- end - start + 1L
    lineup <- lineups[[start]]
    creator <- v2_stagger_coverage(lineup, union(role_sets$eligible$PRIMARY_CREATOR, role_sets$eligible$BALL_HANDLER), union(role_sets$unknown$PRIMARY_CREATOR, role_sets$unknown$BALL_HANDLER))
    handler <- v2_stagger_coverage(lineup, role_sets$eligible$BALL_HANDLER, role_sets$unknown$BALL_HANDLER)
    big <- v2_stagger_coverage(lineup, union(role_sets$eligible$POSITION_C, role_sets$eligible$BACKUP_C), union(role_sets$unknown$POSITION_C, role_sets$unknown$BACKUP_C))
    position <- position_legality(lineup)
    starter_count <- sum(rows$is_starter[match(lineup, rows$player_id)])
    codes <- c(
      if (creator == "REVIEW") "CREATOR_COVERAGE_UNKNOWN" else if (creator == "FAIL") "CREATOR_COVERAGE_UNMET",
      if (handler == "REVIEW") "BALL_HANDLER_COVERAGE_UNKNOWN" else if (handler == "FAIL") "BALL_HANDLER_COVERAGE_UNMET",
      if (big == "REVIEW") "BIG_CENTER_COVERAGE_UNKNOWN" else if (big == "FAIL") "BIG_CENTER_COVERAGE_UNMET",
      position$reason_code,
      if (starter_count == 0L) "ALL_BENCH_LINEUP" else "STARTER_STAGGERED"
    )
    list(
      segment_id = sprintf("SEG-%03d", i),
      period = period,
      start_clock = v2_stagger_clock(period_minutes - (start - 1L - period_start)),
      end_clock = v2_stagger_clock(period_minutes - (end - period_start)),
      duration = duration,
      player_ids = lineup,
      lineup_id = v2_state_id("lineup", sort(lineup)),
      starter_count = as.integer(starter_count),
      bench_count = as.integer(policy$players_on_court - starter_count),
      creator_coverage_status = creator,
      ball_handler_coverage_status = handler,
      big_center_coverage_status = big,
      position_legality_status = position$status,
      assigned_positions = position$assigned_positions,
      validation = if (any(c(creator, handler, big, position$status) == "FAIL")) "FAIL" else if (any(c(creator, handler, big, position$status) == "REVIEW")) "REVIEW" else "PASS",
      reason_codes = unique(codes)
    )
  })
  frame <- data.frame(
    segment_id = vapply(records, `[[`, character(1), "segment_id"),
    period = vapply(records, `[[`, integer(1), "period"),
    start_clock = vapply(records, `[[`, character(1), "start_clock"),
    end_clock = vapply(records, `[[`, character(1), "end_clock"),
    duration = vapply(records, `[[`, integer(1), "duration"),
    lineup_id = vapply(records, `[[`, character(1), "lineup_id"),
    starter_count = vapply(records, `[[`, integer(1), "starter_count"),
    bench_count = vapply(records, `[[`, integer(1), "bench_count"),
    creator_coverage_status = vapply(records, `[[`, character(1), "creator_coverage_status"),
    ball_handler_coverage_status = vapply(records, `[[`, character(1), "ball_handler_coverage_status"),
    big_center_coverage_status = vapply(records, `[[`, character(1), "big_center_coverage_status"),
    position_legality_status = vapply(records, `[[`, character(1), "position_legality_status"),
    validation = vapply(records, `[[`, character(1), "validation"),
    stringsAsFactors = FALSE
  )
  frame$player_ids <- I(lapply(records, `[[`, "player_ids"))
  frame$assigned_positions <- I(lapply(records, `[[`, "assigned_positions"))
  frame$reason_codes <- I(lapply(records, `[[`, "reason_codes"))
  frame
}


v2_stagger_events <- function(lineups, policy = v2_stagger_policy()) {
  events <- list()
  event_number <- 0L
  total_ticks <- as.integer(policy$periods * policy$minutes_per_period)
  period_minutes <- as.integer(policy$minutes_per_period)
  for (tick in seq.int(2L, total_ticks)) {
    previous <- lineups[[tick - 1L]]
    next_lineup <- lineups[[tick]]
    outgoing <- sort(setdiff(previous, next_lineup))
    incoming <- sort(setdiff(next_lineup, previous))
    if (!length(outgoing)) next
    current <- previous
    period <- ((tick - 1L) %/% period_minutes) + 1L
    period_start <- (period - 1L) * period_minutes
    clock <- v2_stagger_clock(period_minutes - (tick - 1L - period_start))
    for (i in seq_along(outgoing)) {
      current[current == outgoing[[i]]] <- incoming[[i]]
      current <- sort(current)
      event_number <- event_number + 1L
      events[[event_number]] <- list(
        event_id = sprintf("SUB-%03d", event_number),
        period = period,
        clock = clock,
        player_out = outgoing[[i]],
        player_in = incoming[[i]],
        resulting_five = current,
        reason = "MINUTE_RECONCILIATION",
        source = "V2_STAGGER_POLICY",
        explanation = "Deterministic substitution reconciles the approved Phase 2A minute ledger."
      )
    }
  }
  if (!length(events)) {
    frame <- data.frame(event_id = character(), period = integer(), clock = character(), player_out = integer(), player_in = integer(), reason = character(), source = character(), explanation = character(), stringsAsFactors = FALSE)
    frame$resulting_five <- I(list())
    return(frame)
  }
  frame <- data.frame(
    event_id = vapply(events, `[[`, character(1), "event_id"),
    period = vapply(events, `[[`, integer(1), "period"),
    clock = vapply(events, `[[`, character(1), "clock"),
    player_out = vapply(events, `[[`, integer(1), "player_out"),
    player_in = vapply(events, `[[`, integer(1), "player_in"),
    reason = vapply(events, `[[`, character(1), "reason"),
    source = vapply(events, `[[`, character(1), "source"),
    explanation = vapply(events, `[[`, character(1), "explanation"),
    stringsAsFactors = FALSE
  )
  frame$resulting_five <- I(lapply(events, `[[`, "resulting_five"))
  frame
}


build_v2_stagger_plan <- function(minute_ledger,
                                  role_ledger = NULL,
                                  policy = v2_stagger_policy()) {
  policy <- validate_v2_stagger_policy(policy)
  rows <- v2_stagger_minute_rows(minute_ledger, policy)
  role_sets <- v2_stagger_role_sets(role_ledger, rows$player_id)
  lineups <- v2_stagger_select_lineups(rows, role_sets, policy)
  segments <- v2_stagger_segments(
    lineups,
    rows,
    role_sets,
    policy = policy,
    role_ledger = role_ledger
  )
  events <- v2_stagger_events(lineups, policy)

  findings <- list()
  add_finding <- function(...) findings[[length(findings) + 1L]] <<- new_v2_validation_finding(...)
  role_status <- unlist(segments[c(
    "creator_coverage_status", "ball_handler_coverage_status",
    "big_center_coverage_status"
  )], use.names = FALSE)
  if (any(role_status == "FAIL")) {
    add_finding("VERIFIED_ROLE_COVERAGE_UNMET", "FAIL", "At least one segment lacks required verified creator, handler, or center coverage.")
  }
  unknown_codes <- intersect(unique(unlist(segments$reason_codes, use.names = FALSE)), c("CREATOR_COVERAGE_UNKNOWN", "BALL_HANDLER_COVERAGE_UNKNOWN", "BIG_CENTER_COVERAGE_UNKNOWN"))
  if (length(unknown_codes)) {
    add_finding("ROLE_COVERAGE_UNKNOWN", "REVIEW", "Segment role coverage remains unknown because governed evidence is incomplete.", missing_fields = "verified_segment_role_coverage")
  }
  if (any(segments$position_legality_status == "FAIL")) {
    add_finding(
      "SEGMENT_POSITION_ASSIGNMENT_IMPOSSIBLE",
      "FAIL",
      "At least one segment has no legal PG/SG/SF/PF/C assignment under known position eligibility.",
      is_blocking = TRUE,
      evidence_fields = "verified_position_eligibility"
    )
  } else if (any(segments$position_legality_status == "REVIEW")) {
    add_finding(
      "SEGMENT_POSITION_ASSIGNMENT_UNKNOWN",
      "REVIEW",
      "At least one segment lacks enough verified position evidence to prove a legal PG/SG/SF/PF/C assignment.",
      missing_fields = "verified_segment_position_eligibility"
    )
  }
  if (any(segments$starter_count == 0L)) {
    add_finding("ALL_BENCH_LINEUP", "REVIEW", "An all-bench segment could not be avoided while reconciling exact minutes.")
  }
  if (identical(minute_ledger$status, "REVIEW")) {
    add_finding("UPSTREAM_MINUTE_LEDGER_REVIEW", "REVIEW", "The minute ledger contains unresolved evidence review findings.")
  }
  validation <- aggregate_v2_validation(findings)
  identity <- list(
    model_version = "2.0.0-phase2b",
    minute_signature = minute_ledger$input_signature,
    role_signature = role_sets$signature,
    policy = policy,
    segments = segments,
    substitution_events = events
  )
  list(
    contract_type = "tbi-v2-stagger-plan",
    contract_version = "1.0.0",
    model_name = "tbi-staggering",
    model_version = "2.0.0-phase2b",
    regulation_minutes = 48L,
    total_player_minutes = as.integer(sum(segments$duration) * policy$players_on_court),
    segments = segments,
    substitution_events = events,
    status = validation$status,
    is_blocked = validation$is_blocked,
    validation = validation,
    input_signature = v2_input_signature(identity)
  )
}
