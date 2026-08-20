# ============================================================
# TBI V2 Phase 1C shadow routing and diagnostics
# ============================================================

tbi_rotation_route <- function(value = Sys.getenv("TBI_ROTATION_MODEL", unset = "v1")) {
  raw <- tolower(trimws(as.character(value %||% "")))
  if (length(raw) != 1L || is.na(raw) || !nzchar(raw)) raw <- "v1"
  if (raw %in% c("v1", "v2_shadow")) {
    return(list(model = raw, status = "PASS", diagnostic = NULL))
  }
  list(
    model = "v1",
    status = "REVIEW",
    diagnostic = paste0("Unsupported TBI_ROTATION_MODEL '", raw, "'; defaulted to v1.")
  )
}


tbi_rotation_model <- function(value = Sys.getenv("TBI_ROTATION_MODEL", unset = "v1")) {
  tbi_rotation_route(value)$model
}


v2_shadow_roster <- function(roster) {
  if (!is.data.frame(roster)) stop("roster must be a data frame.", call. = FALSE)
  result <- roster
  aliases <- list(
    player_name = c("player_name", "name"),
    position = c("position", "primary_position"),
    depth_order = c("depth_order", "depth_rank"),
    availability_status = c("availability_status", "availability")
  )
  for (target in names(aliases)) {
    if (!target %in% names(result)) {
      source <- aliases[[target]][aliases[[target]] %in% names(result)]
      result[[target]] <- if (length(source)) result[[source[[1]]]] else NA
    }
  }
  defaults <- list(
    bie_rating = NA_real_, projected_bie_rating = NA_real_, impact_score = NA_real_,
    is_preseason_rookie = NA, tbi_performance_available = NA
  )
  for (column in names(defaults)) {
    if (!column %in% names(result)) result[[column]] <- defaults[[column]]
  }
  result
}


v2_shadow_starter_state <- function(team, season, roster, approved_lineup,
                                    scenario_id = NULL,
                                    role_ledger = NULL) {
  roster <- v2_shadow_roster(roster)
  positions <- c("PG", "SG", "SF", "PF", "C")
  ids <- suppressWarnings(as.integer(approved_lineup[positions]))
  matched <- match(ids, suppressWarnings(as.integer(roster$player_id)))
  names <- rep(NA_character_, 5L)
  availability <- rep("UNKNOWN", 5L)
  found <- !is.na(matched)
  names[found] <- as.character(roster$player_name[matched[found]])
  availability[found] <- vapply(
    roster$availability_status[matched[found]], v2_rotation_availability, character(1)
  )
  position_eligibility <- rep("REVIEW", 5L)
  if (!is.null(role_ledger)) {
    position_eligibility <- vapply(seq_along(positions), function(i) {
      if (is.na(ids[[i]])) return("FAIL")
      contract <- v2_role_record(
        role_ledger, ids[[i]], paste0("POSITION_", positions[[i]])
      )
      if (identical(contract$eligibility, "ELIGIBLE")) return("PASS")
      if (identical(contract$eligibility, "NOT_ELIGIBLE")) return("FAIL")
      "REVIEW"
    }, character(1))
  }
  lock_findings <- list()
  if ("approved_lock_conflict" %in% names(roster) &&
      any(as.logical(roster$approved_lock_conflict), na.rm = TRUE)) {
    lock_findings <- list(new_v2_validation_finding(
      code = "APPROVED_STARTER_LOCK_CONFLICT",
      status = "FAIL",
      message = "Multiple approved starter locks compete for one position; no lock was silently discarded.",
      is_blocking = TRUE,
      evidence_fields = c("is_starter", "has_override", "position")
    ))
  }
  slots <- data.frame(
    position = positions,
    player_id = ids,
    player_name = names,
    lock_status = "LOCKED",
    lock_source = "APPROVED",
    availability_status = availability,
    position_eligibility = position_eligibility,
    stringsAsFactors = FALSE
  )
  new_v2_starter_state(
    team_id = team,
    season = season,
    slots = slots,
    roster_signature = v2_input_signature(roster),
    scenario_id = scenario_id,
    additional_findings = lock_findings
  )
}


v2_shadow_unknown_role_eligibility <- function(roster) {
  ids <- suppressWarnings(as.integer(roster$player_id))
  ids <- unique(ids[!is.na(ids)])
  unlist(lapply(ids, function(id) {
    list(
      new_v2_role_eligibility(id, "BACKUP_PG", missing_fields = "backup_pg_eligibility"),
      new_v2_role_eligibility(id, "BACKUP_C", missing_fields = "backup_c_eligibility")
    )
  }), recursive = FALSE)
}


v2_shadow_lineup_from_roster <- function(roster) {
  positions <- c("PG", "SG", "SF", "PF", "C")
  result <- stats::setNames(rep(NA_integer_, 5L), positions)
  if (!is.data.frame(roster) || !nrow(roster) ||
      !all(c("player_id", "position", "is_starter") %in% names(roster))) return(result)
  starter <- suppressWarnings(as.integer(roster$is_starter)) == 1L
  for (position in positions) {
    ids <- suppressWarnings(as.integer(roster$player_id[starter & roster$position == position]))
    ids <- ids[!is.na(ids)]
    if (length(ids)) result[[position]] <- ids[[1]]
  }
  result
}


v2_shadow_validate_30_teams <- function(season = NULL) {
  season <- season %||% phase15_latest_depth_season()
  teams <- phase15_active_teams()
  if (nrow(teams) != 30L) stop("Expected exactly 30 active NBA teams.", call. = FALSE)
  rows <- lapply(seq_len(nrow(teams)), function(i) {
    started <- proc.time()[["elapsed"]]
    roster <- v2_role_authoritative_roster_snapshot(teams$team_name[[i]], season)
    enriched <- depth_chart_batched_bie_enrich_roster(roster, roster_season = season)
    evaluated <- evaluate_bie_players(enriched)
    approved_lineup <- v2_shadow_lineup_from_roster(roster)
    result <- run_v2_rotation_shadow(
      "v2_shadow", teams$abbreviation[[i]], season, evaluated,
      approved_lineup,
      v1_reference = list(
        roster_signature = v2_input_signature(roster),
        approved_lineup = approved_lineup
      )
    )
    rerun <- run_v2_rotation_shadow(
      "v2_shadow", teams$abbreviation[[i]], season, evaluated,
      approved_lineup,
      v1_reference = result$v1_reference
    )
    data.frame(
      team = teams$abbreviation[[i]],
      season = season,
      execution_status = result$execution_status,
      starter_status = result$starter_state$status %||% NA_character_,
      rotation_10_status = result$rotation_10$status %||% NA_character_,
      rotation_11_status = result$rotation_11$status %||% NA_character_,
      rotation_10_size = result$rotation_10$actual_rotation_size %||% NA_integer_,
      rotation_11_size = result$rotation_11$actual_rotation_size %||% NA_integer_,
      verified_position_eligibility = result$role_diagnostics$verified_position_eligibility %||% NA_integer_,
      unknown_position_eligibility = result$role_diagnostics$unknown_position_eligibility %||% NA_integer_,
      verified_backup_pg_count = result$role_diagnostics$verified_backup_pg_candidates %||% NA_integer_,
      verified_backup_c_count = result$role_diagnostics$verified_backup_c_candidates %||% NA_integer_,
      verified_primary_creator_count = result$role_diagnostics$verified_primary_creator_candidates %||% NA_integer_,
      verified_secondary_creator_count = result$role_diagnostics$verified_secondary_creator_candidates %||% NA_integer_,
      verified_ball_handler_count = result$role_diagnostics$verified_ball_handler_candidates %||% NA_integer_,
      verified_rim_protector_count = result$role_diagnostics$verified_rim_protector_candidates %||% NA_integer_,
      verified_availability_count = result$availability_diagnostics$verified_count %||% NA_integer_,
      unknown_availability_count = result$availability_diagnostics$unknown_count %||% NA_integer_,
      unknown_role_count = result$role_diagnostics$unknown_role_records %||% NA_integer_,
      role_coverage_status = result$role_diagnostics$team_role_coverage_status %||% NA_character_,
      availability_coverage_status = result$availability_diagnostics$coverage_status %||% NA_character_,
      evidence_ledger_seconds = result$execution_timing$evidence_ledger_seconds %||% NA_real_,
      shadow_seconds = result$execution_timing$total_seconds %||% NA_real_,
      review_reasons = paste(unique(vapply(
        Filter(function(x) identical(x$status, "REVIEW"), result$validation_findings),
        `[[`, character(1), "code"
      )), collapse = ";"),
      fail_reasons = paste(unique(vapply(
        Filter(function(x) identical(x$status, "FAIL"), result$validation_findings),
        `[[`, character(1), "code"
      )), collapse = ";"),
      deterministic = identical(result$starter_state, rerun$starter_state) &&
        identical(result$availability_ledger, rerun$availability_ledger) &&
        identical(result$role_ledger, rerun$role_ledger) &&
        identical(result$rotation_10, rerun$rotation_10) &&
        identical(result$rotation_11, rerun$rotation_11),
      input_signature = result$input_signature %||% NA_character_,
      elapsed_seconds = proc.time()[["elapsed"]] - started,
      error = result$error$message %||% "",
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}


run_v2_rotation_shadow <- function(rotation_model,
                                   team,
                                   season,
                                   roster,
                                   approved_lineup,
                                   scenario = NULL,
                                   v1_reference = NULL,
                                   role_eligibility = NULL,
                                   manual_role_evidence = NULL,
                                   manual_availability_evidence = NULL,
                                   availability_as_of_date = NULL,
                                   rotation_builder = build_v2_rotation) {
  route <- tbi_rotation_route(rotation_model)
  base <- list(
    route = route,
    team = team,
    season = season,
    scenario_signature = v2_input_signature(scenario),
    v1_reference = v1_reference,
    starter_state = NULL,
    availability_ledger = NULL,
    availability_diagnostics = NULL,
    role_ledger = NULL,
    role_diagnostics = NULL,
    evidence_diagnostics = NULL,
    rotation_10 = NULL,
    rotation_11 = NULL,
    validation_findings = list(),
    execution_status = "DISABLED",
    execution_timing = list(
      total_seconds = 0, evidence_ledger_seconds = 0,
      size_10_seconds = 0, size_11_seconds = 0
    ),
    error = NULL
  )
  if (!identical(route$model, "v2_shadow")) return(base)

  started <- proc.time()[["elapsed"]]
  tryCatch({
    prepared <- v2_shadow_roster(roster)
    if (!is.null(role_eligibility) && !is.null(manual_role_evidence)) {
      stop("role_eligibility and manual_role_evidence cannot be supplied together.", call. = FALSE)
    }
    if (!is.null(role_eligibility) && any(vapply(role_eligibility, function(contract) {
      is.list(contract) && !is.null(contract$eligibility) &&
        identical(contract$contract_version, "1.0.0") &&
        contract$eligibility %in% c("ELIGIBLE", "NOT_ELIGIBLE")
    }, logical(1)))) {
      stop(
        "Known shadow role evidence must use manual_role_evidence so precedence and provenance are validated.",
        call. = FALSE
      )
    }
    roster_team <- if ("team_id" %in% names(prepared)) {
      values <- unique(as.character(prepared$team_id))
      values <- values[!is.na(values) & nzchar(trimws(values))]
      if (length(values) != 1L) stop("roster must contain exactly one team context.", call. = FALSE)
      values[[1]]
    } else {
      as.character(team)
    }
    roster_season <- if ("season" %in% names(prepared)) {
      values <- unique(as.character(prepared$season))
      values <- values[!is.na(values) & nzchar(trimws(values))]
      if (length(values) != 1L) stop("roster must contain exactly one season context.", call. = FALSE)
      values[[1]]
    } else {
      as.character(season)
    }
    roster_team_label <- if ("team_abbreviation" %in% names(prepared)) {
      labels <- unique(as.character(prepared$team_abbreviation))
      labels <- labels[!is.na(labels) & nzchar(trimws(labels))]
      if (length(labels) != 1L) stop("roster must contain exactly one team label.", call. = FALSE)
      labels[[1]]
    } else {
      roster_team
    }
    if (!identical(as.character(roster_team_label), as.character(team))) {
      stop("roster team does not match the requested shadow team.", call. = FALSE)
    }
    if (!identical(as.character(roster_season), as.character(season))) {
      stop("roster season does not match the requested shadow season.", call. = FALSE)
    }
    evidence_started <- proc.time()[["elapsed"]]
    base$availability_ledger <- build_v2_availability_evidence_ledger(
      prepared, roster_team, roster_season,
      manual_evidence = manual_availability_evidence,
      as_of_date = availability_as_of_date
    )
    base$availability_diagnostics <- summarize_v2_availability_completeness(
      base$availability_ledger
    )
    prepared <- v2_rotation_roster(
      prepared, v2_availability_for_rotation(base$availability_ledger)
    )
    base$role_ledger <- build_v2_role_eligibility_ledger(
      prepared, roster_team, roster_season,
      manual_evidence = manual_role_evidence
    )
    base$role_diagnostics <- summarize_v2_role_completeness(base$role_ledger, prepared)
    base$evidence_diagnostics <- summarize_v2_evidence_completeness(
      base$role_ledger, base$availability_ledger
    )
    base$execution_timing$evidence_ledger_seconds <-
      proc.time()[["elapsed"]] - evidence_started
    base$starter_state <- v2_shadow_starter_state(
      team, season, prepared, approved_lineup,
      scenario_id = base$scenario_signature,
      role_ledger = base$role_ledger
    )
    roles <- role_eligibility %||% v2_rotation_role_records(base$role_ledger)
    base$input_signature <- v2_input_signature(list(
      roster = prepared,
      approved_lineup = approved_lineup,
      scenario = scenario,
      availability_evidence_signature = base$availability_ledger$input_signature,
      role_evidence_signature = if (is.null(role_eligibility)) {
        base$role_ledger$input_signature
      } else {
        v2_input_signature(roles)
      }
    ))
    size_10_started <- proc.time()[["elapsed"]]
    base$rotation_10 <- rotation_builder(
      prepared, base$starter_state, 10L, roles,
      availability = NULL
    )
    base$execution_timing$size_10_seconds <- proc.time()[["elapsed"]] - size_10_started
    size_11_started <- proc.time()[["elapsed"]]
    base$rotation_11 <- rotation_builder(
      prepared, base$starter_state, 11L, roles,
      availability = NULL
    )
    base$execution_timing$size_11_seconds <- proc.time()[["elapsed"]] - size_11_started
    base$validation_findings <- c(
      base$starter_state$validation$findings,
      base$rotation_10$validation$findings,
      base$rotation_11$validation$findings
    )
    base$execution_status <- "COMPLETED"
    base
  }, error = function(error) {
    base$execution_status <- "ERROR"
    base$error <- list(
      class = class(error),
      message = conditionMessage(error),
      call = if (is.null(conditionCall(error))) NULL else deparse(conditionCall(error))
    )
    base
  }) -> result
  result$execution_timing$total_seconds <- proc.time()[["elapsed"]] - started
  result
}
