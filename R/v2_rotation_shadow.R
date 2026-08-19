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
                                    scenario_id = NULL) {
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
  slots <- data.frame(
    position = positions,
    player_id = ids,
    player_name = names,
    lock_status = "LOCKED",
    lock_source = "APPROVED",
    availability_status = availability,
    position_eligibility = "REVIEW",
    stringsAsFactors = FALSE
  )
  new_v2_starter_state(
    team_id = team,
    season = season,
    slots = slots,
    roster_signature = v2_input_signature(roster),
    scenario_id = scenario_id
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
    roster <- get_depth_chart_records(teams$team_name[[i]], season)
    enriched <- depth_chart_batched_bie_enrich_roster(roster, roster_season = season)
    evaluated <- evaluate_bie_players(enriched)
    result <- run_v2_rotation_shadow(
      "v2_shadow", teams$abbreviation[[i]], season, evaluated,
      v2_shadow_lineup_from_roster(roster),
      v1_reference = list(
        roster_signature = v2_input_signature(roster),
        approved_lineup = v2_shadow_lineup_from_roster(roster)
      )
    )
    rerun <- run_v2_rotation_shadow(
      "v2_shadow", teams$abbreviation[[i]], season, evaluated,
      v2_shadow_lineup_from_roster(roster),
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
      deterministic = identical(result$starter_state, rerun$starter_state) &&
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
                                   rotation_builder = build_v2_rotation) {
  route <- tbi_rotation_route(rotation_model)
  base <- list(
    route = route,
    team = team,
    season = season,
    scenario_signature = v2_input_signature(scenario),
    v1_reference = v1_reference,
    starter_state = NULL,
    rotation_10 = NULL,
    rotation_11 = NULL,
    validation_findings = list(),
    execution_status = "DISABLED",
    execution_timing = list(total_seconds = 0, size_10_seconds = 0, size_11_seconds = 0),
    error = NULL
  )
  if (!identical(route$model, "v2_shadow")) return(base)

  started <- proc.time()[["elapsed"]]
  tryCatch({
    prepared <- v2_shadow_roster(roster)
    base$starter_state <- v2_shadow_starter_state(
      team, season, prepared, approved_lineup,
      scenario_id = base$scenario_signature
    )
    roles <- role_eligibility %||% v2_shadow_unknown_role_eligibility(prepared)
    base$input_signature <- v2_input_signature(list(
      roster = prepared,
      approved_lineup = approved_lineup,
      scenario = scenario,
      role_eligibility = roles
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
