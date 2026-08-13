# ============================================================
# TBI NBA Basketball Operations Platform
# PHASE 3.1 — CURRENT ROSTER EVALUATION CONTEXT
#
# Permanent architecture:
#
#   CURRENT ROSTER MEMBERSHIP
#          +
#   BEST AVAILABLE EVALUATION EVIDENCE
#
# Historical performance rows remain attached to the team and
# season where the performance actually occurred.
#
# A player can therefore be:
#
#   current_team_id      = PHI
#   current_roster_season = 2026-27
#   performance_team_id  = BOS
#   performance_season   = 2025-26
#
# Rookies with no NBA performance remain on the current roster
# with evaluation_source = ROOKIE / PROJECTION PENDING.
# ============================================================


tbi_eval_context_normalize_team <- function(x) {
  toupper(
    gsub(
      "[^A-Z0-9]",
      "",
      trimws(as.character(x))
    )
  )
}


tbi_eval_context_resolve_team <- function(
    team_name,
    con = NULL) {
  
  if (
    exists(
      "bie_phase3_resolve_team",
      mode = "function",
      inherits = TRUE
    )
  ) {
    return(
      bie_phase3_resolve_team(
        team_name = team_name,
        con = con
      )
    )
  }
  
  owns_connection <- is.null(con)
  
  if (owns_connection) {
    con <- connect_db(read_only = TRUE)
  }
  
  if (owns_connection) {
    on.exit(disconnect_db(con), add = TRUE)
  }
  
  teams <- DBI::dbGetQuery(
    con,
    "
    SELECT team_id, team_name, abbreviation
    FROM teams
    ORDER BY team_name
    "
  )
  
  query <- tbi_eval_context_normalize_team(team_name)
  
  aliases <- c(
    "ATLANTAHAWKS" = "ATL",
    "BOSTONCELTICS" = "BOS",
    "BROOKLYNNETS" = "BKN",
    "CHARLOTTEHORNETS" = "CHA",
    "CHICAGOBULLS" = "CHI",
    "CLEVELANDCAVALIERS" = "CLE",
    "DALLASMAVERICKS" = "DAL",
    "DENVERNUGGETS" = "DEN",
    "DETROITPISTONS" = "DET",
    "GOLDENSTATEWARRIORS" = "GSW",
    "HOUSTONROCKETS" = "HOU",
    "INDIANAPACERS" = "IND",
    "LACLIPPERS" = "LAC",
    "LOSANGELESCLIPPERS" = "LAC",
    "LALAKERS" = "LAL",
    "LOSANGELESLAKERS" = "LAL",
    "MEMPHISGRIZZLIES" = "MEM",
    "MIAMIHEAT" = "MIA",
    "MILWAUKEEBUCKS" = "MIL",
    "MINNESOTATIMBERWOLVES" = "MIN",
    "NEWORLEANSPELICANS" = "NOP",
    "NEWYORKKNICKS" = "NYK",
    "OKLAHOMACITYTHUNDER" = "OKC",
    "ORLANDOMAGIC" = "ORL",
    "PHILADELPHIA76ERS" = "PHI",
    "PHOENIXSUNS" = "PHX",
    "PORTLANDTRAILBLAZERS" = "POR",
    "SACRAMENTOKINGS" = "SAC",
    "SANANTONIOSPURS" = "SAS",
    "TORONTORAPTORS" = "TOR",
    "UTAHJAZZ" = "UTA",
    "WASHINGTONWIZARDS" = "WAS"
  )
  
  if (query %in% names(aliases)) {
    query <- tbi_eval_context_normalize_team(
      aliases[[query]]
    )
  }
  
  hit <- which(
    tbi_eval_context_normalize_team(teams$team_name) == query |
      tbi_eval_context_normalize_team(teams$abbreviation) == query
  )
  
  if (!length(hit)) {
    stop(
      paste0(
        "Could not resolve team: ",
        team_name
      )
    )
  }
  
  row <- teams[
    hit[[1]],
    ,
    drop = FALSE
  ]
  
  list(
    team_id = as.integer(row$team_id[[1]]),
    team_name = as.character(row$team_name[[1]]),
    abbreviation = as.character(row$abbreviation[[1]])
  )
}


tbi_eval_context_roster_table <- function(con) {
  
  tables <- DBI::dbListTables(con)
  
  candidates <- c(
    "roster_history",
    "rosters",
    "team_rosters"
  )
  
  hit <- candidates[
    candidates %in% tables
  ]
  
  if (!length(hit)) {
    stop(
      "No roster table found. Expected roster_history, rosters, or team_rosters."
    )
  }
  
  hit[[1]]
}


tbi_eval_context_available_roster_seasons <- function(
    team_id,
    con) {
  
  roster_table <- tbi_eval_context_roster_table(con)
  fields <- DBI::dbListFields(con, roster_table)
  
  if (!"season" %in% fields) {
    return(character())
  }
  
  sql <- paste0(
    "
    SELECT DISTINCT season
    FROM ",
    as.character(
      DBI::dbQuoteIdentifier(
        con,
        roster_table
      )
    ),
    "
    WHERE team_id = ?
      AND season IS NOT NULL
      AND TRIM(season) <> ''
    ORDER BY season DESC
    "
  )
  
  DBI::dbGetQuery(
    con,
    sql,
    params = list(
      as.integer(team_id)
    )
  )$season
}


tbi_eval_context_resolve_roster_season <- function(
    team_id,
    requested_season = NULL,
    con) {
  
  roster_table <- tbi_eval_context_roster_table(con)
  fields <- DBI::dbListFields(con, roster_table)
  
  if (!"season" %in% fields) {
    return(
      list(
        season = requested_season,
        method = "ROSTER TABLE HAS NO SEASON FIELD"
      )
    )
  }
  
  available <- tbi_eval_context_available_roster_seasons(
    team_id = team_id,
    con = con
  )
  
  if (
    !is.null(requested_season) &&
    requested_season %in% available
  ) {
    return(
      list(
        season = requested_season,
        method = "REQUESTED"
      )
    )
  }
  
  if (length(available)) {
    return(
      list(
        season = available[[1]],
        method = "LATEST AVAILABLE"
      )
    )
  }
  
  list(
    season = requested_season,
    method = "NO SEASON ROWS"
  )
}


get_tbi_current_roster_context <- function(
    team_name,
    current_roster_season = "2026-27",
    con = NULL) {
  
  owns_connection <- is.null(con)
  
  if (owns_connection) {
    con <- connect_db(read_only = TRUE)
  }
  
  if (owns_connection) {
    on.exit(disconnect_db(con), add = TRUE)
  }
  
  resolved <- tbi_eval_context_resolve_team(
    team_name = team_name,
    con = con
  )
  
  roster_table <- tbi_eval_context_roster_table(con)
  fields <- DBI::dbListFields(con, roster_table)
  
  required <- c(
    "player_id",
    "team_id"
  )
  
  missing <- setdiff(required, fields)
  
  if (length(missing)) {
    stop(
      paste0(
        roster_table,
        " is missing: ",
        paste(missing, collapse = ", ")
      )
    )
  }
  
  roster_season <- tbi_eval_context_resolve_roster_season(
    team_id = resolved$team_id,
    requested_season = current_roster_season,
    con = con
  )
  
  season_clause <- ""
  params <- list(resolved$team_id)
  
  if (
    "season" %in% fields &&
    !is.null(roster_season$season)
  ) {
    season_clause <- " AND r.season = ?"
    params <- c(
      params,
      list(roster_season$season)
    )
  }
  
  roster_status <- if ("roster_status" %in% fields) {
    "r.roster_status"
  } else {
    "NULL AS roster_status"
  }
  
  two_way <- if ("two_way_flag" %in% fields) {
    "COALESCE(r.two_way_flag, 0) AS two_way_flag"
  } else {
    "0 AS two_way_flag"
  }
  
  sql <- paste0(
    "
    SELECT DISTINCT
      p.player_id,
      p.player_name,
      p.primary_position,
      p.player_age,
      p.height_inches,
      p.weight_lbs,

      r.team_id AS current_team_id,
      t.team_name AS current_team_name,
      t.abbreviation AS current_team_abbreviation,

      ",
    roster_status,
    ",
      ",
    two_way,
    "

    FROM ",
    as.character(
      DBI::dbQuoteIdentifier(
        con,
        roster_table
      )
    ),
    " r

    INNER JOIN players p
      ON p.player_id = r.player_id

    INNER JOIN teams t
      ON t.team_id = r.team_id

    WHERE r.team_id = ?
    ",
    season_clause,
    "

    ORDER BY p.player_name
    "
  )
  
  roster <- DBI::dbGetQuery(
    con,
    sql,
    params = params
  )
  
  roster$current_roster_season <-
    roster_season$season
  
  roster$current_roster_season_method <-
    roster_season$method
  
  roster
}


get_tbi_best_prior_nba_evidence <- function(
    player_ids,
    performance_season = "2025-26",
    con = NULL) {
  
  owns_connection <- is.null(con)
  
  if (owns_connection) {
    con <- connect_db(read_only = TRUE)
  }
  
  if (owns_connection) {
    on.exit(disconnect_db(con), add = TRUE)
  }
  
  player_ids <- unique(
    suppressWarnings(
      as.integer(player_ids)
    )
  )
  
  player_ids <- player_ids[
    is.finite(player_ids)
  ]
  
  if (!length(player_ids)) {
    return(data.frame())
  }
  
  placeholders <- paste(
    rep("?", length(player_ids)),
    collapse = ", "
  )
  
  tables <- DBI::dbListTables(con)
  
  advanced_join <- if (
    "player_season_advanced" %in% tables
  ) {
    "
    LEFT JOIN player_season_advanced adv
      ON adv.player_id = impact.player_id
      AND adv.team_id = impact.team_id
      AND adv.season = impact.season
    "
  } else {
    ""
  }
  
  advanced_fields <- if (
    "player_season_advanced" %in% tables
  ) {
    "
      adv.box_plus_minus,
      adv.offensive_box_plus_minus,
      adv.defensive_box_plus_minus,
      adv.value_over_replacement,
      adv.win_shares_per_48,
      adv.net_rating,
    "
  } else {
    "
      NULL AS box_plus_minus,
      NULL AS offensive_box_plus_minus,
      NULL AS defensive_box_plus_minus,
      NULL AS value_over_replacement,
      NULL AS win_shares_per_48,
      NULL AS net_rating,
    "
  }
  
  sql <- paste0(
    "
    WITH ranked AS (
      SELECT
        impact.player_id,
        impact.team_id AS performance_team_id,
        t.team_name AS performance_team_name,
        t.abbreviation AS performance_team_abbreviation,
        impact.season AS performance_season,

        impact.games_played,
        impact.minutes,
        impact.minutes_per_game,

        stats.points_per_game,
        stats.rebounds_per_game,
        stats.assists_per_game,
        stats.steals_per_game,
        stats.blocks_per_game,
        stats.turnovers_per_game,
        stats.field_goal_pct,
        stats.three_point_pct,
        stats.free_throw_pct,

        shoot.shooting_efficiency_score,
        shoot.spacing_score,
        shoot.shooting_gravity_score,
        shoot.rim_pressure_proxy_score,
        shoot.stabilized_three_point_pct,
        shoot.true_shooting_pct,
        shoot.effective_field_goal_pct,
        shoot.shooting_confidence,
        shoot.spacing_tier,

        play.creation_score,
        play.passing_control_score,
        play.secondary_creation_score,
        play.ball_security_score,
        play.creation_role,
        play.playmaking_confidence,

        dr.defense_proxy_score,
        dr.rebounding_score,
        dr.interior_impact_score,
        dr.disruption_score,
        dr.defensive_role,
        dr.rebounding_role,
        dr.defense_confidence,

        role.primary_role,
        role.secondary_role,
        role.tertiary_role,
        role.archetype,
        role.role_family,
        role.scoring_role_score,
        role.spacing_role_score,
        role.creation_role_score,
        role.connector_role_score,
        role.defense_role_score,
        role.rebounding_role_score,
        role.interior_role_score,
        role.offensive_role_score,
        role.defensive_role_score AS role_defensive_overall_score,
        role.two_way_role_score,
        role.role_confidence,

        impact.shooting_component,
        impact.creation_component,
        impact.defense_component,
        impact.rebounding_component,
        impact.role_component,
        impact.availability_component,
        impact.advanced_impact_component,
        impact.offensive_impact_score,
        impact.defensive_impact_score,
        impact.all_around_impact_score,
        impact.bie_performance_rating,
        impact.bie_performance_percentile,
        impact.impact_tier,
        impact.impact_confidence,
        impact.data_scope,

        ",
    advanced_fields,
    "

        ROW_NUMBER() OVER (
          PARTITION BY impact.player_id
          ORDER BY
            impact.minutes DESC,
            impact.games_played DESC,
            impact.team_id
        ) AS evidence_rank

      FROM player_season_impact impact

      INNER JOIN teams t
        ON t.team_id = impact.team_id

      LEFT JOIN player_season_stats stats
        ON stats.player_id = impact.player_id
        AND stats.team_id = impact.team_id
        AND stats.season = impact.season

      LEFT JOIN player_season_shooting shoot
        ON shoot.player_id = impact.player_id
        AND shoot.team_id = impact.team_id
        AND shoot.season = impact.season

      LEFT JOIN player_season_playmaking play
        ON play.player_id = impact.player_id
        AND play.team_id = impact.team_id
        AND play.season = impact.season

      LEFT JOIN player_season_defense_rebounding dr
        ON dr.player_id = impact.player_id
        AND dr.team_id = impact.team_id
        AND dr.season = impact.season

      LEFT JOIN player_season_roles role
        ON role.player_id = impact.player_id
        AND role.team_id = impact.team_id
        AND role.season = impact.season

      ",
    advanced_join,
    "

      WHERE impact.season = ?
        AND impact.player_id IN (",
    placeholders,
    ")
    )

    SELECT *
    FROM ranked
    WHERE evidence_rank = 1
    "
  )
  
  result <- DBI::dbGetQuery(
    con,
    sql,
    params = c(
      list(performance_season),
      as.list(player_ids)
    )
  )
  
  if ("evidence_rank" %in% names(result)) {
    result$evidence_rank <- NULL
  }
  
  result
}


get_tbi_current_roster_evaluation_context <- function(
    team_name,
    current_roster_season = "2026-27",
    performance_season = "2025-26",
    con = NULL) {
  
  owns_connection <- is.null(con)
  
  if (owns_connection) {
    con <- connect_db(read_only = TRUE)
  }
  
  if (owns_connection) {
    on.exit(disconnect_db(con), add = TRUE)
  }
  
  roster <- get_tbi_current_roster_context(
    team_name = team_name,
    current_roster_season = current_roster_season,
    con = con
  )
  
  if (!nrow(roster)) {
    return(roster)
  }
  
  evidence <- get_tbi_best_prior_nba_evidence(
    player_ids = roster$player_id,
    performance_season = performance_season,
    con = con
  )
  
  roster$.row_order <- seq_len(nrow(roster))
  
  if (nrow(evidence)) {
    output <- merge(
      roster,
      evidence,
      by = "player_id",
      all.x = TRUE,
      sort = FALSE
    )
  } else {
    output <- roster
  }
  
  output <- output[
    order(output$.row_order),
    ,
    drop = FALSE
  ]
  
  output$.row_order <- NULL
  
  if (!"performance_season" %in% names(output)) {
    output$performance_season <- NA_character_
  }
  
  if (!"performance_team_id" %in% names(output)) {
    output$performance_team_id <- NA_integer_
  }
  
  if (!"performance_team_name" %in% names(output)) {
    output$performance_team_name <- NA_character_
  }
  
  has_nba_evidence <-
    !is.na(output$performance_season) &
    nzchar(output$performance_season)
  
  output$evaluation_source <- ifelse(
    has_nba_evidence,
    "PRIOR-SEASON NBA",
    "ROOKIE / PROJECTION PENDING"
  )
  
  output$evaluation_status <- ifelse(
    has_nba_evidence,
    "NBA PERFORMANCE AVAILABLE",
    "NO PRIOR NBA PERFORMANCE"
  )
  
  output$changed_teams_since_evidence <- ifelse(
    has_nba_evidence &
      !is.na(output$performance_team_id),
    output$current_team_id != output$performance_team_id,
    FALSE
  )
  
  output$evaluation_label <- ifelse(
    has_nba_evidence,
    paste0(
      output$performance_season,
      " • ",
      output$performance_team_name
    ),
    "Rookie / projection pending"
  )
  
  # Compatibility aliases:
  # team_id now means CURRENT TEAM in all current-roster decision layers.
  output$team_id <- output$current_team_id
  output$team_name <- output$current_team_name
  output$abbreviation <- output$current_team_abbreviation
  output$season <- output$current_roster_season
  
  # Existing BIE decision functions expect these aliases.
  if ("bie_performance_rating" %in% names(output)) {
    output$bie_score <- output$bie_performance_rating
    output$bie_overall_score <- output$bie_performance_rating
  }
  
  if ("offensive_impact_score" %in% names(output)) {
    output$bie_offense_score <- output$offensive_impact_score
  }
  
  if ("defensive_impact_score" %in% names(output)) {
    output$bie_defense_score <- output$defensive_impact_score
  }
  
  if ("shooting_efficiency_score" %in% names(output)) {
    output$bie_efficiency_score <- output$shooting_efficiency_score
  }
  
  if ("spacing_score" %in% names(output)) {
    output$bie_spacing_score <- output$spacing_score
  }
  
  if ("creation_score" %in% names(output)) {
    output$bie_playmaking_score <- output$creation_score
    output$bie_creation_score <- output$creation_score
  }
  
  if ("rebounding_score" %in% names(output)) {
    output$bie_rebounding_score <- output$rebounding_score
  }
  
  if ("availability_component" %in% names(output)) {
    output$bie_availability_score <- output$availability_component
  }
  
  output$bie_score_source <- ifelse(
    has_nba_evidence,
    "PRIOR_SEASON_NBA_PERFORMANCE",
    "NO_NBA_PERFORMANCE"
  )
  
  if ("impact_confidence" %in% names(output)) {
    output$bie_confidence <- output$impact_confidence
  }
  
  if ("primary_role" %in% names(output)) {
    output$bie_primary_role <- output$primary_role
  }
  
  if ("archetype" %in% names(output)) {
    output$bie_archetype <- output$archetype
  }
  
  output
}


# ------------------------------------------------------------
# Optional current contract attachment
# ------------------------------------------------------------

attach_tbi_current_contract_context <- function(
    roster,
    current_roster_season = "2026-27",
    con = NULL) {
  
  if (
    is.null(roster) ||
    !is.data.frame(roster) ||
    !nrow(roster)
  ) {
    return(roster)
  }
  
  owns_connection <- is.null(con)
  
  if (owns_connection) {
    con <- connect_db(read_only = TRUE)
  }
  
  if (owns_connection) {
    on.exit(disconnect_db(con), add = TRUE)
  }
  
  tables <- DBI::dbListTables(con)
  
  if (!"contract_years" %in% tables) {
    roster$base_salary <- NA_real_
    roster$cap_hit <- NA_real_
    return(roster)
  }
  
  fields <- DBI::dbListFields(con, "contract_years")
  
  if (!all(c("player_id", "team_id", "season") %in% fields)) {
    roster$base_salary <- NA_real_
    roster$cap_hit <- NA_real_
    return(roster)
  }
  
  salary_field <- if ("base_salary" %in% fields) {
    "base_salary"
  } else {
    "NULL AS base_salary"
  }
  
  cap_field <- if ("cap_hit" %in% fields) {
    "cap_hit"
  } else {
    "NULL AS cap_hit"
  }
  
  contracts <- DBI::dbGetQuery(
    con,
    paste0(
      "
      SELECT
        player_id,
        team_id AS current_team_id,
        ",
      salary_field,
      ",
        ",
      cap_field,
      "
      FROM contract_years
      WHERE season = ?
      "
    ),
    params = list(current_roster_season)
  )
  
  if (!nrow(contracts)) {
    roster$base_salary <- NA_real_
    roster$cap_hit <- NA_real_
    return(roster)
  }
  
  merge(
    roster,
    contracts,
    by = c(
      "player_id",
      "current_team_id"
    ),
    all.x = TRUE,
    sort = FALSE
  )
}


# ------------------------------------------------------------
# Context health check
# ------------------------------------------------------------

tbi_evaluation_context_healthcheck <- function(
    team_name = "Philadelphia 76ers",
    current_roster_season = "2026-27",
    performance_season = "2025-26") {
  
  result <- tryCatch(
    get_tbi_current_roster_evaluation_context(
      team_name = team_name,
      current_roster_season = current_roster_season,
      performance_season = performance_season
    ),
    error = function(e) {
      structure(
        data.frame(),
        error = conditionMessage(e)
      )
    }
  )
  
  if (!nrow(result)) {
    return(
      list(
        status = "REVIEW",
        issue = attr(result, "error") %||%
          "No current-roster rows returned."
      )
    )
  }
  
  has_evidence <-
    !is.na(result$performance_season) &
    nzchar(result$performance_season)
  
  list(
    phase = "Phase 3.1",
    step = "Current Roster Evaluation Context",
    status = "READY",
    team = unique(result$current_team_name)[[1]],
    requested_current_roster_season = current_roster_season,
    actual_current_roster_season =
      unique(result$current_roster_season)[[1]],
    performance_season = performance_season,
    roster_players = nrow(result),
    veterans_with_nba_evidence = sum(has_evidence),
    rookies_or_no_prior_nba = sum(!has_evidence),
    players_changed_teams = sum(
      result$changed_teams_since_evidence,
      na.rm = TRUE
    ),
    rule =
      "CURRENT ROSTER TEAM IS SEPARATE FROM PERFORMANCE TEAM"
  )
}