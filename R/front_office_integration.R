# ============================================================
# TBI NBA Basketball Operations Platform
# PHASE 3 — STEP 10
# Full Decision-Engine Integration
#
# Purpose:
#   Feed the Phase-3 performance layer into the existing
#   Phase-2 front-office decision functions without replacing
#   the core Basketball Intelligence Engine yet.
#
# Integrates:
#   - Player Impact / BIE Performance Rating
#   - Player Role Classification
#   - Shooting + Spacing
#   - Playmaking + Creation
#   - Defense + Rebounding
#   - Lineup / Rotation Optimization
#
# Front-office outputs:
#   - Roster Decision Intelligence
#   - Roster Needs / Gap Analysis
#   - Acquisition Target Fit
#   - Trade Basketball Impact
#   - Extension Value
#   - Executive Front Office Intelligence
#
# Architecture:
#   Existing Phase-2 functions remain the decision authority.
#   This module enriches their input frames with real Phase-3
#   performance evidence before calling them.
# ============================================================


# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------

p3s10_num <- function(
    x,
    default = NA_real_) {
  
  value <- suppressWarnings(
    as.numeric(x)
  )
  
  if (
    !length(value) ||
    is.na(value[[1]]) ||
    !is.finite(value[[1]])
  ) {
    return(default)
  }
  
  value[[1]]
}


p3s10_text <- function(
    x,
    default = "") {
  
  value <- as.character(x)
  
  if (
    !length(value) ||
    is.na(value[[1]]) ||
    !nzchar(
      trimws(
        value[[1]]
      )
    )
  ) {
    return(default)
  }
  
  trimws(
    value[[1]]
  )
}


# ------------------------------------------------------------
# Required Phase-3 tables
# ------------------------------------------------------------

p3s10_required_tables <- function() {
  
  c(
    "player_season_stats",
    "player_season_shooting",
    "player_season_playmaking",
    "player_season_defense_rebounding",
    "player_season_roles",
    "player_season_impact"
  )
}


# ------------------------------------------------------------
# Required legacy BIE functions
# ------------------------------------------------------------

p3s10_required_bie_functions <- function() {
  
  c(
    "evaluate_bie_roster_decisions",
    "evaluate_bie_roster_needs",
    "evaluate_bie_acquisition_target",
    "evaluate_bie_trade_basketball_impact",
    "evaluate_bie_extension_value",
    "evaluate_bie_executive_front_office"
  )
}


# ------------------------------------------------------------
# Integration readiness
# ------------------------------------------------------------

phase3_step10_integration_readiness <- function() {
  
  con <- connect_db(
    read_only = TRUE
  )
  
  on.exit(
    disconnect_db(con),
    add = TRUE
  )
  
  tables <- DBI::dbListTables(
    con
  )
  
  required_tables <-
    p3s10_required_tables()
  
  required_functions <-
    p3s10_required_bie_functions()
  
  table_status <-
    setNames(
      required_tables %in%
        tables,
      required_tables
    )
  
  function_status <-
    setNames(
      vapply(
        required_functions,
        exists,
        logical(1),
        mode = "function",
        inherits = TRUE
      ),
      required_functions
    )
  
  list(
    status = if (
      all(table_status) &&
      all(function_status)
    ) {
      "READY"
    } else {
      "REVIEW"
    },
    tables =
      table_status,
    functions =
      function_status,
    missing_tables =
      names(
        table_status[
          !table_status
        ]
      ),
    missing_functions =
      names(
        function_status[
          !function_status
        ]
      )
  )
}


# ------------------------------------------------------------
# Pull the complete Phase-3 performance layer for a season
# ------------------------------------------------------------

get_phase3_decision_performance_layer <- function(
    season,
    team_id = NULL,
    player_id = NULL,
    con = NULL) {
  
  owns_connection <- is.null(con)
  
  if (owns_connection) {
    con <- connect_db(
      read_only = TRUE
    )
  }
  
  if (owns_connection) {
    on.exit(
      disconnect_db(con),
      add = TRUE
    )
  }
  
  tables <- DBI::dbListTables(
    con
  )
  
  missing <- setdiff(
    p3s10_required_tables(),
    tables
  )
  
  if (length(missing)) {
    stop(
      paste0(
        "Phase 3 integration tables are missing: ",
        paste(
          missing,
          collapse = ", "
        )
      )
    )
  }
  
  where <- c(
    "impact.season = ?"
  )
  
  params <- list(
    as.character(
      season
    )
  )
  
  if (!is.null(team_id)) {
    
    where <- c(
      where,
      "impact.team_id = ?"
    )
    
    params <- c(
      params,
      list(
        as.integer(
          team_id
        )
      )
    )
  }
  
  if (!is.null(player_id)) {
    
    where <- c(
      where,
      "impact.player_id = ?"
    )
    
    params <- c(
      params,
      list(
        as.integer(
          player_id
        )
      )
    )
  }
  
  where_sql <- paste(
    "WHERE",
    paste(
      where,
      collapse = " AND "
    )
  )
  
  sql <- paste0(
    "
    SELECT
      impact.player_id,
      impact.team_id,
      impact.season,

      impact.games_played,
      impact.minutes,
      impact.minutes_per_game,

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
      role.two_way_role_score,
      role.role_confidence,

      shoot.shooting_efficiency_score,
      shoot.spacing_score,
      shoot.shooting_gravity_score,
      shoot.rim_pressure_proxy_score,
      shoot.stabilized_three_point_pct,
      shoot.true_shooting_pct,
      shoot.effective_field_goal_pct,
      shoot.shooting_confidence,

      play.creation_score,
      play.passing_control_score,
      play.secondary_creation_score,
      play.ball_security_score,
      play.assist_turnover_ratio,
      play.playmaking_confidence,

      dr.defense_proxy_score,
      dr.rebounding_score,
      dr.interior_impact_score,
      dr.disruption_score,
      dr.defense_confidence

    FROM player_season_impact impact

    LEFT JOIN player_season_roles role
      ON role.player_id = impact.player_id
      AND role.team_id = impact.team_id
      AND role.season = impact.season

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

    ",
    where_sql,
    "
    "
  )
  
  DBI::dbGetQuery(
    con,
    sql,
    params = params
  )
}


# ------------------------------------------------------------
# Resolve team if Step-9 resolver exists
# ------------------------------------------------------------

p3s10_resolve_team <- function(
    team_name,
    con) {
  
  if (
    exists(
      "resolve_phase3_team",
      mode = "function"
    )
  ) {
    
    return(
      resolve_phase3_team(
        team =
          team_name,
        con =
          con
      )
    )
  }
  
  teams <- DBI::dbGetQuery(
    con,
    "
    SELECT
      team_id,
      team_name,
      abbreviation
    FROM teams
    "
  )
  
  query <- toupper(
    gsub(
      "[^A-Z0-9]",
      "",
      trimws(
        team_name
      )
    )
  )
  
  name_key <- toupper(
    gsub(
      "[^A-Z0-9]",
      "",
      teams$team_name
    )
  )
  
  abbr_key <- toupper(
    gsub(
      "[^A-Z0-9]",
      "",
      teams$abbreviation
    )
  )
  
  hit <- which(
    name_key == query |
      abbr_key == query
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
    team_id =
      as.integer(
        row$team_id[[1]]
      ),
    team_name =
      as.character(
        row$team_name[[1]]
      ),
    abbreviation =
      as.character(
        row$abbreviation[[1]]
      )
  )
}


# ------------------------------------------------------------
# Attach Phase-3 evidence to an arbitrary player frame
# ------------------------------------------------------------

attach_phase3_decision_intelligence <- function(
    players,
    season,
    team_id = NULL,
    con = NULL) {
  
  if (
    is.null(players) ||
    !is.data.frame(players) ||
    !nrow(players) ||
    !"player_id" %in%
    names(players)
  ) {
    return(players)
  }
  
  owns_connection <- is.null(con)
  
  if (owns_connection) {
    con <- connect_db(
      read_only = TRUE
    )
  }
  
  if (owns_connection) {
    on.exit(
      disconnect_db(con),
      add = TRUE
    )
  }
  
  performance <-
    get_phase3_decision_performance_layer(
      season =
        season,
      team_id =
        team_id,
      con =
        con
    )
  
  if (!nrow(performance)) {
    return(players)
  }
  
  # ----------------------------------------------------------
  # Prefer exact player/team join when both are present
  # ----------------------------------------------------------
  
  if (
    "team_id" %in%
    names(players)
  ) {
    
    joined <- merge(
      players,
      performance,
      by = c(
        "player_id",
        "team_id"
      ),
      all.x = TRUE,
      sort = FALSE,
      suffixes = c(
        "",
        "_p3"
      )
    )
    
  } else {
    
    # When a frame has no team_id, use one best player record.
    performance <- performance[
      order(
        performance$minutes,
        decreasing = TRUE,
        na.last = TRUE
      ),
      ,
      drop = FALSE
    ]
    
    performance <- performance[
      !duplicated(
        performance$player_id
      ),
      ,
      drop = FALSE
    ]
    
    performance$team_id <- NULL
    
    joined <- merge(
      players,
      performance,
      by = "player_id",
      all.x = TRUE,
      sort = FALSE,
      suffixes = c(
        "",
        "_p3"
      )
    )
  }
  
  # ----------------------------------------------------------
  # Explicit BIE aliases expected by legacy engine
  # ----------------------------------------------------------
  
  joined$bie_performance_rating <-
    suppressWarnings(
      as.numeric(
        joined$
          bie_performance_rating
      )
    )
  
  joined$bie_score <-
    joined$
    bie_performance_rating
  
  joined$bie_overall_score <-
    joined$
    bie_performance_rating
  
  joined$bie_offense_score <-
    suppressWarnings(
      as.numeric(
        joined$
          offensive_impact_score
      )
    )
  
  joined$bie_defense_score <-
    suppressWarnings(
      as.numeric(
        joined$
          defensive_impact_score
      )
    )
  
  joined$bie_efficiency_score <-
    suppressWarnings(
      as.numeric(
        joined$
          shooting_efficiency_score
      )
    )
  
  joined$bie_spacing_score <-
    suppressWarnings(
      as.numeric(
        joined$
          spacing_score
      )
    )
  
  joined$bie_playmaking_score <-
    suppressWarnings(
      as.numeric(
        joined$
          creation_score
      )
    )
  
  joined$bie_creation_score <-
    suppressWarnings(
      as.numeric(
        joined$
          creation_score
      )
    )
  
  joined$bie_rebounding_score <-
    suppressWarnings(
      as.numeric(
        joined$
          rebounding_score
      )
    )
  
  joined$bie_availability_score <-
    suppressWarnings(
      as.numeric(
        joined$
          availability_component
      )
    )
  
  joined$bie_score_source <- ifelse(
    is.finite(
      joined$
        bie_performance_rating
    ),
    "PHASE3_PERFORMANCE",
    "FOUNDATION"
  )
  
  joined$bie_confidence <- as.character(
    joined$
      impact_confidence
  )
  
  joined$bie_primary_role <- as.character(
    joined$
      primary_role
  )
  
  joined$bie_archetype <- as.character(
    joined$
      archetype
  )
  
  joined
}


# ------------------------------------------------------------
# Load a team roster + contracts + Phase-3 performance
# ------------------------------------------------------------

get_phase3_integrated_team_roster <- function(
    team_name,
    season,
    con = NULL) {
  
  owns_connection <- is.null(con)
  
  if (owns_connection) {
    con <- connect_db(
      read_only = TRUE
    )
  }
  
  if (owns_connection) {
    on.exit(
      disconnect_db(con),
      add = TRUE
    )
  }
  
  resolved_team <-
    p3s10_resolve_team(
      team_name =
        team_name,
      con =
        con
    )
  
  tables <- DBI::dbListTables(
    con
  )
  
  roster_exists <-
    "roster_history" %in%
    tables
  
  contracts_exists <-
    "contract_years" %in%
    tables
  
  contract_master_exists <-
    "contracts" %in%
    tables
  
  roster_join <- if (
    roster_exists
  ) {
    "
    LEFT JOIN roster_history rh
      ON rh.player_id = p.player_id
      AND rh.team_id = ?
      AND rh.season = ?
    "
  } else {
    ""
  }
  
  roster_fields <- if (
    roster_exists
  ) {
    "
      rh.roster_status,
      COALESCE(rh.two_way_flag, 0) AS two_way_flag,
    "
  } else {
    "
      NULL AS roster_status,
      0 AS two_way_flag,
    "
  }
  
  contract_join <- if (
    contracts_exists
  ) {
    "
    LEFT JOIN contract_years cy
      ON cy.player_id = p.player_id
      AND cy.team_id = ?
      AND cy.season = ?
    "
  } else {
    ""
  }
  
  contract_fields <- if (
    contracts_exists
  ) {
    "
      cy.base_salary,
      cy.cap_hit,
    "
  } else {
    "
      NULL AS base_salary,
      NULL AS cap_hit,
    "
  }
  
  master_join <- if (
    contracts_exists &&
    contract_master_exists
  ) {
    "
    LEFT JOIN contracts c
      ON c.contract_id = cy.contract_id
    "
  } else {
    ""
  }
  
  master_fields <- if (
    contracts_exists &&
    contract_master_exists
  ) {
    "
      c.contract_end_season,
      c.free_agent_year,
    "
  } else {
    "
      NULL AS contract_end_season,
      NULL AS free_agent_year,
    "
  }
  
  # Base player universe comes from the performance table so the
  # full real player set survives incomplete roster_history rows.
  sql <- paste0(
    "
    SELECT DISTINCT
      p.player_id,
      p.player_name,
      p.primary_position,
      p.player_age,
      p.height_inches,
      p.weight_lbs,

      ? AS team_id,

      ",
    roster_fields,
    contract_fields,
    master_fields,
    "

      ? AS season

    FROM player_season_impact impact

    INNER JOIN players p
      ON p.player_id = impact.player_id

    ",
    roster_join,
    contract_join,
    master_join,
    "

    WHERE impact.team_id = ?
      AND impact.season = ?

    ORDER BY p.player_name
    "
  )
  
  params <- list(
    resolved_team$team_id
  )
  
  if (roster_exists) {
    params <- c(
      params,
      list(
        resolved_team$team_id,
        season
      )
    )
  }
  
  if (contracts_exists) {
    params <- c(
      params,
      list(
        resolved_team$team_id,
        season
      )
    )
  }
  
  params <- c(
    params,
    list(
      season,
      resolved_team$team_id,
      season
    )
  )
  
  roster <- DBI::dbGetQuery(
    con,
    sql,
    params =
      params
  )
  
  integrated <-
    attach_phase3_decision_intelligence(
      players =
        roster,
      season =
        season,
      team_id =
        resolved_team$team_id,
      con =
        con
    )
  
  attr(
    integrated,
    "resolved_team"
  ) <- resolved_team
  
  integrated
}


# ------------------------------------------------------------
# ROSTER DECISION — Phase-3 integrated
# ------------------------------------------------------------

evaluate_phase3_roster_decisions <- function(
    team_name,
    season = "2025-26",
    salary_cap = NA_real_,
    con = NULL) {
  
  roster <-
    get_phase3_integrated_team_roster(
      team_name =
        team_name,
      season =
        season,
      con =
        con
    )
  
  if (!nrow(roster)) {
    
    return(
      list(
        status =
          "NO_ROSTER",
        confidence =
          "FOUNDATION",
        explanation =
          "No integrated team roster was available."
      )
    )
  }
  
  result <-
    evaluate_bie_roster_decisions(
      roster_players =
        roster,
      salary_cap =
        salary_cap
    )
  
  result$phase3_integrated <-
    TRUE
  
  result$performance_rows <-
    sum(
      is.finite(
        suppressWarnings(
          as.numeric(
            roster$
              bie_performance_rating
          )
        )
      )
    )
  
  result$performance_scope <-
    "PHASE 3 PERFORMANCE-BACKED"
  
  result
}


# ------------------------------------------------------------
# ROSTER NEEDS — Phase-3 integrated
# ------------------------------------------------------------

evaluate_phase3_roster_needs <- function(
    team_name,
    season = "2025-26",
    salary_cap = NA_real_,
    con = NULL) {
  
  roster <-
    get_phase3_integrated_team_roster(
      team_name =
        team_name,
      season =
        season,
      con =
        con
    )
  
  decision <-
    evaluate_bie_roster_decisions(
      roster_players =
        roster,
      salary_cap =
        salary_cap
    )
  
  needs <-
    evaluate_bie_roster_needs(
      roster_players =
        roster,
      roster_decision =
        decision
    )
  
  needs$phase3_integrated <-
    TRUE
  
  needs$performance_scope <-
    "PHASE 3 PERFORMANCE-BACKED"
  
  needs
}


# ------------------------------------------------------------
# ACQUISITION TARGET — Phase-3 integrated
# ------------------------------------------------------------

evaluate_phase3_acquisition_target <- function(
    target_player_id,
    target_team_id = NULL,
    current_team_name,
    season = "2025-26",
    con = NULL) {
  
  owns_connection <- is.null(con)
  
  if (owns_connection) {
    con <- connect_db(
      read_only = TRUE
    )
  }
  
  if (owns_connection) {
    on.exit(
      disconnect_db(con),
      add = TRUE
    )
  }
  
  roster <-
    get_phase3_integrated_team_roster(
      team_name =
        current_team_name,
      season =
        season,
      con =
        con
    )
  
  needs <-
    evaluate_bie_roster_needs(
      roster_players =
        roster
    )
  
  target_base <- DBI::dbGetQuery(
    con,
    "
    SELECT
      p.player_id,
      p.player_name,
      p.primary_position,
      p.player_age,
      p.height_inches,
      p.weight_lbs
    FROM players p
    WHERE p.player_id = ?
    ",
    params = list(
      as.integer(
        target_player_id
      )
    )
  )
  
  if (!nrow(target_base)) {
    
    return(
      list(
        status =
          "NO_TARGET",
        fit_score =
          NA_real_,
        confidence =
          "FOUNDATION",
        explanation =
          "The target player_id was not found."
      )
    )
  }
  
  if (!is.null(target_team_id)) {
    target_base$team_id <-
      as.integer(
        target_team_id
      )
  }
  
  target <-
    attach_phase3_decision_intelligence(
      players =
        target_base,
      season =
        season,
      team_id =
        target_team_id,
      con =
        con
    )
  
  result <-
    evaluate_bie_acquisition_target(
      target_row =
        target,
      current_roster =
        roster,
      needs_result =
        needs
    )
  
  result$phase3_integrated <-
    TRUE
  
  result$target_performance_rating <-
    if (
      "bie_performance_rating" %in%
      names(target)
    ) {
      p3s10_num(
        target$
          bie_performance_rating[[1]]
      )
    } else {
      NA_real_
    }
  
  result$target_archetype <-
    if (
      "bie_archetype" %in%
      names(target)
    ) {
      p3s10_text(
        target$
          bie_archetype[[1]],
        "Unavailable"
      )
    } else {
      "Unavailable"
    }
  
  result
}


# ------------------------------------------------------------
# TRADE IMPACT — Phase-3 integrated
# ------------------------------------------------------------

evaluate_phase3_trade_impact <- function(
    current_team_name,
    outgoing_players,
    incoming_players,
    season = "2025-26",
    current_lineup = NULL,
    rotation_size = 9L,
    con = NULL) {
  
  roster <-
    get_phase3_integrated_team_roster(
      team_name =
        current_team_name,
      season =
        season,
      con =
        con
    )
  
  outgoing_enriched <-
    attach_phase3_decision_intelligence(
      players =
        outgoing_players,
      season =
        season,
      con =
        con
    )
  
  incoming_enriched <-
    attach_phase3_decision_intelligence(
      players =
        incoming_players,
      season =
        season,
      con =
        con
    )
  
  result <-
    evaluate_bie_trade_basketball_impact(
      current_players =
        roster,
      outgoing_players =
        outgoing_enriched,
      incoming_players =
        incoming_enriched,
      current_lineup =
        current_lineup,
      rotation_size =
        rotation_size
    )
  
  result$phase3_integrated <-
    TRUE
  
  result$performance_scope <-
    "PHASE 3 PERFORMANCE-BACKED"
  
  result
}


# ------------------------------------------------------------
# EXTENSION VALUE — Phase-3 integrated
# ------------------------------------------------------------

evaluate_phase3_extension_value <- function(
    team_name,
    player_id,
    proposal,
    season = "2025-26",
    extension_result = NULL,
    con = NULL) {
  
  roster <-
    get_phase3_integrated_team_roster(
      team_name =
        team_name,
      season =
        season,
      con =
        con
    )
  
  player <- roster[
    roster$player_id ==
      as.integer(
        player_id
      ),
    ,
    drop = FALSE
  ]
  
  if (!nrow(player)) {
    
    return(
      list(
        status =
          "NO_PLAYER",
        recommendation =
          "REVIEW",
        confidence =
          "FOUNDATION",
        explanation =
          "The selected player is not in the integrated team roster."
      )
    )
  }
  
  result <-
    evaluate_bie_extension_value(
      player_row =
        player,
      proposal =
        proposal,
      extension_result =
        extension_result,
      evaluated_roster =
        roster
    )
  
  result$phase3_integrated <-
    TRUE
  
  result$performance_rating <-
    p3s10_num(
      player$
        bie_performance_rating[[1]]
    )
  
  result$performance_tier <-
    p3s10_text(
      player$
        impact_tier[[1]],
      "Unavailable"
    )
  
  result$archetype <-
    p3s10_text(
      player$
        bie_archetype[[1]],
      "Unavailable"
    )
  
  result
}


# ------------------------------------------------------------
# EXECUTIVE INTELLIGENCE — Phase-3 integrated
# ------------------------------------------------------------

evaluate_phase3_executive_front_office <- function(
    team_name,
    season = "2025-26",
    competitive_tier = "Unknown",
    financial_status = "Unknown",
    active_trade = NULL,
    salary_cap = NA_real_,
    con = NULL) {
  
  roster <-
    get_phase3_integrated_team_roster(
      team_name =
        team_name,
      season =
        season,
      con =
        con
    )
  
  roster_decision <-
    evaluate_bie_roster_decisions(
      roster_players =
        roster,
      salary_cap =
        salary_cap
    )
  
  roster_needs <-
    evaluate_bie_roster_needs(
      roster_players =
        roster,
      roster_decision =
        roster_decision
    )
  
  result <-
    evaluate_bie_executive_front_office(
      roster_decision =
        roster_decision,
      roster_needs =
        roster_needs,
      competitive_tier =
        competitive_tier,
      financial_status =
        financial_status,
      active_trade =
        active_trade
    )
  
  performance_rows <- sum(
    is.finite(
      suppressWarnings(
        as.numeric(
          roster$
            bie_performance_rating
        )
      )
    )
  )
  
  average_rating <- mean(
    suppressWarnings(
      as.numeric(
        roster$
          bie_performance_rating
      )
    ),
    na.rm = TRUE
  )
  
  if (!is.finite(average_rating)) {
    average_rating <- NA_real_
  }
  
  result$phase3_integrated <-
    TRUE
  
  result$performance_rows <-
    performance_rows
  
  result$average_bie_performance_rating <-
    average_rating
  
  result$performance_scope <-
    if (
      performance_rows > 0
    ) {
      "PHASE 3 PERFORMANCE-BACKED"
    } else {
      "FOUNDATION"
    }
  
  result
}


# ------------------------------------------------------------
# Full team decision package
# ------------------------------------------------------------

run_phase3_front_office_decision_package <- function(
    team_name,
    season = "2025-26",
    competitive_tier = "Unknown",
    financial_status = "Unknown",
    salary_cap = NA_real_,
    con = NULL) {
  
  roster <-
    get_phase3_integrated_team_roster(
      team_name =
        team_name,
      season =
        season,
      con =
        con
    )
  
  if (!nrow(roster)) {
    
    return(
      list(
        status =
          "NO TEAM PERFORMANCE DATA",
        team =
          team_name,
        season =
          season
      )
    )
  }
  
  roster_decision <-
    evaluate_bie_roster_decisions(
      roster_players =
        roster,
      salary_cap =
        salary_cap
    )
  
  roster_needs <-
    evaluate_bie_roster_needs(
      roster_players =
        roster,
      roster_decision =
        roster_decision
    )
  
  executive <-
    evaluate_bie_executive_front_office(
      roster_decision =
        roster_decision,
      roster_needs =
        roster_needs,
      competitive_tier =
        competitive_tier,
      financial_status =
        financial_status
    )
  
  lineup_rotation <- if (
    exists(
      "run_phase3_lineup_rotation_optimization",
      mode = "function"
    )
  ) {
    
    tryCatch(
      run_phase3_lineup_rotation_optimization(
        team_name =
          team_name,
        season =
          season
      ),
      error = function(e) {
        list(
          status =
            "LINEUP OPTIMIZATION ERROR",
          explanation =
            conditionMessage(e)
        )
      }
    )
    
  } else {
    
    list(
      status =
        "STEP 9 NOT LOADED"
    )
  }
  
  list(
    status =
      "OK",
    team =
      team_name,
    season =
      season,
    roster =
      roster,
    roster_decision =
      roster_decision,
    roster_needs =
      roster_needs,
    lineup_rotation =
      lineup_rotation,
    executive =
      executive,
    performance_rows =
      sum(
        is.finite(
          suppressWarnings(
            as.numeric(
              roster$
                bie_performance_rating
            )
          )
        )
      ),
    performance_scope =
      "PHASE 3 PERFORMANCE-BACKED"
  )
}


# ------------------------------------------------------------
# Step 10 health check
# ------------------------------------------------------------

phase3_step10_healthcheck <- function(
    team_name,
    season = "2025-26") {
  
  readiness <-
    phase3_step10_integration_readiness()
  
  if (
    !identical(
      readiness$status,
      "READY"
    )
  ) {
    
    return(
      list(
        phase =
          "Phase 3",
        step =
          "Step 10 — Full Decision-Engine Integration",
        status =
          "REVIEW",
        missing_tables =
          readiness$missing_tables,
        missing_functions =
          readiness$missing_functions
      )
    )
  }
  
  result <-
    tryCatch(
      run_phase3_front_office_decision_package(
        team_name =
          team_name,
        season =
          season
      ),
      error = function(e) {
        list(
          status =
            "ERROR",
          explanation =
            conditionMessage(e)
        )
      }
    )
  
  if (
    !identical(
      result$status,
      "OK"
    )
  ) {
    
    return(
      list(
        phase =
          "Phase 3",
        step =
          "Step 10 — Full Decision-Engine Integration",
        status =
          "REVIEW",
        issue =
          result$status,
        explanation =
          result$explanation %||%
          ""
      )
    )
  }
  
  roster_status <-
    result$
    roster_decision$
    status %||%
    "UNKNOWN"
  
  needs_status <-
    result$
    roster_needs$
    status %||%
    "UNKNOWN"
  
  lineup_status <-
    result$
    lineup_rotation$
    status %||%
    "UNKNOWN"
  
  executive_status <-
    result$
    executive$
    status %||%
    "UNKNOWN"
  
  list(
    phase =
      "Phase 3",
    step =
      "Step 10 — Full Decision-Engine Integration",
    status = if (
      identical(
        roster_status,
        "OK"
      ) &&
      identical(
        needs_status,
        "OK"
      ) &&
      identical(
        lineup_status,
        "OK"
      ) &&
      identical(
        executive_status,
        "OK"
      ) &&
      result$performance_rows >
      0
    ) {
      "READY"
    } else {
      "REVIEW"
    },
    team =
      team_name,
    season =
      season,
    performance_rows =
      result$performance_rows,
    roster_decision_status =
      roster_status,
    roster_needs_status =
      needs_status,
    lineup_rotation_status =
      lineup_status,
    executive_status =
      executive_status,
    performance_scope =
      result$performance_scope
  )
}


# ============================================================
# PHASE 3.1 OVERRIDE — CURRENT ROSTER / PRIOR PERFORMANCE
# ============================================================

get_phase3_integrated_team_roster <- function(
    team_name,
    season = "2025-26",
    con = NULL,
    current_roster_season = "2026-27") {
  
  if (
    !exists(
      "get_tbi_current_roster_evaluation_context",
      mode = "function",
      inherits = TRUE
    )
  ) {
    stop(
      "Current roster evaluation context is not loaded."
    )
  }
  
  roster <- get_tbi_current_roster_evaluation_context(
    team_name = team_name,
    current_roster_season = current_roster_season,
    performance_season = season,
    con = con
  )
  
  if (!nrow(roster)) {
    return(roster)
  }
  
  roster <- attach_tbi_current_contract_context(
    roster = roster,
    current_roster_season =
      unique(roster$current_roster_season)[[1]],
    con = con
  )
  
  roster
}


run_phase3_front_office_decision_package <- function(
    team_name,
    season = "2025-26",
    competitive_tier = "Unknown",
    financial_status = "Unknown",
    salary_cap = NA_real_,
    con = NULL,
    current_roster_season = "2026-27") {
  
  roster <- get_phase3_integrated_team_roster(
    team_name = team_name,
    season = season,
    con = con,
    current_roster_season = current_roster_season
  )
  
  if (!nrow(roster)) {
    return(
      list(
        status = "NO CURRENT ROSTER DATA",
        team = team_name,
        performance_season = season,
        current_roster_season = current_roster_season
      )
    )
  }
  
  roster_decision <- evaluate_bie_roster_decisions(
    roster_players = roster,
    salary_cap = salary_cap
  )
  
  roster_needs <- evaluate_bie_roster_needs(
    roster_players = roster,
    roster_decision = roster_decision
  )
  
  executive <- evaluate_bie_executive_front_office(
    roster_decision = roster_decision,
    roster_needs = roster_needs,
    competitive_tier = competitive_tier,
    financial_status = financial_status
  )
  
  lineup_rotation <- if (
    exists(
      "run_phase3_lineup_rotation_optimization",
      mode = "function",
      inherits = TRUE
    )
  ) {
    tryCatch(
      run_phase3_lineup_rotation_optimization(
        team_name = team_name,
        season = season,
        current_roster_season =
          unique(roster$current_roster_season)[[1]]
      ),
      error = function(e) {
        list(
          status = "LINEUP OPTIMIZATION ERROR",
          explanation = conditionMessage(e)
        )
      }
    )
  } else {
    list(status = "STEP 9 NOT LOADED")
  }
  
  list(
    status = "OK",
    team = unique(roster$current_team_name)[[1]],
    current_roster_season =
      unique(roster$current_roster_season)[[1]],
    performance_season = season,
    roster = roster,
    roster_decision = roster_decision,
    roster_needs = roster_needs,
    lineup_rotation = lineup_rotation,
    executive = executive,
    performance_rows =
      sum(roster$evaluation_source == "PRIOR-SEASON NBA"),
    rookie_or_pending_rows =
      sum(roster$evaluation_source != "PRIOR-SEASON NBA"),
    moved_player_rows =
      sum(roster$changed_teams_since_evidence, na.rm = TRUE),
    performance_scope =
      "CURRENT ROSTER + PRIOR-SEASON NBA PERFORMANCE"
  )
}


phase3_step10_healthcheck <- function(
    team_name,
    season = "2025-26",
    current_roster_season = "2026-27") {
  
  result <- tryCatch(
    run_phase3_front_office_decision_package(
      team_name = team_name,
      season = season,
      current_roster_season = current_roster_season
    ),
    error = function(e) {
      list(
        status = "ERROR",
        explanation = conditionMessage(e)
      )
    }
  )
  
  if (!identical(result$status, "OK")) {
    return(
      list(
        phase = "Phase 3.1",
        step = "Front Office Integration — Current Roster Context",
        status = "REVIEW",
        issue = result$status,
        explanation = result$explanation %||% ""
      )
    )
  }
  
  list(
    phase = "Phase 3.1",
    step = "Front Office Integration — Current Roster Context",
    status = if (
      identical(result$roster_decision$status %||% "OK", "OK") &&
      identical(result$roster_needs$status %||% "OK", "OK") &&
      identical(result$lineup_rotation$status, "OK") &&
      identical(result$executive$status %||% "OK", "OK")
    ) {
      "READY"
    } else {
      "REVIEW"
    },
    team = result$team,
    current_roster_season = result$current_roster_season,
    performance_season = result$performance_season,
    current_roster_players = nrow(result$roster),
    veterans_with_prior_nba_performance = result$performance_rows,
    rookies_or_pending = result$rookie_or_pending_rows,
    moved_players = result$moved_player_rows,
    lineup_rotation_status = result$lineup_rotation$status,
    performance_scope = result$performance_scope
  )
}