# ============================================================
# TBI NBA Basketball Operations Platform
# PHASE 3 — STEP 11.1
# Front Office Recommendation Calibration
#
# Full replacement for:
#   R/front_office_recommendations.R
#
# Purpose:
#   Convert Phase-3 player intelligence into front-office
#   recommendations that are both technically valid and
#   basketball-sane.
#
# Major calibration changes:
#   - team-relative player importance
#   - minutes / workload context
#   - core-role protection
#   - real roster-need fit scoring
#   - contract actions only when contract data exists
#   - conservative UPGRADE / SHOP thresholds
#   - automatic sanity flags
#
# Recommendations:
#   KEEP
#   EXTEND
#   SHOP
#   UPGRADE
#   REVIEW
#
# Acquisition actions:
#   TARGET
#   MONITOR
#   PASS / REVIEW
# ============================================================


# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------

p3s11_num <- function(
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


p3s11_text <- function(
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


p3s11_weighted_mean <- function(
    values,
    weights) {
  
  values <- suppressWarnings(
    as.numeric(values)
  )
  
  weights <- suppressWarnings(
    as.numeric(weights)
  )
  
  valid <-
    is.finite(values) &
    is.finite(weights) &
    weights > 0
  
  if (!any(valid)) {
    return(NA_real_)
  }
  
  sum(
    values[valid] *
      weights[valid]
  ) /
    sum(
      weights[valid]
    )
}


p3s11_percentile <- function(x) {
  
  value <- suppressWarnings(
    as.numeric(x)
  )
  
  valid <- is.finite(value)
  
  output <- rep(
    NA_real_,
    length(value)
  )
  
  if (!any(valid)) {
    return(output)
  }
  
  if (sum(valid) == 1L) {
    output[valid] <- 50
    return(output)
  }
  
  ranked <- rank(
    value[valid],
    ties.method = "average"
  )
  
  output[valid] <-
    100 *
    (
      ranked - 1
    ) /
    (
      sum(valid) - 1
    )
  
  output
}


# ------------------------------------------------------------
# Self-contained integrated roster loader
#
# Uses Step-10 helper when available.
# Falls back directly to Phase-3 tables when it is not.
# ------------------------------------------------------------

p3s11_get_integrated_team_roster <- function(
    team_name,
    season = "2025-26",
    con = NULL) {
  
  # Prefer the Step-10 implementation when it is loaded.
  if (
    exists(
      "get_phase3_integrated_team_roster",
      mode = "function",
      inherits = TRUE
    )
  ) {
    
    return(
      get_phase3_integrated_team_roster(
        team_name =
          team_name,
        season =
          season,
        con =
          con
      )
    )
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
  
  tables <- DBI::dbListTables(
    con
  )
  
  required <- c(
    "teams",
    "players",
    "player_season_impact",
    "player_season_roles"
  )
  
  missing <- setdiff(
    required,
    tables
  )
  
  if (length(missing)) {
    stop(
      paste0(
        "Step 11 requires these tables: ",
        paste(
          missing,
          collapse = ", "
        )
      )
    )
  }
  
  # ----------------------------------------------------------
  # Resolve team
  # ----------------------------------------------------------
  
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
  
  norm <- function(x) {
    toupper(
      gsub(
        "[^A-Z0-9]",
        "",
        trimws(
          as.character(x)
        )
      )
    )
  }
  
  query_key <- norm(
    team_name
  )
  
  team_name_key <- norm(
    teams$team_name
  )
  
  team_abbr_key <- norm(
    teams$abbreviation
  )
  
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
  
  if (
    query_key %in%
    names(aliases)
  ) {
    query_key <- norm(
      aliases[[query_key]]
    )
  }
  
  hit <- which(
    team_name_key == query_key |
      team_abbr_key == query_key
  )
  
  if (!length(hit)) {
    stop(
      paste0(
        "Could not resolve team: ",
        team_name
      )
    )
  }
  
  team_id <- suppressWarnings(
    as.integer(
      teams$team_id[
        hit[[1]]
      ]
    )
  )
  
  # ----------------------------------------------------------
  # Optional contract support
  # ----------------------------------------------------------
  
  has_contract_years <-
    "contract_years" %in%
    tables
  
  contract_join <- if (
    has_contract_years
  ) {
    "
    LEFT JOIN contract_years cy
      ON cy.player_id = impact.player_id
      AND cy.team_id = impact.team_id
      AND cy.season = impact.season
    "
  } else {
    ""
  }
  
  contract_fields <- if (
    has_contract_years
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
  
  sql <- paste0(
    "
    SELECT
      impact.player_id,
      impact.team_id,
      impact.season,

      p.player_name,
      p.primary_position,
      p.player_age,
      p.height_inches,
      p.weight_lbs,

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

      ",
    contract_fields,
    "

      NULL AS roster_status,
      0 AS two_way_flag

    FROM player_season_impact impact

    INNER JOIN players p
      ON p.player_id = impact.player_id

    LEFT JOIN player_season_roles role
      ON role.player_id = impact.player_id
      AND role.team_id = impact.team_id
      AND role.season = impact.season

    ",
    contract_join,
    "

    WHERE impact.team_id = ?
      AND impact.season = ?

    ORDER BY
      impact.bie_performance_rating DESC,
      impact.minutes_per_game DESC,
      p.player_name
    "
  )
  
  roster <- DBI::dbGetQuery(
    con,
    sql,
    params = list(
      team_id,
      as.character(
        season
      )
    )
  )
  
  # Legacy BIE aliases needed by downstream decision functions.
  if (nrow(roster)) {
    
    roster$bie_score <-
      roster$bie_performance_rating
    
    roster$bie_overall_score <-
      roster$bie_performance_rating
    
    roster$bie_offense_score <-
      roster$offensive_impact_score
    
    roster$bie_defense_score <-
      roster$defensive_impact_score
    
    roster$bie_rebounding_score <-
      roster$rebounding_component
    
    roster$bie_availability_score <-
      roster$availability_component
    
    roster$bie_score_source <-
      ifelse(
        is.finite(
          suppressWarnings(
            as.numeric(
              roster$
                bie_performance_rating
            )
          )
        ),
        "PHASE3_PERFORMANCE",
        "FOUNDATION"
      )
    
    roster$bie_confidence <-
      roster$impact_confidence
    
    roster$bie_primary_role <-
      roster$primary_role
    
    roster$bie_archetype <-
      roster$archetype
  }
  
  roster
}


# ------------------------------------------------------------
# Core-role detection
# ------------------------------------------------------------

p3s11_is_core_role <- function(
    primary_role,
    archetype,
    role_family) {
  
  text <- toupper(
    paste(
      p3s11_text(
        primary_role,
        ""
      ),
      p3s11_text(
        archetype,
        ""
      ),
      p3s11_text(
        role_family,
        ""
      )
    )
  )
  
  core_patterns <- c(
    "SCORER",
    "CREATOR",
    "PRIMARY OFFENSIVE ENGINE",
    "TWO-WAY SCORING",
    "TWO-WAY CONNECTOR",
    "3-AND-D",
    "OFFENSIVE"
  )
  
  any(
    vapply(
      core_patterns,
      function(pattern) {
        grepl(
          pattern,
          text,
          fixed = TRUE
        )
      },
      logical(1)
    )
  )
}


# ------------------------------------------------------------
# Team-relative importance score
# ------------------------------------------------------------

p3s11_team_importance <- function(
    performance_team_pct,
    minutes_team_pct,
    offensive_impact,
    defensive_impact,
    role_component,
    core_role) {
  
  core_bonus <- if (
    isTRUE(
      core_role
    )
  ) {
    82
  } else {
    NA_real_
  }
  
  p3s11_weighted_mean(
    c(
      performance_team_pct,
      minutes_team_pct,
      offensive_impact,
      defensive_impact,
      role_component,
      core_bonus
    ),
    c(
      0.26,
      0.24,
      0.18,
      0.13,
      0.11,
      0.08
    )
  )
}


# ------------------------------------------------------------
# Roster-need fit
# ------------------------------------------------------------

p3s11_need_fit_score <- function(
    need_text,
    scoring_score,
    spacing_score,
    creation_score,
    connector_score,
    defense_score,
    rebounding_score,
    interior_score) {
  
  need <- toupper(
    p3s11_text(
      need_text,
      ""
    )
  )
  
  if (!nzchar(need)) {
    return(NA_real_)
  }
  
  scoring_score <- p3s11_num(
    scoring_score
  )
  
  spacing_score <- p3s11_num(
    spacing_score
  )
  
  creation_score <- p3s11_num(
    creation_score
  )
  
  connector_score <- p3s11_num(
    connector_score
  )
  
  defense_score <- p3s11_num(
    defense_score
  )
  
  rebounding_score <- p3s11_num(
    rebounding_score
  )
  
  interior_score <- p3s11_num(
    interior_score
  )
  
  # Shooting / spacing need
  if (
    grepl(
      "SHOOT",
      need
    ) ||
    grepl(
      "SPAC",
      need
    ) ||
    grepl(
      "THREE",
      need
    )
  ) {
    return(
      p3s11_weighted_mean(
        c(
          spacing_score,
          scoring_score
        ),
        c(
          0.78,
          0.22
        )
      )
    )
  }
  
  # Creation / playmaking need
  if (
    grepl(
      "CREAT",
      need
    ) ||
    grepl(
      "PLAYMAK",
      need
    ) ||
    grepl(
      "BALL HAND",
      need
    ) ||
    grepl(
      "POINT",
      need
    )
  ) {
    return(
      p3s11_weighted_mean(
        c(
          creation_score,
          connector_score
        ),
        c(
          0.75,
          0.25
        )
      )
    )
  }
  
  # Defense need
  if (
    grepl(
      "DEFEN",
      need
    ) ||
    grepl(
      "WING",
      need
    ) ||
    grepl(
      "PERIMETER",
      need
    )
  ) {
    return(
      p3s11_weighted_mean(
        c(
          defense_score,
          connector_score
        ),
        c(
          0.82,
          0.18
        )
      )
    )
  }
  
  # Rebounding / size / interior need
  if (
    grepl(
      "REBOUND",
      need
    ) ||
    grepl(
      "INTERIOR",
      need
    ) ||
    grepl(
      "RIM",
      need
    ) ||
    grepl(
      "BIG",
      need
    ) ||
    grepl(
      "CENTER",
      need
    ) ||
    grepl(
      "SIZE",
      need
    )
  ) {
    return(
      p3s11_weighted_mean(
        c(
          rebounding_score,
          interior_score,
          defense_score
        ),
        c(
          0.40,
          0.40,
          0.20
        )
      )
    )
  }
  
  # Scoring need
  if (
    grepl(
      "SCOR",
      need
    ) ||
    grepl(
      "OFFEN",
      need
    )
  ) {
    return(
      p3s11_weighted_mean(
        c(
          scoring_score,
          creation_score,
          spacing_score
        ),
        c(
          0.48,
          0.30,
          0.22
        )
      )
    )
  }
  
  # Unknown need type: use broad role versatility instead of a
  # fake fixed score.
  p3s11_weighted_mean(
    c(
      scoring_score,
      spacing_score,
      creation_score,
      connector_score,
      defense_score,
      rebounding_score,
      interior_score
    ),
    c(
      0.16,
      0.14,
      0.16,
      0.10,
      0.18,
      0.14,
      0.12
    )
  )
}


# ------------------------------------------------------------
# Recommendation confidence
# ------------------------------------------------------------

p3s11_confidence <- function(
    impact_confidence,
    role_confidence,
    contract_data_available,
    needs_data_available,
    team_importance = NA_real_) {
  
  impact_confidence <-
    p3s11_text(
      impact_confidence,
      "LIMITED"
    )
  
  role_confidence <-
    p3s11_text(
      role_confidence,
      "LIMITED"
    )
  
  team_importance <-
    p3s11_num(
      team_importance
    )
  
  # HIGH requires strong data quality. Contract data is not
  # mandatory for a KEEP decision, but it is required for strong
  # contract-specific actions such as EXTEND / SHOP.
  if (
    impact_confidence == "HIGH" &&
    role_confidence == "HIGH" &&
    isTRUE(
      needs_data_available
    ) &&
    (
      is.na(
        team_importance
      ) ||
      team_importance >= 55
    )
  ) {
    return("HIGH")
  }
  
  if (
    impact_confidence %in%
    c(
      "HIGH",
      "MODERATE"
    ) &&
    role_confidence %in%
    c(
      "HIGH",
      "MODERATE"
    )
  ) {
    return("MODERATE")
  }
  
  "LIMITED"
}


# ------------------------------------------------------------
# Calibrated player action
# ------------------------------------------------------------

p3s11_player_action <- function(
    performance_rating,
    performance_team_pct,
    minutes_team_pct,
    team_importance,
    offensive_impact,
    defensive_impact,
    role_score,
    availability_score,
    core_role,
    age = NA_real_,
    salary = NA_real_,
    salary_percentile = NA_real_,
    roster_need_match = NA_real_,
    impact_confidence = "LIMITED",
    contract_data_available = FALSE) {
  
  performance_rating <-
    p3s11_num(
      performance_rating
    )
  
  performance_team_pct <-
    p3s11_num(
      performance_team_pct
    )
  
  minutes_team_pct <-
    p3s11_num(
      minutes_team_pct
    )
  
  team_importance <-
    p3s11_num(
      team_importance
    )
  
  offensive_impact <-
    p3s11_num(
      offensive_impact
    )
  
  defensive_impact <-
    p3s11_num(
      defensive_impact
    )
  
  role_score <-
    p3s11_num(
      role_score
    )
  
  availability_score <-
    p3s11_num(
      availability_score
    )
  
  age <- p3s11_num(
    age
  )
  
  salary_percentile <-
    p3s11_num(
      salary_percentile
    )
  
  roster_need_match <-
    p3s11_num(
      roster_need_match
    )
  
  impact_confidence <-
    p3s11_text(
      impact_confidence,
      "LIMITED"
    )
  
  core_role <- isTRUE(
    core_role
  )
  
  contract_data_available <- isTRUE(
    contract_data_available
  )
  
  # ----------------------------------------------------------
  # 1. Protect obvious team core players from UPGRADE logic.
  # ----------------------------------------------------------
  
  core_evidence <-
    core_role &&
    (
      (
        !is.na(
          minutes_team_pct
        ) &&
          minutes_team_pct >= 70
      ) ||
        (
          !is.na(
            team_importance
          ) &&
            team_importance >= 68
        )
    )
  
  if (core_evidence) {
    
    # EXTEND only when contract context exists.
    if (
      contract_data_available &&
      !is.na(age) &&
      age <= 29 &&
      (
        is.na(
          salary_percentile
        ) ||
        salary_percentile <= 85
      ) &&
      (
        is.na(
          team_importance
        ) ||
        team_importance >= 72
      )
    ) {
      return("EXTEND")
    }
    
    return("KEEP")
  }
  
  # ----------------------------------------------------------
  # 2. High-value players should remain.
  # ----------------------------------------------------------
  
  if (
    (
      !is.na(
        performance_team_pct
      ) &&
      performance_team_pct >= 75
    ) ||
    (
      !is.na(
        team_importance
      ) &&
      team_importance >= 72
    )
  ) {
    return("KEEP")
  }
  
  # ----------------------------------------------------------
  # 3. Strong need-fit rotation players are useful.
  # ----------------------------------------------------------
  
  if (
    !is.na(
      roster_need_match
    ) &&
    roster_need_match >= 70 &&
    (
      (
        !is.na(
          performance_team_pct
        ) &&
        performance_team_pct >= 45
      ) ||
      (
        !is.na(
          minutes_team_pct
        ) &&
        minutes_team_pct >= 45
      )
    )
  ) {
    return("KEEP")
  }
  
  # ----------------------------------------------------------
  # 4. SHOP requires real contract information.
  # ----------------------------------------------------------
  
  if (
    contract_data_available &&
    !is.na(
      salary_percentile
    ) &&
    salary_percentile >= 80 &&
    !is.na(
      performance_team_pct
    ) &&
    performance_team_pct < 35 &&
    (
      is.na(
        minutes_team_pct
      ) ||
      minutes_team_pct < 60
    ) &&
    !core_role
  ) {
    return("SHOP")
  }
  
  # ----------------------------------------------------------
  # 5. UPGRADE is intentionally conservative.
  #
  # A player must be low in both performance and team workload,
  # and cannot carry a protected core role.
  # ----------------------------------------------------------
  
  upgrade_candidate <-
    !core_role &&
    !is.na(
      performance_team_pct
    ) &&
    performance_team_pct <= 25 &&
    !is.na(
      minutes_team_pct
    ) &&
    minutes_team_pct <= 45 &&
    (
      is.na(
        team_importance
      ) ||
        team_importance < 45
    ) &&
    (
      is.na(
        roster_need_match
      ) ||
        roster_need_match < 55
    )
  
  if (upgrade_candidate) {
    return("UPGRADE")
  }
  
  # ----------------------------------------------------------
  # 6. Useful all-around players remain KEEP candidates.
  # ----------------------------------------------------------
  
  balanced_impact <-
    p3s11_weighted_mean(
      c(
        offensive_impact,
        defensive_impact,
        role_score,
        availability_score
      ),
      c(
        0.34,
        0.30,
        0.20,
        0.16
      )
    )
  
  if (
    !is.na(
      balanced_impact
    ) &&
    balanced_impact >= 58
  ) {
    return("KEEP")
  }
  
  # ----------------------------------------------------------
  # 7. Low-confidence / ambiguous profiles remain REVIEW.
  # ----------------------------------------------------------
  
  "REVIEW"
}


# ------------------------------------------------------------
# Recommendation rationale
# ------------------------------------------------------------

p3s11_recommendation_reason <- function(
    action,
    player_name,
    performance_rating,
    performance_team_pct,
    minutes_team_pct,
    team_importance,
    impact_tier,
    archetype,
    roster_need_match,
    salary_percentile,
    confidence,
    core_role) {
  
  pieces <- character()
  
  rating <- p3s11_num(
    performance_rating
  )
  
  performance_team_pct <-
    p3s11_num(
      performance_team_pct
    )
  
  minutes_team_pct <-
    p3s11_num(
      minutes_team_pct
    )
  
  team_importance <-
    p3s11_num(
      team_importance
    )
  
  roster_need_match <-
    p3s11_num(
      roster_need_match
    )
  
  salary_percentile <-
    p3s11_num(
      salary_percentile
    )
  
  if (!is.na(rating)) {
    pieces <- c(
      pieces,
      paste0(
        "BIE rating ",
        round(
          rating,
          1
        ),
        "."
      )
    )
  }
  
  if (!is.na(performance_team_pct)) {
    pieces <- c(
      pieces,
      paste0(
        "Team performance percentile ",
        round(
          performance_team_pct,
          0
        ),
        "."
      )
    )
  }
  
  if (!is.na(minutes_team_pct)) {
    pieces <- c(
      pieces,
      paste0(
        "Team workload percentile ",
        round(
          minutes_team_pct,
          0
        ),
        "."
      )
    )
  }
  
  if (!is.na(team_importance)) {
    pieces <- c(
      pieces,
      paste0(
        "Team importance ",
        round(
          team_importance,
          1
        ),
        "."
      )
    )
  }
  
  pieces <- c(
    pieces,
    paste0(
      "Impact tier: ",
      p3s11_text(
        impact_tier,
        "Unrated"
      ),
      "."
    ),
    paste0(
      "Archetype: ",
      p3s11_text(
        archetype,
        "Mixed profile"
      ),
      "."
    )
  )
  
  if (isTRUE(core_role)) {
    pieces <- c(
      pieces,
      "Core-role evidence active."
    )
  }
  
  if (!is.na(roster_need_match)) {
    pieces <- c(
      pieces,
      paste0(
        "Roster-need fit ",
        round(
          roster_need_match,
          1
        ),
        "."
      )
    )
  }
  
  if (!is.na(salary_percentile)) {
    pieces <- c(
      pieces,
      paste0(
        "Salary percentile ",
        round(
          salary_percentile,
          0
        ),
        "."
      )
    )
  }
  
  pieces <- c(
    pieces,
    paste0(
      "Confidence: ",
      confidence,
      "."
    )
  )
  
  paste(
    p3s11_text(
      player_name,
      "Player"
    ),
    "—",
    action,
    paste(
      pieces,
      collapse = " "
    )
  )
}


# ------------------------------------------------------------
# Build calibrated player recommendations
# ------------------------------------------------------------

build_phase3_player_recommendations <- function(
    team_name,
    season = "2025-26",
    salary_cap = NA_real_,
    con = NULL) {
  
  roster <-
    p3s11_get_integrated_team_roster(
      team_name =
        team_name,
      season =
        season,
      con =
        con
    )
  
  if (
    is.null(roster) ||
    !is.data.frame(roster) ||
    !nrow(roster)
  ) {
    return(
      data.frame()
    )
  }
  
  needs <-
    evaluate_bie_roster_needs(
      roster_players =
        roster
    )
  
  n <- nrow(roster)
  
  # ----------------------------------------------------------
  # Team-relative context
  # ----------------------------------------------------------
  
  performance_team_pct <-
    p3s11_percentile(
      roster$
        bie_performance_rating
    )
  
  minutes_team_pct <-
    p3s11_percentile(
      roster$
        minutes_per_game
    )
  
  # ----------------------------------------------------------
  # Salary context
  # ----------------------------------------------------------
  
  salary <- if (
    "cap_hit" %in%
    names(roster)
  ) {
    suppressWarnings(
      as.numeric(
        roster$cap_hit
      )
    )
  } else if (
    "base_salary" %in%
    names(roster)
  ) {
    suppressWarnings(
      as.numeric(
        roster$base_salary
      )
    )
  } else {
    rep(
      NA_real_,
      n
    )
  }
  
  salary_percentile <-
    p3s11_percentile(
      salary
    )
  
  # ----------------------------------------------------------
  # Primary team need
  # ----------------------------------------------------------
  
  primary_need <- if (
    !is.null(needs) &&
    identical(
      needs$status %||% "",
      "OK"
    )
  ) {
    p3s11_text(
      needs$primary_need,
      ""
    )
  } else {
    ""
  }
  
  # ----------------------------------------------------------
  # Output arrays
  # ----------------------------------------------------------
  
  action <- rep(
    "REVIEW",
    n
  )
  
  confidence <- rep(
    "LIMITED",
    n
  )
  
  roster_need_match <- rep(
    NA_real_,
    n
  )
  
  team_importance <- rep(
    NA_real_,
    n
  )
  
  core_role <- rep(
    FALSE,
    n
  )
  
  rationale <- rep(
    "",
    n
  )
  
  for (
    i in seq_len(n)
  ) {
    
    core_role[[i]] <-
      p3s11_is_core_role(
        primary_role =
          roster$
          primary_role[[i]],
        archetype =
          roster$
          archetype[[i]],
        role_family =
          roster$
          role_family[[i]]
      )
    
    roster_need_match[[i]] <-
      p3s11_need_fit_score(
        need_text =
          primary_need,
        scoring_score =
          roster$
          scoring_role_score[[i]],
        spacing_score =
          roster$
          spacing_role_score[[i]],
        creation_score =
          roster$
          creation_role_score[[i]],
        connector_score =
          roster$
          connector_role_score[[i]],
        defense_score =
          roster$
          defense_role_score[[i]],
        rebounding_score =
          roster$
          rebounding_role_score[[i]],
        interior_score =
          roster$
          interior_role_score[[i]]
      )
    
    team_importance[[i]] <-
      p3s11_team_importance(
        performance_team_pct =
          performance_team_pct[[i]],
        minutes_team_pct =
          minutes_team_pct[[i]],
        offensive_impact =
          roster$
          offensive_impact_score[[i]],
        defensive_impact =
          roster$
          defensive_impact_score[[i]],
        role_component =
          roster$
          role_component[[i]],
        core_role =
          core_role[[i]]
      )
    
    contract_available <-
      is.finite(
        salary[[i]]
      )
    
    needs_available <-
      !is.null(needs) &&
      identical(
        needs$status %||% "",
        "OK"
      )
    
    confidence[[i]] <-
      p3s11_confidence(
        impact_confidence =
          roster$
          impact_confidence[[i]],
        role_confidence =
          roster$
          role_confidence[[i]],
        contract_data_available =
          contract_available,
        needs_data_available =
          needs_available,
        team_importance =
          team_importance[[i]]
      )
    
    action[[i]] <-
      p3s11_player_action(
        performance_rating =
          roster$
          bie_performance_rating[[i]],
        performance_team_pct =
          performance_team_pct[[i]],
        minutes_team_pct =
          minutes_team_pct[[i]],
        team_importance =
          team_importance[[i]],
        offensive_impact =
          roster$
          offensive_impact_score[[i]],
        defensive_impact =
          roster$
          defensive_impact_score[[i]],
        role_score =
          roster$
          role_component[[i]],
        availability_score =
          roster$
          availability_component[[i]],
        core_role =
          core_role[[i]],
        age =
          roster$
          player_age[[i]],
        salary =
          salary[[i]],
        salary_percentile =
          salary_percentile[[i]],
        roster_need_match =
          roster_need_match[[i]],
        impact_confidence =
          roster$
          impact_confidence[[i]],
        contract_data_available =
          contract_available
      )
    
    rationale[[i]] <-
      p3s11_recommendation_reason(
        action =
          action[[i]],
        player_name =
          roster$
          player_name[[i]],
        performance_rating =
          roster$
          bie_performance_rating[[i]],
        performance_team_pct =
          performance_team_pct[[i]],
        minutes_team_pct =
          minutes_team_pct[[i]],
        team_importance =
          team_importance[[i]],
        impact_tier =
          roster$
          impact_tier[[i]],
        archetype =
          roster$
          archetype[[i]],
        roster_need_match =
          roster_need_match[[i]],
        salary_percentile =
          salary_percentile[[i]],
        confidence =
          confidence[[i]],
        core_role =
          core_role[[i]]
      )
  }
  
  data.frame(
    player_id =
      roster$player_id,
    player_name =
      roster$player_name,
    primary_position =
      roster$primary_position,
    recommendation =
      action,
    recommendation_confidence =
      confidence,
    core_role =
      core_role,
    team_importance_score =
      team_importance,
    team_performance_percentile =
      performance_team_pct,
    team_minutes_percentile =
      minutes_team_pct,
    bie_performance_rating =
      roster$bie_performance_rating,
    bie_performance_percentile =
      roster$bie_performance_percentile,
    impact_tier =
      roster$impact_tier,
    primary_role =
      roster$primary_role,
    archetype =
      roster$archetype,
    role_family =
      roster$role_family,
    roster_need =
      primary_need,
    roster_need_match =
      roster_need_match,
    salary =
      salary,
    salary_percentile =
      salary_percentile,
    rationale =
      rationale,
    stringsAsFactors = FALSE
  )
}


# ------------------------------------------------------------
# Acquisition target recommendation
# ------------------------------------------------------------

recommend_phase3_acquisition_target <- function(
    target_player_id,
    current_team_name,
    season = "2025-26",
    target_team_id = NULL,
    con = NULL) {
  
  result <-
    evaluate_phase3_acquisition_target(
      target_player_id =
        target_player_id,
      target_team_id =
        target_team_id,
      current_team_name =
        current_team_name,
      season =
        season,
      con =
        con
    )
  
  if (
    !identical(
      result$status %||% "",
      "OK"
    )
  ) {
    
    result$recommendation <-
      "REVIEW"
    
    return(result)
  }
  
  fit_score <-
    p3s11_num(
      result$fit_score
    )
  
  performance_rating <-
    p3s11_num(
      result$
        target_performance_rating
    )
  
  recommendation <- if (
    !is.na(fit_score) &&
    fit_score >= 75 &&
    (
      is.na(
        performance_rating
      ) ||
      performance_rating >= 60
    )
  ) {
    "TARGET"
  } else if (
    !is.na(fit_score) &&
    fit_score >= 60
  ) {
    "MONITOR"
  } else {
    "PASS / REVIEW"
  }
  
  result$recommendation <-
    recommendation
  
  result
}


# ------------------------------------------------------------
# Sanity checks
#
# Flags recommendations that should be reviewed before freeze.
# ------------------------------------------------------------

phase3_step11_sanity_check <- function(
    recommendations) {
  
  if (
    is.null(recommendations) ||
    !is.data.frame(recommendations) ||
    !nrow(recommendations)
  ) {
    return(
      data.frame()
    )
  }
  
  flags <- list()
  
  add_flag <- function(
    row,
    reason) {
    
    flags[[length(flags) + 1L]] <<-
      data.frame(
        player_id =
          row$player_id[[1]],
        player_name =
          row$player_name[[1]],
        recommendation =
          row$recommendation[[1]],
        reason =
          reason,
        stringsAsFactors = FALSE
      )
  }
  
  # Core players should never receive UPGRADE from this layer.
  bad_core <- recommendations[
    recommendations$core_role &
      recommendations$recommendation ==
      "UPGRADE",
    ,
    drop = FALSE
  ]
  
  if (nrow(bad_core)) {
    for (
      i in seq_len(
        nrow(bad_core)
      )
    ) {
      add_flag(
        bad_core[
          i,
          ,
          drop = FALSE
        ],
        "Core-role player labeled UPGRADE."
      )
    }
  }
  
  # Top workload players should not be UPGRADE.
  bad_minutes <- recommendations[
    recommendations$
      team_minutes_percentile >=
      80 &
      recommendations$
      recommendation ==
      "UPGRADE",
    ,
    drop = FALSE
  ]
  
  if (nrow(bad_minutes)) {
    for (
      i in seq_len(
        nrow(bad_minutes)
      )
    ) {
      add_flag(
        bad_minutes[
          i,
          ,
          drop = FALSE
        ],
        "Top-workload player labeled UPGRADE."
      )
    }
  }
  
  # Top team performers should not be UPGRADE.
  bad_performance <- recommendations[
    recommendations$
      team_performance_percentile >=
      75 &
      recommendations$
      recommendation ==
      "UPGRADE",
    ,
    drop = FALSE
  ]
  
  if (nrow(bad_performance)) {
    for (
      i in seq_len(
        nrow(bad_performance)
      )
    ) {
      add_flag(
        bad_performance[
          i,
          ,
          drop = FALSE
        ],
        "Top team performer labeled UPGRADE."
      )
    }
  }
  
  # SHOP requires salary data.
  bad_shop <- recommendations[
    recommendations$
      recommendation ==
      "SHOP" &
      !is.finite(
        suppressWarnings(
          as.numeric(
            recommendations$
              salary
          )
        )
      ),
    ,
    drop = FALSE
  ]
  
  if (nrow(bad_shop)) {
    for (
      i in seq_len(
        nrow(bad_shop)
      )
    ) {
      add_flag(
        bad_shop[
          i,
          ,
          drop = FALSE
        ],
        "SHOP recommendation lacks salary data."
      )
    }
  }
  
  if (!length(flags)) {
    return(
      data.frame(
        player_id =
          integer(),
        player_name =
          character(),
        recommendation =
          character(),
        reason =
          character(),
        stringsAsFactors = FALSE
      )
    )
  }
  
  unique(
    do.call(
      rbind,
      flags
    )
  )
}


# ------------------------------------------------------------
# Team priorities
# ------------------------------------------------------------

build_phase3_team_priorities <- function(
    team_name,
    season = "2025-26",
    con = NULL) {
  
  roster <-
    p3s11_get_integrated_team_roster(
      team_name =
        team_name,
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
  
  recommendations <-
    build_phase3_player_recommendations(
      team_name =
        team_name,
      season =
        season,
      con =
        con
    )
  
  review_players <- recommendations[
    recommendations$recommendation %in%
      c(
        "UPGRADE",
        "SHOP"
      ),
    ,
    drop = FALSE
  ]
  
  priority_text <- character()
  
  if (
    !is.null(needs) &&
    identical(
      needs$status %||% "",
      "OK"
    )
  ) {
    
    primary_need <-
      p3s11_text(
        needs$primary_need,
        ""
      )
    
    secondary_need <-
      p3s11_text(
        needs$secondary_need,
        ""
      )
    
    if (nzchar(primary_need)) {
      priority_text <- c(
        priority_text,
        paste0(
          "Primary roster need: ",
          primary_need
        )
      )
    }
    
    if (nzchar(secondary_need)) {
      priority_text <- c(
        priority_text,
        paste0(
          "Secondary roster need: ",
          secondary_need
        )
      )
    }
  }
  
  if (
    nrow(
      review_players
    )
  ) {
    
    priority_text <- c(
      priority_text,
      paste0(
        "Review upgrade / shop decisions for ",
        paste(
          utils::head(
            review_players$
              player_name,
            5
          ),
          collapse = ", "
        ),
        "."
      )
    )
  }
  
  list(
    status =
      "OK",
    team =
      team_name,
    season =
      season,
    priorities =
      priority_text,
    roster_needs =
      needs,
    player_recommendations =
      recommendations
  )
}


# ------------------------------------------------------------
# Full recommendation package
# ------------------------------------------------------------

run_phase3_recommendation_package <- function(
    team_name,
    season = "2025-26",
    con = NULL) {
  
  player_recommendations <-
    build_phase3_player_recommendations(
      team_name =
        team_name,
      season =
        season,
      con =
        con
    )
  
  priorities <-
    build_phase3_team_priorities(
      team_name =
        team_name,
      season =
        season,
      con =
        con
    )
  
  sanity_flags <-
    phase3_step11_sanity_check(
      player_recommendations
    )
  
  recommendation_counts <- if (
    nrow(
      player_recommendations
    )
  ) {
    
    as.data.frame(
      sort(
        table(
          player_recommendations$
            recommendation
        ),
        decreasing = TRUE
      )
    )
    
  } else {
    
    data.frame()
  }
  
  if (
    nrow(
      recommendation_counts
    )
  ) {
    names(
      recommendation_counts
    ) <- c(
      "recommendation",
      "players"
    )
  }
  
  list(
    status = if (
      nrow(
        player_recommendations
      )
    ) {
      "OK"
    } else {
      "NO RECOMMENDATIONS"
    },
    team =
      team_name,
    season =
      season,
    player_recommendations =
      player_recommendations,
    recommendation_counts =
      recommendation_counts,
    priorities =
      priorities$priorities,
    sanity_flags =
      sanity_flags,
    sanity_flag_count =
      nrow(
        sanity_flags
      ),
    recommendation_scope =
      "PHASE 3 CALIBRATED PERFORMANCE-BACKED FRONT OFFICE ADVISORY"
  )
}


# ------------------------------------------------------------
# Step 11.1 health check
# ------------------------------------------------------------

phase3_step11_healthcheck <- function(
    team_name,
    season = "2025-26") {
  
  result <-
    tryCatch(
      run_phase3_recommendation_package(
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
          "Step 11.2 — Recommendation Calibration + Self-Contained Roster",
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
  
  recs <-
    result$
    player_recommendations
  
  valid_actions <- c(
    "KEEP",
    "EXTEND",
    "SHOP",
    "UPGRADE",
    "REVIEW"
  )
  
  action_ok <- all(
    recs$recommendation %in%
      valid_actions
  )
  
  confidence_ok <- all(
    recs$
      recommendation_confidence %in%
      c(
        "HIGH",
        "MODERATE",
        "LIMITED"
      )
  )
  
  list(
    phase =
      "Phase 3",
    step =
      "Step 11.2 — Recommendation Calibration + Self-Contained Roster",
    status = if (
      nrow(recs) > 0 &&
      action_ok &&
      confidence_ok &&
      result$
      sanity_flag_count ==
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
    recommendation_rows =
      nrow(
        recs
      ),
    recommendation_types =
      sort(
        unique(
          recs$
            recommendation
        )
      ),
    high_confidence_rows =
      sum(
        recs$
          recommendation_confidence ==
          "HIGH"
      ),
    moderate_confidence_rows =
      sum(
        recs$
          recommendation_confidence ==
          "MODERATE"
      ),
    limited_confidence_rows =
      sum(
        recs$
          recommendation_confidence ==
          "LIMITED"
      ),
    sanity_flag_count =
      result$
      sanity_flag_count,
    scope =
      result$
      recommendation_scope
  )
}