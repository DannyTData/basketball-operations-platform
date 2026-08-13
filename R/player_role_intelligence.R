# ============================================================
# TBI NBA Basketball Operations Platform
# PHASE 3 — STEP 7
# Player Role Classification
#
# Purpose:
#   Convert the real Phase-3 performance signals into basketball
#   role / archetype labels that can later feed BIE.
#
# Inputs:
#   - Step 1: player_season_stats
#   - Step 4: shooting / spacing intelligence
#   - Step 5: playmaking / creation intelligence
#   - Step 6: defense / rebounding intelligence
#
# Safeguards:
#   - no tracking-based role claims are invented
#   - "two-way" requires both offense and defense evidence
#   - low-confidence samples are labeled accordingly
#   - roles are evidence-based and can be multi-dimensional
# ============================================================


# ------------------------------------------------------------
# Canonical role table
# ------------------------------------------------------------

tbi_player_role_table <- function() {
  "player_season_roles"
}


# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------

p3s7_num <- function(
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


p3s7_text <- function(
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


p3s7_weighted_mean <- function(
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


p3s7_role_strength <- function(score) {
  
  score <- p3s7_num(
    score
  )
  
  if (is.na(score)) {
    return("UNRATED")
  }
  
  if (score >= 85) {
    return("ELITE")
  }
  
  if (score >= 70) {
    return("PLUS")
  }
  
  if (score >= 55) {
    return("SOLID")
  }
  
  if (score >= 40) {
    return("LIMITED")
  }
  
  "LOW"
}


# ------------------------------------------------------------
# Create table
# ------------------------------------------------------------

create_player_season_roles_table <- function(
    con = NULL) {
  
  owns_connection <- is.null(con)
  
  if (owns_connection) {
    con <- connect_db()
  }
  
  if (owns_connection) {
    on.exit(
      disconnect_db(con),
      add = TRUE
    )
  }
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE IF NOT EXISTS player_season_roles (

      player_id INTEGER NOT NULL,
      team_id INTEGER NOT NULL,
      season TEXT NOT NULL,

      games_played INTEGER,
      minutes DOUBLE,

      primary_role TEXT,
      secondary_role TEXT,
      tertiary_role TEXT,

      archetype TEXT,
      role_family TEXT,

      scoring_role_score DOUBLE,
      spacing_role_score DOUBLE,
      creation_role_score DOUBLE,
      connector_role_score DOUBLE,
      defense_role_score DOUBLE,
      rebounding_role_score DOUBLE,
      interior_role_score DOUBLE,
      transition_role_score DOUBLE,

      offensive_role_score DOUBLE,
      defensive_role_score DOUBLE,
      two_way_role_score DOUBLE,

      role_confidence TEXT,
      evidence_count INTEGER,

      shooting_tier TEXT,
      creation_role_source TEXT,
      defensive_role_source TEXT,
      rebounding_role_source TEXT,

      source_name TEXT,
      metric_version TEXT DEFAULT 'P3S7_v1',

      imported_at TEXT DEFAULT CURRENT_TIMESTAMP,
      updated_at TEXT DEFAULT CURRENT_TIMESTAMP,

      PRIMARY KEY (
        player_id,
        team_id,
        season
      )
    )
    "
  )
  
  indexes <- c(
    "
    CREATE INDEX IF NOT EXISTS idx_player_roles_player
    ON player_season_roles(player_id)
    ",
    "
    CREATE INDEX IF NOT EXISTS idx_player_roles_team
    ON player_season_roles(team_id)
    ",
    "
    CREATE INDEX IF NOT EXISTS idx_player_roles_season
    ON player_season_roles(season)
    "
  )
  
  for (statement in indexes) {
    try(
      DBI::dbExecute(
        con,
        statement
      ),
      silent = TRUE
    )
  }
  
  invisible(TRUE)
}


# ------------------------------------------------------------
# Read source data
# ------------------------------------------------------------

get_phase3_role_source <- function(
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
  
  tables <- DBI::dbListTables(
    con
  )
  
  required <- c(
    "player_season_stats",
    "player_season_shooting",
    "player_season_playmaking",
    "player_season_defense_rebounding"
  )
  
  missing <- setdiff(
    required,
    tables
  )
  
  if (length(missing)) {
    stop(
      paste0(
        "Step 7 requires completed Phase 3 performance tables. Missing: ",
        paste(
          missing,
          collapse = ", "
        )
      )
    )
  }
  
  DBI::dbGetQuery(
    con,
    "
    SELECT
      base.player_id,
      base.team_id,
      base.season,
      base.games_played,
      base.minutes,
      base.points_per_game,

      shoot.shooting_efficiency_score,
      shoot.spacing_score,
      shoot.shooting_gravity_score,
      shoot.rim_pressure_proxy_score,
      shoot.spacing_tier,
      shoot.shooting_confidence,

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

      base.source_name

    FROM player_season_stats base

    LEFT JOIN player_season_shooting shoot
      ON shoot.player_id = base.player_id
      AND shoot.team_id = base.team_id
      AND shoot.season = base.season

    LEFT JOIN player_season_playmaking play
      ON play.player_id = base.player_id
      AND play.team_id = base.team_id
      AND play.season = base.season

    LEFT JOIN player_season_defense_rebounding dr
      ON dr.player_id = base.player_id
      AND dr.team_id = base.team_id
      AND dr.season = base.season

    WHERE base.season = ?
    ",
    params = list(
      as.character(
        season
      )
    )
  )
}


# ------------------------------------------------------------
# Scoring role score
#
# Uses available scoring production + shooting efficiency +
# rim-pressure proxy. We do not claim shot-creation type yet.
# ------------------------------------------------------------

derive_p3s7_scoring_score <- function(
    ppg,
    shooting_efficiency,
    rim_pressure) {
  
  ppg <- p3s7_num(
    ppg
  )
  
  shooting_efficiency <-
    p3s7_num(
      shooting_efficiency
    )
  
  rim_pressure <-
    p3s7_num(
      rim_pressure
    )
  
  # PPG is translated to a bounded 0-100 signal.
  ppg_score <- if (
    is.na(ppg)
  ) {
    NA_real_
  } else {
    min(
      100,
      max(
        0,
        ppg / 30 * 100
      )
    )
  }
  
  p3s7_weighted_mean(
    c(
      ppg_score,
      shooting_efficiency,
      rim_pressure
    ),
    c(
      0.46,
      0.38,
      0.16
    )
  )
}


# ------------------------------------------------------------
# Connector role score
# ------------------------------------------------------------

derive_p3s7_connector_score <- function(
    secondary_creation,
    passing_control,
    spacing) {
  
  p3s7_weighted_mean(
    c(
      secondary_creation,
      passing_control,
      spacing
    ),
    c(
      0.42,
      0.38,
      0.20
    )
  )
}


# ------------------------------------------------------------
# Choose ranked role labels
# ------------------------------------------------------------

p3s7_rank_roles <- function(
    scores) {
  
  scores <- suppressWarnings(
    as.numeric(scores)
  )
  
  names(scores) <- c(
    "SCORER",
    "SPACER",
    "CREATOR",
    "CONNECTOR",
    "DEFENDER",
    "REBOUNDER",
    "INTERIOR PRESENCE"
  )
  
  valid <- is.finite(scores)
  
  if (!any(valid)) {
    return(
      c(
        primary = "UNRATED",
        secondary = "UNRATED",
        tertiary = "UNRATED"
      )
    )
  }
  
  ordered <- sort(
    scores[valid],
    decreasing = TRUE
  )
  
  labels <- names(
    ordered
  )
  
  # Require at least a functional threshold for role claims.
  labels[
    ordered < 45
  ] <- "LIMITED ROLE SIGNAL"
  
  labels <- c(
    labels,
    rep(
      "UNRATED",
      3
    )
  )
  
  c(
    primary = labels[[1]],
    secondary = labels[[2]],
    tertiary = labels[[3]]
  )
}


# ------------------------------------------------------------
# Archetype construction
# ------------------------------------------------------------

p3s7_archetype <- function(
    scoring,
    spacing,
    creation,
    connector,
    defense,
    rebounding,
    interior) {
  
  scoring <- p3s7_num(scoring)
  spacing <- p3s7_num(spacing)
  creation <- p3s7_num(creation)
  connector <- p3s7_num(connector)
  defense <- p3s7_num(defense)
  rebounding <- p3s7_num(rebounding)
  interior <- p3s7_num(interior)
  
  # Two-way combinations.
  if (
    !is.na(scoring) &&
    scoring >= 70 &&
    !is.na(defense) &&
    defense >= 70
  ) {
    return("TWO-WAY SCORING IMPACT")
  }
  
  if (
    !is.na(spacing) &&
    spacing >= 70 &&
    !is.na(defense) &&
    defense >= 70
  ) {
    return("3-AND-D IMPACT")
  }
  
  if (
    !is.na(creation) &&
    creation >= 75 &&
    !is.na(scoring) &&
    scoring >= 65
  ) {
    return("PRIMARY OFFENSIVE ENGINE")
  }
  
  if (
    !is.na(creation) &&
    creation >= 65 &&
    !is.na(spacing) &&
    spacing >= 60
  ) {
    return("SECONDARY CREATOR / SPACER")
  }
  
  if (
    !is.na(connector) &&
    connector >= 70 &&
    !is.na(defense) &&
    defense >= 60
  ) {
    return("TWO-WAY CONNECTOR")
  }
  
  if (
    !is.na(interior) &&
    interior >= 72 &&
    !is.na(rebounding) &&
    rebounding >= 68
  ) {
    return("INTERIOR ANCHOR PROFILE")
  }
  
  if (
    !is.na(rebounding) &&
    rebounding >= 75 &&
    !is.na(defense) &&
    defense >= 55
  ) {
    return("DEFENSIVE REBOUNDER")
  }
  
  if (
    !is.na(spacing) &&
    spacing >= 75
  ) {
    return("SPACING SPECIALIST")
  }
  
  if (
    !is.na(scoring) &&
    scoring >= 70
  ) {
    return("SCORING SPECIALIST")
  }
  
  if (
    !is.na(creation) &&
    creation >= 70
  ) {
    return("CREATION SPECIALIST")
  }
  
  if (
    !is.na(defense) &&
    defense >= 70
  ) {
    return("DEFENSIVE SPECIALIST")
  }
  
  if (
    !is.na(connector) &&
    connector >= 65
  ) {
    return("CONNECTOR")
  }
  
  if (
    !is.na(rebounding) &&
    rebounding >= 68
  ) {
    return("REBOUNDING SPECIALIST")
  }
  
  "ROLE PLAYER / MIXED PROFILE"
}


# ------------------------------------------------------------
# Role family
# ------------------------------------------------------------

p3s7_role_family <- function(
    offensive_score,
    defensive_score,
    two_way_score) {
  
  offensive_score <- p3s7_num(
    offensive_score
  )
  
  defensive_score <- p3s7_num(
    defensive_score
  )
  
  two_way_score <- p3s7_num(
    two_way_score
  )
  
  if (
    !is.na(two_way_score) &&
    two_way_score >= 70
  ) {
    return("TWO-WAY")
  }
  
  if (
    !is.na(offensive_score) &&
    offensive_score >= 65 &&
    (
      is.na(defensive_score) ||
      offensive_score >=
      defensive_score + 10
    )
  ) {
    return("OFFENSIVE")
  }
  
  if (
    !is.na(defensive_score) &&
    defensive_score >= 65 &&
    (
      is.na(offensive_score) ||
      defensive_score >=
      offensive_score + 10
    )
  ) {
    return("DEFENSIVE")
  }
  
  "BALANCED / SUPPORT"
}


# ------------------------------------------------------------
# Confidence roll-up
# ------------------------------------------------------------

p3s7_role_confidence <- function(
    games,
    minutes,
    shooting_confidence,
    playmaking_confidence,
    defense_confidence,
    evidence_count) {
  
  games <- p3s7_num(
    games,
    0
  )
  
  minutes <- p3s7_num(
    minutes,
    0
  )
  
  evidence_count <- p3s7_num(
    evidence_count,
    0
  )
  
  source_confidence <- c(
    p3s7_text(
      shooting_confidence,
      "LIMITED"
    ),
    p3s7_text(
      playmaking_confidence,
      "LIMITED"
    ),
    p3s7_text(
      defense_confidence,
      "LIMITED"
    )
  )
  
  high_count <- sum(
    source_confidence ==
      "HIGH"
  )
  
  moderate_or_high <- sum(
    source_confidence %in%
      c(
        "HIGH",
        "MODERATE"
      )
  )
  
  if (
    games >= 50 &&
    minutes >= 1200 &&
    evidence_count >= 5 &&
    high_count >= 2
  ) {
    return("HIGH")
  }
  
  if (
    games >= 25 &&
    minutes >= 500 &&
    evidence_count >= 4 &&
    moderate_or_high >= 2
  ) {
    return("MODERATE")
  }
  
  "LIMITED"
}


# ------------------------------------------------------------
# Derive role classifications
# ------------------------------------------------------------

derive_phase3_player_roles <- function(
    source_data) {
  
  if (
    is.null(source_data) ||
    !is.data.frame(source_data) ||
    !nrow(source_data)
  ) {
    return(
      data.frame()
    )
  }
  
  d <- source_data
  n <- nrow(d)
  
  d$scoring_role_score <- rep(
    NA_real_,
    n
  )
  
  d$spacing_role_score <- suppressWarnings(
    as.numeric(
      d$spacing_score
    )
  )
  
  d$creation_role_score <- suppressWarnings(
    as.numeric(
      d$creation_score
    )
  )
  
  d$connector_role_score <- rep(
    NA_real_,
    n
  )
  
  d$defense_role_score <- suppressWarnings(
    as.numeric(
      d$defense_proxy_score
    )
  )
  
  d$rebounding_role_score <- suppressWarnings(
    as.numeric(
      d$rebounding_score
    )
  )
  
  d$interior_role_score <- suppressWarnings(
    as.numeric(
      d$interior_impact_score
    )
  )
  
  # Transition play is not available yet.
  d$transition_role_score <- NA_real_
  
  d$offensive_role_score <- rep(
    NA_real_,
    n
  )
  
  d$defensive_role_score <- rep(
    NA_real_,
    n
  )
  
  d$two_way_role_score <- rep(
    NA_real_,
    n
  )
  
  d$primary_role <- rep(
    "UNRATED",
    n
  )
  
  d$secondary_role <- rep(
    "UNRATED",
    n
  )
  
  d$tertiary_role <- rep(
    "UNRATED",
    n
  )
  
  d$archetype <- rep(
    "ROLE PLAYER / MIXED PROFILE",
    n
  )
  
  d$role_family <- rep(
    "BALANCED / SUPPORT",
    n
  )
  
  d$evidence_count <- rep(
    0L,
    n
  )
  
  d$role_confidence <- rep(
    "LIMITED",
    n
  )
  
  for (
    i in seq_len(n)
  ) {
    
    d$scoring_role_score[[i]] <-
      derive_p3s7_scoring_score(
        ppg =
          d$points_per_game[[i]],
        shooting_efficiency =
          d$shooting_efficiency_score[[i]],
        rim_pressure =
          d$rim_pressure_proxy_score[[i]]
      )
    
    d$connector_role_score[[i]] <-
      derive_p3s7_connector_score(
        secondary_creation =
          d$secondary_creation_score[[i]],
        passing_control =
          d$passing_control_score[[i]],
        spacing =
          d$spacing_score[[i]]
      )
    
    offensive_components <- c(
      d$scoring_role_score[[i]],
      d$spacing_role_score[[i]],
      d$creation_role_score[[i]],
      d$connector_role_score[[i]]
    )
    
    d$offensive_role_score[[i]] <-
      p3s7_weighted_mean(
        offensive_components,
        c(
          0.30,
          0.25,
          0.30,
          0.15
        )
      )
    
    defensive_components <- c(
      d$defense_role_score[[i]],
      d$rebounding_role_score[[i]],
      d$interior_role_score[[i]]
    )
    
    d$defensive_role_score[[i]] <-
      p3s7_weighted_mean(
        defensive_components,
        c(
          0.60,
          0.22,
          0.18
        )
      )
    
    d$two_way_role_score[[i]] <-
      if (
        is.finite(
          d$offensive_role_score[[i]]
        ) &&
        is.finite(
          d$defensive_role_score[[i]]
        )
      ) {
        min(
          d$offensive_role_score[[i]],
          d$defensive_role_score[[i]]
        )
      } else {
        NA_real_
      }
    
    role_scores <- c(
      d$scoring_role_score[[i]],
      d$spacing_role_score[[i]],
      d$creation_role_score[[i]],
      d$connector_role_score[[i]],
      d$defense_role_score[[i]],
      d$rebounding_role_score[[i]],
      d$interior_role_score[[i]]
    )
    
    ranked <- p3s7_rank_roles(
      role_scores
    )
    
    d$primary_role[[i]] <-
      ranked[["primary"]]
    
    d$secondary_role[[i]] <-
      ranked[["secondary"]]
    
    d$tertiary_role[[i]] <-
      ranked[["tertiary"]]
    
    d$archetype[[i]] <-
      p3s7_archetype(
        scoring =
          d$scoring_role_score[[i]],
        spacing =
          d$spacing_role_score[[i]],
        creation =
          d$creation_role_score[[i]],
        connector =
          d$connector_role_score[[i]],
        defense =
          d$defense_role_score[[i]],
        rebounding =
          d$rebounding_role_score[[i]],
        interior =
          d$interior_role_score[[i]]
      )
    
    d$role_family[[i]] <-
      p3s7_role_family(
        offensive_score =
          d$offensive_role_score[[i]],
        defensive_score =
          d$defensive_role_score[[i]],
        two_way_score =
          d$two_way_role_score[[i]]
      )
    
    evidence <- c(
      d$scoring_role_score[[i]],
      d$spacing_role_score[[i]],
      d$creation_role_score[[i]],
      d$connector_role_score[[i]],
      d$defense_role_score[[i]],
      d$rebounding_role_score[[i]],
      d$interior_role_score[[i]]
    )
    
    d$evidence_count[[i]] <-
      sum(
        is.finite(
          evidence
        )
      )
    
    d$role_confidence[[i]] <-
      p3s7_role_confidence(
        games =
          d$games_played[[i]],
        minutes =
          d$minutes[[i]],
        shooting_confidence =
          d$shooting_confidence[[i]],
        playmaking_confidence =
          d$playmaking_confidence[[i]],
        defense_confidence =
          d$defense_confidence[[i]],
        evidence_count =
          d$evidence_count[[i]]
      )
  }
  
  d$shooting_tier <-
    as.character(
      d$spacing_tier
    )
  
  d$creation_role_source <-
    as.character(
      d$creation_role
    )
  
  d$defensive_role_source <-
    as.character(
      d$defensive_role
    )
  
  d$rebounding_role_source <-
    as.character(
      d$rebounding_role
    )
  
  d$metric_version <-
    "P3S7_v1"
  
  d$updated_at <-
    Sys.time()
  
  keep <- c(
    "player_id",
    "team_id",
    "season",
    "games_played",
    "minutes",
    "primary_role",
    "secondary_role",
    "tertiary_role",
    "archetype",
    "role_family",
    "scoring_role_score",
    "spacing_role_score",
    "creation_role_score",
    "connector_role_score",
    "defense_role_score",
    "rebounding_role_score",
    "interior_role_score",
    "transition_role_score",
    "offensive_role_score",
    "defensive_role_score",
    "two_way_role_score",
    "role_confidence",
    "evidence_count",
    "shooting_tier",
    "creation_role_source",
    "defensive_role_source",
    "rebounding_role_source",
    "source_name",
    "metric_version",
    "updated_at"
  )
  
  d[
    ,
    keep,
    drop = FALSE
  ]
}


# ------------------------------------------------------------
# SQLite-safe upsert
# ------------------------------------------------------------

upsert_phase3_player_roles <- function(
    df,
    con = NULL) {
  
  if (
    is.null(df) ||
    !is.data.frame(df) ||
    !nrow(df)
  ) {
    return(
      invisible(
        list(
          rows_processed = 0L
        )
      )
    )
  }
  
  owns_connection <- is.null(con)
  
  if (owns_connection) {
    con <- connect_db()
  }
  
  if (owns_connection) {
    on.exit(
      disconnect_db(con),
      add = TRUE
    )
  }
  
  create_player_season_roles_table(
    con
  )
  
  temp_table <- paste0(
    "tmp_player_roles_",
    as.integer(
      Sys.time()
    ),
    "_",
    sample.int(
      999999L,
      1L
    )
  )
  
  DBI::dbWriteTable(
    con,
    temp_table,
    df,
    temporary = TRUE,
    overwrite = TRUE
  )
  
  temp_q <- as.character(
    DBI::dbQuoteIdentifier(
      con,
      temp_table
    )
  )
  
  columns_q <- paste(
    as.character(
      DBI::dbQuoteIdentifier(
        con,
        names(df)
      )
    ),
    collapse = ", "
  )
  
  DBI::dbBegin(con)
  
  tryCatch(
    {
      
      DBI::dbExecute(
        con,
        paste0(
          "
          DELETE FROM player_season_roles
          WHERE EXISTS (
            SELECT 1
            FROM ",
          temp_q,
          " src
            WHERE src.player_id = player_season_roles.player_id
              AND src.team_id = player_season_roles.team_id
              AND src.season = player_season_roles.season
          )
          "
        )
      )
      
      DBI::dbExecute(
        con,
        paste0(
          "
          INSERT INTO player_season_roles (
            ",
          columns_q,
          "
          )
          SELECT
            ",
          columns_q,
          "
          FROM ",
          temp_q
        )
      )
      
      DBI::dbCommit(con)
      
    },
    error = function(e) {
      
      try(
        DBI::dbRollback(con),
        silent = TRUE
      )
      
      stop(e)
    }
  )
  
  invisible(
    list(
      rows_processed =
        nrow(df),
      seasons =
        unique(
          df$season
        ),
      method =
        "SQLITE TRANSACTIONAL UPSERT"
    )
  )
}


# ------------------------------------------------------------
# Build season
# ------------------------------------------------------------

build_phase3_player_roles <- function(
    season = "2025-26",
    con = NULL) {
  
  owns_connection <- is.null(con)
  
  if (owns_connection) {
    con <- connect_db()
  }
  
  if (owns_connection) {
    on.exit(
      disconnect_db(con),
      add = TRUE
    )
  }
  
  source <-
    get_phase3_role_source(
      season =
        season,
      con =
        con
    )
  
  if (!nrow(source)) {
    
    return(
      invisible(
        list(
          status =
            "NO PERFORMANCE DATA",
          rows_processed =
            0L
        )
      )
    )
  }
  
  derived <-
    derive_phase3_player_roles(
      source
    )
  
  result <-
    upsert_phase3_player_roles(
      derived,
      con = con
    )
  
  invisible(
    c(
      list(
        status =
          "PLAYER ROLES BUILT"
      ),
      result
    )
  )
}


# ------------------------------------------------------------
# Read player roles
# ------------------------------------------------------------

get_phase3_player_roles <- function(
    season = NULL,
    team_name = NULL,
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
  
  if (
    !tbi_player_role_table() %in%
    DBI::dbListTables(con)
  ) {
    return(
      data.frame()
    )
  }
  
  where <- character()
  params <- list()
  
  if (!is.null(season)) {
    where <- c(
      where,
      "role.season = ?"
    )
    params <- c(
      params,
      list(
        as.character(
          season
        )
      )
    )
  }
  
  if (!is.null(team_name)) {
    where <- c(
      where,
      "t.team_name = ?"
    )
    params <- c(
      params,
      list(
        as.character(
          team_name
        )
      )
    )
  }
  
  if (!is.null(player_id)) {
    where <- c(
      where,
      "role.player_id = ?"
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
  
  where_sql <- if (
    length(where)
  ) {
    paste(
      "WHERE",
      paste(
        where,
        collapse = " AND "
      )
    )
  } else {
    ""
  }
  
  sql <- paste0(
    "
    SELECT
      role.*,
      p.player_name,
      p.primary_position,
      p.player_age,
      t.team_name,
      t.abbreviation

    FROM player_season_roles role

    LEFT JOIN players p
      ON p.player_id = role.player_id

    LEFT JOIN teams t
      ON t.team_id = role.team_id

    ",
    where_sql,
    "

    ORDER BY
      role.two_way_role_score DESC,
      role.offensive_role_score DESC,
      role.defensive_role_score DESC,
      p.player_name
    "
  )
  
  DBI::dbGetQuery(
    con,
    sql,
    params = params
  )
}


# ------------------------------------------------------------
# Inspect top archetypes / roles
# ------------------------------------------------------------

phase3_step7_role_summary <- function(
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
  
  roles <- get_phase3_player_roles(
    season =
      season,
    con =
      con
  )
  
  if (!nrow(roles)) {
    return(
      list(
        archetypes =
          data.frame(),
        primary_roles =
          data.frame()
      )
    )
  }
  
  archetypes <- as.data.frame(
    sort(
      table(
        roles$archetype
      ),
      decreasing = TRUE
    )
  )
  
  names(archetypes) <- c(
    "archetype",
    "players"
  )
  
  primary_roles <- as.data.frame(
    sort(
      table(
        roles$primary_role
      ),
      decreasing = TRUE
    )
  )
  
  names(primary_roles) <- c(
    "primary_role",
    "players"
  )
  
  list(
    archetypes =
      archetypes,
    primary_roles =
      primary_roles
  )
}


# ------------------------------------------------------------
# Step 7 health check
# ------------------------------------------------------------

phase3_step7_healthcheck <- function(
    season = "2025-26") {
  
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
  
  exists_table <-
    tbi_player_role_table() %in%
    tables
  
  rows <- if (
    exists_table
  ) {
    DBI::dbGetQuery(
      con,
      "
      SELECT COUNT(*) AS n
      FROM player_season_roles
      WHERE season = ?
      ",
      params = list(
        season
      )
    )$n[[1]]
  } else {
    0L
  }
  
  classified <- if (
    exists_table &&
    rows > 0
  ) {
    DBI::dbGetQuery(
      con,
      "
      SELECT COUNT(*) AS n
      FROM player_season_roles
      WHERE season = ?
        AND primary_role IS NOT NULL
        AND primary_role <> 'UNRATED'
      ",
      params = list(
        season
      )
    )$n[[1]]
  } else {
    0L
  }
  
  high_confidence <- if (
    exists_table &&
    rows > 0
  ) {
    DBI::dbGetQuery(
      con,
      "
      SELECT COUNT(*) AS n
      FROM player_season_roles
      WHERE season = ?
        AND role_confidence = 'HIGH'
      ",
      params = list(
        season
      )
    )$n[[1]]
  } else {
    0L
  }
  
  list(
    phase = "Phase 3",
    step =
      "Step 7 — Player Role Classification",
    status = if (
      rows > 0 &&
      classified > 0
    ) {
      "READY"
    } else {
      "BUILD REQUIRED"
    },
    season =
      season,
    role_rows =
      rows,
    classified_rows =
      classified,
    high_confidence_rows =
      high_confidence,
    transition_role_status =
      "NOT LOADED",
    tracking_role_status =
      "NOT LOADED — NO TRACKING-SPECIFIC ARCHETYPES"
  )
}

# ============================================================
# PHASE 3.2 — STEP 7 V2
# Player Role Classification
#
# Fixes:
#   - preserves the raw Step-6 defense proxy instead of
#     overwriting it with NA before role construction
#   - keeps output defensive_role_score as the combined
#     defense/rebounding/interior role signal
# ============================================================


derive_phase3_player_roles <- function(
    source_data) {
  
  if (
    is.null(source_data) ||
    !is.data.frame(source_data) ||
    !nrow(source_data)
  ) {
    return(
      data.frame()
    )
  }
  
  d <- source_data
  n <- nrow(d)
  
  raw_defense_proxy <-
    suppressWarnings(
      as.numeric(
        d$defense_proxy_score
      )
    )
  
  # Preserve the raw Step-6 defensive proxy in the canonical
  # Step-7 output column expected by the schema.
  d$defense_role_score <-
    raw_defense_proxy
  
  d$scoring_role_score <- rep(
    NA_real_,
    n
  )
  
  d$spacing_role_score <-
    suppressWarnings(
      as.numeric(
        d$spacing_score
      )
    )
  
  d$creation_role_score <-
    suppressWarnings(
      as.numeric(
        d$creation_score
      )
    )
  
  d$connector_role_score <- rep(
    NA_real_,
    n
  )
  
  d$rebounding_role_score <-
    suppressWarnings(
      as.numeric(
        d$rebounding_score
      )
    )
  
  d$interior_role_score <-
    suppressWarnings(
      as.numeric(
        d$interior_impact_score
      )
    )
  
  d$transition_role_score <- NA_real_
  
  d$offensive_role_score <- rep(
    NA_real_,
    n
  )
  
  d$defensive_role_score <- rep(
    NA_real_,
    n
  )
  
  d$two_way_role_score <- rep(
    NA_real_,
    n
  )
  
  d$primary_role <- rep(
    "UNRATED",
    n
  )
  
  d$secondary_role <- rep(
    "UNRATED",
    n
  )
  
  d$tertiary_role <- rep(
    "UNRATED",
    n
  )
  
  d$archetype <- rep(
    "ROLE PLAYER / MIXED PROFILE",
    n
  )
  
  d$role_family <- rep(
    "BALANCED / SUPPORT",
    n
  )
  
  d$evidence_count <- rep(
    0L,
    n
  )
  
  d$role_confidence <- rep(
    "LIMITED",
    n
  )
  
  for (i in seq_len(n)) {
    
    d$scoring_role_score[[i]] <-
      derive_p3s7_scoring_score(
        ppg =
          d$points_per_game[[i]],
        shooting_efficiency =
          d$shooting_efficiency_score[[i]],
        rim_pressure =
          d$rim_pressure_proxy_score[[i]]
      )
    
    d$connector_role_score[[i]] <-
      derive_p3s7_connector_score(
        secondary_creation =
          d$secondary_creation_score[[i]],
        passing_control =
          d$passing_control_score[[i]],
        spacing =
          d$spacing_score[[i]]
      )
    
    d$offensive_role_score[[i]] <-
      p3s7_weighted_mean(
        c(
          d$scoring_role_score[[i]],
          d$spacing_role_score[[i]],
          d$creation_role_score[[i]],
          d$connector_role_score[[i]]
        ),
        c(
          0.34,
          0.22,
          0.30,
          0.14
        )
      )
    
    d$defensive_role_score[[i]] <-
      p3s7_weighted_mean(
        c(
          raw_defense_proxy[[i]],
          d$rebounding_role_score[[i]],
          d$interior_role_score[[i]]
        ),
        c(
          0.68,
          0.18,
          0.14
        )
      )
    
    d$two_way_role_score[[i]] <-
      if (
        is.finite(
          d$offensive_role_score[[i]]
        ) &&
        is.finite(
          d$defensive_role_score[[i]]
        )
      ) {
        min(
          d$offensive_role_score[[i]],
          d$defensive_role_score[[i]]
        )
      } else {
        NA_real_
      }
    
    role_scores <- c(
      d$scoring_role_score[[i]],
      d$spacing_role_score[[i]],
      d$creation_role_score[[i]],
      d$connector_role_score[[i]],
      d$defensive_role_score[[i]],
      d$rebounding_role_score[[i]],
      d$interior_role_score[[i]]
    )
    
    ranked <- p3s7_rank_roles(
      role_scores
    )
    
    d$primary_role[[i]] <-
      ranked[["primary"]]
    
    d$secondary_role[[i]] <-
      ranked[["secondary"]]
    
    d$tertiary_role[[i]] <-
      ranked[["tertiary"]]
    
    d$archetype[[i]] <-
      p3s7_archetype(
        scoring =
          d$scoring_role_score[[i]],
        spacing =
          d$spacing_role_score[[i]],
        creation =
          d$creation_role_score[[i]],
        connector =
          d$connector_role_score[[i]],
        defense =
          d$defensive_role_score[[i]],
        rebounding =
          d$rebounding_role_score[[i]],
        interior =
          d$interior_role_score[[i]]
      )
    
    d$role_family[[i]] <-
      p3s7_role_family(
        offensive_score =
          d$offensive_role_score[[i]],
        defensive_score =
          d$defensive_role_score[[i]],
        two_way_score =
          d$two_way_role_score[[i]]
      )
    
    evidence <- c(
      d$scoring_role_score[[i]],
      d$spacing_role_score[[i]],
      d$creation_role_score[[i]],
      d$connector_role_score[[i]],
      d$defensive_role_score[[i]],
      d$rebounding_role_score[[i]],
      d$interior_role_score[[i]]
    )
    
    d$evidence_count[[i]] <-
      sum(
        is.finite(
          evidence
        )
      )
    
    d$role_confidence[[i]] <-
      p3s7_role_confidence(
        games =
          d$games_played[[i]],
        minutes =
          d$minutes[[i]],
        shooting_confidence =
          d$shooting_confidence[[i]],
        playmaking_confidence =
          d$playmaking_confidence[[i]],
        defense_confidence =
          d$defense_confidence[[i]],
        evidence_count =
          d$evidence_count[[i]]
      )
  }
  
  d$shooting_tier <-
    as.character(
      d$spacing_tier
    )
  
  d$creation_role_source <-
    as.character(
      d$creation_role
    )
  
  d$defensive_role_source <-
    as.character(
      d$defensive_role
    )
  
  d$rebounding_role_source <-
    as.character(
      d$rebounding_role
    )
  
  d$metric_version <-
    "P3S7_v2_CALIBRATED"
  
  d$updated_at <-
    Sys.time()
  
  keep <- c(
    "player_id",
    "team_id",
    "season",
    "games_played",
    "minutes",
    "primary_role",
    "secondary_role",
    "tertiary_role",
    "archetype",
    "role_family",
    "scoring_role_score",
    "spacing_role_score",
    "creation_role_score",
    "connector_role_score",
    "defense_role_score",
    "rebounding_role_score",
    "interior_role_score",
    "transition_role_score",
    "offensive_role_score",
    "defensive_role_score",
    "two_way_role_score",
    "role_confidence",
    "evidence_count",
    "shooting_tier",
    "creation_role_source",
    "defensive_role_source",
    "rebounding_role_source",
    "source_name",
    "metric_version",
    "updated_at"
  )
  
  d[
    ,
    unique(keep),
    drop = FALSE
  ]
}


phase3_step7_v2_healthcheck <- function(
    season = "2025-26") {
  
  source <-
    get_phase3_role_source(
      season = season
    )
  
  d <-
    derive_phase3_player_roles(
      source
    )
  
  list(
    phase = "Phase 3.2",
    step =
      "Step 7 — Player Role Classification V2",
    status = if (
      nrow(d) > 0 &&
      all(
        is.finite(
          d$defensive_role_score[
            is.finite(
              d$rebounding_role_score
            )
          ]
        )
      )
    ) {
      "READY FOR REBUILD"
    } else {
      "REVIEW"
    },
    rows = nrow(d),
    classified_rows =
      sum(
        d$primary_role !=
          "UNRATED"
      ),
    metric_version =
      "P3S7_v2_CALIBRATED",
    bug_fix =
      "RAW DEFENSE PROXY IS NO LONGER OVERWRITTEN BEFORE ROLE CALCULATION"
  )
}