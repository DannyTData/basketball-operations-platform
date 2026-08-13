# ============================================================
# TBI NBA Basketball Operations Platform
# PHASE 3 — STEP 2
# Per-Possession + Advanced Metrics Architecture
#
# Purpose:
#   Create and maintain the canonical advanced player-season
#   metrics table that sits on top of player_season_stats.
#
# Design:
#   - one row per player / team / season
#   - joins directly to player_season_stats keys
#   - stores possession-normalized production
#   - stores efficiency / rate metrics
#   - stores advanced impact fields when supplied later
#   - safe to run before real NBA data is loaded
#   - safe to run repeatedly
# ============================================================


# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------

tbi_advanced_stats_table <- function() {
  "player_season_advanced"
}


tbi_adv_safe_num <- function(
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


tbi_adv_safe_divide <- function(
    numerator,
    denominator,
    default = NA_real_) {
  
  numerator <- suppressWarnings(
    as.numeric(numerator)
  )
  
  denominator <- suppressWarnings(
    as.numeric(denominator)
  )
  
  n <- max(
    length(numerator),
    length(denominator)
  )
  
  numerator <- rep(
    numerator,
    length.out = n
  )
  
  denominator <- rep(
    denominator,
    length.out = n
  )
  
  output <- rep(
    default,
    n
  )
  
  valid <-
    is.finite(numerator) &
    is.finite(denominator) &
    denominator != 0
  
  output[valid] <-
    numerator[valid] /
    denominator[valid]
  
  output
}


tbi_adv_clamp <- function(
    x,
    lower = 0,
    upper = 1) {
  
  value <- suppressWarnings(
    as.numeric(x)
  )
  
  pmin(
    upper,
    pmax(
      lower,
      value
    )
  )
}


# ------------------------------------------------------------
# Create canonical advanced metrics table
# ------------------------------------------------------------

create_player_season_advanced_table <- function(
    con = NULL) {
  
  owns_connection <- is.null(con)
  
  if (owns_connection) {
    
    if (
      !exists(
        "connect_db",
        mode = "function"
      )
    ) {
      stop(
        "connect_db() is not available. Load the TBI project before running this function."
      )
    }
    
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
    CREATE TABLE IF NOT EXISTS player_season_advanced (

      player_id BIGINT NOT NULL,
      team_id BIGINT NOT NULL,
      season VARCHAR NOT NULL,

      estimated_possessions DOUBLE,
      team_estimated_possessions DOUBLE,

      points_per_100 DOUBLE,
      rebounds_per_100 DOUBLE,
      assists_per_100 DOUBLE,
      steals_per_100 DOUBLE,
      blocks_per_100 DOUBLE,
      turnovers_per_100 DOUBLE,

      offensive_rebounds_per_100 DOUBLE,
      defensive_rebounds_per_100 DOUBLE,

      field_goal_pct DOUBLE,
      three_point_pct DOUBLE,
      free_throw_pct DOUBLE,

      effective_field_goal_pct DOUBLE,
      true_shooting_pct DOUBLE,

      assist_turnover_ratio DOUBLE,
      three_point_attempt_rate DOUBLE,
      free_throw_rate DOUBLE,

      usage_rate DOUBLE,
      assist_pct DOUBLE,
      rebound_pct DOUBLE,
      offensive_rebound_pct DOUBLE,
      defensive_rebound_pct DOUBLE,
      steal_pct DOUBLE,
      block_pct DOUBLE,
      turnover_pct DOUBLE,

      offensive_rating DOUBLE,
      defensive_rating DOUBLE,
      net_rating DOUBLE,

      player_efficiency_rating DOUBLE,
      win_shares DOUBLE,
      win_shares_per_48 DOUBLE,
      box_plus_minus DOUBLE,
      offensive_box_plus_minus DOUBLE,
      defensive_box_plus_minus DOUBLE,
      value_over_replacement DOUBLE,

      on_court_net_rating DOUBLE,
      on_off_net_rating DOUBLE,

      pace DOUBLE,

      source_name VARCHAR,
      source_player_id VARCHAR,

      metric_version VARCHAR DEFAULT 'P3S2_v1',

      imported_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

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
    CREATE INDEX IF NOT EXISTS idx_player_advanced_player
    ON player_season_advanced(player_id)
    ",
    "
    CREATE INDEX IF NOT EXISTS idx_player_advanced_team
    ON player_season_advanced(team_id)
    ",
    "
    CREATE INDEX IF NOT EXISTS idx_player_advanced_season
    ON player_season_advanced(season)
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
# Evolve schema safely
# ------------------------------------------------------------

ensure_player_season_advanced_schema <- function(
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
  
  create_player_season_advanced_table(
    con
  )
  
  existing_fields <- DBI::dbListFields(
    con,
    tbi_advanced_stats_table()
  )
  
  required_columns <- list(
    player_id = "BIGINT",
    team_id = "BIGINT",
    season = "VARCHAR",
    estimated_possessions = "DOUBLE",
    team_estimated_possessions = "DOUBLE",
    points_per_100 = "DOUBLE",
    rebounds_per_100 = "DOUBLE",
    assists_per_100 = "DOUBLE",
    steals_per_100 = "DOUBLE",
    blocks_per_100 = "DOUBLE",
    turnovers_per_100 = "DOUBLE",
    offensive_rebounds_per_100 = "DOUBLE",
    defensive_rebounds_per_100 = "DOUBLE",
    field_goal_pct = "DOUBLE",
    three_point_pct = "DOUBLE",
    free_throw_pct = "DOUBLE",
    effective_field_goal_pct = "DOUBLE",
    true_shooting_pct = "DOUBLE",
    assist_turnover_ratio = "DOUBLE",
    three_point_attempt_rate = "DOUBLE",
    free_throw_rate = "DOUBLE",
    usage_rate = "DOUBLE",
    assist_pct = "DOUBLE",
    rebound_pct = "DOUBLE",
    offensive_rebound_pct = "DOUBLE",
    defensive_rebound_pct = "DOUBLE",
    steal_pct = "DOUBLE",
    block_pct = "DOUBLE",
    turnover_pct = "DOUBLE",
    offensive_rating = "DOUBLE",
    defensive_rating = "DOUBLE",
    net_rating = "DOUBLE",
    player_efficiency_rating = "DOUBLE",
    win_shares = "DOUBLE",
    win_shares_per_48 = "DOUBLE",
    box_plus_minus = "DOUBLE",
    offensive_box_plus_minus = "DOUBLE",
    defensive_box_plus_minus = "DOUBLE",
    value_over_replacement = "DOUBLE",
    on_court_net_rating = "DOUBLE",
    on_off_net_rating = "DOUBLE",
    pace = "DOUBLE",
    source_name = "VARCHAR",
    source_player_id = "VARCHAR",
    metric_version = "VARCHAR",
    imported_at = "TIMESTAMP",
    updated_at = "TIMESTAMP"
  )
  
  missing_columns <- setdiff(
    names(required_columns),
    existing_fields
  )
  
  for (column in missing_columns) {
    
    DBI::dbExecute(
      con,
      paste(
        "ALTER TABLE",
        tbi_advanced_stats_table(),
        "ADD COLUMN",
        column,
        required_columns[[column]]
      )
    )
  }
  
  invisible(
    list(
      table =
        tbi_advanced_stats_table(),
      added_columns =
        missing_columns,
      fields =
        DBI::dbListFields(
          con,
          tbi_advanced_stats_table()
        )
    )
  )
}


# ------------------------------------------------------------
# Estimate possessions from individual box-score totals
# ------------------------------------------------------------
#
# Uses the common possession proxy:
#   FGA + 0.44*FTA - ORB + TOV
#
# This is an estimate only. When official possession data is loaded,
# that value should replace this estimate.
# ------------------------------------------------------------

estimate_player_possessions <- function(df) {
  
  if (
    is.null(df) ||
    !is.data.frame(df)
  ) {
    return(numeric())
  }
  
  fga <- if (
    "field_goals_attempted" %in%
    names(df)
  ) {
    suppressWarnings(
      as.numeric(
        df$field_goals_attempted
      )
    )
  } else {
    rep(
      NA_real_,
      nrow(df)
    )
  }
  
  fta <- if (
    "free_throws_attempted" %in%
    names(df)
  ) {
    suppressWarnings(
      as.numeric(
        df$free_throws_attempted
      )
    )
  } else {
    rep(
      NA_real_,
      nrow(df)
    )
  }
  
  orb <- if (
    "offensive_rebounds" %in%
    names(df)
  ) {
    suppressWarnings(
      as.numeric(
        df$offensive_rebounds
      )
    )
  } else {
    rep(
      0,
      nrow(df)
    )
  }
  
  tov <- if (
    "turnovers" %in%
    names(df)
  ) {
    suppressWarnings(
      as.numeric(
        df$turnovers
      )
    )
  } else {
    rep(
      0,
      nrow(df)
    )
  }
  
  possessions <-
    fga +
    0.44 * fta -
    orb +
    tov
  
  possessions[
    !is.finite(possessions) |
      possessions <= 0
  ] <- NA_real_
  
  possessions
}


# ------------------------------------------------------------
# Derive advanced metrics from Step-1 counting stats
# ------------------------------------------------------------

derive_player_advanced_metrics <- function(
    player_stats) {
  
  if (
    is.null(player_stats) ||
    !is.data.frame(player_stats)
  ) {
    stop(
      "derive_player_advanced_metrics() requires a data.frame."
    )
  }
  
  d <- player_stats
  
  required_keys <- c(
    "player_id",
    "team_id",
    "season"
  )
  
  missing_keys <- setdiff(
    required_keys,
    names(d)
  )
  
  if (length(missing_keys)) {
    stop(
      paste0(
        "Missing required key columns: ",
        paste(
          missing_keys,
          collapse = ", "
        )
      )
    )
  }
  
  numeric_or_na <- function(column) {
    
    if (column %in% names(d)) {
      suppressWarnings(
        as.numeric(
          d[[column]]
        )
      )
    } else {
      rep(
        NA_real_,
        nrow(d)
      )
    }
  }
  
  fgm <- numeric_or_na(
    "field_goals_made"
  )
  
  fga <- numeric_or_na(
    "field_goals_attempted"
  )
  
  three_pm <- numeric_or_na(
    "three_pointers_made"
  )
  
  three_pa <- numeric_or_na(
    "three_pointers_attempted"
  )
  
  ftm <- numeric_or_na(
    "free_throws_made"
  )
  
  fta <- numeric_or_na(
    "free_throws_attempted"
  )
  
  points <- numeric_or_na(
    "points"
  )
  
  rebounds <- numeric_or_na(
    "rebounds"
  )
  
  orb <- numeric_or_na(
    "offensive_rebounds"
  )
  
  drb <- numeric_or_na(
    "defensive_rebounds"
  )
  
  assists <- numeric_or_na(
    "assists"
  )
  
  steals <- numeric_or_na(
    "steals"
  )
  
  blocks <- numeric_or_na(
    "blocks"
  )
  
  turnovers <- numeric_or_na(
    "turnovers"
  )
  
  possessions <- if (
    "estimated_possessions" %in%
    names(d)
  ) {
    suppressWarnings(
      as.numeric(
        d$estimated_possessions
      )
    )
  } else {
    estimate_player_possessions(
      d
    )
  }
  
  output <- data.frame(
    player_id =
      suppressWarnings(
        as.integer(
          d$player_id
        )
      ),
    team_id =
      suppressWarnings(
        as.integer(
          d$team_id
        )
      ),
    season =
      as.character(
        d$season
      ),
    estimated_possessions =
      possessions,
    stringsAsFactors = FALSE
  )
  
  output$points_per_100 <-
    100 *
    tbi_adv_safe_divide(
      points,
      possessions
    )
  
  output$rebounds_per_100 <-
    100 *
    tbi_adv_safe_divide(
      rebounds,
      possessions
    )
  
  output$assists_per_100 <-
    100 *
    tbi_adv_safe_divide(
      assists,
      possessions
    )
  
  output$steals_per_100 <-
    100 *
    tbi_adv_safe_divide(
      steals,
      possessions
    )
  
  output$blocks_per_100 <-
    100 *
    tbi_adv_safe_divide(
      blocks,
      possessions
    )
  
  output$turnovers_per_100 <-
    100 *
    tbi_adv_safe_divide(
      turnovers,
      possessions
    )
  
  output$offensive_rebounds_per_100 <-
    100 *
    tbi_adv_safe_divide(
      orb,
      possessions
    )
  
  output$defensive_rebounds_per_100 <-
    100 *
    tbi_adv_safe_divide(
      drb,
      possessions
    )
  
  output$field_goal_pct <-
    tbi_adv_safe_divide(
      fgm,
      fga
    )
  
  output$three_point_pct <-
    tbi_adv_safe_divide(
      three_pm,
      three_pa
    )
  
  output$free_throw_pct <-
    tbi_adv_safe_divide(
      ftm,
      fta
    )
  
  output$effective_field_goal_pct <-
    tbi_adv_safe_divide(
      fgm +
        0.5 * three_pm,
      fga
    )
  
  output$true_shooting_pct <-
    tbi_adv_safe_divide(
      points,
      2 *
        (
          fga +
            0.44 * fta
        )
    )
  
  output$assist_turnover_ratio <-
    tbi_adv_safe_divide(
      assists,
      turnovers
    )
  
  output$three_point_attempt_rate <-
    tbi_adv_safe_divide(
      three_pa,
      fga
    )
  
  output$free_throw_rate <-
    tbi_adv_safe_divide(
      fta,
      fga
    )
  
  # ----------------------------------------------------------
  # Metrics that require team / opponent context
  # ----------------------------------------------------------
  # These remain NA until official or richer contextual data is
  # loaded. We do not fabricate them from incomplete inputs.
  # ----------------------------------------------------------
  
  output$team_estimated_possessions <- NA_real_
  
  output$usage_rate <- NA_real_
  output$assist_pct <- NA_real_
  output$rebound_pct <- NA_real_
  output$offensive_rebound_pct <- NA_real_
  output$defensive_rebound_pct <- NA_real_
  output$steal_pct <- NA_real_
  output$block_pct <- NA_real_
  output$turnover_pct <- NA_real_
  
  output$offensive_rating <- NA_real_
  output$defensive_rating <- NA_real_
  output$net_rating <- NA_real_
  
  output$player_efficiency_rating <- NA_real_
  output$win_shares <- NA_real_
  output$win_shares_per_48 <- NA_real_
  output$box_plus_minus <- NA_real_
  output$offensive_box_plus_minus <- NA_real_
  output$defensive_box_plus_minus <- NA_real_
  output$value_over_replacement <- NA_real_
  
  output$on_court_net_rating <- NA_real_
  output$on_off_net_rating <- NA_real_
  
  output$pace <- NA_real_
  
  output$source_name <- if (
    "source_name" %in%
    names(d)
  ) {
    as.character(
      d$source_name
    )
  } else {
    NA_character_
  }
  
  output$source_player_id <- if (
    "source_player_id" %in%
    names(d)
  ) {
    as.character(
      d$source_player_id
    )
  } else {
    NA_character_
  }
  
  output$metric_version <-
    "P3S2_v1"
  
  output$updated_at <-
    Sys.time()
  
  output
}


# ------------------------------------------------------------
# Standardize externally supplied advanced metrics
# ------------------------------------------------------------

standardize_player_advanced_metrics <- function(df) {
  
  if (
    is.null(df) ||
    !is.data.frame(df)
  ) {
    stop(
      "standardize_player_advanced_metrics() requires a data.frame."
    )
  }
  
  d <- df
  
  expected_columns <- c(
    "player_id",
    "team_id",
    "season",
    "estimated_possessions",
    "team_estimated_possessions",
    "points_per_100",
    "rebounds_per_100",
    "assists_per_100",
    "steals_per_100",
    "blocks_per_100",
    "turnovers_per_100",
    "offensive_rebounds_per_100",
    "defensive_rebounds_per_100",
    "field_goal_pct",
    "three_point_pct",
    "free_throw_pct",
    "effective_field_goal_pct",
    "true_shooting_pct",
    "assist_turnover_ratio",
    "three_point_attempt_rate",
    "free_throw_rate",
    "usage_rate",
    "assist_pct",
    "rebound_pct",
    "offensive_rebound_pct",
    "defensive_rebound_pct",
    "steal_pct",
    "block_pct",
    "turnover_pct",
    "offensive_rating",
    "defensive_rating",
    "net_rating",
    "player_efficiency_rating",
    "win_shares",
    "win_shares_per_48",
    "box_plus_minus",
    "offensive_box_plus_minus",
    "defensive_box_plus_minus",
    "value_over_replacement",
    "on_court_net_rating",
    "on_off_net_rating",
    "pace",
    "source_name",
    "source_player_id",
    "metric_version",
    "updated_at"
  )
  
  for (
    column in setdiff(
      expected_columns,
      names(d)
    )
  ) {
    d[[column]] <- NA
  }
  
  d$player_id <- suppressWarnings(
    as.integer(
      d$player_id
    )
  )
  
  d$team_id <- suppressWarnings(
    as.integer(
      d$team_id
    )
  )
  
  d$season <- as.character(
    d$season
  )
  
  character_columns <- c(
    "source_name",
    "source_player_id",
    "metric_version"
  )
  
  numeric_columns <- setdiff(
    expected_columns,
    c(
      "player_id",
      "team_id",
      "season",
      character_columns,
      "updated_at"
    )
  )
  
  for (column in numeric_columns) {
    
    d[[column]] <- suppressWarnings(
      as.numeric(
        d[[column]]
      )
    )
  }
  
  for (column in character_columns) {
    
    d[[column]] <- as.character(
      d[[column]]
    )
  }
  
  d$metric_version[
    is.na(
      d$metric_version
    ) |
      !nzchar(
        trimws(
          d$metric_version
        )
      )
  ] <- "P3S2_v1"
  
  d$updated_at <- Sys.time()
  
  d[
    ,
    expected_columns,
    drop = FALSE
  ]
}


# ------------------------------------------------------------
# Validate advanced metrics
# ------------------------------------------------------------

validate_player_advanced_metrics <- function(df) {
  
  issues <- character()
  
  if (
    is.null(df) ||
    !is.data.frame(df)
  ) {
    return(
      list(
        valid = FALSE,
        issues =
          "Input is not a data.frame."
      )
    )
  }
  
  required <- c(
    "player_id",
    "team_id",
    "season"
  )
  
  missing <- setdiff(
    required,
    names(df)
  )
  
  if (length(missing)) {
    
    issues <- c(
      issues,
      paste0(
        "Missing required columns: ",
        paste(
          missing,
          collapse = ", "
        )
      )
    )
  }
  
  if (
    all(
      required %in%
      names(df)
    )
  ) {
    
    if (any(is.na(df$player_id))) {
      issues <- c(
        issues,
        "At least one row has a missing player_id."
      )
    }
    
    if (any(is.na(df$team_id))) {
      issues <- c(
        issues,
        "At least one row has a missing team_id."
      )
    }
    
    if (
      any(
        is.na(df$season) |
        !nzchar(
          trimws(
            as.character(
              df$season
            )
          )
        )
      )
    ) {
      issues <- c(
        issues,
        "At least one row has a missing season."
      )
    }
    
    duplicate_key <- duplicated(
      paste(
        df$player_id,
        df$team_id,
        df$season,
        sep = "|"
      )
    )
    
    if (any(duplicate_key)) {
      issues <- c(
        issues,
        paste0(
          sum(duplicate_key),
          " duplicate player/team/season rows were found."
        )
      )
    }
  }
  
  pct_columns <- intersect(
    c(
      "field_goal_pct",
      "three_point_pct",
      "free_throw_pct",
      "effective_field_goal_pct",
      "true_shooting_pct",
      "three_point_attempt_rate",
      "free_throw_rate"
    ),
    names(df)
  )
  
  for (column in pct_columns) {
    
    value <- suppressWarnings(
      as.numeric(
        df[[column]]
      )
    )
    
    if (
      any(
        is.finite(value) &
        value < 0
      )
    ) {
      issues <- c(
        issues,
        paste0(
          column,
          " contains negative values."
        )
      )
    }
  }
  
  list(
    valid = !length(issues),
    issues = unique(
      issues
    )
  )
}


# ------------------------------------------------------------
# Upsert advanced metrics
# ------------------------------------------------------------

upsert_player_advanced_metrics <- function(
    df,
    con = NULL) {
  
  standardized <-
    standardize_player_advanced_metrics(
      df
    )
  
  validation <-
    validate_player_advanced_metrics(
      standardized
    )
  
  if (!validation$valid) {
    stop(
      paste(
        validation$issues,
        collapse = "\n"
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
  
  ensure_player_season_advanced_schema(
    con
  )
  
  if (!nrow(standardized)) {
    return(
      invisible(
        list(
          rows_processed = 0L,
          players = 0L,
          seasons = character()
        )
      )
    )
  }
  
  temp_table <- paste0(
    "tmp_player_season_advanced_",
    as.integer(Sys.time()),
    "_",
    sample.int(999999L, 1L)
  )
  
  DBI::dbWriteTable(
    con,
    temp_table,
    standardized,
    temporary = TRUE,
    overwrite = TRUE
  )
  
  table_q <- as.character(
    DBI::dbQuoteIdentifier(
      con,
      temp_table
    )
  )
  
  columns <- names(
    standardized
  )
  
  quoted_columns <- paste(
    as.character(
      DBI::dbQuoteIdentifier(
        con,
        columns
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
          DELETE FROM player_season_advanced
          WHERE EXISTS (
            SELECT 1
            FROM ",
          table_q,
          " AS src
            WHERE src.player_id = player_season_advanced.player_id
              AND src.team_id = player_season_advanced.team_id
              AND src.season = player_season_advanced.season
          )
          "
        )
      )
      
      DBI::dbExecute(
        con,
        paste0(
          "
          INSERT INTO player_season_advanced (
            ",
          quoted_columns,
          "
          )
          SELECT
            ",
          quoted_columns,
          "
          FROM ",
          table_q
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
  
  try(
    DBI::dbExecute(
      con,
      paste(
        "DROP TABLE IF EXISTS",
        table_q
      )
    ),
    silent = TRUE
  )
  
  invisible(
    list(
      rows_processed =
        nrow(standardized),
      players =
        length(
          unique(
            standardized$player_id
          )
        ),
      seasons =
        unique(
          standardized$season
        ),
      database_method =
        "SQLITE TRANSACTIONAL UPSERT"
    )
  )
}

# ------------------------------------------------------------
# Build Step-2 metrics directly from Step-1 stats
# ------------------------------------------------------------

build_advanced_metrics_from_player_stats <- function(
    season = NULL,
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
  
  if (
    !exists(
      "get_player_season_stats",
      mode = "function"
    )
  ) {
    stop(
      "get_player_season_stats() is not available. Ensure Phase 3 Step 1 is loaded."
    )
  }
  
  base_stats <- get_player_season_stats(
    season = season,
    con = con
  )
  
  if (!nrow(base_stats)) {
    
    return(
      invisible(
        list(
          rows_processed = 0L,
          status =
            "NO STEP-1 DATA LOADED"
        )
      )
    )
  }
  
  derived <-
    derive_player_advanced_metrics(
      base_stats
    )
  
  upsert_player_advanced_metrics(
    derived,
    con = con
  )
  
  invisible(
    list(
      rows_processed =
        nrow(derived),
      seasons =
        unique(
          derived$season
        ),
      status =
        "ADVANCED METRICS BUILT"
    )
  )
}


# ------------------------------------------------------------
# Read advanced metrics
# ------------------------------------------------------------

get_player_advanced_metrics <- function(
    season = NULL,
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
  
  if (
    !tbi_advanced_stats_table() %in%
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
      "adv.season = ?"
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
  
  if (!is.null(team_id)) {
    
    where <- c(
      where,
      "adv.team_id = ?"
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
      "adv.player_id = ?"
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
      adv.*,
      p.player_name,
      p.primary_position,
      p.player_age,
      t.team_name,
      t.abbreviation

    FROM player_season_advanced adv

    LEFT JOIN players p
      ON p.player_id = adv.player_id

    LEFT JOIN teams t
      ON t.team_id = adv.team_id

    ",
    where_sql,
    "

    ORDER BY
      adv.season DESC,
      adv.points_per_100 DESC NULLS LAST,
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
# Unified Step-1 + Step-2 player data frame
# ------------------------------------------------------------

get_bie_player_performance_data <- function(
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
  
  step1_exists <-
    "player_season_stats" %in%
    DBI::dbListTables(con)
  
  step2_exists <-
    tbi_advanced_stats_table() %in%
    DBI::dbListTables(con)
  
  if (
    !step1_exists ||
    !step2_exists
  ) {
    return(
      data.frame()
    )
  }
  
  DBI::dbGetQuery(
    con,
    "
    SELECT
      p.player_id,
      p.player_name,
      p.primary_position,
      p.player_age,
      p.height_inches,
      p.weight_lbs,

      t.team_id,
      t.team_name,
      t.abbreviation,

      base.season,
      base.games_played,
      base.games_started,
      base.minutes,
      base.minutes_per_game,
      base.points,
      base.points_per_game,
      base.rebounds,
      base.rebounds_per_game,
      base.assists,
      base.assists_per_game,
      base.steals,
      base.steals_per_game,
      base.blocks,
      base.blocks_per_game,
      base.turnovers,
      base.turnovers_per_game,

      base.field_goal_pct,
      base.three_point_pct,
      base.free_throw_pct,

      adv.estimated_possessions,
      adv.points_per_100,
      adv.rebounds_per_100,
      adv.assists_per_100,
      adv.steals_per_100,
      adv.blocks_per_100,
      adv.turnovers_per_100,

      adv.effective_field_goal_pct,
      adv.true_shooting_pct,
      adv.assist_turnover_ratio,
      adv.three_point_attempt_rate,
      adv.free_throw_rate,

      adv.usage_rate,
      adv.assist_pct,
      adv.rebound_pct,
      adv.offensive_rebound_pct,
      adv.defensive_rebound_pct,
      adv.steal_pct,
      adv.block_pct,
      adv.turnover_pct,

      adv.offensive_rating,
      adv.defensive_rating,
      adv.net_rating,

      adv.player_efficiency_rating,
      adv.win_shares,
      adv.win_shares_per_48,
      adv.box_plus_minus,
      adv.offensive_box_plus_minus,
      adv.defensive_box_plus_minus,
      adv.value_over_replacement,

      adv.on_court_net_rating,
      adv.on_off_net_rating,
      adv.pace,

      adv.metric_version

    FROM player_season_stats base

    INNER JOIN player_season_advanced adv
      ON adv.player_id = base.player_id
      AND adv.team_id = base.team_id
      AND adv.season = base.season

    INNER JOIN players p
      ON p.player_id = base.player_id

    INNER JOIN teams t
      ON t.team_id = base.team_id

    WHERE t.team_name = ?
      AND base.season = ?

    ORDER BY
      base.minutes_per_game DESC NULLS LAST,
      adv.points_per_100 DESC NULLS LAST,
      p.player_name
    ",
    params = list(
      as.character(
        team_name
      ),
      as.character(
        season
      )
    )
  )
}


# ------------------------------------------------------------
# Phase 3 Step 2 health check
# ------------------------------------------------------------

phase3_step2_healthcheck <- function() {
  
  if (
    !exists(
      "connect_db",
      mode = "function"
    )
  ) {
    return(
      list(
        status = "REVIEW",
        issue =
          "connect_db() is not loaded."
      )
    )
  }
  
  con <- connect_db()
  
  on.exit(
    disconnect_db(con),
    add = TRUE
  )
  
  ensure_player_season_advanced_schema(
    con
  )
  
  tables <- DBI::dbListTables(
    con
  )
  
  step1_ready <-
    "player_season_stats" %in%
    tables
  
  step2_ready <-
    tbi_advanced_stats_table() %in%
    tables
  
  fields <- if (
    step2_ready
  ) {
    DBI::dbListFields(
      con,
      tbi_advanced_stats_table()
    )
  } else {
    character()
  }
  
  required_fields <- c(
    "player_id",
    "team_id",
    "season",
    "estimated_possessions",
    "points_per_100",
    "rebounds_per_100",
    "assists_per_100",
    "effective_field_goal_pct",
    "true_shooting_pct",
    "assist_turnover_ratio",
    "three_point_attempt_rate",
    "free_throw_rate",
    "usage_rate",
    "offensive_rating",
    "defensive_rating",
    "net_rating"
  )
  
  missing <- setdiff(
    required_fields,
    fields
  )
  
  step1_rows <- if (
    step1_ready
  ) {
    DBI::dbGetQuery(
      con,
      "
      SELECT COUNT(*) AS n
      FROM player_season_stats
      "
    )$n[[1]]
  } else {
    0L
  }
  
  step2_rows <- if (
    step2_ready
  ) {
    DBI::dbGetQuery(
      con,
      "
      SELECT COUNT(*) AS n
      FROM player_season_advanced
      "
    )$n[[1]]
  } else {
    0L
  }
  
  list(
    phase = "Phase 3",
    step =
      "Step 2 — Per-Possession + Advanced Metrics Architecture",
    status = if (
      step1_ready &&
      step2_ready &&
      !length(missing)
    ) {
      "READY"
    } else {
      "REVIEW"
    },
    step1_table_ready =
      step1_ready,
    step2_table_ready =
      step2_ready,
    advanced_table =
      tbi_advanced_stats_table(),
    advanced_fields =
      length(fields),
    missing_required_fields =
      missing,
    step1_rows_loaded =
      step1_rows,
    step2_rows_loaded =
      step2_rows,
    data_status = if (
      step1_rows > 0 &&
      step2_rows > 0
    ) {
      "STEP 1 + STEP 2 DATA LOADED"
    } else if (
      step1_rows > 0
    ) {
      "STEP 1 DATA READY — BUILD STEP 2 METRICS"
    } else {
      "ARCHITECTURE READY — DATA CAN BE LOADED LATER"
    }
  )
}

# ============================================================
# PHASE 3.2 — STEP 2 V2
# Correct possession-normalized player metrics
#
# Permanent correction:
#   - team possessions are estimated from TEAM totals
#   - player on-court possessions are estimated from team pace
#     and player minutes
#   - per-100 metrics use estimated on-court possessions
#   - usage is derived from player share of team usage events
#   - metrics requiring opponent / play-by-play / official
#     advanced context remain NA unless externally supplied
#   - rebuilding derived metrics preserves previously loaded
#     non-NA official advanced fields
# ============================================================


p3s2_v2_num <- function(x) {
  suppressWarnings(as.numeric(x))
}


p3s2_v2_team_context <- function(d) {
  
  numeric_or_zero <- function(column) {
    if (column %in% names(d)) {
      x <- suppressWarnings(as.numeric(d[[column]]))
      x[!is.finite(x)] <- 0
      x
    } else {
      rep(0, nrow(d))
    }
  }
  
  fga <- numeric_or_zero("field_goals_attempted")
  fta <- numeric_or_zero("free_throws_attempted")
  orb <- numeric_or_zero("offensive_rebounds")
  tov <- numeric_or_zero("turnovers")
  minutes <- numeric_or_zero("minutes")
  
  team_key <- paste(
    d$team_id,
    d$season,
    sep = "|"
  )
  
  rows <- split(
    seq_len(nrow(d)),
    team_key
  )
  
  team_poss <- rep(NA_real_, nrow(d))
  team_minutes <- rep(NA_real_, nrow(d))
  team_usage_events <- rep(NA_real_, nrow(d))
  
  for (idx in rows) {
    
    tm_fga <- sum(fga[idx], na.rm = TRUE)
    tm_fta <- sum(fta[idx], na.rm = TRUE)
    tm_orb <- sum(orb[idx], na.rm = TRUE)
    tm_tov <- sum(tov[idx], na.rm = TRUE)
    tm_minutes <- sum(minutes[idx], na.rm = TRUE)
    
    poss <-
      tm_fga +
      0.44 * tm_fta -
      tm_orb +
      tm_tov
    
    usage_events <-
      tm_fga +
      0.44 * tm_fta +
      tm_tov
    
    if (!is.finite(poss) || poss <= 0) {
      poss <- NA_real_
    }
    
    if (!is.finite(tm_minutes) || tm_minutes <= 0) {
      tm_minutes <- NA_real_
    }
    
    if (!is.finite(usage_events) || usage_events <= 0) {
      usage_events <- NA_real_
    }
    
    team_poss[idx] <- poss
    team_minutes[idx] <- tm_minutes
    team_usage_events[idx] <- usage_events
  }
  
  data.frame(
    team_estimated_possessions = team_poss,
    team_player_minutes = team_minutes,
    team_usage_events = team_usage_events
  )
}


estimate_player_possessions <- function(df) {
  
  if (
    is.null(df) ||
    !is.data.frame(df) ||
    !nrow(df)
  ) {
    return(numeric())
  }
  
  ctx <- p3s2_v2_team_context(df)
  
  player_minutes <- if ("minutes" %in% names(df)) {
    suppressWarnings(as.numeric(df$minutes))
  } else {
    rep(NA_real_, nrow(df))
  }
  
  team_game_minutes <-
    ctx$team_player_minutes / 5
  
  possessions <-
    ctx$team_estimated_possessions *
    tbi_adv_safe_divide(
      player_minutes,
      team_game_minutes
    )
  
  possessions[
    !is.finite(possessions) |
      possessions <= 0
  ] <- NA_real_
  
  possessions
}


derive_player_advanced_metrics <- function(
    player_stats) {
  
  if (
    is.null(player_stats) ||
    !is.data.frame(player_stats)
  ) {
    stop(
      "derive_player_advanced_metrics() requires a data.frame."
    )
  }
  
  d <- player_stats
  
  required_keys <- c(
    "player_id",
    "team_id",
    "season"
  )
  
  missing_keys <- setdiff(
    required_keys,
    names(d)
  )
  
  if (length(missing_keys)) {
    stop(
      paste0(
        "Missing required key columns: ",
        paste(missing_keys, collapse = ", ")
      )
    )
  }
  
  numeric_or_na <- function(column) {
    if (column %in% names(d)) {
      suppressWarnings(as.numeric(d[[column]]))
    } else {
      rep(NA_real_, nrow(d))
    }
  }
  
  fgm <- numeric_or_na("field_goals_made")
  fga <- numeric_or_na("field_goals_attempted")
  three_pm <- numeric_or_na("three_pointers_made")
  three_pa <- numeric_or_na("three_pointers_attempted")
  ftm <- numeric_or_na("free_throws_made")
  fta <- numeric_or_na("free_throws_attempted")
  points <- numeric_or_na("points")
  rebounds <- numeric_or_na("rebounds")
  orb <- numeric_or_na("offensive_rebounds")
  drb <- numeric_or_na("defensive_rebounds")
  assists <- numeric_or_na("assists")
  steals <- numeric_or_na("steals")
  blocks <- numeric_or_na("blocks")
  turnovers <- numeric_or_na("turnovers")
  minutes <- numeric_or_na("minutes")
  
  ctx <- p3s2_v2_team_context(d)
  
  team_game_minutes <-
    ctx$team_player_minutes / 5
  
  player_possessions <-
    ctx$team_estimated_possessions *
    tbi_adv_safe_divide(
      minutes,
      team_game_minutes
    )
  
  player_possessions[
    !is.finite(player_possessions) |
      player_possessions <= 0
  ] <- NA_real_
  
  player_usage_events <-
    fga +
    0.44 * fta +
    turnovers
  
  usage_rate <-
    tbi_adv_safe_divide(
      player_usage_events *
        team_game_minutes,
      minutes *
        ctx$team_usage_events
    )
  
  usage_rate[
    !is.finite(usage_rate) |
      usage_rate < 0 |
      usage_rate > 1
  ] <- NA_real_
  
  output <- data.frame(
    player_id =
      suppressWarnings(as.integer(d$player_id)),
    team_id =
      suppressWarnings(as.integer(d$team_id)),
    season =
      as.character(d$season),
    estimated_possessions =
      player_possessions,
    team_estimated_possessions =
      ctx$team_estimated_possessions,
    stringsAsFactors = FALSE
  )
  
  output$points_per_100 <-
    100 *
    tbi_adv_safe_divide(
      points,
      player_possessions
    )
  
  output$rebounds_per_100 <-
    100 *
    tbi_adv_safe_divide(
      rebounds,
      player_possessions
    )
  
  output$assists_per_100 <-
    100 *
    tbi_adv_safe_divide(
      assists,
      player_possessions
    )
  
  output$steals_per_100 <-
    100 *
    tbi_adv_safe_divide(
      steals,
      player_possessions
    )
  
  output$blocks_per_100 <-
    100 *
    tbi_adv_safe_divide(
      blocks,
      player_possessions
    )
  
  output$turnovers_per_100 <-
    100 *
    tbi_adv_safe_divide(
      turnovers,
      player_possessions
    )
  
  output$offensive_rebounds_per_100 <-
    100 *
    tbi_adv_safe_divide(
      orb,
      player_possessions
    )
  
  output$defensive_rebounds_per_100 <-
    100 *
    tbi_adv_safe_divide(
      drb,
      player_possessions
    )
  
  output$field_goal_pct <-
    tbi_adv_safe_divide(fgm, fga)
  
  output$three_point_pct <-
    tbi_adv_safe_divide(three_pm, three_pa)
  
  output$free_throw_pct <-
    tbi_adv_safe_divide(ftm, fta)
  
  output$effective_field_goal_pct <-
    tbi_adv_safe_divide(
      fgm + 0.5 * three_pm,
      fga
    )
  
  output$true_shooting_pct <-
    tbi_adv_safe_divide(
      points,
      2 * (
        fga +
          0.44 * fta
      )
    )
  
  output$assist_turnover_ratio <-
    tbi_adv_safe_divide(
      assists,
      turnovers
    )
  
  output$three_point_attempt_rate <-
    tbi_adv_safe_divide(
      three_pa,
      fga
    )
  
  output$free_throw_rate <-
    tbi_adv_safe_divide(
      fta,
      fga
    )
  
  # Usage can be estimated from player/team offensive events.
  output$usage_rate <- usage_rate
  
  # These require teammate/opponent or official advanced context.
  # They remain NA until an actual source supplies them.
  output$assist_pct <- NA_real_
  output$rebound_pct <- NA_real_
  output$offensive_rebound_pct <- NA_real_
  output$defensive_rebound_pct <- NA_real_
  output$steal_pct <- NA_real_
  output$block_pct <- NA_real_
  output$turnover_pct <- NA_real_
  
  output$offensive_rating <- NA_real_
  output$defensive_rating <- NA_real_
  output$net_rating <- NA_real_
  
  output$player_efficiency_rating <- NA_real_
  output$win_shares <- NA_real_
  output$win_shares_per_48 <- NA_real_
  output$box_plus_minus <- NA_real_
  output$offensive_box_plus_minus <- NA_real_
  output$defensive_box_plus_minus <- NA_real_
  output$value_over_replacement <- NA_real_
  
  output$on_court_net_rating <- NA_real_
  output$on_off_net_rating <- NA_real_
  output$pace <- NA_real_
  
  output$source_name <- if (
    "source_name" %in% names(d)
  ) {
    as.character(d$source_name)
  } else {
    NA_character_
  }
  
  output$source_player_id <- if (
    "source_player_id" %in% names(d)
  ) {
    as.character(d$source_player_id)
  } else {
    NA_character_
  }
  
  output$metric_version <-
    "P3S2_v2_TEAM_PACE"
  
  output$updated_at <-
    Sys.time()
  
  output
}


p3s2_v2_preserve_external_metrics <- function(
    derived,
    existing) {
  
  if (
    is.null(existing) ||
    !is.data.frame(existing) ||
    !nrow(existing)
  ) {
    return(derived)
  }
  
  keys <- c(
    "player_id",
    "team_id",
    "season"
  )
  
  preserve <- c(
    "assist_pct",
    "rebound_pct",
    "offensive_rebound_pct",
    "defensive_rebound_pct",
    "steal_pct",
    "block_pct",
    "turnover_pct",
    "offensive_rating",
    "defensive_rating",
    "net_rating",
    "player_efficiency_rating",
    "win_shares",
    "win_shares_per_48",
    "box_plus_minus",
    "offensive_box_plus_minus",
    "defensive_box_plus_minus",
    "value_over_replacement",
    "on_court_net_rating",
    "on_off_net_rating",
    "pace"
  )
  
  match_key <- paste(
    derived$player_id,
    derived$team_id,
    derived$season,
    sep = "|"
  )
  
  existing_key <- paste(
    existing$player_id,
    existing$team_id,
    existing$season,
    sep = "|"
  )
  
  idx <- match(
    match_key,
    existing_key
  )
  
  for (column in intersect(preserve, names(existing))) {
    
    if (!column %in% names(derived)) {
      derived[[column]] <- NA_real_
    }
    
    old <- suppressWarnings(
      as.numeric(
        existing[[column]][idx]
      )
    )
    
    use_old <-
      is.finite(old)
    
    derived[[column]][use_old] <-
      old[use_old]
  }
  
  derived
}


phase3_step2_v2_sanity <- function(
    df) {
  
  if (
    is.null(df) ||
    !is.data.frame(df) ||
    !nrow(df)
  ) {
    return(
      list(
        valid = FALSE,
        issues = "No rows."
      )
    )
  }
  
  issues <- character()
  
  mins <- if ("minutes" %in% names(df)) {
    suppressWarnings(as.numeric(df$minutes))
  } else {
    rep(NA_real_, nrow(df))
  }
  
  qualified <-
    is.finite(mins) &
    mins >= 100
  
  checks <- list(
    points_per_100 = c(0, 80),
    rebounds_per_100 = c(0, 40),
    assists_per_100 = c(0, 30),
    steals_per_100 = c(0, 8),
    blocks_per_100 = c(0, 8),
    turnovers_per_100 = c(0, 15)
  )
  
  for (nm in names(checks)) {
    
    if (!nm %in% names(df)) {
      next
    }
    
    x <- suppressWarnings(
      as.numeric(df[[nm]])
    )
    
    bad <-
      qualified &
      is.finite(x) &
      (
        x < checks[[nm]][[1]] |
          x > checks[[nm]][[2]]
      )
    
    if (any(bad, na.rm = TRUE)) {
      issues <- c(
        issues,
        paste0(
          nm,
          ": ",
          sum(bad, na.rm = TRUE),
          " implausible qualified-player rows"
        )
      )
    }
  }
  
  list(
    valid = !length(issues),
    issues = issues
  )
}


build_advanced_metrics_from_player_stats <- function(
    season = NULL,
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
  
  if (
    !exists(
      "get_player_season_stats",
      mode = "function"
    )
  ) {
    stop(
      "get_player_season_stats() is not available. Ensure Phase 3 Step 1 is loaded."
    )
  }
  
  base_stats <- get_player_season_stats(
    season = season,
    con = con
  )
  
  if (!nrow(base_stats)) {
    return(
      invisible(
        list(
          rows_processed = 0L,
          status =
            "NO STEP-1 DATA LOADED"
        )
      )
    )
  }
  
  derived <-
    derive_player_advanced_metrics(
      base_stats
    )
  
  existing <- if (
    tbi_advanced_stats_table() %in%
    DBI::dbListTables(con)
  ) {
    get_player_advanced_metrics(
      season = season,
      con = con
    )
  } else {
    data.frame()
  }
  
  derived <-
    p3s2_v2_preserve_external_metrics(
      derived,
      existing
    )
  
  # Attach minutes only for sanity checking.
  sanity_frame <- merge(
    derived,
    base_stats[
      ,
      intersect(
        c(
          "player_id",
          "team_id",
          "season",
          "minutes"
        ),
        names(base_stats)
      ),
      drop = FALSE
    ],
    by = c(
      "player_id",
      "team_id",
      "season"
    ),
    all.x = TRUE,
    sort = FALSE
  )
  
  sanity <-
    phase3_step2_v2_sanity(
      sanity_frame
    )
  
  if (!sanity$valid) {
    stop(
      paste(
        c(
          "Step 2 V2 sanity check failed.",
          sanity$issues
        ),
        collapse = "\n"
      )
    )
  }
  
  upsert_player_advanced_metrics(
    derived,
    con = con
  )
  
  invisible(
    list(
      rows_processed =
        nrow(derived),
      seasons =
        unique(derived$season),
      status =
        "ADVANCED METRICS V2 BUILT",
      metric_version =
        "P3S2_v2_TEAM_PACE"
    )
  )
}


preview_phase3_step2_v2 <- function(
    season = "2025-26",
    player_names = NULL) {
  
  con <- connect_db(
    read_only = TRUE
  )
  
  on.exit(
    disconnect_db(con),
    add = TRUE
  )
  
  base <- get_player_season_stats(
    season = season,
    con = con
  )
  
  d <- derive_player_advanced_metrics(
    base
  )
  
  players <- DBI::dbGetQuery(
    con,
    "
    SELECT player_id, player_name
    FROM players
    "
  )
  
  d <- merge(
    d,
    players,
    by = "player_id",
    all.x = TRUE,
    sort = FALSE
  )
  
  if (!is.null(player_names)) {
    d <- d[
      tolower(d$player_name) %in%
        tolower(player_names),
      ,
      drop = FALSE
    ]
  }
  
  keep <- intersect(
    c(
      "player_name",
      "team_id",
      "season",
      "estimated_possessions",
      "team_estimated_possessions",
      "points_per_100",
      "rebounds_per_100",
      "assists_per_100",
      "steals_per_100",
      "blocks_per_100",
      "turnovers_per_100",
      "true_shooting_pct",
      "usage_rate",
      "defensive_rating",
      "defensive_box_plus_minus",
      "metric_version"
    ),
    names(d)
  )
  
  d[, keep, drop = FALSE]
}


phase3_step2_v2_healthcheck <- function(
    season = "2025-26") {
  
  con <- connect_db(
    read_only = TRUE
  )
  
  on.exit(
    disconnect_db(con),
    add = TRUE
  )
  
  base <- get_player_season_stats(
    season = season,
    con = con
  )
  
  d <- derive_player_advanced_metrics(
    base
  )
  
  sanity_frame <- merge(
    d,
    base[
      ,
      intersect(
        c(
          "player_id",
          "team_id",
          "season",
          "minutes"
        ),
        names(base)
      ),
      drop = FALSE
    ],
    by = c(
      "player_id",
      "team_id",
      "season"
    ),
    all.x = TRUE,
    sort = FALSE
  )
  
  sanity <- phase3_step2_v2_sanity(
    sanity_frame
  )
  
  list(
    phase = "Phase 3.2",
    step = "Step 2 — Corrected Per-100 Architecture",
    status = if (
      nrow(d) > 0 &&
      sanity$valid
    ) {
      "READY FOR REBUILD"
    } else {
      "REVIEW"
    },
    rows = nrow(d),
    metric_version =
      "P3S2_v2_TEAM_PACE",
    sanity_issues =
      sanity$issues,
    official_defensive_metrics_status =
      "PRESERVED IF EXTERNALLY LOADED; OTHERWISE NA"
  )
}

# ============================================================
# PHASE 3.3 — STEP 2
# OFFICIAL NBA ADVANCED METRICS + OFFICIAL POSSESSION OVERRIDE
# ============================================================

# NOTE:
# These definitions intentionally appear after the Phase 3.2 versions.
# R uses the final definition loaded from the file. This preserves the
# working Phase 3.2 implementation while upgrading the active interface.

p3s2_v33_schema_columns <- function() {
  c(
    official_possessions = "DOUBLE",
    possession_source = "VARCHAR",
    player_impact_estimate = "DOUBLE"
  )
}

ensure_player_season_advanced_schema <- function(con = NULL) {
  owns_connection <- is.null(con)
  if (owns_connection) con <- connect_db()
  if (owns_connection) on.exit(disconnect_db(con), add = TRUE)
  
  create_player_season_advanced_table(con)
  
  existing_fields <- DBI::dbListFields(
    con,
    tbi_advanced_stats_table()
  )
  
  required_columns <- c(
    player_id = "BIGINT",
    team_id = "BIGINT",
    season = "VARCHAR",
    estimated_possessions = "DOUBLE",
    team_estimated_possessions = "DOUBLE",
    official_possessions = "DOUBLE",
    possession_source = "VARCHAR",
    points_per_100 = "DOUBLE",
    rebounds_per_100 = "DOUBLE",
    assists_per_100 = "DOUBLE",
    steals_per_100 = "DOUBLE",
    blocks_per_100 = "DOUBLE",
    turnovers_per_100 = "DOUBLE",
    offensive_rebounds_per_100 = "DOUBLE",
    defensive_rebounds_per_100 = "DOUBLE",
    field_goal_pct = "DOUBLE",
    three_point_pct = "DOUBLE",
    free_throw_pct = "DOUBLE",
    effective_field_goal_pct = "DOUBLE",
    true_shooting_pct = "DOUBLE",
    assist_turnover_ratio = "DOUBLE",
    three_point_attempt_rate = "DOUBLE",
    free_throw_rate = "DOUBLE",
    usage_rate = "DOUBLE",
    assist_pct = "DOUBLE",
    rebound_pct = "DOUBLE",
    offensive_rebound_pct = "DOUBLE",
    defensive_rebound_pct = "DOUBLE",
    steal_pct = "DOUBLE",
    block_pct = "DOUBLE",
    turnover_pct = "DOUBLE",
    offensive_rating = "DOUBLE",
    defensive_rating = "DOUBLE",
    net_rating = "DOUBLE",
    player_impact_estimate = "DOUBLE",
    player_efficiency_rating = "DOUBLE",
    win_shares = "DOUBLE",
    win_shares_per_48 = "DOUBLE",
    box_plus_minus = "DOUBLE",
    offensive_box_plus_minus = "DOUBLE",
    defensive_box_plus_minus = "DOUBLE",
    value_over_replacement = "DOUBLE",
    on_court_net_rating = "DOUBLE",
    on_off_net_rating = "DOUBLE",
    pace = "DOUBLE",
    source_name = "VARCHAR",
    source_player_id = "VARCHAR",
    metric_version = "VARCHAR",
    imported_at = "TIMESTAMP",
    updated_at = "TIMESTAMP"
  )
  
  missing_columns <- setdiff(
    names(required_columns),
    existing_fields
  )
  
  for (column in missing_columns) {
    DBI::dbExecute(
      con,
      paste(
        "ALTER TABLE",
        tbi_advanced_stats_table(),
        "ADD COLUMN",
        column,
        required_columns[[column]]
      )
    )
  }
  
  invisible(
    list(
      table = tbi_advanced_stats_table(),
      added_columns = missing_columns,
      fields = DBI::dbListFields(con, tbi_advanced_stats_table())
    )
  )
}

standardize_player_advanced_metrics <- function(df) {
  if (is.null(df) || !is.data.frame(df)) {
    stop("standardize_player_advanced_metrics() requires a data.frame.")
  }
  
  d <- df
  
  expected_columns <- c(
    "player_id", "team_id", "season",
    "estimated_possessions", "team_estimated_possessions",
    "official_possessions", "possession_source",
    "points_per_100", "rebounds_per_100", "assists_per_100",
    "steals_per_100", "blocks_per_100", "turnovers_per_100",
    "offensive_rebounds_per_100", "defensive_rebounds_per_100",
    "field_goal_pct", "three_point_pct", "free_throw_pct",
    "effective_field_goal_pct", "true_shooting_pct",
    "assist_turnover_ratio", "three_point_attempt_rate",
    "free_throw_rate", "usage_rate", "assist_pct", "rebound_pct",
    "offensive_rebound_pct", "defensive_rebound_pct", "steal_pct",
    "block_pct", "turnover_pct", "offensive_rating",
    "defensive_rating", "net_rating", "player_impact_estimate",
    "player_efficiency_rating", "win_shares", "win_shares_per_48",
    "box_plus_minus", "offensive_box_plus_minus",
    "defensive_box_plus_minus", "value_over_replacement",
    "on_court_net_rating", "on_off_net_rating", "pace",
    "source_name", "source_player_id", "metric_version",
    "imported_at", "updated_at"
  )
  
  for (column in setdiff(expected_columns, names(d))) {
    d[[column]] <- NA
  }
  
  d$player_id <- suppressWarnings(as.integer(d$player_id))
  d$team_id <- suppressWarnings(as.integer(d$team_id))
  d$season <- as.character(d$season)
  
  character_columns <- c(
    "source_name",
    "source_player_id",
    "metric_version",
    "possession_source"
  )
  
  timestamp_columns <- c("imported_at", "updated_at")
  
  numeric_columns <- setdiff(
    expected_columns,
    c("player_id", "team_id", "season", character_columns, timestamp_columns)
  )
  
  for (column in numeric_columns) {
    d[[column]] <- suppressWarnings(as.numeric(d[[column]]))
  }
  
  for (column in character_columns) {
    d[[column]] <- as.character(d[[column]])
  }
  
  d$metric_version[
    is.na(d$metric_version) |
      !nzchar(trimws(d$metric_version))
  ] <- "P3S2_v3_3_NBA_ADVANCED"
  
  d$updated_at <- Sys.time()
  
  d[, expected_columns, drop = FALSE]
}

p3s2_v33_coalesce_numeric <- function(primary, fallback) {
  primary <- suppressWarnings(as.numeric(primary))
  fallback <- suppressWarnings(as.numeric(fallback))
  n <- max(length(primary), length(fallback))
  primary <- rep(primary, length.out = n)
  fallback <- rep(fallback, length.out = n)
  out <- primary
  use_fallback <- !is.finite(out) & is.finite(fallback)
  out[use_fallback] <- fallback[use_fallback]
  out
}

# ------------------------------------------------------------
# PHASE 3.4 — NBA Stats identity bridge
# ------------------------------------------------------------

p3s2_v34_name_key <- function(
    x,
    strip_suffix = FALSE) {
  
  x <- as.character(x)
  
  # Transliterate Unicode/diacritics first:
  # Dončić -> Doncic, Jokić -> Jokic, Schröder -> Schroder, etc.
  ascii <- suppressWarnings(
    iconv(
      x,
      from = "",
      to = "ASCII//TRANSLIT"
    )
  )
  
  ascii[
    is.na(ascii)
  ] <- x[
    is.na(ascii)
  ]
  
  key <- tolower(
    gsub(
      "[^a-z0-9]",
      "",
      ascii
    )
  )
  
  if (strip_suffix) {
    key <- sub(
      "(jr|sr|ii|iii|iv|v)$",
      "",
      key
    )
  }
  
  key
}


p3s2_v34_unique_name_match <- function(
    source_names,
    players) {
  
  source_key <- p3s2_v34_name_key(
    source_names,
    strip_suffix = FALSE
  )
  
  player_key <- p3s2_v34_name_key(
    players$player_name,
    strip_suffix = FALSE
  )
  
  counts <- table(
    player_key
  )
  
  idx <- match(
    source_key,
    player_key
  )
  
  unique_hit <-
    !is.na(idx) &
    counts[
      source_key
    ] == 1
  
  out <- rep(
    NA_integer_,
    length(source_names)
  )
  
  out[
    unique_hit
  ] <- suppressWarnings(
    as.integer(
      players$player_id[
        idx[
          unique_hit
        ]
      ]
    )
  )
  
  # Second safe pass: allow Jr/II/III/etc differences only when
  # the suffix-stripped canonical key is unique.
  still_missing <- is.na(
    out
  )
  
  if (any(still_missing)) {
    
    source_key2 <- p3s2_v34_name_key(
      source_names[
        still_missing
      ],
      strip_suffix = TRUE
    )
    
    player_key2 <- p3s2_v34_name_key(
      players$player_name,
      strip_suffix = TRUE
    )
    
    counts2 <- table(
      player_key2
    )
    
    idx2 <- match(
      source_key2,
      player_key2
    )
    
    unique_hit2 <-
      !is.na(idx2) &
      counts2[
        source_key2
      ] == 1
    
    missing_positions <- which(
      still_missing
    )
    
    out[
      missing_positions[
        unique_hit2
      ]
    ] <- suppressWarnings(
      as.integer(
        players$player_id[
          idx2[
            unique_hit2
          ]
        ]
      )
    )
  }
  
  out
}


p3s2_v34_ensure_nba_identity_table <- function(
    con) {
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE IF NOT EXISTS external_player_identity (

      source_name TEXT NOT NULL,
      source_player_id TEXT NOT NULL,
      player_id INTEGER NOT NULL,

      source_player_name TEXT,
      first_seen_season TEXT,
      last_seen_season TEXT,

      created_at TEXT DEFAULT CURRENT_TIMESTAMP,
      updated_at TEXT DEFAULT CURRENT_TIMESTAMP,

      PRIMARY KEY (
        source_name,
        source_player_id
      )
    )
    "
  )
  
  invisible(TRUE)
}


p3s2_v34_persist_nba_identities <- function(
    con,
    mapped,
    season) {
  
  if (
    is.null(mapped) ||
    !is.data.frame(mapped) ||
    !nrow(mapped)
  ) {
    return(
      invisible(0L)
    )
  }
  
  keep <-
    !is.na(
      mapped$player_id
    ) &
    !is.na(
      mapped$PLAYER_ID
    )
  
  d <- mapped[
    keep,
    c(
      "PLAYER_ID",
      "PLAYER_NAME",
      "player_id"
    ),
    drop = FALSE
  ]
  
  if (!nrow(d)) {
    return(
      invisible(0L)
    )
  }
  
  d <- d[
    !duplicated(
      as.character(
        d$PLAYER_ID
      )
    ),
    ,
    drop = FALSE
  ]
  
  for (i in seq_len(nrow(d))) {
    
    source_id <- as.character(
      d$PLAYER_ID[[i]]
    )
    
    player_id <- suppressWarnings(
      as.integer(
        d$player_id[[i]]
      )
    )
    
    player_name <- as.character(
      d$PLAYER_NAME[[i]]
    )
    
    existing <- DBI::dbGetQuery(
      con,
      "
      SELECT
        player_id
      FROM external_player_identity
      WHERE source_name = 'NBA_STATS'
        AND source_player_id = ?
      ",
      params =
        list(
          source_id
        )
    )
    
    if (nrow(existing)) {
      
      DBI::dbExecute(
        con,
        "
        UPDATE external_player_identity
        SET
          player_id = ?,
          source_player_name = ?,
          last_seen_season = ?,
          updated_at = CURRENT_TIMESTAMP
        WHERE source_name = 'NBA_STATS'
          AND source_player_id = ?
        ",
        params =
          list(
            player_id,
            player_name,
            as.character(
              season
            ),
            source_id
          )
      )
      
    } else {
      
      DBI::dbExecute(
        con,
        "
        INSERT INTO external_player_identity (
          source_name,
          source_player_id,
          player_id,
          source_player_name,
          first_seen_season,
          last_seen_season
        )
        VALUES (
          'NBA_STATS',
          ?,
          ?,
          ?,
          ?,
          ?
        )
        ",
        params =
          list(
            source_id,
            player_id,
            player_name,
            as.character(
              season
            ),
            as.character(
              season
            )
          )
      )
    }
  }
  
  invisible(
    nrow(d)
  )
}


p3s2_v33_nba_advanced_map <- function(
    nba_advanced,
    season,
    con) {
  
  if (
    is.list(nba_advanced) &&
    "LeagueDashPlayerStats" %in%
    names(nba_advanced)
  ) {
    nba_advanced <-
      nba_advanced$LeagueDashPlayerStats
  }
  
  if (
    is.null(nba_advanced) ||
    !is.data.frame(nba_advanced) ||
    !nrow(nba_advanced)
  ) {
    return(
      data.frame()
    )
  }
  
  d <- as.data.frame(
    nba_advanced,
    stringsAsFactors = FALSE
  )
  
  required_source <- c(
    "PLAYER_ID",
    "PLAYER_NAME",
    "TEAM_ABBREVIATION",
    "GP",
    "MIN",
    "POSS",
    "OFF_RATING",
    "DEF_RATING",
    "NET_RATING",
    "AST_PCT",
    "OREB_PCT",
    "DREB_PCT",
    "REB_PCT",
    "TM_TOV_PCT",
    "EFG_PCT",
    "TS_PCT",
    "USG_PCT",
    "PACE",
    "PIE"
  )
  
  missing_source <- setdiff(
    required_source,
    names(d)
  )
  
  if (length(missing_source)) {
    stop(
      paste0(
        "NBA Advanced response is missing required columns: ",
        paste(
          missing_source,
          collapse = ", "
        )
      )
    )
  }
  
  p3s2_v34_ensure_nba_identity_table(
    con
  )
  
  # IMPORTANT:
  # NBA Stats gets its OWN provider namespace. We no longer
  # compare NBA PLAYER_ID against generic hoopR/box-score IDs.
  identity <- DBI::dbGetQuery(
    con,
    "
    SELECT
      source_player_id,
      player_id
    FROM external_player_identity
    WHERE source_name = 'NBA_STATS'
    "
  )
  
  players <- DBI::dbGetQuery(
    con,
    "
    SELECT
      player_id,
      player_name
    FROM players
    "
  )
  
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
  
  step1 <- DBI::dbGetQuery(
    con,
    "
    SELECT
      player_id,
      team_id,
      season,
      minutes
    FROM player_season_stats
    WHERE season = ?
    ",
    params =
      list(
        as.character(
          season
        )
      )
  )
  
  d$source_player_id <-
    as.character(
      d$PLAYER_ID
    )
  
  d$source_team_abbr <-
    toupper(
      trimws(
        as.character(
          d$TEAM_ABBREVIATION
        )
      )
    )
  
  # ----------------------------------------------------------
  # 1. Persistent NBA_STATS identity
  # ----------------------------------------------------------
  
  idx_identity <- match(
    d$source_player_id,
    as.character(
      identity$source_player_id
    )
  )
  
  d$player_id <- suppressWarnings(
    as.integer(
      identity$player_id[
        idx_identity
      ]
    )
  )
  
  # ----------------------------------------------------------
  # 2. Safe canonical-name bootstrap
  # ----------------------------------------------------------
  # Only unique normalized names can bootstrap a missing NBA ID.
  # Once matched, the NBA ID is persisted and future seasons do
  # not depend on names.
  
  missing_player <- is.na(
    d$player_id
  )
  
  if (any(missing_player)) {
    
    d$player_id[
      missing_player
    ] <-
      p3s2_v34_unique_name_match(
        d$PLAYER_NAME[
          missing_player
        ],
        players
      )
  }
  
  # Persist every successful NBA Stats -> TBI identity.
  p3s2_v34_persist_nba_identities(
    con = con,
    mapped = d,
    season = season
  )
  
  # ----------------------------------------------------------
  # Team mapping
  # ----------------------------------------------------------
  
  team_abbr <- toupper(
    trimws(
      as.character(
        teams$abbreviation
      )
    )
  )
  
  team_abbr[
    team_abbr == "PHO"
  ] <- "PHX"
  
  team_abbr[
    team_abbr == "BRK"
  ] <- "BKN"
  
  source_abbr <- d$source_team_abbr
  
  source_abbr[
    source_abbr == "PHO"
  ] <- "PHX"
  
  source_abbr[
    source_abbr == "BRK"
  ] <- "BKN"
  
  source_abbr[
    source_abbr == "CHO"
  ] <- "CHA"
  
  idx_team <- match(
    source_abbr,
    team_abbr
  )
  
  d$mapped_team_id <- suppressWarnings(
    as.integer(
      teams$team_id[
        idx_team
      ]
    )
  )
  
  # ----------------------------------------------------------
  # Collapse duplicate official rows per NBA player
  # ----------------------------------------------------------
  # LeagueDashPlayerStats should normally be one row/player.
  # If the endpoint ever returns duplicates, keep the most
  # representative row (highest GP, then MIN) before attaching
  # it to team-stint Step-1 rows.
  
  gp_num <- suppressWarnings(
    as.numeric(
      d$GP
    )
  )
  
  min_num <- suppressWarnings(
    as.numeric(
      d$MIN
    )
  )
  
  gp_num[
    !is.finite(gp_num)
  ] <- -Inf
  
  min_num[
    !is.finite(min_num)
  ] <- -Inf
  
  d$.gp_order <- gp_num
  d$.min_order <- min_num
  
  d <- d[
    order(
      d$player_id,
      -d$.gp_order,
      -d$.min_order,
      na.last = TRUE
    ),
    ,
    drop = FALSE
  ]
  
  d <- d[
    !is.na(
      d$player_id
    ) &
      !duplicated(
        d$player_id
      ),
    ,
    drop = FALSE
  ]
  
  # ----------------------------------------------------------
  # Attach one official player-season record to every Step-1
  # player/team stint.
  # ----------------------------------------------------------
  
  mapped <- merge(
    step1,
    d,
    by = "player_id",
    all.x = TRUE,
    sort = FALSE
  )
  
  if (nrow(mapped)) {
    
    mapped$team_match <-
      is.na(
        mapped$mapped_team_id
      ) |
      mapped$team_id ==
      mapped$mapped_team_id
    
    keys <- paste(
      mapped$player_id,
      mapped$team_id,
      mapped$season,
      sep = "|"
    )
    
    split_rows <- split(
      seq_len(
        nrow(mapped)
      ),
      keys
    )
    
    keep <- vapply(
      split_rows,
      function(ix) {
        
        hit <- ix[
          mapped$team_match[
            ix
          ] %in%
            TRUE
        ]
        
        if (length(hit)) {
          hit[[1]]
        } else {
          ix[[1]]
        }
      },
      integer(1)
    )
    
    mapped <- mapped[
      keep,
      ,
      drop = FALSE
    ]
  }
  
  out <- data.frame(
    player_id =
      suppressWarnings(
        as.integer(
          mapped$player_id
        )
      ),
    
    team_id =
      suppressWarnings(
        as.integer(
          mapped$team_id
        )
      ),
    
    season =
      as.character(
        mapped$season
      ),
    
    official_possessions =
      suppressWarnings(
        as.numeric(
          mapped$POSS
        )
      ),
    
    usage_rate =
      suppressWarnings(
        as.numeric(
          mapped$USG_PCT
        )
      ),
    
    assist_pct =
      suppressWarnings(
        as.numeric(
          mapped$AST_PCT
        )
      ),
    
    rebound_pct =
      suppressWarnings(
        as.numeric(
          mapped$REB_PCT
        )
      ),
    
    offensive_rebound_pct =
      suppressWarnings(
        as.numeric(
          mapped$OREB_PCT
        )
      ),
    
    defensive_rebound_pct =
      suppressWarnings(
        as.numeric(
          mapped$DREB_PCT
        )
      ),
    
    turnover_pct =
      suppressWarnings(
        as.numeric(
          mapped$TM_TOV_PCT
        )
      ),
    
    effective_field_goal_pct =
      suppressWarnings(
        as.numeric(
          mapped$EFG_PCT
        )
      ),
    
    true_shooting_pct =
      suppressWarnings(
        as.numeric(
          mapped$TS_PCT
        )
      ),
    
    offensive_rating =
      suppressWarnings(
        as.numeric(
          mapped$OFF_RATING
        )
      ),
    
    defensive_rating =
      suppressWarnings(
        as.numeric(
          mapped$DEF_RATING
        )
      ),
    
    net_rating =
      suppressWarnings(
        as.numeric(
          mapped$NET_RATING
        )
      ),
    
    pace =
      suppressWarnings(
        as.numeric(
          mapped$PACE
        )
      ),
    
    player_impact_estimate =
      suppressWarnings(
        as.numeric(
          mapped$PIE
        )
      ),
    
    source_name =
      "NBA Stats via hoopR — Advanced",
    
    source_player_id =
      as.character(
        mapped$PLAYER_ID
      ),
    
    metric_version =
      "P3S2_v3_4_NBA_STATS_IDENTITY",
    
    stringsAsFactors = FALSE
  )
  
  out <- out[
    !is.na(
      out$player_id
    ) &
      !is.na(
        out$team_id
      ),
    ,
    drop = FALSE
  ]
  
  out
}


p3s2_v33_merge_official_advanced <- function(
    derived,
    official) {
  
  if (is.null(official) || !is.data.frame(official) || !nrow(official)) {
    derived$official_possessions <- NA_real_
    derived$possession_source <- "TEAM_PACE_ESTIMATE"
    derived$player_impact_estimate <- NA_real_
    derived$metric_version <- "P3S2_v3_3_TEAM_PACE_FALLBACK"
    return(derived)
  }
  
  keys <- c("player_id", "team_id", "season")
  m <- merge(
    derived,
    official,
    by = keys,
    all.x = TRUE,
    suffixes = c("", ".official"),
    sort = FALSE
  )
  
  official_poss <- suppressWarnings(as.numeric(m$official_possessions))
  old_poss <- suppressWarnings(as.numeric(m$estimated_possessions))
  use_official_poss <- is.finite(official_poss) & official_poss > 0
  
  # estimated_possessions remains the canonical downstream denominator for
  # compatibility, but now contains official NBA POSS when available.
  m$estimated_possessions[use_official_poss] <- official_poss[use_official_poss]
  m$possession_source <- ifelse(
    use_official_poss,
    "NBA_STATS_OFFICIAL_POSS",
    "TEAM_PACE_ESTIMATE"
  )
  
  # Recompute per-100 fields with the canonical possession denominator.
  # Counts are not present here, so build_advanced_metrics_from_player_stats()
  # performs the final recompute after this merge.
  
  official_columns <- c(
    "usage_rate", "assist_pct", "rebound_pct",
    "offensive_rebound_pct", "defensive_rebound_pct",
    "turnover_pct", "effective_field_goal_pct", "true_shooting_pct",
    "offensive_rating", "defensive_rating", "net_rating", "pace",
    "player_impact_estimate", "source_player_id"
  )
  
  for (column in official_columns) {
    official_name <- paste0(column, ".official")
    if (!official_name %in% names(m)) next
    
    if (column == "source_player_id") {
      value <- as.character(m[[official_name]])
      use <- !is.na(value) & nzchar(value)
      if (!column %in% names(m)) m[[column]] <- NA_character_
      m[[column]][use] <- value[use]
    } else {
      value <- suppressWarnings(as.numeric(m[[official_name]]))
      use <- is.finite(value)
      if (!column %in% names(m)) m[[column]] <- NA_real_
      m[[column]][use] <- value[use]
    }
  }
  
  m$source_name <- ifelse(
    use_official_poss |
      is.finite(suppressWarnings(as.numeric(m$offensive_rating))) |
      is.finite(suppressWarnings(as.numeric(m$usage_rate))),
    "NBA Stats via hoopR — Advanced",
    m$source_name
  )
  
  m$metric_version <- ifelse(
    use_official_poss,
    "P3S2_v3_3_NBA_ADVANCED",
    "P3S2_v3_3_TEAM_PACE_FALLBACK"
  )
  
  drop <- grep("\\.official$", names(m), value = TRUE)
  m[, setdiff(names(m), drop), drop = FALSE]
}

build_advanced_metrics_from_player_stats <- function(
    season = NULL,
    con = NULL,
    official_advanced = NULL) {
  
  owns_connection <- is.null(con)
  if (owns_connection) con <- connect_db()
  if (owns_connection) on.exit(disconnect_db(con), add = TRUE)
  
  ensure_player_season_advanced_schema(con)
  
  base_stats <- get_player_season_stats(
    season = season,
    con = con
  )
  
  if (!nrow(base_stats)) {
    return(invisible(list(
      rows_processed = 0L,
      status = "NO STEP-1 DATA LOADED"
    )))
  }
  
  derived <- derive_player_advanced_metrics(base_stats)
  
  # Existing externally loaded values remain fallback evidence.
  existing <- if (
    tbi_advanced_stats_table() %in% DBI::dbListTables(con)
  ) {
    get_player_advanced_metrics(season = season, con = con)
  } else {
    data.frame()
  }
  
  derived <- p3s2_v2_preserve_external_metrics(
    derived,
    existing
  )
  
  official_mapped <- data.frame()
  
  if (!is.null(official_advanced)) {
    official_mapped <- p3s2_v33_nba_advanced_map(
      nba_advanced = official_advanced,
      season = season,
      con = con
    )
    
    derived <- p3s2_v33_merge_official_advanced(
      derived,
      official_mapped
    )
  } else {
    if (!"official_possessions" %in% names(derived)) {
      derived$official_possessions <- NA_real_
    }
    if (!"possession_source" %in% names(derived)) {
      derived$possession_source <- "TEAM_PACE_ESTIMATE"
    }
    if (!"player_impact_estimate" %in% names(derived)) {
      derived$player_impact_estimate <- NA_real_
    }
  }
  
  # Official NBA POSS becomes the denominator where available.
  poss <- suppressWarnings(as.numeric(derived$estimated_possessions))
  points <- suppressWarnings(as.numeric(base_stats$points))
  rebounds <- suppressWarnings(as.numeric(base_stats$rebounds))
  assists <- suppressWarnings(as.numeric(base_stats$assists))
  steals <- suppressWarnings(as.numeric(base_stats$steals))
  blocks <- suppressWarnings(as.numeric(base_stats$blocks))
  turnovers <- suppressWarnings(as.numeric(base_stats$turnovers))
  orb <- suppressWarnings(as.numeric(base_stats$offensive_rebounds))
  drb <- suppressWarnings(as.numeric(base_stats$defensive_rebounds))
  
  # Align base counts to derived keys explicitly.
  base_key <- paste(base_stats$player_id, base_stats$team_id, base_stats$season, sep = "|")
  derived_key <- paste(derived$player_id, derived$team_id, derived$season, sep = "|")
  idx <- match(derived_key, base_key)
  
  per100 <- function(x) {
    x <- suppressWarnings(as.numeric(x[idx]))
    out <- rep(NA_real_, length(poss))
    ok <- is.finite(x) & is.finite(poss) & poss > 0
    out[ok] <- 100 * x[ok] / poss[ok]
    out
  }
  
  derived$points_per_100 <- per100(points)
  derived$rebounds_per_100 <- per100(rebounds)
  derived$assists_per_100 <- per100(assists)
  derived$steals_per_100 <- per100(steals)
  derived$blocks_per_100 <- per100(blocks)
  derived$turnovers_per_100 <- per100(turnovers)
  derived$offensive_rebounds_per_100 <- per100(orb)
  derived$defensive_rebounds_per_100 <- per100(drb)
  
  sanity_frame <- merge(
    derived,
    base_stats[, intersect(c("player_id", "team_id", "season", "minutes"), names(base_stats)), drop = FALSE],
    by = c("player_id", "team_id", "season"),
    all.x = TRUE,
    sort = FALSE
  )
  
  sanity <- phase3_step2_v2_sanity(sanity_frame)
  if (!sanity$valid) {
    stop(paste(c("Step 2 V3.3 sanity check failed.", sanity$issues), collapse = "\n"))
  }
  
  upsert_player_advanced_metrics(
    derived,
    con = con
  )
  
  invisible(
    list(
      rows_processed = nrow(derived),
      official_advanced_rows = nrow(official_mapped),
      official_possession_rows = sum(
        derived$possession_source == "NBA_STATS_OFFICIAL_POSS",
        na.rm = TRUE
      ),
      usage_rows = sum(is.finite(suppressWarnings(as.numeric(derived$usage_rate))), na.rm = TRUE),
      offensive_rating_rows = sum(is.finite(suppressWarnings(as.numeric(derived$offensive_rating))), na.rm = TRUE),
      defensive_rating_rows = sum(is.finite(suppressWarnings(as.numeric(derived$defensive_rating))), na.rm = TRUE),
      net_rating_rows = sum(is.finite(suppressWarnings(as.numeric(derived$net_rating))), na.rm = TRUE),
      seasons = unique(derived$season),
      status = "ADVANCED METRICS V3.3 BUILT",
      metric_version = "P3S2_v3_3_NBA_ADVANCED"
    )
  )
}

phase33_step2_official_advanced_healthcheck <- function(
    season = "2025-26") {
  
  con <- connect_db(read_only = TRUE)
  on.exit(disconnect_db(con), add = TRUE)
  
  d <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS rows,
      SUM(CASE WHEN official_possessions IS NOT NULL THEN 1 ELSE 0 END) AS official_poss,
      SUM(CASE WHEN usage_rate IS NOT NULL THEN 1 ELSE 0 END) AS usage,
      SUM(CASE WHEN offensive_rating IS NOT NULL THEN 1 ELSE 0 END) AS off_rtg,
      SUM(CASE WHEN defensive_rating IS NOT NULL THEN 1 ELSE 0 END) AS def_rtg,
      SUM(CASE WHEN net_rating IS NOT NULL THEN 1 ELSE 0 END) AS net_rtg,
      SUM(CASE WHEN player_impact_estimate IS NOT NULL THEN 1 ELSE 0 END) AS pie
    FROM player_season_advanced
    WHERE season = ?
    ",
    params = list(season)
  )
  
  list(
    phase = "Phase 3.3",
    step = "Step 2 — Official NBA Advanced Metrics",
    status = if (
      d$rows[[1]] > 0 &&
      d$usage[[1]] > 0 &&
      d$off_rtg[[1]] > 0 &&
      d$def_rtg[[1]] > 0 &&
      d$net_rtg[[1]] > 0
    ) "READY" else "REVIEW",
    season = season,
    rows = d$rows[[1]],
    official_possession_rows = d$official_poss[[1]],
    usage_rows = d$usage[[1]],
    offensive_rating_rows = d$off_rtg[[1]],
    defensive_rating_rows = d$def_rtg[[1]],
    net_rating_rows = d$net_rtg[[1]],
    pie_rows = d$pie[[1]],
    rule = "NBA ADVANCED IS PRIMARY; TEAM-PACE POSSESSION IS FALLBACK ONLY"
  )
}
