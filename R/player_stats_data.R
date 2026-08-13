# ============================================================
# TBI NBA Basketball Operations Platform
# PHASE 3 — STEP 1
# Player Stats Data Architecture
# ============================================================
# Safe to run before real NBA stats are loaded.
# One canonical row = player + team + season.
# ============================================================

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------

tbi_stats_table <- function() {
  "player_season_stats"
}

safe_stats_divide <- function(numerator, denominator) {
  numerator <- suppressWarnings(as.numeric(numerator))
  denominator <- suppressWarnings(as.numeric(denominator))
  
  out <- rep(NA_real_, max(length(numerator), length(denominator)))
  valid <- is.finite(numerator) & is.finite(denominator) & denominator != 0
  out[valid] <- numerator[valid] / denominator[valid]
  out
}

# ------------------------------------------------------------
# Create canonical player-season stats table
# ------------------------------------------------------------

create_player_season_stats_table <- function(con = NULL) {
  owns_connection <- is.null(con)
  
  if (owns_connection) {
    if (!exists("connect_db", mode = "function")) {
      stop("connect_db() is not available. Load the TBI project first.")
    }
    con <- connect_db()
  }
  
  if (owns_connection) {
    on.exit(disconnect_db(con), add = TRUE)
  }
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE IF NOT EXISTS player_season_stats (
      player_id BIGINT NOT NULL,
      team_id BIGINT NOT NULL,
      season VARCHAR NOT NULL,

      games_played INTEGER,
      games_started INTEGER,
      minutes DOUBLE,
      points DOUBLE,

      offensive_rebounds DOUBLE,
      defensive_rebounds DOUBLE,
      rebounds DOUBLE,
      assists DOUBLE,
      steals DOUBLE,
      blocks DOUBLE,
      turnovers DOUBLE,

      field_goals_made DOUBLE,
      field_goals_attempted DOUBLE,
      three_pointers_made DOUBLE,
      three_pointers_attempted DOUBLE,
      free_throws_made DOUBLE,
      free_throws_attempted DOUBLE,
      personal_fouls DOUBLE,

      minutes_per_game DOUBLE,
      points_per_game DOUBLE,
      rebounds_per_game DOUBLE,
      assists_per_game DOUBLE,
      steals_per_game DOUBLE,
      blocks_per_game DOUBLE,
      turnovers_per_game DOUBLE,

      field_goal_pct DOUBLE,
      three_point_pct DOUBLE,
      free_throw_pct DOUBLE,

      source_name VARCHAR,
      source_player_id VARCHAR,
      source_team VARCHAR,
      imported_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

      PRIMARY KEY (player_id, team_id, season)
    )
    "
  )
  
  index_sql <- c(
    "CREATE INDEX IF NOT EXISTS idx_pss_player ON player_season_stats(player_id)",
    "CREATE INDEX IF NOT EXISTS idx_pss_team ON player_season_stats(team_id)",
    "CREATE INDEX IF NOT EXISTS idx_pss_season ON player_season_stats(season)"
  )
  
  for (sql in index_sql) {
    try(DBI::dbExecute(con, sql), silent = TRUE)
  }
  
  invisible(TRUE)
}

# ------------------------------------------------------------
# Ensure schema exists / evolve safely
# ------------------------------------------------------------

ensure_player_season_stats_schema <- function(con = NULL) {
  owns_connection <- is.null(con)
  
  if (owns_connection) {
    con <- connect_db()
  }
  
  if (owns_connection) {
    on.exit(disconnect_db(con), add = TRUE)
  }
  
  create_player_season_stats_table(con)
  
  existing <- DBI::dbListFields(con, tbi_stats_table())
  
  required <- c(
    player_id = "BIGINT",
    team_id = "BIGINT",
    season = "VARCHAR",
    games_played = "INTEGER",
    games_started = "INTEGER",
    minutes = "DOUBLE",
    points = "DOUBLE",
    offensive_rebounds = "DOUBLE",
    defensive_rebounds = "DOUBLE",
    rebounds = "DOUBLE",
    assists = "DOUBLE",
    steals = "DOUBLE",
    blocks = "DOUBLE",
    turnovers = "DOUBLE",
    field_goals_made = "DOUBLE",
    field_goals_attempted = "DOUBLE",
    three_pointers_made = "DOUBLE",
    three_pointers_attempted = "DOUBLE",
    free_throws_made = "DOUBLE",
    free_throws_attempted = "DOUBLE",
    personal_fouls = "DOUBLE",
    minutes_per_game = "DOUBLE",
    points_per_game = "DOUBLE",
    rebounds_per_game = "DOUBLE",
    assists_per_game = "DOUBLE",
    steals_per_game = "DOUBLE",
    blocks_per_game = "DOUBLE",
    turnovers_per_game = "DOUBLE",
    field_goal_pct = "DOUBLE",
    three_point_pct = "DOUBLE",
    free_throw_pct = "DOUBLE",
    source_name = "VARCHAR",
    source_player_id = "VARCHAR",
    source_team = "VARCHAR",
    imported_at = "TIMESTAMP",
    updated_at = "TIMESTAMP"
  )
  
  missing <- setdiff(names(required), existing)
  
  for (column in missing) {
    DBI::dbExecute(
      con,
      paste(
        "ALTER TABLE",
        tbi_stats_table(),
        "ADD COLUMN",
        column,
        required[[column]]
      )
    )
  }
  
  invisible(list(
    table = tbi_stats_table(),
    added_columns = missing,
    fields = DBI::dbListFields(con, tbi_stats_table())
  ))
}

# ------------------------------------------------------------
# Standardize imported stats
# ------------------------------------------------------------

standardize_player_season_stats <- function(df) {
  if (is.null(df) || !is.data.frame(df)) {
    stop("Input must be a data.frame.")
  }
  
  d <- df
  
  raw_columns <- c(
    "player_id", "team_id", "season",
    "games_played", "games_started", "minutes", "points",
    "offensive_rebounds", "defensive_rebounds", "rebounds",
    "assists", "steals", "blocks", "turnovers",
    "field_goals_made", "field_goals_attempted",
    "three_pointers_made", "three_pointers_attempted",
    "free_throws_made", "free_throws_attempted",
    "personal_fouls", "source_name", "source_player_id", "source_team"
  )
  
  for (column in setdiff(raw_columns, names(d))) {
    d[[column]] <- NA
  }
  
  integer_columns <- c(
    "player_id", "team_id", "games_played", "games_started"
  )
  
  numeric_columns <- c(
    "minutes", "points", "offensive_rebounds", "defensive_rebounds",
    "rebounds", "assists", "steals", "blocks", "turnovers",
    "field_goals_made", "field_goals_attempted",
    "three_pointers_made", "three_pointers_attempted",
    "free_throws_made", "free_throws_attempted", "personal_fouls"
  )
  
  for (column in integer_columns) {
    d[[column]] <- suppressWarnings(as.integer(d[[column]]))
  }
  
  for (column in numeric_columns) {
    d[[column]] <- suppressWarnings(as.numeric(d[[column]]))
  }
  
  d$season <- as.character(d$season)
  d$source_name <- as.character(d$source_name)
  d$source_player_id <- as.character(d$source_player_id)
  d$source_team <- as.character(d$source_team)
  
  d$minutes_per_game <- safe_stats_divide(d$minutes, d$games_played)
  d$points_per_game <- safe_stats_divide(d$points, d$games_played)
  d$rebounds_per_game <- safe_stats_divide(d$rebounds, d$games_played)
  d$assists_per_game <- safe_stats_divide(d$assists, d$games_played)
  d$steals_per_game <- safe_stats_divide(d$steals, d$games_played)
  d$blocks_per_game <- safe_stats_divide(d$blocks, d$games_played)
  d$turnovers_per_game <- safe_stats_divide(d$turnovers, d$games_played)
  
  d$field_goal_pct <- safe_stats_divide(
    d$field_goals_made,
    d$field_goals_attempted
  )
  
  d$three_point_pct <- safe_stats_divide(
    d$three_pointers_made,
    d$three_pointers_attempted
  )
  
  d$free_throw_pct <- safe_stats_divide(
    d$free_throws_made,
    d$free_throws_attempted
  )
  
  d$updated_at <- Sys.time()
  
  final_columns <- c(
    "player_id", "team_id", "season",
    "games_played", "games_started", "minutes", "points",
    "offensive_rebounds", "defensive_rebounds", "rebounds",
    "assists", "steals", "blocks", "turnovers",
    "field_goals_made", "field_goals_attempted",
    "three_pointers_made", "three_pointers_attempted",
    "free_throws_made", "free_throws_attempted", "personal_fouls",
    "minutes_per_game", "points_per_game", "rebounds_per_game",
    "assists_per_game", "steals_per_game", "blocks_per_game",
    "turnovers_per_game", "field_goal_pct", "three_point_pct",
    "free_throw_pct", "source_name", "source_player_id", "source_team",
    "updated_at"
  )
  
  d[, final_columns, drop = FALSE]
}

# ------------------------------------------------------------
# Validate stats before database load
# ------------------------------------------------------------

validate_player_season_stats <- function(df) {
  issues <- character()
  
  if (is.null(df) || !is.data.frame(df)) {
    return(list(valid = FALSE, issues = "Input is not a data.frame."))
  }
  
  required <- c("player_id", "team_id", "season")
  missing <- setdiff(required, names(df))
  
  if (length(missing)) {
    issues <- c(
      issues,
      paste0("Missing required columns: ", paste(missing, collapse = ", "))
    )
  }
  
  if (all(required %in% names(df))) {
    if (any(is.na(df$player_id))) {
      issues <- c(issues, "Missing player_id found.")
    }
    
    if (any(is.na(df$team_id))) {
      issues <- c(issues, "Missing team_id found.")
    }
    
    bad_season <- is.na(df$season) | !nzchar(trimws(as.character(df$season)))
    if (any(bad_season)) {
      issues <- c(issues, "Missing season found.")
    }
    
    key <- paste(df$player_id, df$team_id, df$season, sep = "|")
    duplicate_key <- duplicated(key)
    
    if (any(duplicate_key)) {
      issues <- c(
        issues,
        paste0(sum(duplicate_key), " duplicate player/team/season rows found.")
      )
    }
  }
  
  nonnegative_columns <- intersect(
    c(
      "games_played", "games_started", "minutes", "points", "rebounds",
      "assists", "steals", "blocks", "turnovers",
      "field_goals_made", "field_goals_attempted",
      "three_pointers_made", "three_pointers_attempted",
      "free_throws_made", "free_throws_attempted"
    ),
    names(df)
  )
  
  for (column in nonnegative_columns) {
    value <- suppressWarnings(as.numeric(df[[column]]))
    if (any(is.finite(value) & value < 0)) {
      issues <- c(issues, paste0(column, " contains negative values."))
    }
  }
  
  list(valid = !length(issues), issues = unique(issues))
}

# ------------------------------------------------------------
# Upsert stats into DuckDB
# ------------------------------------------------------------

upsert_player_season_stats <- function(df, con = NULL) {
  
  d <- standardize_player_season_stats(df)
  validation <- validate_player_season_stats(d)
  
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
  
  ensure_player_season_stats_schema(con)
  
  if (!nrow(d)) {
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
    "tmp_player_season_stats_",
    as.integer(Sys.time()),
    "_",
    sample.int(999999L, 1L)
  )
  
  DBI::dbWriteTable(
    con,
    temp_table,
    d,
    temporary = TRUE,
    overwrite = TRUE
  )
  
  table_q <- as.character(
    DBI::dbQuoteIdentifier(
      con,
      temp_table
    )
  )
  
  columns <- names(d)
  
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
          DELETE FROM player_season_stats
          WHERE EXISTS (
            SELECT 1
            FROM ",
          table_q,
          " AS src
            WHERE src.player_id = player_season_stats.player_id
              AND src.team_id = player_season_stats.team_id
              AND src.season = player_season_stats.season
          )
          "
        )
      )
      
      DBI::dbExecute(
        con,
        paste0(
          "
          INSERT INTO player_season_stats (
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
      rows_processed = nrow(d),
      players = length(
        unique(
          d$player_id
        )
      ),
      seasons = unique(
        d$season
      ),
      database_method =
        "SQLITE TRANSACTIONAL UPSERT"
    )
  )
}

# ------------------------------------------------------------
# Read canonical player-season stats
# ------------------------------------------------------------

get_player_season_stats <- function(
    season = NULL,
    team_id = NULL,
    player_id = NULL,
    con = NULL) {
  
  owns_connection <- is.null(con)
  
  if (owns_connection) {
    con <- connect_db(read_only = TRUE)
  }
  
  if (owns_connection) {
    on.exit(disconnect_db(con), add = TRUE)
  }
  
  if (!tbi_stats_table() %in% DBI::dbListTables(con)) {
    return(data.frame())
  }
  
  where <- character()
  params <- list()
  
  if (!is.null(season)) {
    where <- c(where, "pss.season = ?")
    params <- c(params, list(as.character(season)))
  }
  
  if (!is.null(team_id)) {
    where <- c(where, "pss.team_id = ?")
    params <- c(params, list(as.integer(team_id)))
  }
  
  if (!is.null(player_id)) {
    where <- c(where, "pss.player_id = ?")
    params <- c(params, list(as.integer(player_id)))
  }
  
  where_sql <- if (length(where)) {
    paste("WHERE", paste(where, collapse = " AND "))
  } else {
    ""
  }
  
  sql <- paste0(
    "SELECT pss.*, p.player_name, p.primary_position, p.player_age, ",
    "t.team_name, t.abbreviation ",
    "FROM player_season_stats pss ",
    "LEFT JOIN players p ON p.player_id = pss.player_id ",
    "LEFT JOIN teams t ON t.team_id = pss.team_id ",
    where_sql,
    " ORDER BY pss.season DESC, pss.points_per_game DESC NULLS LAST, p.player_name"
  )
  
  DBI::dbGetQuery(con, sql, params = params)
}

# ------------------------------------------------------------
# BIE-ready team-season reader
# ------------------------------------------------------------

get_bie_team_player_stats <- function(team_name, season, con = NULL) {
  owns_connection <- is.null(con)
  
  if (owns_connection) {
    con <- connect_db(read_only = TRUE)
  }
  
  if (owns_connection) {
    on.exit(disconnect_db(con), add = TRUE)
  }
  
  if (!tbi_stats_table() %in% DBI::dbListTables(con)) {
    return(data.frame())
  }
  
  DBI::dbGetQuery(
    con,
    "
    SELECT
      pss.*,
      p.player_name,
      p.primary_position,
      p.player_age,
      p.height_inches,
      p.weight_lbs,
      t.team_name,
      t.abbreviation
    FROM player_season_stats pss
    INNER JOIN players p
      ON p.player_id = pss.player_id
    INNER JOIN teams t
      ON t.team_id = pss.team_id
    WHERE t.team_name = ?
      AND pss.season = ?
    ORDER BY
      pss.minutes_per_game DESC NULLS LAST,
      pss.points_per_game DESC NULLS LAST,
      p.player_name
    ",
    params = list(
      as.character(team_name),
      as.character(season)
    )
  )
}

# ------------------------------------------------------------
# Step 1 health check
# ------------------------------------------------------------

phase3_step1_healthcheck <- function() {
  if (!exists("connect_db", mode = "function")) {
    return(list(
      status = "REVIEW",
      issue = "connect_db() is not loaded."
    ))
  }
  
  con <- connect_db()
  on.exit(disconnect_db(con), add = TRUE)
  
  ensure_player_season_stats_schema(con)
  
  tables <- DBI::dbListTables(con)
  fields <- if (tbi_stats_table() %in% tables) {
    DBI::dbListFields(con, tbi_stats_table())
  } else {
    character()
  }
  
  required <- c(
    "player_id", "team_id", "season", "games_played", "minutes",
    "points", "rebounds", "assists", "field_goals_attempted",
    "three_pointers_attempted", "free_throws_attempted",
    "points_per_game", "field_goal_pct"
  )
  
  missing <- setdiff(required, fields)
  
  row_count <- if (tbi_stats_table() %in% tables) {
    DBI::dbGetQuery(
      con,
      "SELECT COUNT(*) AS n FROM player_season_stats"
    )$n[[1]]
  } else {
    0L
  }
  
  list(
    phase = "Phase 3",
    step = "Step 1 — Player Stats Data Architecture",
    status = if (!length(missing)) "READY" else "REVIEW",
    table = tbi_stats_table(),
    fields = length(fields),
    missing_required_fields = missing,
    rows_loaded = row_count,
    data_status = if (row_count > 0) {
      "DATA LOADED"
    } else {
      "SCHEMA READY — DATA CAN BE LOADED LATER"
    }
  )
}