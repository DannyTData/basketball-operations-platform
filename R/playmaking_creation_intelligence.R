# ============================================================
# TBI NBA Basketball Operations Platform
# PHASE 3 — STEP 5
# Playmaking + Creation Intelligence
#
# Uses real Step-1 / Step-2 player-season data.
#
# Principles:
#   - creation is not just raw assists
#   - assist volume + per-100 playmaking both matter
#   - assist/turnover balance matters
#   - turnover burden is penalized
#   - usage-based creation only activates when usage is loaded
#   - low-minute / tiny-sample profiles get lower confidence
# ============================================================


# ------------------------------------------------------------
# Canonical playmaking table
# ------------------------------------------------------------

tbi_playmaking_table <- function() {
  "player_season_playmaking"
}


# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------

p3s5_num <- function(
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


p3s5_divide <- function(
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
  
  out <- rep(
    default,
    n
  )
  
  valid <-
    is.finite(numerator) &
    is.finite(denominator) &
    denominator != 0
  
  out[valid] <-
    numerator[valid] /
    denominator[valid]
  
  out
}


p3s5_percentile <- function(x) {
  
  value <- suppressWarnings(
    as.numeric(x)
  )
  
  valid <- is.finite(value)
  
  out <- rep(
    NA_real_,
    length(value)
  )
  
  if (!any(valid)) {
    return(out)
  }
  
  if (sum(valid) == 1L) {
    out[valid] <- 50
    return(out)
  }
  
  ranked <- rank(
    value[valid],
    ties.method = "average"
  )
  
  out[valid] <-
    100 *
    (ranked - 1) /
    (sum(valid) - 1)
  
  out
}


p3s5_reverse_percentile <- function(x) {
  
  value <- suppressWarnings(
    as.numeric(x)
  )
  
  pct <- p3s5_percentile(
    value
  )
  
  ifelse(
    is.finite(pct),
    100 - pct,
    NA_real_
  )
}


p3s5_weighted_mean <- function(
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


# ------------------------------------------------------------
# Confidence label
# ------------------------------------------------------------

p3s5_confidence_label <- function(
    games,
    minutes,
    assists,
    turnovers) {
  
  games <- p3s5_num(
    games,
    0
  )
  
  minutes <- p3s5_num(
    minutes,
    0
  )
  
  assists <- p3s5_num(
    assists,
    0
  )
  
  turnovers <- p3s5_num(
    turnovers,
    0
  )
  
  if (
    games >= 50 &&
    minutes >= 1200 &&
    assists >= 150
  ) {
    return("HIGH")
  }
  
  if (
    games >= 25 &&
    minutes >= 500 &&
    (
      assists >= 60 ||
      turnovers >= 40
    )
  ) {
    return("MODERATE")
  }
  
  "LIMITED"
}


# ------------------------------------------------------------
# Playmaking role classification
# ------------------------------------------------------------

p3s5_creation_role <- function(
    creation_score,
    control_score,
    assist_volume_pct,
    usage_rate = NA_real_) {
  
  creation_score <- p3s5_num(
    creation_score
  )
  
  control_score <- p3s5_num(
    control_score
  )
  
  assist_volume_pct <- p3s5_num(
    assist_volume_pct
  )
  
  usage_rate <- p3s5_num(
    usage_rate
  )
  
  if (
    !is.na(creation_score) &&
    creation_score >= 82 &&
    (
      is.na(usage_rate) ||
      usage_rate >= 0.22
    )
  ) {
    return("PRIMARY CREATOR")
  }
  
  if (
    !is.na(creation_score) &&
    creation_score >= 68
  ) {
    return("SECONDARY CREATOR")
  }
  
  if (
    !is.na(control_score) &&
    control_score >= 68 &&
    !is.na(assist_volume_pct) &&
    assist_volume_pct >= 45
  ) {
    return("CONNECTOR")
  }
  
  if (
    !is.na(assist_volume_pct) &&
    assist_volume_pct < 30
  ) {
    return("LOW-CREATION ROLE")
  }
  
  "SUPPORT PLAYMAKER"
}


# ------------------------------------------------------------
# Create table
# ------------------------------------------------------------

create_player_season_playmaking_table <- function(
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
    CREATE TABLE IF NOT EXISTS player_season_playmaking (

      player_id INTEGER NOT NULL,
      team_id INTEGER NOT NULL,
      season TEXT NOT NULL,

      games_played INTEGER,
      minutes DOUBLE,

      assists DOUBLE,
      turnovers DOUBLE,

      assists_per_game DOUBLE,
      turnovers_per_game DOUBLE,

      assists_per_100 DOUBLE,
      turnovers_per_100 DOUBLE,

      assist_turnover_ratio DOUBLE,
      assist_pct DOUBLE,
      usage_rate DOUBLE,

      assist_volume_percentile DOUBLE,
      assist_rate_percentile DOUBLE,
      assist_turnover_percentile DOUBLE,
      ball_security_percentile DOUBLE,
      usage_percentile DOUBLE,

      creation_score DOUBLE,
      passing_control_score DOUBLE,
      secondary_creation_score DOUBLE,
      ball_security_score DOUBLE,

      creation_role TEXT,
      playmaking_confidence TEXT,

      source_name TEXT,
      metric_version TEXT DEFAULT 'P3S5_v1',

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
    CREATE INDEX IF NOT EXISTS idx_player_playmaking_player
    ON player_season_playmaking(player_id)
    ",
    "
    CREATE INDEX IF NOT EXISTS idx_player_playmaking_team
    ON player_season_playmaking(team_id)
    ",
    "
    CREATE INDEX IF NOT EXISTS idx_player_playmaking_season
    ON player_season_playmaking(season)
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
# Read Step-1 + Step-2 source data
# ------------------------------------------------------------

get_phase3_playmaking_source <- function(
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
  
  if (
    !"player_season_stats" %in%
    tables
  ) {
    return(
      data.frame()
    )
  }
  
  advanced_exists <-
    "player_season_advanced" %in%
    tables
  
  join_sql <- if (
    advanced_exists
  ) {
    "
    LEFT JOIN player_season_advanced adv
      ON adv.player_id = base.player_id
      AND adv.team_id = base.team_id
      AND adv.season = base.season
    "
  } else {
    ""
  }
  
  advanced_fields <- if (
    advanced_exists
  ) {
    "
      adv.assists_per_100,
      adv.turnovers_per_100,
      adv.assist_turnover_ratio,
      adv.assist_pct,
      adv.usage_rate,
    "
  } else {
    "
      NULL AS assists_per_100,
      NULL AS turnovers_per_100,
      NULL AS assist_turnover_ratio,
      NULL AS assist_pct,
      NULL AS usage_rate,
    "
  }
  
  sql <- paste0(
    "
    SELECT
      base.player_id,
      base.team_id,
      base.season,
      base.games_played,
      base.minutes,
      base.assists,
      base.turnovers,
      base.assists_per_game,
      base.turnovers_per_game,
      ",
    advanced_fields,
    "
      base.source_name

    FROM player_season_stats base
    ",
    join_sql,
    "
    WHERE base.season = ?
    "
  )
  
  DBI::dbGetQuery(
    con,
    sql,
    params = list(
      as.character(
        season
      )
    )
  )
}


# ------------------------------------------------------------
# Derive playmaking intelligence
# ------------------------------------------------------------

derive_phase3_playmaking_intelligence <- function(
    season_data) {
  
  if (
    is.null(season_data) ||
    !is.data.frame(season_data) ||
    !nrow(season_data)
  ) {
    return(
      data.frame()
    )
  }
  
  d <- season_data
  
  if (
    all(
      !is.finite(
        suppressWarnings(
          as.numeric(
            d$assists_per_100
          )
        )
      )
    )
  ) {
    
    # Fallback only when Step-2 per-100 isn't populated.
    d$assists_per_100 <-
      100 *
      p3s5_divide(
        d$assists,
        d$minutes
      ) *
      48 / 100
  }
  
  if (
    all(
      !is.finite(
        suppressWarnings(
          as.numeric(
            d$turnovers_per_100
          )
        )
      )
    )
  ) {
    
    d$turnovers_per_100 <-
      100 *
      p3s5_divide(
        d$turnovers,
        d$minutes
      ) *
      48 / 100
  }
  
  if (
    all(
      !is.finite(
        suppressWarnings(
          as.numeric(
            d$assist_turnover_ratio
          )
        )
      )
    )
  ) {
    
    d$assist_turnover_ratio <-
      p3s5_divide(
        d$assists,
        d$turnovers
      )
  }
  
  # ----------------------------------------------------------
  # Percentiles
  # ----------------------------------------------------------
  
  d$assist_volume_percentile <-
    p3s5_percentile(
      d$assists_per_game
    )
  
  d$assist_rate_percentile <-
    p3s5_percentile(
      d$assists_per_100
    )
  
  d$assist_turnover_percentile <-
    p3s5_percentile(
      d$assist_turnover_ratio
    )
  
  d$ball_security_percentile <-
    p3s5_reverse_percentile(
      d$turnovers_per_100
    )
  
  d$usage_percentile <-
    p3s5_percentile(
      d$usage_rate
    )
  
  n <- nrow(d)
  
  d$creation_score <-
    rep(
      NA_real_,
      n
    )
  
  d$passing_control_score <-
    rep(
      NA_real_,
      n
    )
  
  d$secondary_creation_score <-
    rep(
      NA_real_,
      n
    )
  
  d$ball_security_score <-
    d$ball_security_percentile
  
  for (
    i in seq_len(n)
  ) {
    
    d$passing_control_score[[i]] <-
      p3s5_weighted_mean(
        c(
          d$assist_turnover_percentile[[i]],
          d$ball_security_percentile[[i]]
        ),
        c(
          0.58,
          0.42
        )
      )
    
    base_creation <-
      p3s5_weighted_mean(
        c(
          d$assist_volume_percentile[[i]],
          d$assist_rate_percentile[[i]],
          d$passing_control_score[[i]]
        ),
        c(
          0.40,
          0.36,
          0.24
        )
      )
    
    # Usage only contributes if real usage data exists.
    if (
      is.finite(
        d$usage_percentile[[i]]
      )
    ) {
      
      d$creation_score[[i]] <-
        p3s5_weighted_mean(
          c(
            base_creation,
            d$usage_percentile[[i]]
          ),
          c(
            0.82,
            0.18
          )
        )
      
    } else {
      
      d$creation_score[[i]] <-
        base_creation
    }
    
    d$secondary_creation_score[[i]] <-
      p3s5_weighted_mean(
        c(
          d$assist_rate_percentile[[i]],
          d$passing_control_score[[i]]
        ),
        c(
          0.58,
          0.42
        )
      )
  }
  
  d$playmaking_confidence <- vapply(
    seq_len(n),
    function(i) {
      p3s5_confidence_label(
        games =
          d$games_played[[i]],
        minutes =
          d$minutes[[i]],
        assists =
          d$assists[[i]],
        turnovers =
          d$turnovers[[i]]
      )
    },
    character(1)
  )
  
  d$creation_role <- vapply(
    seq_len(n),
    function(i) {
      p3s5_creation_role(
        creation_score =
          d$creation_score[[i]],
        control_score =
          d$passing_control_score[[i]],
        assist_volume_pct =
          d$assist_volume_percentile[[i]],
        usage_rate =
          d$usage_rate[[i]]
      )
    },
    character(1)
  )
  
  d$metric_version <-
    "P3S5_v1"
  
  d$updated_at <-
    Sys.time()
  
  keep <- c(
    "player_id",
    "team_id",
    "season",
    "games_played",
    "minutes",
    "assists",
    "turnovers",
    "assists_per_game",
    "turnovers_per_game",
    "assists_per_100",
    "turnovers_per_100",
    "assist_turnover_ratio",
    "assist_pct",
    "usage_rate",
    "assist_volume_percentile",
    "assist_rate_percentile",
    "assist_turnover_percentile",
    "ball_security_percentile",
    "usage_percentile",
    "creation_score",
    "passing_control_score",
    "secondary_creation_score",
    "ball_security_score",
    "creation_role",
    "playmaking_confidence",
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

upsert_phase3_playmaking_intelligence <- function(
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
  
  create_player_season_playmaking_table(
    con
  )
  
  temp_table <- paste0(
    "tmp_player_playmaking_",
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
  
  columns <- names(df)
  
  columns_q <- paste(
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
          DELETE FROM player_season_playmaking
          WHERE EXISTS (
            SELECT 1
            FROM ",
          temp_q,
          " src
            WHERE src.player_id = player_season_playmaking.player_id
              AND src.team_id = player_season_playmaking.team_id
              AND src.season = player_season_playmaking.season
          )
          "
        )
      )
      
      DBI::dbExecute(
        con,
        paste0(
          "
          INSERT INTO player_season_playmaking (
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
# Build one season
# ------------------------------------------------------------

build_phase3_playmaking_intelligence <- function(
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
    get_phase3_playmaking_source(
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
            "NO PLAYER DATA",
          rows_processed =
            0L
        )
      )
    )
  }
  
  derived <-
    derive_phase3_playmaking_intelligence(
      source
    )
  
  result <-
    upsert_phase3_playmaking_intelligence(
      derived,
      con = con
    )
  
  invisible(
    c(
      list(
        status =
          "PLAYMAKING INTELLIGENCE BUILT"
      ),
      result
    )
  )
}


# ------------------------------------------------------------
# Read intelligence
# ------------------------------------------------------------

get_phase3_playmaking_intelligence <- function(
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
    !tbi_playmaking_table() %in%
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
      "play.season = ?"
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
      "play.player_id = ?"
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
      play.*,
      p.player_name,
      p.primary_position,
      p.player_age,
      t.team_name,
      t.abbreviation

    FROM player_season_playmaking play

    LEFT JOIN players p
      ON p.player_id = play.player_id

    LEFT JOIN teams t
      ON t.team_id = play.team_id

    ",
    where_sql,
    "

    ORDER BY
      play.creation_score DESC,
      play.passing_control_score DESC,
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
# Attach playmaking to a BIE roster frame
# ------------------------------------------------------------

attach_phase3_playmaking_to_players <- function(
    players,
    season,
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
  
  playmaking <-
    get_phase3_playmaking_intelligence(
      season =
        season,
      con =
        con
    )
  
  if (!nrow(playmaking)) {
    return(players)
  }
  
  keep <- c(
    "player_id",
    "team_id",
    "assists_per_100",
    "turnovers_per_100",
    "assist_turnover_ratio",
    "assist_pct",
    "usage_rate",
    "creation_score",
    "passing_control_score",
    "secondary_creation_score",
    "ball_security_score",
    "creation_role",
    "playmaking_confidence"
  )
  
  playmaking <- playmaking[
    ,
    intersect(
      keep,
      names(playmaking)
    ),
    drop = FALSE
  ]
  
  if (
    "team_id" %in%
    names(players) &&
    "team_id" %in%
    names(playmaking)
  ) {
    
    return(
      merge(
        players,
        playmaking,
        by = c(
          "player_id",
          "team_id"
        ),
        all.x = TRUE,
        sort = FALSE
      )
    )
  }
  
  playmaking <- playmaking[
    !duplicated(
      playmaking$player_id
    ),
    ,
    drop = FALSE
  ]
  
  playmaking$team_id <- NULL
  
  merge(
    players,
    playmaking,
    by = "player_id",
    all.x = TRUE,
    sort = FALSE
  )
}


# ------------------------------------------------------------
# Step 5 health check
# ------------------------------------------------------------

phase3_step5_healthcheck <- function(
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
    tbi_playmaking_table() %in%
    tables
  
  rows <- if (
    exists_table
  ) {
    DBI::dbGetQuery(
      con,
      "
      SELECT COUNT(*) AS n
      FROM player_season_playmaking
      WHERE season = ?
      ",
      params = list(
        season
      )
    )$n[[1]]
  } else {
    0L
  }
  
  rated <- if (
    exists_table &&
    rows > 0
  ) {
    DBI::dbGetQuery(
      con,
      "
      SELECT COUNT(*) AS n
      FROM player_season_playmaking
      WHERE season = ?
        AND creation_score IS NOT NULL
      ",
      params = list(
        season
      )
    )$n[[1]]
  } else {
    0L
  }
  
  usage_rows <- if (
    exists_table &&
    rows > 0
  ) {
    DBI::dbGetQuery(
      con,
      "
      SELECT COUNT(*) AS n
      FROM player_season_playmaking
      WHERE season = ?
        AND usage_rate IS NOT NULL
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
      "Step 5 — Playmaking + Creation Intelligence",
    status = if (
      rows > 0 &&
      rated > 0
    ) {
      "READY"
    } else {
      "BUILD REQUIRED"
    },
    season =
      season,
    playmaking_rows =
      rows,
    creation_rated_rows =
      rated,
    usage_rate_rows =
      usage_rows,
    usage_status = if (
      usage_rows > 0
    ) {
      "USAGE DATA ACTIVE"
    } else {
      "USAGE DATA NOT LOADED — CREATION SCORE USES ASSIST/CONTROL SIGNALS ONLY"
    }
  )
}

# ============================================================
# PHASE 3.2 — STEP 5 V2
# Playmaking + Creation calibration
# ============================================================


p3s5_v2_percentile <- function(
    x,
    reference_mask = NULL,
    higher_is_better = TRUE) {
  
  value <- suppressWarnings(
    as.numeric(x)
  )
  
  valid <- is.finite(value)
  out <- rep(NA_real_, length(value))
  
  if (is.null(reference_mask)) {
    reference_mask <- valid
  } else {
    reference_mask <-
      as.logical(reference_mask) &
      valid
  }
  
  reference <- value[
    reference_mask
  ]
  
  if (!length(reference)) {
    return(out)
  }
  
  for (i in seq_along(value)) {
    
    if (!valid[[i]]) {
      next
    }
    
    below <- sum(
      reference < value[[i]],
      na.rm = TRUE
    )
    
    equal <- sum(
      reference == value[[i]],
      na.rm = TRUE
    )
    
    pct <-
      100 *
      (
        below +
          0.5 * equal
      ) /
      length(reference)
    
    if (!isTRUE(higher_is_better)) {
      pct <- 100 - pct
    }
    
    out[[i]] <-
      max(
        0,
        min(
          100,
          pct
        )
      )
  }
  
  out
}


p3s5_v2_reliability <- function(
    games,
    minutes,
    assists) {
  
  games <- p3s5_num(games, 0)
  minutes <- p3s5_num(minutes, 0)
  assists <- p3s5_num(assists, 0)
  
  max(
    0.65,
    min(
      1,
      0.35 * min(1, games / 40) +
        0.40 * min(1, minutes / 900) +
        0.25 * min(1, assists / 200)
    )
  )
}


p3s5_v2_shrink <- function(
    score,
    reliability) {
  
  score <- p3s5_num(score)
  
  if (is.na(score)) {
    return(NA_real_)
  }
  
  50 +
    (
      score -
        50
    ) *
    reliability
}


derive_phase3_playmaking_intelligence <- function(
    season_data) {
  
  if (
    is.null(season_data) ||
    !is.data.frame(season_data) ||
    !nrow(season_data)
  ) {
    return(
      data.frame()
    )
  }
  
  d <- season_data
  
  if (
    all(
      !is.finite(
        suppressWarnings(
          as.numeric(
            d$assist_turnover_ratio
          )
        )
      )
    )
  ) {
    d$assist_turnover_ratio <-
      p3s5_divide(
        d$assists,
        d$turnovers
      )
  }
  
  games <- suppressWarnings(
    as.numeric(
      d$games_played
    )
  )
  
  minutes <- suppressWarnings(
    as.numeric(
      d$minutes
    )
  )
  
  qualified <-
    is.finite(games) &
    is.finite(minutes) &
    games >= 20 &
    minutes >= 300
  
  if (
    sum(
      qualified,
      na.rm = TRUE
    ) < 100
  ) {
    qualified <-
      is.finite(games) &
      is.finite(minutes)
  }
  
  d$assist_volume_percentile <-
    p3s5_v2_percentile(
      d$assists_per_game,
      qualified
    )
  
  d$assist_rate_percentile <-
    p3s5_v2_percentile(
      d$assists_per_100,
      qualified
    )
  
  d$assist_turnover_percentile <-
    p3s5_v2_percentile(
      d$assist_turnover_ratio,
      qualified
    )
  
  d$ball_security_percentile <-
    p3s5_v2_percentile(
      d$turnovers_per_100,
      qualified,
      higher_is_better = FALSE
    )
  
  d$usage_percentile <-
    p3s5_v2_percentile(
      d$usage_rate,
      qualified
    )
  
  n <- nrow(d)
  
  d$creation_score <- rep(
    NA_real_,
    n
  )
  
  d$passing_control_score <- rep(
    NA_real_,
    n
  )
  
  d$secondary_creation_score <- rep(
    NA_real_,
    n
  )
  
  d$ball_security_score <- rep(
    NA_real_,
    n
  )
  
  for (i in seq_len(n)) {
    
    reliability <-
      p3s5_v2_reliability(
        d$games_played[[i]],
        d$minutes[[i]],
        d$assists[[i]]
      )
    
    raw_security <-
      p3s5_weighted_mean(
        c(
          d$ball_security_percentile[[i]],
          d$assist_turnover_percentile[[i]]
        ),
        c(
          0.55,
          0.45
        )
      )
    
    # Passing control now requires actual passing volume.
    raw_control <-
      p3s5_weighted_mean(
        c(
          d$assist_turnover_percentile[[i]],
          d$ball_security_percentile[[i]],
          d$assist_volume_percentile[[i]]
        ),
        c(
          0.50,
          0.25,
          0.25
        )
      )
    
    raw_creation <-
      p3s5_weighted_mean(
        c(
          d$assist_volume_percentile[[i]],
          d$assist_rate_percentile[[i]],
          raw_control
        ),
        c(
          0.46,
          0.38,
          0.16
        )
      )
    
    if (
      is.finite(
        d$usage_percentile[[i]]
      )
    ) {
      raw_creation <-
        p3s5_weighted_mean(
          c(
            raw_creation,
            d$usage_percentile[[i]]
          ),
          c(
            0.85,
            0.15
          )
        )
    }
    
    raw_secondary <-
      p3s5_weighted_mean(
        c(
          d$assist_rate_percentile[[i]],
          d$assist_volume_percentile[[i]],
          raw_control
        ),
        c(
          0.50,
          0.30,
          0.20
        )
      )
    
    d$ball_security_score[[i]] <-
      p3s5_v2_shrink(
        raw_security,
        reliability
      )
    
    d$passing_control_score[[i]] <-
      p3s5_v2_shrink(
        raw_control,
        reliability
      )
    
    d$creation_score[[i]] <-
      p3s5_v2_shrink(
        raw_creation,
        reliability
      )
    
    d$secondary_creation_score[[i]] <-
      p3s5_v2_shrink(
        raw_secondary,
        reliability
      )
  }
  
  d$playmaking_confidence <- vapply(
    seq_len(n),
    function(i) {
      p3s5_confidence_label(
        games =
          d$games_played[[i]],
        minutes =
          d$minutes[[i]],
        assists =
          d$assists[[i]],
        turnovers =
          d$turnovers[[i]]
      )
    },
    character(1)
  )
  
  d$creation_role <- vapply(
    seq_len(n),
    function(i) {
      p3s5_creation_role(
        creation_score =
          d$creation_score[[i]],
        control_score =
          d$passing_control_score[[i]],
        assist_volume_pct =
          d$assist_volume_percentile[[i]],
        usage_rate =
          d$usage_rate[[i]]
      )
    },
    character(1)
  )
  
  d$metric_version <-
    "P3S5_v2_CALIBRATED"
  
  d$updated_at <-
    Sys.time()
  
  keep <- c(
    "player_id",
    "team_id",
    "season",
    "games_played",
    "minutes",
    "assists",
    "turnovers",
    "assists_per_game",
    "turnovers_per_game",
    "assists_per_100",
    "turnovers_per_100",
    "assist_turnover_ratio",
    "assist_pct",
    "usage_rate",
    "assist_volume_percentile",
    "assist_rate_percentile",
    "assist_turnover_percentile",
    "ball_security_percentile",
    "usage_percentile",
    "creation_score",
    "passing_control_score",
    "secondary_creation_score",
    "ball_security_score",
    "creation_role",
    "playmaking_confidence",
    "source_name",
    "metric_version",
    "updated_at"
  )
  
  d[
    ,
    intersect(
      keep,
      names(d)
    ),
    drop = FALSE
  ]
}


phase3_step5_v2_healthcheck <- function(
    season = "2025-26") {
  
  source <-
    get_phase3_playmaking_source(
      season = season
    )
  
  d <-
    derive_phase3_playmaking_intelligence(
      source
    )
  
  list(
    phase = "Phase 3.2",
    step =
      "Step 5 — Playmaking + Creation Calibration V2",
    status = if (
      nrow(d) > 0 &&
      any(
        is.finite(
          d$creation_score
        )
      )
    ) {
      "READY FOR REBUILD"
    } else {
      "REVIEW"
    },
    rows = nrow(d),
    metric_version =
      "P3S5_v2_CALIBRATED"
  )
}

# ============================================================
# PHASE 3.3 — STEP 5 V3
# Official Usage + Assist% Playmaking Calibration
#
# Architecture:
#   - official AST% is a direct creation signal
#   - official USG% is responsibility/context, not a raw
#     "higher is always better" talent score
#   - assist volume + AST% + per-100 assists anchor creation
#   - AST/TO + turnover control anchor passing control
#   - sample reliability remains games/minutes aware
#   - missing official metrics never fabricated
# ============================================================


p3s5_v3_usage_context <- function(
    usage_rate,
    usage_percentile) {
  
  usage_rate <- suppressWarnings(
    as.numeric(usage_rate)
  )
  
  usage_percentile <- suppressWarnings(
    as.numeric(usage_percentile)
  )
  
  n <- max(
    length(usage_rate),
    length(usage_percentile)
  )
  
  usage_rate <- rep(
    usage_rate,
    length.out = n
  )
  
  usage_percentile <- rep(
    usage_percentile,
    length.out = n
  )
  
  out <- rep(
    NA_real_,
    n
  )
  
  # Usage is treated as offensive responsibility context.
  # Extreme usage does not automatically equal elite creation.
  #
  # The centered score rewards players carrying meaningful
  # offensive responsibility without making usage itself a
  # dominant performance signal.
  valid_rate <- is.finite(
    usage_rate
  )
  
  out[valid_rate] <-
    pmax(
      0,
      pmin(
        100,
        50 +
          (
            usage_rate[valid_rate] -
              0.20
          ) /
          0.15 *
          35
      )
    )
  
  # If the league-relative percentile is available, blend it
  # with the centered usage context.
  both <-
    valid_rate &
    is.finite(
      usage_percentile
    )
  
  out[both] <-
    0.60 *
    out[both] +
    0.40 *
    usage_percentile[both]
  
  only_pct <-
    !valid_rate &
    is.finite(
      usage_percentile
    )
  
  out[only_pct] <-
    usage_percentile[
      only_pct
    ]
  
  out
}


derive_phase3_playmaking_intelligence <- function(
    season_data) {
  
  if (
    is.null(season_data) ||
    !is.data.frame(season_data) ||
    !nrow(season_data)
  ) {
    return(
      data.frame()
    )
  }
  
  d <- season_data
  
  required_numeric <- c(
    "games_played",
    "minutes",
    "assists",
    "turnovers",
    "assists_per_game",
    "turnovers_per_game",
    "assists_per_100",
    "turnovers_per_100",
    "assist_turnover_ratio",
    "assist_pct",
    "usage_rate"
  )
  
  for (nm in required_numeric) {
    
    if (!nm %in% names(d)) {
      d[[nm]] <- NA_real_
    }
    
    d[[nm]] <- suppressWarnings(
      as.numeric(
        d[[nm]]
      )
    )
  }
  
  if (
    all(
      !is.finite(
        d$assist_turnover_ratio
      )
    )
  ) {
    d$assist_turnover_ratio <-
      p3s5_divide(
        d$assists,
        d$turnovers
      )
  }
  
  games <- d$games_played
  minutes <- d$minutes
  
  qualified <-
    is.finite(games) &
    is.finite(minutes) &
    games >= 20 &
    minutes >= 300
  
  if (
    sum(
      qualified,
      na.rm = TRUE
    ) < 100
  ) {
    qualified <-
      is.finite(games) &
      is.finite(minutes)
  }
  
  # ----------------------------------------------------------
  # League-relative creation signals
  # ----------------------------------------------------------
  
  d$assist_volume_percentile <-
    p3s5_v2_percentile(
      d$assists_per_game,
      qualified
    )
  
  d$assist_rate_percentile <-
    p3s5_v2_percentile(
      d$assists_per_100,
      qualified
    )
  
  # Official NBA AST% is now an active creation input.
  d$assist_pct_percentile <-
    p3s5_v2_percentile(
      d$assist_pct,
      qualified
    )
  
  d$assist_turnover_percentile <-
    p3s5_v2_percentile(
      d$assist_turnover_ratio,
      qualified
    )
  
  d$ball_security_percentile <-
    p3s5_v2_percentile(
      d$turnovers_per_100,
      qualified,
      higher_is_better = FALSE
    )
  
  d$usage_percentile <-
    p3s5_v2_percentile(
      d$usage_rate,
      qualified
    )
  
  d$usage_context_score <-
    p3s5_v3_usage_context(
      d$usage_rate,
      d$usage_percentile
    )
  
  n <- nrow(d)
  
  d$creation_score <- rep(
    NA_real_,
    n
  )
  
  d$passing_control_score <- rep(
    NA_real_,
    n
  )
  
  d$secondary_creation_score <- rep(
    NA_real_,
    n
  )
  
  d$ball_security_score <- rep(
    NA_real_,
    n
  )
  
  # ----------------------------------------------------------
  # Player-level calibrated scoring
  # ----------------------------------------------------------
  
  for (i in seq_len(n)) {
    
    reliability <-
      p3s5_v2_reliability(
        d$games_played[[i]],
        d$minutes[[i]],
        d$assists[[i]]
      )
    
    raw_security <-
      p3s5_weighted_mean(
        c(
          d$ball_security_percentile[[i]],
          d$assist_turnover_percentile[[i]]
        ),
        c(
          0.55,
          0.45
        )
      )
    
    # Passing control:
    #   quality/control first, then actual creation volume.
    raw_control <-
      p3s5_weighted_mean(
        c(
          d$assist_turnover_percentile[[i]],
          d$ball_security_percentile[[i]],
          d$assist_pct_percentile[[i]],
          d$assist_volume_percentile[[i]]
        ),
        c(
          0.35,
          0.20,
          0.30,
          0.15
        )
      )
    
    # Primary creation:
    #
    # 30% official AST%
    # 27% assists / 100
    # 23% assists / game
    # 12% passing control
    #  8% usage responsibility context
    #
    # Missing inputs are automatically reweighted by
    # p3s5_weighted_mean(); no metric is fabricated.
    raw_creation <-
      p3s5_weighted_mean(
        c(
          d$assist_pct_percentile[[i]],
          d$assist_rate_percentile[[i]],
          d$assist_volume_percentile[[i]],
          raw_control,
          d$usage_context_score[[i]]
        ),
        c(
          0.30,
          0.27,
          0.23,
          0.12,
          0.08
        )
      )
    
    # Secondary creation emphasizes scalable passing rather
    # than pure ball dominance.
    raw_secondary <-
      p3s5_weighted_mean(
        c(
          d$assist_pct_percentile[[i]],
          d$assist_rate_percentile[[i]],
          raw_control,
          d$assist_volume_percentile[[i]],
          d$usage_context_score[[i]]
        ),
        c(
          0.30,
          0.28,
          0.22,
          0.15,
          0.05
        )
      )
    
    d$ball_security_score[[i]] <-
      p3s5_v2_shrink(
        raw_security,
        reliability
      )
    
    d$passing_control_score[[i]] <-
      p3s5_v2_shrink(
        raw_control,
        reliability
      )
    
    d$creation_score[[i]] <-
      p3s5_v2_shrink(
        raw_creation,
        reliability
      )
    
    d$secondary_creation_score[[i]] <-
      p3s5_v2_shrink(
        raw_secondary,
        reliability
      )
  }
  
  d$playmaking_confidence <- vapply(
    seq_len(n),
    function(i) {
      
      base <- p3s5_confidence_label(
        games =
          d$games_played[[i]],
        minutes =
          d$minutes[[i]],
        assists =
          d$assists[[i]],
        turnovers =
          d$turnovers[[i]]
      )
      
      # Official AST% + USG% make the role/context evidence
      # more complete, but do not override low sample size.
      official_context <-
        is.finite(
          d$assist_pct[[i]]
        ) &&
        is.finite(
          d$usage_rate[[i]]
        )
      
      if (
        identical(
          base,
          "HIGH"
        ) &&
        official_context
      ) {
        "HIGH"
      } else {
        base
      }
    },
    character(1)
  )
  
  d$creation_role <- vapply(
    seq_len(n),
    function(i) {
      p3s5_creation_role(
        creation_score =
          d$creation_score[[i]],
        control_score =
          d$passing_control_score[[i]],
        assist_volume_pct =
          d$assist_volume_percentile[[i]],
        usage_rate =
          d$usage_rate[[i]]
      )
    },
    character(1)
  )
  
  d$source_name[
    is.na(d$source_name) |
      !nzchar(
        as.character(
          d$source_name
        )
      )
  ] <- "TBI / NBA Advanced"
  
  d$metric_version <-
    "P3S5_v3_OFFICIAL_USAGE_ASTPCT"
  
  d$updated_at <-
    Sys.time()
  
  # Keep the canonical Step-5 table stable.
  # Diagnostic context remains available in preview output
  # when the table schema is expanded later.
  keep <- c(
    "player_id",
    "team_id",
    "season",
    "games_played",
    "minutes",
    "assists",
    "turnovers",
    "assists_per_game",
    "turnovers_per_game",
    "assists_per_100",
    "turnovers_per_100",
    "assist_turnover_ratio",
    "assist_pct",
    "usage_rate",
    "assist_volume_percentile",
    "assist_rate_percentile",
    "assist_turnover_percentile",
    "ball_security_percentile",
    "usage_percentile",
    "creation_score",
    "passing_control_score",
    "secondary_creation_score",
    "ball_security_score",
    "creation_role",
    "playmaking_confidence",
    "source_name",
    "metric_version",
    "updated_at"
  )
  
  d[
    ,
    intersect(
      keep,
      names(d)
    ),
    drop = FALSE
  ]
}


phase3_step5_v3_healthcheck <- function(
    season = "2025-26") {
  
  source <- tryCatch(
    get_phase3_playmaking_source(
      season = season
    ),
    error = function(e) {
      structure(
        data.frame(),
        error =
          conditionMessage(e)
      )
    }
  )
  
  if (!nrow(source)) {
    return(
      list(
        phase =
          "Phase 3.3",
        step =
          "Step 5 — Official Usage + AST% Creation",
        status =
          "REVIEW",
        explanation =
          attr(
            source,
            "error"
          ) %||%
          "No Step-5 source rows."
      )
    )
  }
  
  d <- derive_phase3_playmaking_intelligence(
    source
  )
  
  numeric_count <- function(x) {
    sum(
      is.finite(
        suppressWarnings(
          as.numeric(x)
        )
      )
    )
  }
  
  list(
    phase =
      "Phase 3.3",
    
    step =
      "Step 5 — Official Usage + AST% Creation",
    
    status = if (
      nrow(d) > 0 &&
      numeric_count(
        source$usage_rate
      ) > 0 &&
      numeric_count(
        source$assist_pct
      ) > 0 &&
      numeric_count(
        d$creation_score
      ) > 0
    ) {
      "READY FOR REBUILD"
    } else {
      "REVIEW"
    },
    
    season =
      season,
    
    rows =
      nrow(d),
    
    usage_rows =
      numeric_count(
        source$usage_rate
      ),
    
    assist_pct_rows =
      numeric_count(
        source$assist_pct
      ),
    
    creation_score_rows =
      numeric_count(
        d$creation_score
      ),
    
    passing_control_rows =
      numeric_count(
        d$passing_control_score
      ),
    
    metric_version =
      "P3S5_v3_OFFICIAL_USAGE_ASTPCT",
    
    formula_rule =
      "AST% + AST/100 + AST/G + CONTROL + SMALL USAGE CONTEXT; USAGE IS NOT TREATED AS PURE TALENT"
  )
}


preview_phase3_playmaking_v3 <- function(
    season = "2025-26",
    player_names = NULL) {
  
  d <- derive_phase3_playmaking_intelligence(
    get_phase3_playmaking_source(
      season = season
    )
  )
  
  if (!nrow(d)) {
    return(d)
  }
  
  con <- connect_db(
    read_only = TRUE
  )
  
  on.exit(
    disconnect_db(con),
    add = TRUE
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
  
  d <- merge(
    d,
    players,
    by = "player_id",
    all.x = TRUE,
    sort = FALSE
  )
  
  if (!is.null(player_names)) {
    d <- d[
      tolower(
        d$player_name
      ) %in%
        tolower(
          player_names
        ),
      ,
      drop = FALSE
    ]
  }
  
  keep <- intersect(
    c(
      "player_name",
      "season",
      "games_played",
      "minutes",
      "assists_per_game",
      "assists_per_100",
      "assist_pct",
      "usage_rate",
      "assist_volume_percentile",
      "assist_rate_percentile",
      "assist_turnover_percentile",
      "ball_security_percentile",
      "usage_percentile",
      "creation_score",
      "passing_control_score",
      "secondary_creation_score",
      "ball_security_score",
      "creation_role",
      "playmaking_confidence",
      "metric_version"
    ),
    names(d)
  )
  
  d[
    ,
    keep,
    drop = FALSE
  ]
}
