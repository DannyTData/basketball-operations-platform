# ============================================================
# TBI NBA Basketball Operations Platform
# PHASE 3 — STEP 4
# Shooting + Spacing Intelligence
#
# Uses real Step-1 / Step-2 player-season data.
#
# Important:
#   - league-context percentiles
#   - stabilized 3P% to protect against tiny samples
#   - 3PA volume matters
#   - TS% and eFG% matter
#   - FT rate is only a rim-pressure proxy
#   - no shot-location claims are fabricated
# ============================================================


# ------------------------------------------------------------
# Canonical shooting table
# ------------------------------------------------------------

tbi_shooting_table <- function() {
  "player_season_shooting"
}


# ------------------------------------------------------------
# Generic helpers
# ------------------------------------------------------------

p3s4_num <- function(
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


p3s4_divide <- function(
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


p3s4_percentile <- function(
    x,
    minimum_sample = NULL,
    sample_value = NULL) {
  
  value <- suppressWarnings(
    as.numeric(x)
  )
  
  valid <- is.finite(value)
  
  if (
    !is.null(minimum_sample) &&
    !is.null(sample_value)
  ) {
    
    sample_value <- suppressWarnings(
      as.numeric(
        sample_value
      )
    )
    
    valid <- valid &
      is.finite(sample_value) &
      sample_value >=
      minimum_sample
  }
  
  output <- rep(
    NA_real_,
    length(value)
  )
  
  if (!any(valid)) {
    return(output)
  }
  
  ranked <- rank(
    value[valid],
    ties.method = "average"
  )
  
  if (sum(valid) == 1L) {
    output[valid] <- 50
    return(output)
  }
  
  output[valid] <-
    100 *
    (ranked - 1) /
    (sum(valid) - 1)
  
  output
}


p3s4_weighted_mean <- function(
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
# Create shooting-intelligence table
# ------------------------------------------------------------

create_player_season_shooting_table <- function(
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
    CREATE TABLE IF NOT EXISTS player_season_shooting (

      player_id INTEGER NOT NULL,
      team_id INTEGER NOT NULL,
      season TEXT NOT NULL,

      games_played INTEGER,
      minutes DOUBLE,

      field_goals_attempted DOUBLE,
      three_pointers_made DOUBLE,
      three_pointers_attempted DOUBLE,
      free_throws_attempted DOUBLE,

      three_pointers_per_game DOUBLE,
      three_point_attempts_per_game DOUBLE,

      field_goal_pct DOUBLE,
      three_point_pct DOUBLE,
      stabilized_three_point_pct DOUBLE,
      free_throw_pct DOUBLE,

      effective_field_goal_pct DOUBLE,
      true_shooting_pct DOUBLE,

      three_point_attempt_rate DOUBLE,
      free_throw_rate DOUBLE,

      shooting_efficiency_percentile DOUBLE,
      three_point_accuracy_percentile DOUBLE,
      three_point_volume_percentile DOUBLE,
      spacing_percentile DOUBLE,
      rim_pressure_proxy_percentile DOUBLE,

      shooting_efficiency_score DOUBLE,
      spacing_score DOUBLE,
      shooting_gravity_score DOUBLE,
      rim_pressure_proxy_score DOUBLE,

      three_point_reliability DOUBLE,
      shooting_confidence TEXT,
      spacing_tier TEXT,

      source_name TEXT,
      metric_version TEXT DEFAULT 'P3S4_v1',

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
    CREATE INDEX IF NOT EXISTS idx_player_shooting_player
    ON player_season_shooting(player_id)
    ",
    "
    CREATE INDEX IF NOT EXISTS idx_player_shooting_team
    ON player_season_shooting(team_id)
    ",
    "
    CREATE INDEX IF NOT EXISTS idx_player_shooting_season
    ON player_season_shooting(season)
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
# Stabilize 3P%
#
# Shrinks raw 3P% toward the season league average.
# Prior weight = 75 attempts.
# ------------------------------------------------------------

p3s4_stabilized_three_pct <- function(
    made,
    attempted,
    league_three_pct,
    prior_attempts = 75) {
  
  made <- suppressWarnings(
    as.numeric(made)
  )
  
  attempted <- suppressWarnings(
    as.numeric(attempted)
  )
  
  league_three_pct <-
    p3s4_num(
      league_three_pct,
      0.36
    )
  
  out <-
    (
      made +
        prior_attempts *
        league_three_pct
    ) /
    (
      attempted +
        prior_attempts
    )
  
  out[
    !is.finite(attempted) |
      attempted < 0
  ] <- NA_real_
  
  out
}


# ------------------------------------------------------------
# Reliability from 3PA sample
# ------------------------------------------------------------

p3s4_three_point_reliability <- function(
    attempts) {
  
  attempts <- suppressWarnings(
    as.numeric(attempts)
  )
  
  # Smooth 0-1 curve.
  reliability <-
    attempts /
    (
      attempts +
        100
    )
  
  reliability[
    !is.finite(reliability)
  ] <- 0
  
  pmin(
    1,
    pmax(
      0,
      reliability
    )
  )
}


# ------------------------------------------------------------
# Confidence label
# ------------------------------------------------------------

p3s4_confidence_label <- function(
    games,
    minutes,
    three_pa) {
  
  games <- p3s4_num(
    games,
    0
  )
  
  minutes <- p3s4_num(
    minutes,
    0
  )
  
  three_pa <- p3s4_num(
    three_pa,
    0
  )
  
  if (
    games >= 50 &&
    minutes >= 1200 &&
    three_pa >= 150
  ) {
    return("HIGH")
  }
  
  if (
    games >= 25 &&
    minutes >= 500 &&
    three_pa >= 60
  ) {
    return("MODERATE")
  }
  
  "LIMITED"
}


# ------------------------------------------------------------
# Spacing tier
# ------------------------------------------------------------

p3s4_spacing_tier <- function(score) {
  
  score <- p3s4_num(
    score
  )
  
  if (is.na(score)) {
    return("UNRATED")
  }
  
  if (score >= 85) {
    return("ELITE SPACER")
  }
  
  if (score >= 70) {
    return("PLUS SPACER")
  }
  
  if (score >= 55) {
    return("FUNCTIONAL SPACER")
  }
  
  if (score >= 40) {
    return("LIMITED SPACER")
  }
  
  "NON-SPACING PROFILE"
}


# ------------------------------------------------------------
# Read full season input
# ------------------------------------------------------------

get_phase3_shooting_source <- function(
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
  
  advanced_join <- if (
    "player_season_advanced" %in%
    tables
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
    "player_season_advanced" %in%
    tables
  ) {
    "
      adv.effective_field_goal_pct,
      adv.true_shooting_pct,
      adv.three_point_attempt_rate,
      adv.free_throw_rate,
    "
  } else {
    "
      NULL AS effective_field_goal_pct,
      NULL AS true_shooting_pct,
      NULL AS three_point_attempt_rate,
      NULL AS free_throw_rate,
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
      base.field_goals_attempted,
      base.three_pointers_made,
      base.three_pointers_attempted,
      base.free_throws_attempted,
      base.field_goal_pct,
      base.three_point_pct,
      base.free_throw_pct,
      ",
    advanced_fields,
    "
      base.source_name

    FROM player_season_stats base
    ",
    advanced_join,
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
# Derive shooting + spacing intelligence
# ------------------------------------------------------------

derive_phase3_shooting_intelligence <- function(
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
  
  d$three_pointers_per_game <-
    p3s4_divide(
      d$three_pointers_made,
      d$games_played
    )
  
  d$three_point_attempts_per_game <-
    p3s4_divide(
      d$three_pointers_attempted,
      d$games_played
    )
  
  if (
    all(
      !is.finite(
        suppressWarnings(
          as.numeric(
            d$effective_field_goal_pct
          )
        )
      )
    )
  ) {
    
    # eFG cannot be reconstructed without FGM in this source frame.
    d$effective_field_goal_pct <-
      NA_real_
  }
  
  if (
    all(
      !is.finite(
        suppressWarnings(
          as.numeric(
            d$three_point_attempt_rate
          )
        )
      )
    )
  ) {
    
    d$three_point_attempt_rate <-
      p3s4_divide(
        d$three_pointers_attempted,
        d$field_goals_attempted
      )
  }
  
  if (
    all(
      !is.finite(
        suppressWarnings(
          as.numeric(
            d$free_throw_rate
          )
        )
      )
    )
  ) {
    
    d$free_throw_rate <-
      p3s4_divide(
        d$free_throws_attempted,
        d$field_goals_attempted
      )
  }
  
  three_pct <- suppressWarnings(
    as.numeric(
      d$three_point_pct
    )
  )
  
  three_pa <- suppressWarnings(
    as.numeric(
      d$three_pointers_attempted
    )
  )
  
  three_pm <- suppressWarnings(
    as.numeric(
      d$three_pointers_made
    )
  )
  
  league_three_pct <- if (
    sum(
      three_pa[
        is.finite(
          three_pa
        )
      ],
      na.rm = TRUE
    ) > 0
  ) {
    sum(
      three_pm,
      na.rm = TRUE
    ) /
      sum(
        three_pa,
        na.rm = TRUE
      )
  } else {
    0.36
  }
  
  d$stabilized_three_point_pct <-
    p3s4_stabilized_three_pct(
      made =
        three_pm,
      attempted =
        three_pa,
      league_three_pct =
        league_three_pct,
      prior_attempts = 75
    )
  
  d$three_point_reliability <-
    p3s4_three_point_reliability(
      three_pa
    )
  
  # League-context percentiles.
  d$shooting_efficiency_percentile <-
    p3s4_percentile(
      d$true_shooting_pct
    )
  
  efg_percentile <-
    p3s4_percentile(
      d$effective_field_goal_pct
    )
  
  d$three_point_accuracy_percentile <-
    p3s4_percentile(
      d$stabilized_three_point_pct
    )
  
  d$three_point_volume_percentile <-
    p3s4_percentile(
      d$three_point_attempt_rate
    )
  
  attempts_pg_percentile <-
    p3s4_percentile(
      d$three_point_attempts_per_game
    )
  
  d$rim_pressure_proxy_percentile <-
    p3s4_percentile(
      d$free_throw_rate
    )
  
  n <- nrow(d)
  
  d$shooting_efficiency_score <-
    rep(
      NA_real_,
      n
    )
  
  d$spacing_score <-
    rep(
      NA_real_,
      n
    )
  
  d$shooting_gravity_score <-
    rep(
      NA_real_,
      n
    )
  
  d$rim_pressure_proxy_score <-
    d$rim_pressure_proxy_percentile
  
  for (
    i in seq_len(n)
  ) {
    
    efficiency_values <- c(
      d$shooting_efficiency_percentile[[i]],
      efg_percentile[[i]]
    )
    
    d$shooting_efficiency_score[[i]] <-
      p3s4_weighted_mean(
        efficiency_values,
        c(
          0.65,
          0.35
        )
      )
    
    # Gravity rewards willingness/volume more than raw accuracy.
    d$shooting_gravity_score[[i]] <-
      p3s4_weighted_mean(
        c(
          d$three_point_volume_percentile[[i]],
          attempts_pg_percentile[[i]],
          d$three_point_accuracy_percentile[[i]]
        ),
        c(
          0.42,
          0.33,
          0.25
        )
      )
    
    accuracy_weight <-
      0.36 +
      0.14 *
      d$three_point_reliability[[i]]
    
    volume_weight <- 0.33
    
    efficiency_weight <-
      1 -
      accuracy_weight -
      volume_weight
    
    d$spacing_score[[i]] <-
      p3s4_weighted_mean(
        c(
          d$three_point_accuracy_percentile[[i]],
          d$shooting_gravity_score[[i]],
          d$shooting_efficiency_score[[i]]
        ),
        c(
          accuracy_weight,
          volume_weight,
          efficiency_weight
        )
      )
  }
  
  d$spacing_percentile <-
    p3s4_percentile(
      d$spacing_score
    )
  
  d$shooting_confidence <- vapply(
    seq_len(n),
    function(i) {
      p3s4_confidence_label(
        games =
          d$games_played[[i]],
        minutes =
          d$minutes[[i]],
        three_pa =
          d$three_pointers_attempted[[i]]
      )
    },
    character(1)
  )
  
  d$spacing_tier <- vapply(
    d$spacing_score,
    p3s4_spacing_tier,
    character(1)
  )
  
  d$metric_version <-
    "P3S4_v1"
  
  d$updated_at <-
    Sys.time()
  
  keep <- c(
    "player_id",
    "team_id",
    "season",
    "games_played",
    "minutes",
    "field_goals_attempted",
    "three_pointers_made",
    "three_pointers_attempted",
    "free_throws_attempted",
    "three_pointers_per_game",
    "three_point_attempts_per_game",
    "field_goal_pct",
    "three_point_pct",
    "stabilized_three_point_pct",
    "free_throw_pct",
    "effective_field_goal_pct",
    "true_shooting_pct",
    "three_point_attempt_rate",
    "free_throw_rate",
    "shooting_efficiency_percentile",
    "three_point_accuracy_percentile",
    "three_point_volume_percentile",
    "spacing_percentile",
    "rim_pressure_proxy_percentile",
    "shooting_efficiency_score",
    "spacing_score",
    "shooting_gravity_score",
    "rim_pressure_proxy_score",
    "three_point_reliability",
    "shooting_confidence",
    "spacing_tier",
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

upsert_phase3_shooting_intelligence <- function(
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
  
  create_player_season_shooting_table(
    con
  )
  
  temp_table <- paste0(
    "tmp_player_shooting_",
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
          DELETE FROM player_season_shooting
          WHERE EXISTS (
            SELECT 1
            FROM ",
          temp_q,
          " src
            WHERE src.player_id = player_season_shooting.player_id
              AND src.team_id = player_season_shooting.team_id
              AND src.season = player_season_shooting.season
          )
          "
        )
      )
      
      DBI::dbExecute(
        con,
        paste0(
          "
          INSERT INTO player_season_shooting (
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

build_phase3_shooting_intelligence <- function(
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
    get_phase3_shooting_source(
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
    derive_phase3_shooting_intelligence(
      source
    )
  
  result <-
    upsert_phase3_shooting_intelligence(
      derived,
      con = con
    )
  
  invisible(
    c(
      list(
        status =
          "SHOOTING INTELLIGENCE BUILT"
      ),
      result
    )
  )
}


# ------------------------------------------------------------
# Read shooting intelligence
# ------------------------------------------------------------

get_phase3_shooting_intelligence <- function(
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
    !tbi_shooting_table() %in%
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
      "shoot.season = ?"
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
      "shoot.player_id = ?"
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
      shoot.*,
      p.player_name,
      p.primary_position,
      p.player_age,
      t.team_name,
      t.abbreviation

    FROM player_season_shooting shoot

    LEFT JOIN players p
      ON p.player_id = shoot.player_id

    LEFT JOIN teams t
      ON t.team_id = shoot.team_id

    ",
    where_sql,
    "

    ORDER BY
      shoot.spacing_score DESC,
      shoot.shooting_efficiency_score DESC,
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
# Attach Step-4 shooting metrics to a BIE roster frame
# ------------------------------------------------------------

attach_phase3_shooting_to_players <- function(
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
  
  shooting <-
    get_phase3_shooting_intelligence(
      season =
        season,
      con =
        con
    )
  
  if (!nrow(shooting)) {
    return(players)
  }
  
  keep <- c(
    "player_id",
    "team_id",
    "true_shooting_pct",
    "effective_field_goal_pct",
    "three_point_pct",
    "stabilized_three_point_pct",
    "three_point_attempt_rate",
    "free_throw_rate",
    "shooting_efficiency_score",
    "spacing_score",
    "shooting_gravity_score",
    "rim_pressure_proxy_score",
    "shooting_confidence",
    "spacing_tier"
  )
  
  shooting <- shooting[
    ,
    intersect(
      keep,
      names(shooting)
    ),
    drop = FALSE
  ]
  
  # If the roster has team_id, use the exact player/team/season row.
  if (
    "team_id" %in%
    names(players) &&
    "team_id" %in%
    names(shooting)
  ) {
    
    return(
      merge(
        players,
        shooting,
        by = c(
          "player_id",
          "team_id"
        ),
        all.x = TRUE,
        sort = FALSE
      )
    )
  }
  
  # Otherwise keep the highest-minute / first available player row.
  shooting <- shooting[
    !duplicated(
      shooting$player_id
    ),
    ,
    drop = FALSE
  ]
  
  shooting$team_id <- NULL
  
  merge(
    players,
    shooting,
    by = "player_id",
    all.x = TRUE,
    sort = FALSE
  )
}


# ------------------------------------------------------------
# Step 4 health check
# ------------------------------------------------------------

phase3_step4_healthcheck <- function(
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
    tbi_shooting_table() %in%
    tables
  
  rows <- if (
    exists_table
  ) {
    DBI::dbGetQuery(
      con,
      "
      SELECT COUNT(*) AS n
      FROM player_season_shooting
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
      FROM player_season_shooting
      WHERE season = ?
        AND spacing_score IS NOT NULL
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
      "Step 4 — Shooting + Spacing Intelligence",
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
    shooting_rows =
      rows,
    spacing_rated_rows =
      rated,
    method =
      "LEAGUE-CONTEXT + VOLUME-STABILIZED",
    shot_location_status =
      "NOT LOADED — NO SHOT-ZONE CLAIMS"
  )
}

# ============================================================
# PHASE 3.2 — STEP 4 V2
# Shooting + Spacing calibration
# ============================================================


p3s4_v2_percentile <- function(
    x,
    reference_mask = NULL) {
  
  value <- suppressWarnings(
    as.numeric(x)
  )
  
  valid <- is.finite(value)
  
  out <- rep(
    NA_real_,
    length(value)
  )
  
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
    
    out[[i]] <-
      max(
        0,
        min(
          100,
          100 *
            (
              below +
                0.5 * equal
            ) /
            length(reference)
        )
      )
  }
  
  out
}


p3s4_v2_reliability <- function(
    games,
    minutes,
    three_pa) {
  
  games <- p3s4_num(games, 0)
  minutes <- p3s4_num(minutes, 0)
  three_pa <- p3s4_num(three_pa, 0)
  
  max(
    0.65,
    min(
      1,
      0.35 * min(1, games / 40) +
        0.40 * min(1, minutes / 900) +
        0.25 * min(1, three_pa / 150)
    )
  )
}


p3s4_v2_shrink <- function(
    score,
    reliability) {
  
  score <- p3s4_num(score)
  
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


derive_phase3_shooting_intelligence <- function(
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
  
  d$three_pointers_per_game <-
    p3s4_divide(
      d$three_pointers_made,
      d$games_played
    )
  
  d$three_point_attempts_per_game <-
    p3s4_divide(
      d$three_pointers_attempted,
      d$games_played
    )
  
  if (
    all(
      !is.finite(
        suppressWarnings(
          as.numeric(
            d$three_point_attempt_rate
          )
        )
      )
    )
  ) {
    d$three_point_attempt_rate <-
      p3s4_divide(
        d$three_pointers_attempted,
        d$field_goals_attempted
      )
  }
  
  if (
    all(
      !is.finite(
        suppressWarnings(
          as.numeric(
            d$free_throw_rate
          )
        )
      )
    )
  ) {
    d$free_throw_rate <-
      p3s4_divide(
        d$free_throws_attempted,
        d$field_goals_attempted
      )
  }
  
  three_pa <- suppressWarnings(
    as.numeric(
      d$three_pointers_attempted
    )
  )
  
  three_pm <- suppressWarnings(
    as.numeric(
      d$three_pointers_made
    )
  )
  
  league_three_pct <- if (
    sum(
      three_pa[
        is.finite(three_pa)
      ],
      na.rm = TRUE
    ) > 0
  ) {
    sum(
      three_pm,
      na.rm = TRUE
    ) /
      sum(
        three_pa,
        na.rm = TRUE
      )
  } else {
    0.36
  }
  
  d$stabilized_three_point_pct <-
    p3s4_stabilized_three_pct(
      made = three_pm,
      attempted = three_pa,
      league_three_pct =
        league_three_pct,
      prior_attempts = 100
    )
  
  d$three_point_reliability <-
    p3s4_three_point_reliability(
      three_pa
    )
  
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
    sum(qualified, na.rm = TRUE) < 100
  ) {
    qualified <-
      is.finite(games) &
      is.finite(minutes)
  }
  
  d$shooting_efficiency_percentile <-
    p3s4_v2_percentile(
      d$true_shooting_pct,
      qualified
    )
  
  efg_percentile <-
    p3s4_v2_percentile(
      d$effective_field_goal_pct,
      qualified
    )
  
  d$three_point_accuracy_percentile <-
    p3s4_v2_percentile(
      d$stabilized_three_point_pct,
      qualified
    )
  
  d$three_point_volume_percentile <-
    p3s4_v2_percentile(
      d$three_point_attempt_rate,
      qualified
    )
  
  attempts_pg_percentile <-
    p3s4_v2_percentile(
      d$three_point_attempts_per_game,
      qualified
    )
  
  d$rim_pressure_proxy_percentile <-
    p3s4_v2_percentile(
      d$free_throw_rate,
      qualified
    )
  
  n <- nrow(d)
  
  d$shooting_efficiency_score <- rep(
    NA_real_,
    n
  )
  
  d$spacing_score <- rep(
    NA_real_,
    n
  )
  
  d$shooting_gravity_score <- rep(
    NA_real_,
    n
  )
  
  d$rim_pressure_proxy_score <-
    d$rim_pressure_proxy_percentile
  
  for (i in seq_len(n)) {
    
    reliability <-
      p3s4_v2_reliability(
        d$games_played[[i]],
        d$minutes[[i]],
        d$three_pointers_attempted[[i]]
      )
    
    raw_efficiency <-
      p3s4_weighted_mean(
        c(
          d$shooting_efficiency_percentile[[i]],
          efg_percentile[[i]]
        ),
        c(
          0.68,
          0.32
        )
      )
    
    raw_gravity <-
      p3s4_weighted_mean(
        c(
          d$three_point_volume_percentile[[i]],
          attempts_pg_percentile[[i]],
          d$three_point_accuracy_percentile[[i]]
        ),
        c(
          0.45,
          0.35,
          0.20
        )
      )
    
    raw_spacing <-
      p3s4_weighted_mean(
        c(
          d$three_point_accuracy_percentile[[i]],
          raw_gravity,
          raw_efficiency
        ),
        c(
          0.40,
          0.38,
          0.22
        )
      )
    
    d$shooting_efficiency_score[[i]] <-
      p3s4_v2_shrink(
        raw_efficiency,
        reliability
      )
    
    d$shooting_gravity_score[[i]] <-
      p3s4_v2_shrink(
        raw_gravity,
        reliability
      )
    
    d$spacing_score[[i]] <-
      p3s4_v2_shrink(
        raw_spacing,
        reliability
      )
    
    d$rim_pressure_proxy_score[[i]] <-
      p3s4_v2_shrink(
        d$rim_pressure_proxy_percentile[[i]],
        reliability
      )
  }
  
  d$spacing_percentile <-
    p3s4_v2_percentile(
      d$spacing_score,
      qualified
    )
  
  d$shooting_confidence <- vapply(
    seq_len(n),
    function(i) {
      p3s4_confidence_label(
        games =
          d$games_played[[i]],
        minutes =
          d$minutes[[i]],
        three_pa =
          d$three_pointers_attempted[[i]]
      )
    },
    character(1)
  )
  
  d$spacing_tier <- vapply(
    d$spacing_score,
    p3s4_spacing_tier,
    character(1)
  )
  
  d$metric_version <-
    "P3S4_v2_CALIBRATED"
  
  d$updated_at <-
    Sys.time()
  
  keep <- c(
    "player_id",
    "team_id",
    "season",
    "games_played",
    "minutes",
    "field_goals_attempted",
    "three_pointers_made",
    "three_pointers_attempted",
    "free_throws_attempted",
    "field_goal_pct",
    "three_point_pct",
    "free_throw_pct",
    "effective_field_goal_pct",
    "true_shooting_pct",
    "three_point_attempt_rate",
    "free_throw_rate",
    "three_pointers_per_game",
    "three_point_attempts_per_game",
    "stabilized_three_point_pct",
    "three_point_reliability",
    "shooting_efficiency_percentile",
    "three_point_accuracy_percentile",
    "three_point_volume_percentile",
    "spacing_percentile",
    "rim_pressure_proxy_percentile",
    "shooting_efficiency_score",
    "spacing_score",
    "shooting_gravity_score",
    "rim_pressure_proxy_score",
    "shooting_confidence",
    "spacing_tier",
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


phase3_step4_v2_healthcheck <- function(
    season = "2025-26") {
  
  source <-
    get_phase3_shooting_source(
      season = season
    )
  
  d <-
    derive_phase3_shooting_intelligence(
      source
    )
  
  list(
    phase = "Phase 3.2",
    step =
      "Step 4 — Shooting + Spacing Calibration V2",
    status = if (
      nrow(d) > 0 &&
      any(
        is.finite(
          d$spacing_score
        )
      )
    ) {
      "READY FOR REBUILD"
    } else {
      "REVIEW"
    },
    rows = nrow(d),
    metric_version =
      "P3S4_v2_CALIBRATED"
  )
}