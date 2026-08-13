# ============================================================
# TBI NBA Basketball Operations Platform
# PHASE 3 — STEP 8
# Player Impact / BIE Performance Rating
#
# Purpose:
#   Create the first performance-backed BIE player rating by
#   combining:
#     - Step 4 shooting / spacing
#     - Step 5 playmaking / creation
#     - Step 6 defense / rebounding
#     - Step 7 role / archetype intelligence
#     - Step 2 advanced impact metrics when available
#     - availability / sample-size context
#
# Safeguards:
#   - advanced metrics only contribute when they exist
#   - low-minute samples receive lower confidence
#   - defensive claims remain proxy-based until tracking arrives
#   - no projection claims are made here
# ============================================================


# ------------------------------------------------------------
# Canonical impact table
# ------------------------------------------------------------

tbi_player_impact_table <- function() {
  "player_season_impact"
}


# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------

p3s8_num <- function(
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


p3s8_text <- function(
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


p3s8_percentile <- function(
    x,
    higher_is_better = TRUE) {
  
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
  
  pct <-
    100 *
    (ranked - 1) /
    (sum(valid) - 1)
  
  if (!isTRUE(higher_is_better)) {
    pct <- 100 - pct
  }
  
  output[valid] <- pct
  
  output
}


p3s8_weighted_mean <- function(
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


p3s8_clamp <- function(
    x,
    lower = 0,
    upper = 100) {
  
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
# Rating tier
# ------------------------------------------------------------

p3s8_rating_tier <- function(score) {
  
  score <- p3s8_num(
    score
  )
  
  if (is.na(score)) {
    return("UNRATED")
  }
  
  if (score >= 90) {
    return("ELITE IMPACT")
  }
  
  if (score >= 82) {
    return("HIGH-END STARTER IMPACT")
  }
  
  if (score >= 72) {
    return("PLUS STARTER IMPACT")
  }
  
  if (score >= 62) {
    return("STARTER / HIGH-END ROTATION")
  }
  
  if (score >= 52) {
    return("ROTATION IMPACT")
  }
  
  if (score >= 42) {
    return("DEPTH IMPACT")
  }
  
  "LIMITED IMPACT"
}


# ------------------------------------------------------------
# Confidence
# ------------------------------------------------------------

p3s8_confidence_label <- function(
    games,
    minutes,
    evidence_count,
    role_confidence,
    advanced_count) {
  
  games <- p3s8_num(
    games,
    0
  )
  
  minutes <- p3s8_num(
    minutes,
    0
  )
  
  evidence_count <- p3s8_num(
    evidence_count,
    0
  )
  
  advanced_count <- p3s8_num(
    advanced_count,
    0
  )
  
  role_confidence <- p3s8_text(
    role_confidence,
    "LIMITED"
  )
  
  if (
    games >= 55 &&
    minutes >= 1400 &&
    evidence_count >= 5 &&
    role_confidence == "HIGH" &&
    advanced_count >= 1
  ) {
    return("HIGH")
  }
  
  if (
    games >= 30 &&
    minutes >= 700 &&
    evidence_count >= 4 &&
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
# Create table
# ------------------------------------------------------------

create_player_season_impact_table <- function(
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
    CREATE TABLE IF NOT EXISTS player_season_impact (

      player_id INTEGER NOT NULL,
      team_id INTEGER NOT NULL,
      season TEXT NOT NULL,

      games_played INTEGER,
      minutes DOUBLE,
      minutes_per_game DOUBLE,

      shooting_component DOUBLE,
      creation_component DOUBLE,
      defense_component DOUBLE,
      rebounding_component DOUBLE,
      role_component DOUBLE,
      availability_component DOUBLE,
      advanced_impact_component DOUBLE,

      offensive_impact_score DOUBLE,
      defensive_impact_score DOUBLE,
      all_around_impact_score DOUBLE,

      bie_performance_rating DOUBLE,
      bie_performance_percentile DOUBLE,

      impact_tier TEXT,
      impact_confidence TEXT,

      primary_role TEXT,
      archetype TEXT,
      role_family TEXT,

      evidence_count INTEGER,
      advanced_metrics_count INTEGER,

      data_scope TEXT,
      model_version TEXT DEFAULT 'P3S8_v1',

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
    CREATE INDEX IF NOT EXISTS idx_player_impact_player
    ON player_season_impact(player_id)
    ",
    "
    CREATE INDEX IF NOT EXISTS idx_player_impact_team
    ON player_season_impact(team_id)
    ",
    "
    CREATE INDEX IF NOT EXISTS idx_player_impact_season
    ON player_season_impact(season)
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
# Read all required Phase-3 sources
# ------------------------------------------------------------

get_phase3_player_impact_source <- function(
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
    "player_season_defense_rebounding",
    "player_season_roles"
  )
  
  missing <- setdiff(
    required,
    tables
  )
  
  if (length(missing)) {
    stop(
      paste0(
        "Step 8 requires completed Phase 3 tables. Missing: ",
        paste(
          missing,
          collapse = ", "
        )
      )
    )
  }
  
  advanced_exists <-
    "player_season_advanced" %in%
    tables
  
  advanced_join <- if (
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
    SELECT
      base.player_id,
      base.team_id,
      base.season,
      base.games_played,
      base.minutes,
      base.minutes_per_game,

      shoot.shooting_efficiency_score,
      shoot.spacing_score,
      shoot.shooting_gravity_score,
      shoot.rim_pressure_proxy_score,
      shoot.shooting_confidence,

      play.creation_score,
      play.passing_control_score,
      play.secondary_creation_score,
      play.ball_security_score,
      play.playmaking_confidence,

      dr.defense_proxy_score,
      dr.rebounding_score,
      dr.interior_impact_score,
      dr.disruption_score,
      dr.defense_confidence,

      role.primary_role,
      role.secondary_role,
      role.tertiary_role,
      role.archetype,
      role.role_family,
      role.offensive_role_score,
      role.defensive_role_score,
      role.two_way_role_score,
      role.role_confidence,
      role.evidence_count AS role_evidence_count,

      ",
    advanced_fields,
    "

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

    LEFT JOIN player_season_roles role
      ON role.player_id = base.player_id
      AND role.team_id = base.team_id
      AND role.season = base.season

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
# Derive advanced impact percentile component
# ------------------------------------------------------------

derive_p3s8_advanced_component <- function(d) {
  
  bpm_pct <-
    p3s8_percentile(
      d$box_plus_minus
    )
  
  obpm_pct <-
    p3s8_percentile(
      d$offensive_box_plus_minus
    )
  
  dbpm_pct <-
    p3s8_percentile(
      d$defensive_box_plus_minus
    )
  
  vorp_pct <-
    p3s8_percentile(
      d$value_over_replacement
    )
  
  ws48_pct <-
    p3s8_percentile(
      d$win_shares_per_48
    )
  
  net_pct <-
    p3s8_percentile(
      d$net_rating
    )
  
  n <- nrow(d)
  
  score <- rep(
    NA_real_,
    n
  )
  
  count <- rep(
    0L,
    n
  )
  
  for (
    i in seq_len(n)
  ) {
    
    values <- c(
      bpm_pct[[i]],
      obpm_pct[[i]],
      dbpm_pct[[i]],
      vorp_pct[[i]],
      ws48_pct[[i]],
      net_pct[[i]]
    )
    
    count[[i]] <-
      sum(
        is.finite(
          values
        )
      )
    
    score[[i]] <-
      p3s8_weighted_mean(
        values,
        c(
          0.25,
          0.14,
          0.14,
          0.17,
          0.15,
          0.15
        )
      )
  }
  
  list(
    score = score,
    count = count
  )
}


# ------------------------------------------------------------
# Derive Step-8 player impact ratings
# ------------------------------------------------------------

derive_phase3_player_impact <- function(
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
  
  advanced <-
    derive_p3s8_advanced_component(
      d
    )
  
  d$advanced_impact_component <-
    advanced$score
  
  d$advanced_metrics_count <-
    advanced$count
  
  # ----------------------------------------------------------
  # Core components
  # ----------------------------------------------------------
  
  d$shooting_component <- rep(
    NA_real_,
    n
  )
  
  d$creation_component <- rep(
    NA_real_,
    n
  )
  
  d$defense_component <- rep(
    NA_real_,
    n
  )
  
  d$rebounding_component <-
    suppressWarnings(
      as.numeric(
        d$rebounding_score
      )
    )
  
  d$role_component <- rep(
    NA_real_,
    n
  )
  
  d$availability_component <- rep(
    NA_real_,
    n
  )
  
  d$offensive_impact_score <- rep(
    NA_real_,
    n
  )
  
  d$defensive_impact_score <- rep(
    NA_real_,
    n
  )
  
  d$all_around_impact_score <- rep(
    NA_real_,
    n
  )
  
  d$bie_performance_rating <- rep(
    NA_real_,
    n
  )
  
  d$evidence_count <- rep(
    0L,
    n
  )
  
  d$impact_confidence <- rep(
    "LIMITED",
    n
  )
  
  for (
    i in seq_len(n)
  ) {
    
    d$shooting_component[[i]] <-
      p3s8_weighted_mean(
        c(
          d$shooting_efficiency_score[[i]],
          d$spacing_score[[i]],
          d$shooting_gravity_score[[i]]
        ),
        c(
          0.42,
          0.34,
          0.24
        )
      )
    
    d$creation_component[[i]] <-
      p3s8_weighted_mean(
        c(
          d$creation_score[[i]],
          d$passing_control_score[[i]],
          d$secondary_creation_score[[i]],
          d$ball_security_score[[i]]
        ),
        c(
          0.42,
          0.22,
          0.21,
          0.15
        )
      )
    
    # Step 6 V2 already blends possession-level defense,
    # disruption, rebounding and interior context. Step 8 now
    # treats that calibrated defense proxy as the primary signal
    # rather than double-counting the same subcomponents.
    d$defense_component[[i]] <-
      p3s8_weighted_mean(
        c(
          d$defense_proxy_score[[i]],
          d$interior_impact_score[[i]],
          d$disruption_score[[i]]
        ),
        c(
          0.72,
          0.14,
          0.14
        )
      )
    
    d$role_component[[i]] <-
      p3s8_weighted_mean(
        c(
          d$offensive_role_score[[i]],
          d$defensive_role_score[[i]],
          d$two_way_role_score[[i]]
        ),
        c(
          0.42,
          0.38,
          0.20
        )
      )
    
    # Availability score is deliberately capped and sample-based.
    games <- p3s8_num(
      d$games_played[[i]],
      0
    )
    
    minutes <- p3s8_num(
      d$minutes[[i]],
      0
    )
    
    game_score <-
      min(
        100,
        games / 70 * 100
      )
    
    minute_score <-
      min(
        100,
        minutes / 2000 * 100
      )
    
    d$availability_component[[i]] <-
      p3s8_weighted_mean(
        c(
          game_score,
          minute_score
        ),
        c(
          0.45,
          0.55
        )
      )
    
    d$offensive_impact_score[[i]] <-
      p3s8_weighted_mean(
        c(
          d$shooting_component[[i]],
          d$creation_component[[i]],
          d$offensive_role_score[[i]]
        ),
        c(
          0.35,
          0.38,
          0.27
        )
      )
    
    d$defensive_impact_score[[i]] <-
      p3s8_weighted_mean(
        c(
          d$defense_component[[i]],
          d$rebounding_component[[i]],
          d$defensive_role_score[[i]]
        ),
        c(
          0.56,
          0.24,
          0.20
        )
      )
    
    d$all_around_impact_score[[i]] <-
      p3s8_weighted_mean(
        c(
          d$offensive_impact_score[[i]],
          d$defensive_impact_score[[i]],
          d$role_component[[i]]
        ),
        c(
          0.48,
          0.37,
          0.15
        )
      )
    
    core_values <- c(
      d$shooting_component[[i]],
      d$creation_component[[i]],
      d$defense_component[[i]],
      d$rebounding_component[[i]],
      d$role_component[[i]]
    )
    
    d$evidence_count[[i]] <-
      sum(
        is.finite(
          core_values
        )
      )
    
    # Advanced impact contributes only when available.
    if (
      is.finite(
        d$advanced_impact_component[[i]]
      )
    ) {
      
      d$bie_performance_rating[[i]] <-
        p3s8_weighted_mean(
          c(
            d$all_around_impact_score[[i]],
            d$advanced_impact_component[[i]],
            d$availability_component[[i]]
          ),
          c(
            0.68,
            0.22,
            0.10
          )
        )
      
    } else {
      
      d$bie_performance_rating[[i]] <-
        p3s8_weighted_mean(
          c(
            d$all_around_impact_score[[i]],
            d$availability_component[[i]]
          ),
          c(
            0.90,
            0.10
          )
        )
    }
    
    d$bie_performance_rating[[i]] <-
      p3s8_clamp(
        d$bie_performance_rating[[i]]
      )
    
    d$impact_confidence[[i]] <-
      p3s8_confidence_label(
        games =
          d$games_played[[i]],
        minutes =
          d$minutes[[i]],
        evidence_count =
          d$evidence_count[[i]],
        role_confidence =
          d$role_confidence[[i]],
        advanced_count =
          d$advanced_metrics_count[[i]]
      )
  }
  
  d$bie_performance_percentile <-
    p3s8_percentile(
      d$bie_performance_rating
    )
  
  d$impact_tier <- vapply(
    d$bie_performance_rating,
    p3s8_rating_tier,
    character(1)
  )
  
  d$data_scope <- vapply(
    seq_len(n),
    function(i) {
      
      if (
        d$advanced_metrics_count[[i]] >= 3
      ) {
        return(
          "PHASE 3 PERFORMANCE + ADVANCED"
        )
      }
      
      if (
        d$advanced_metrics_count[[i]] >= 1
      ) {
        return(
          "PHASE 3 PERFORMANCE + PARTIAL ADVANCED"
        )
      }
      
      "PHASE 3 PERFORMANCE"
    },
    character(1)
  )
  
  d$model_version <-
    "P3S8_v1"
  
  d$updated_at <-
    Sys.time()
  
  keep <- c(
    "player_id",
    "team_id",
    "season",
    "games_played",
    "minutes",
    "minutes_per_game",
    "shooting_component",
    "creation_component",
    "defense_component",
    "rebounding_component",
    "role_component",
    "availability_component",
    "advanced_impact_component",
    "offensive_impact_score",
    "defensive_impact_score",
    "all_around_impact_score",
    "bie_performance_rating",
    "bie_performance_percentile",
    "impact_tier",
    "impact_confidence",
    "primary_role",
    "archetype",
    "role_family",
    "evidence_count",
    "advanced_metrics_count",
    "data_scope",
    "model_version",
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

upsert_phase3_player_impact <- function(
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
  
  create_player_season_impact_table(
    con
  )
  
  temp_table <- paste0(
    "tmp_player_impact_",
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
          DELETE FROM player_season_impact
          WHERE EXISTS (
            SELECT 1
            FROM ",
          temp_q,
          " src
            WHERE src.player_id = player_season_impact.player_id
              AND src.team_id = player_season_impact.team_id
              AND src.season = player_season_impact.season
          )
          "
        )
      )
      
      DBI::dbExecute(
        con,
        paste0(
          "
          INSERT INTO player_season_impact (
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

build_phase3_player_impact <- function(
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
    get_phase3_player_impact_source(
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
    derive_phase3_player_impact(
      source
    )
  
  result <-
    upsert_phase3_player_impact(
      derived,
      con = con
    )
  
  invisible(
    c(
      list(
        status =
          "PLAYER IMPACT RATINGS BUILT"
      ),
      result
    )
  )
}


# ------------------------------------------------------------
# Read player impact
# ------------------------------------------------------------

get_phase3_player_impact <- function(
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
    !tbi_player_impact_table() %in%
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
      "impact.season = ?"
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
      impact.*,
      p.player_name,
      p.primary_position,
      p.player_age,
      t.team_name,
      t.abbreviation

    FROM player_season_impact impact

    LEFT JOIN players p
      ON p.player_id = impact.player_id

    LEFT JOIN teams t
      ON t.team_id = impact.team_id

    ",
    where_sql,
    "

    ORDER BY
      impact.bie_performance_rating DESC,
      impact.bie_performance_percentile DESC,
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
# Top player-impact board
# ------------------------------------------------------------

phase3_step8_leaderboard <- function(
    season = "2025-26",
    n = 25,
    con = NULL) {
  
  impact <- get_phase3_player_impact(
    season =
      season,
    con =
      con
  )
  
  if (!nrow(impact)) {
    return(
      data.frame()
    )
  }
  
  keep <- c(
    "player_name",
    "team_name",
    "primary_position",
    "bie_performance_rating",
    "bie_performance_percentile",
    "impact_tier",
    "impact_confidence",
    "offensive_impact_score",
    "defensive_impact_score",
    "primary_role",
    "archetype",
    "data_scope"
  )
  
  impact <- impact[
    ,
    intersect(
      keep,
      names(impact)
    ),
    drop = FALSE
  ]
  
  utils::head(
    impact,
    n
  )
}


# ------------------------------------------------------------
# Step 8 health check
# ------------------------------------------------------------

phase3_step8_healthcheck <- function(
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
    tbi_player_impact_table() %in%
    tables
  
  rows <- if (
    exists_table
  ) {
    DBI::dbGetQuery(
      con,
      "
      SELECT COUNT(*) AS n
      FROM player_season_impact
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
      FROM player_season_impact
      WHERE season = ?
        AND bie_performance_rating IS NOT NULL
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
      FROM player_season_impact
      WHERE season = ?
        AND impact_confidence = 'HIGH'
      ",
      params = list(
        season
      )
    )$n[[1]]
  } else {
    0L
  }
  
  advanced_supported <- if (
    exists_table &&
    rows > 0
  ) {
    DBI::dbGetQuery(
      con,
      "
      SELECT COUNT(*) AS n
      FROM player_season_impact
      WHERE season = ?
        AND advanced_metrics_count > 0
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
      "Step 8 — Player Impact / BIE Performance Rating",
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
    impact_rows =
      rows,
    rated_rows =
      rated,
    high_confidence_rows =
      high_confidence,
    advanced_supported_rows =
      advanced_supported,
    rating_scope =
      "PERFORMANCE-BACKED BIE PLAYER RATING",
    projection_status =
      "NO PROJECTION CLAIMS"
  )
}

# ============================================================
# PHASE 3.2 — BIE CALIBRATION V2
#
# Why this exists:
#   The original P3S8_v1 model could overrate low-usage role
#   players because:
#     1) niche component percentiles could dominate,
#     2) ball security / passing control could inflate creation,
#     3) availability could lift ratings when advanced impact
#        was unavailable,
#     4) there was no direct production/workload anchor.
#
# V2 keeps the existing player_season_impact table schema, so
# every downstream TBI module continues to work.
#
# New rating architecture:
#   Skill / role impact
#       +
#   NBA production / workload anchor
#       +
#   advanced impact when available
#       +
#   small availability contribution
#
# Historical rows remain team-season specific.
# ============================================================


# ------------------------------------------------------------
# Qualified-reference percentile
# ------------------------------------------------------------

p3s8_v2_reference_percentile <- function(
    x,
    reference_mask = NULL,
    higher_is_better = TRUE) {
  
  values <- suppressWarnings(
    as.numeric(x)
  )
  
  n <- length(values)
  
  output <- rep(
    NA_real_,
    n
  )
  
  valid <- is.finite(values)
  
  if (
    is.null(reference_mask)
  ) {
    reference_mask <- valid
  } else {
    reference_mask <-
      as.logical(reference_mask) &
      valid
  }
  
  reference <- values[
    reference_mask
  ]
  
  if (!length(reference)) {
    return(output)
  }
  
  for (i in seq_len(n)) {
    
    if (!valid[[i]]) {
      next
    }
    
    # Mid-rank empirical percentile against the qualified pool.
    below <- sum(
      reference <
        values[[i]],
      na.rm = TRUE
    )
    
    equal <- sum(
      reference ==
        values[[i]],
      na.rm = TRUE
    )
    
    pct <- 100 *
      (
        below +
          0.5 * equal
      ) /
      length(reference)
    
    if (!isTRUE(higher_is_better)) {
      pct <- 100 - pct
    }
    
    output[[i]] <-
      p3s8_clamp(
        pct
      )
  }
  
  output
}


# ------------------------------------------------------------
# V2 player-impact source
#
# Adds raw production fields to the original Step-8 source.
# ------------------------------------------------------------

get_phase3_player_impact_source <- function(
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
    "player_season_defense_rebounding",
    "player_season_roles"
  )
  
  missing <- setdiff(
    required,
    tables
  )
  
  if (length(missing)) {
    stop(
      paste0(
        "BIE Calibration V2 requires completed Phase-3 tables. Missing: ",
        paste(
          missing,
          collapse = ", "
        )
      )
    )
  }
  
  stats_fields <- DBI::dbListFields(
    con,
    "player_season_stats"
  )
  
  stat_sql <- function(
    field,
    alias = field) {
    
    if (field %in% stats_fields) {
      paste0(
        "base.",
        field,
        " AS ",
        alias
      )
    } else {
      paste0(
        "NULL AS ",
        alias
      )
    }
  }
  
  advanced_exists <-
    "player_season_advanced" %in%
    tables
  
  advanced_join <- if (
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
    SELECT
      base.player_id,
      base.team_id,
      base.season,
      base.games_played,
      base.minutes,
      base.minutes_per_game,

      ",
    stat_sql(
      "points_per_game"
    ),
    ",
      ",
    stat_sql(
      "rebounds_per_game"
    ),
    ",
      ",
    stat_sql(
      "assists_per_game"
    ),
    ",
      ",
    stat_sql(
      "steals_per_game"
    ),
    ",
      ",
    stat_sql(
      "blocks_per_game"
    ),
    ",
      ",
    stat_sql(
      "turnovers_per_game"
    ),
    ",

      shoot.shooting_efficiency_score,
      shoot.spacing_score,
      shoot.shooting_gravity_score,
      shoot.rim_pressure_proxy_score,
      shoot.shooting_confidence,

      play.creation_score,
      play.passing_control_score,
      play.secondary_creation_score,
      play.ball_security_score,
      play.playmaking_confidence,

      dr.defense_proxy_score,
      dr.rebounding_score,
      dr.interior_impact_score,
      dr.disruption_score,
      dr.defense_confidence,

      role.primary_role,
      role.secondary_role,
      role.tertiary_role,
      role.archetype,
      role.role_family,
      role.offensive_role_score,
      role.defensive_role_score,
      role.two_way_role_score,
      role.role_confidence,
      role.evidence_count AS role_evidence_count,

      ",
    advanced_fields,
    "

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

    LEFT JOIN player_season_roles role
      ON role.player_id = base.player_id
      AND role.team_id = base.team_id
      AND role.season = base.season

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
# V2 full derivation
#
# production_component is intentionally kept as an internal
# diagnostic so the persistent table schema does not change.
# ------------------------------------------------------------

derive_phase3_player_impact_v2_full <- function(
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
  
  advanced <-
    derive_p3s8_advanced_component(
      d
    )
  
  d$advanced_impact_component <-
    advanced$score
  
  d$advanced_metrics_count <-
    advanced$count
  
  # ----------------------------------------------------------
  # Qualified reference population
  #
  # Avoid benchmarking every component against tiny samples.
  # ----------------------------------------------------------
  
  games_vec <- suppressWarnings(
    as.numeric(
      d$games_played
    )
  )
  
  mpg_vec <- suppressWarnings(
    as.numeric(
      d$minutes_per_game
    )
  )
  
  qualified <-
    is.finite(games_vec) &
    is.finite(mpg_vec) &
    games_vec >= 20 &
    mpg_vec >= 10
  
  # If data is unusually sparse, fall back to all finite rows.
  if (
    sum(
      qualified,
      na.rm = TRUE
    ) < 100
  ) {
    qualified <-
      is.finite(games_vec) &
      is.finite(mpg_vec)
  }
  
  # ----------------------------------------------------------
  # Production / workload anchor
  # ----------------------------------------------------------
  
  ppg_pct <-
    p3s8_v2_reference_percentile(
      d$points_per_game,
      qualified
    )
  
  apg_pct <-
    p3s8_v2_reference_percentile(
      d$assists_per_game,
      qualified
    )
  
  rpg_pct <-
    p3s8_v2_reference_percentile(
      d$rebounds_per_game,
      qualified
    )
  
  mpg_pct <-
    p3s8_v2_reference_percentile(
      d$minutes_per_game,
      qualified
    )
  
  d$production_component <- rep(
    NA_real_,
    n
  )
  
  # ----------------------------------------------------------
  # Core components
  # ----------------------------------------------------------
  
  d$shooting_component <- rep(
    NA_real_,
    n
  )
  
  d$creation_component <- rep(
    NA_real_,
    n
  )
  
  d$defense_component <- rep(
    NA_real_,
    n
  )
  
  d$rebounding_component <-
    suppressWarnings(
      as.numeric(
        d$rebounding_score
      )
    )
  
  d$role_component <- rep(
    NA_real_,
    n
  )
  
  d$availability_component <- rep(
    NA_real_,
    n
  )
  
  d$offensive_impact_score <- rep(
    NA_real_,
    n
  )
  
  d$defensive_impact_score <- rep(
    NA_real_,
    n
  )
  
  d$all_around_impact_score <- rep(
    NA_real_,
    n
  )
  
  d$bie_performance_rating <- rep(
    NA_real_,
    n
  )
  
  d$evidence_count <- rep(
    0L,
    n
  )
  
  d$impact_confidence <- rep(
    "LIMITED",
    n
  )
  
  for (
    i in seq_len(n)
  ) {
    
    d$production_component[[i]] <-
      p3s8_weighted_mean(
        c(
          ppg_pct[[i]],
          apg_pct[[i]],
          rpg_pct[[i]],
          mpg_pct[[i]]
        ),
        c(
          0.38,
          0.24,
          0.13,
          0.25
        )
      )
    
    d$shooting_component[[i]] <-
      p3s8_weighted_mean(
        c(
          d$shooting_efficiency_score[[i]],
          d$spacing_score[[i]],
          d$shooting_gravity_score[[i]]
        ),
        c(
          0.42,
          0.34,
          0.24
        )
      )
    
    # V2: creation_score carries more of the creation grade.
    # Passing control / low turnovers cannot independently make
    # a low-volume player look like a primary creator.
    d$creation_component[[i]] <-
      p3s8_weighted_mean(
        c(
          d$creation_score[[i]],
          d$secondary_creation_score[[i]],
          d$passing_control_score[[i]],
          d$ball_security_score[[i]]
        ),
        c(
          0.58,
          0.20,
          0.13,
          0.09
        )
      )
    
    d$defense_component[[i]] <-
      p3s8_weighted_mean(
        c(
          d$defense_proxy_score[[i]],
          d$interior_impact_score[[i]],
          d$disruption_score[[i]]
        ),
        c(
          0.56,
          0.24,
          0.20
        )
      )
    
    d$role_component[[i]] <-
      p3s8_weighted_mean(
        c(
          d$offensive_role_score[[i]],
          d$defensive_role_score[[i]],
          d$two_way_role_score[[i]]
        ),
        c(
          0.42,
          0.38,
          0.20
        )
      )
    
    # Availability remains informative, but is no longer a
    # meaningful driver of overall player quality.
    games <- p3s8_num(
      d$games_played[[i]],
      0
    )
    
    minutes <- p3s8_num(
      d$minutes[[i]],
      0
    )
    
    game_score <-
      min(
        100,
        games / 70 * 100
      )
    
    minute_score <-
      min(
        100,
        minutes / 2000 * 100
      )
    
    d$availability_component[[i]] <-
      p3s8_weighted_mean(
        c(
          game_score,
          minute_score
        ),
        c(
          0.45,
          0.55
        )
      )
    
    d$offensive_impact_score[[i]] <-
      p3s8_weighted_mean(
        c(
          d$shooting_component[[i]],
          d$creation_component[[i]],
          d$offensive_role_score[[i]]
        ),
        c(
          0.34,
          0.43,
          0.23
        )
      )
    
    d$defensive_impact_score[[i]] <-
      p3s8_weighted_mean(
        c(
          d$defense_component[[i]],
          d$rebounding_component[[i]],
          d$defensive_role_score[[i]]
        ),
        c(
          0.58,
          0.24,
          0.18
        )
      )
    
    # V2 slightly reduces defense/role dominance in the single
    # all-around score. Specialists can still rate strongly,
    # but overall NBA workload/production is handled separately.
    d$all_around_impact_score[[i]] <-
      p3s8_weighted_mean(
        c(
          d$offensive_impact_score[[i]],
          d$defensive_impact_score[[i]],
          d$role_component[[i]]
        ),
        c(
          0.55,
          0.30,
          0.15
        )
      )
    
    core_values <- c(
      d$shooting_component[[i]],
      d$creation_component[[i]],
      d$defense_component[[i]],
      d$rebounding_component[[i]],
      d$role_component[[i]],
      d$production_component[[i]]
    )
    
    d$evidence_count[[i]] <-
      sum(
        is.finite(
          core_values
        )
      )
    
    # --------------------------------------------------------
    # V2 final rating
    #
    # Advanced available:
    #   40% skill / role impact
    #   40% production / workload
    #   15% advanced impact
    #    5% availability
    #
    # Advanced unavailable:
    #   52% skill / role impact
    #   43% production / workload
    #    5% availability
    #
    # This prevents availability or niche-percentile strength
    # from substituting for actual NBA workload/production.
    # --------------------------------------------------------
    
    if (
      is.finite(
        d$advanced_impact_component[[i]]
      )
    ) {
      
      d$bie_performance_rating[[i]] <-
        p3s8_weighted_mean(
          c(
            d$all_around_impact_score[[i]],
            d$production_component[[i]],
            d$advanced_impact_component[[i]],
            d$availability_component[[i]]
          ),
          c(
            0.40,
            0.40,
            0.15,
            0.05
          )
        )
      
    } else {
      
      d$bie_performance_rating[[i]] <-
        p3s8_weighted_mean(
          c(
            d$all_around_impact_score[[i]],
            d$production_component[[i]],
            d$availability_component[[i]]
          ),
          c(
            0.52,
            0.43,
            0.05
          )
        )
    }
    
    # Tiny-sample protection.
    # Do not allow very small NBA roles to present as elite
    # overall impact solely from component percentiles.
    if (
      is.finite(
        d$bie_performance_rating[[i]]
      ) &&
      (
        games < 20 ||
        p3s8_num(
          d$minutes_per_game[[i]],
          0
        ) < 10
      )
    ) {
      
      sample_factor <-
        max(
          0.72,
          min(
            1,
            0.72 +
              0.28 *
              min(
                1,
                max(
                  games / 20,
                  p3s8_num(
                    d$minutes_per_game[[i]],
                    0
                  ) / 10
                )
              )
          )
        )
      
      d$bie_performance_rating[[i]] <-
        50 +
        (
          d$bie_performance_rating[[i]] -
            50
        ) *
        sample_factor
    }
    
    d$bie_performance_rating[[i]] <-
      p3s8_clamp(
        d$bie_performance_rating[[i]]
      )
    
    d$impact_confidence[[i]] <-
      p3s8_confidence_label(
        games =
          d$games_played[[i]],
        minutes =
          d$minutes[[i]],
        evidence_count =
          d$evidence_count[[i]],
        role_confidence =
          d$role_confidence[[i]],
        advanced_count =
          d$advanced_metrics_count[[i]]
      )
  }
  
  # Final BIE percentile is benchmarked against qualified NBA
  # rotation players. Unqualified players can still receive a
  # percentile, but the reference distribution is not polluted
  # by tiny samples.
  d$bie_performance_percentile <-
    p3s8_v2_reference_percentile(
      d$bie_performance_rating,
      qualified
    )
  
  d$impact_tier <- vapply(
    d$bie_performance_rating,
    p3s8_rating_tier,
    character(1)
  )
  
  d$data_scope <- vapply(
    seq_len(n),
    function(i) {
      
      if (
        d$advanced_metrics_count[[i]] >= 3
      ) {
        return(
          "PHASE 3.2 CALIBRATED + ADVANCED"
        )
      }
      
      if (
        d$advanced_metrics_count[[i]] >= 1
      ) {
        return(
          "PHASE 3.2 CALIBRATED + PARTIAL ADVANCED"
        )
      }
      
      "PHASE 3.2 CALIBRATED PERFORMANCE"
    },
    character(1)
  )
  
  d$model_version <-
    "P3S8_v2_1_CALIBRATED_DEFENSE"
  
  d$updated_at <-
    Sys.time()
  
  d
}


# ------------------------------------------------------------
# Persistent-schema wrapper
# ------------------------------------------------------------

derive_phase3_player_impact <- function(
    source_data) {
  
  d <-
    derive_phase3_player_impact_v2_full(
      source_data
    )
  
  if (!nrow(d)) {
    return(d)
  }
  
  keep <- c(
    "player_id",
    "team_id",
    "season",
    "games_played",
    "minutes",
    "minutes_per_game",
    "shooting_component",
    "creation_component",
    "defense_component",
    "rebounding_component",
    "role_component",
    "availability_component",
    "advanced_impact_component",
    "offensive_impact_score",
    "defensive_impact_score",
    "all_around_impact_score",
    "bie_performance_rating",
    "bie_performance_percentile",
    "impact_tier",
    "impact_confidence",
    "primary_role",
    "archetype",
    "role_family",
    "evidence_count",
    "advanced_metrics_count",
    "data_scope",
    "model_version",
    "updated_at"
  )
  
  d[
    ,
    keep,
    drop = FALSE
  ]
}


# ------------------------------------------------------------
# Preview calibration BEFORE committing
# ------------------------------------------------------------

preview_phase3_player_impact_v2 <- function(
    season = "2025-26",
    player_names = NULL,
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
  
  source <- get_phase3_player_impact_source(
    season =
      season,
    con =
      con
  )
  
  d <- derive_phase3_player_impact_v2_full(
    source
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
  
  if (
    !is.null(
      player_names
    )
  ) {
    
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
      "team_id",
      "season",
      "games_played",
      "minutes_per_game",
      "points_per_game",
      "rebounds_per_game",
      "assists_per_game",
      "shooting_component",
      "creation_component",
      "defense_component",
      "rebounding_component",
      "role_component",
      "production_component",
      "availability_component",
      "advanced_impact_component",
      "offensive_impact_score",
      "defensive_impact_score",
      "all_around_impact_score",
      "bie_performance_rating",
      "bie_performance_percentile",
      "impact_tier",
      "impact_confidence",
      "model_version"
    ),
    names(d)
  )
  
  d[
    order(
      -d$bie_performance_rating
    ),
    keep,
    drop = FALSE
  ]
}


# ------------------------------------------------------------
# V2 calibration health check
# ------------------------------------------------------------

phase3_step8_v2_healthcheck <- function(
    season = "2025-26") {
  
  preview <- tryCatch(
    preview_phase3_player_impact_v2(
      season =
        season
    ),
    error = function(e) {
      structure(
        data.frame(),
        error =
          conditionMessage(e)
      )
    }
  )
  
  if (!nrow(preview)) {
    return(
      list(
        phase =
          "Phase 3.2",
        step =
          "BIE Calibration V2",
        status =
          "REVIEW",
        explanation =
          attr(
            preview,
            "error"
          ) %||%
          "No preview rows."
      )
    )
  }
  
  list(
    phase =
      "Phase 3.2",
    step =
      "BIE Calibration V2",
    status =
      "READY FOR SPOT CHECK",
    season =
      season,
    preview_rows =
      nrow(preview),
    finite_ratings =
      sum(
        is.finite(
          preview$
            bie_performance_rating
        )
      ),
    qualified_percentile_rows =
      sum(
        is.finite(
          preview$
            bie_performance_percentile
        )
      ),
    model_version =
      unique(
        preview$
          model_version
      )[[1]],
    rule =
      "SKILL + PRODUCTION + ADVANCED WHEN AVAILABLE + SMALL AVAILABILITY WEIGHT"
  )
}

# ============================================================
# PHASE 3.2 — STEP 8 V2.1 DEFENSE-INTEGRATION CHECK
# ============================================================

phase3_step8_v21_healthcheck <- function(
    season = "2025-26") {
  
  step6_check <- if (
    exists(
      "phase3_step6_v2_healthcheck",
      mode = "function",
      inherits = TRUE
    )
  ) {
    phase3_step6_v2_healthcheck(
      season =
        season
    )
  } else {
    list(
      status =
        "STEP 6 V2 FUNCTION NOT LOADED"
    )
  }
  
  step8_check <-
    phase3_step8_v2_healthcheck(
      season =
        season
    )
  
  list(
    phase =
      "Phase 3.2",
    step =
      "Step 6 + Step 8 Calibration Integration",
    status = if (
      identical(
        step6_check$status,
        "READY FOR SPOT CHECK"
      ) &&
      identical(
        step8_check$status,
        "READY FOR SPOT CHECK"
      )
    ) {
      "READY FOR COMBINED SPOT CHECK"
    } else {
      "REVIEW"
    },
    defense_status =
      step6_check$status,
    impact_status =
      step8_check$status,
    impact_model =
      "P3S8_v2_1_CALIBRATED_DEFENSE",
    integration_rule =
      "STEP 8 CONSUMES STEP 6 CALIBRATED DEFENSE WITHOUT HEAVY SUBCOMPONENT DOUBLE-COUNTING"
  )
}

# ============================================================
# PHASE 3.2 — STEP 8 V2.2
# Final calibrated BIE integration
# ============================================================


phase3_step8_v22_healthcheck <- function(
    season = "2025-26") {
  
  checks <- list(
    step2 = if (
      exists(
        "phase3_step2_v2_healthcheck",
        mode = "function"
      )
    ) {
      phase3_step2_v2_healthcheck(
        season
      )
    } else {
      list(status = "NOT LOADED")
    },
    
    step4 = if (
      exists(
        "phase3_step4_v2_healthcheck",
        mode = "function"
      )
    ) {
      phase3_step4_v2_healthcheck(
        season
      )
    } else {
      list(status = "NOT LOADED")
    },
    
    step5 = if (
      exists(
        "phase3_step5_v2_healthcheck",
        mode = "function"
      )
    ) {
      phase3_step5_v2_healthcheck(
        season
      )
    } else {
      list(status = "NOT LOADED")
    },
    
    step6 = if (
      exists(
        "phase3_step6_v3_healthcheck",
        mode = "function"
      )
    ) {
      phase3_step6_v3_healthcheck(
        season
      )
    } else {
      list(status = "NOT LOADED")
    },
    
    step7 = if (
      exists(
        "phase3_step7_v2_healthcheck",
        mode = "function"
      )
    ) {
      phase3_step7_v2_healthcheck(
        season
      )
    } else {
      list(status = "NOT LOADED")
    }
  )
  
  statuses <- vapply(
    checks,
    function(x) {
      as.character(
        x$status %||%
          "UNKNOWN"
      )
    },
    character(1)
  )
  
  preview <- tryCatch(
    preview_phase3_player_impact_v2(
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
  
  list(
    phase = "Phase 3.2",
    step =
      "Steps 2/4/5/6/7/8 Calibration Chain",
    status = if (
      all(
        grepl(
          "^READY",
          statuses
        )
      ) &&
      nrow(preview) > 0
    ) {
      "READY FOR CONTROLLED REBUILD"
    } else {
      "REVIEW"
    },
    upstream_status =
      statuses,
    preview_rows =
      nrow(preview),
    impact_model =
      "P3S8_v2_2_CALIBRATED_CHAIN",
    rule =
      "NO DOWNSTREAM SCORE MAY DEPEND ON THE OLD INDIVIDUAL-OFFENSIVE-POSSESSION DENOMINATOR"
  )
}


# Override only model version label; V2.1 rating math remains.
derive_phase3_player_impact <- local({
  
  previous <-
    derive_phase3_player_impact
  
  function(source_data) {
    
    d <- previous(
      source_data
    )
    
    if (
      is.data.frame(d) &&
      nrow(d) &&
      "model_version" %in% names(d)
    ) {
      d$model_version <-
        "P3S8_v2_2_CALIBRATED_CHAIN"
    }
    
    d
  }
})