# ============================================================
# TBI NBA Basketball Operations Platform
# PHASE 3 — STEP 6
# Defense + Rebounding Intelligence
#
# Uses real Step-1 / Step-2 player-season data.
#
# Important:
#   - current defense signal is a BOX-SCORE / IMPACT PROXY
#   - steals, blocks, defensive rating, DBPM (when available)
#     and rebounding are used
#   - no matchup, tracking, rim-deterrence, switchability,
#     screen navigation, or contest claims are fabricated
# ============================================================


# ------------------------------------------------------------
# Canonical table
# ------------------------------------------------------------

tbi_defense_rebounding_table <- function() {
  "player_season_defense_rebounding"
}


# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------

p3s6_num <- function(
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


p3s6_percentile <- function(
    x,
    higher_is_better = TRUE) {
  
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
  
  pct <-
    100 *
    (ranked - 1) /
    (sum(valid) - 1)
  
  if (!isTRUE(higher_is_better)) {
    pct <- 100 - pct
  }
  
  out[valid] <- pct
  
  out
}


p3s6_weighted_mean <- function(
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
# Confidence
# ------------------------------------------------------------

p3s6_confidence_label <- function(
    games,
    minutes,
    components_used) {
  
  games <- p3s6_num(
    games,
    0
  )
  
  minutes <- p3s6_num(
    minutes,
    0
  )
  
  components_used <- p3s6_num(
    components_used,
    0
  )
  
  if (
    games >= 50 &&
    minutes >= 1200 &&
    components_used >= 4
  ) {
    return("HIGH")
  }
  
  if (
    games >= 25 &&
    minutes >= 500 &&
    components_used >= 3
  ) {
    return("MODERATE")
  }
  
  "LIMITED"
}


# ------------------------------------------------------------
# Defensive role proxy
# ------------------------------------------------------------

p3s6_defensive_role <- function(
    defense_score,
    block_pct,
    steal_pct,
    rebound_score) {
  
  defense_score <- p3s6_num(
    defense_score
  )
  
  block_pct <- p3s6_num(
    block_pct
  )
  
  steal_pct <- p3s6_num(
    steal_pct
  )
  
  rebound_score <- p3s6_num(
    rebound_score
  )
  
  if (
    !is.na(block_pct) &&
    block_pct >= 80 &&
    !is.na(rebound_score) &&
    rebound_score >= 60
  ) {
    return("INTERIOR DEFENSIVE IMPACT")
  }
  
  if (
    !is.na(steal_pct) &&
    steal_pct >= 80 &&
    !is.na(defense_score) &&
    defense_score >= 60
  ) {
    return("DISRUPTIVE PERIMETER IMPACT")
  }
  
  if (
    !is.na(defense_score) &&
    defense_score >= 75
  ) {
    return("PLUS DEFENSIVE IMPACT")
  }
  
  if (
    !is.na(defense_score) &&
    defense_score >= 55
  ) {
    return("FUNCTIONAL DEFENSIVE IMPACT")
  }
  
  if (
    !is.na(defense_score) &&
    defense_score < 40
  ) {
    return("DEFENSIVE CONCERN")
  }
  
  "UNRATED / CONTEXT NEEDED"
}


# ------------------------------------------------------------
# Rebounding role
# ------------------------------------------------------------

p3s6_rebounding_role <- function(
    rebound_score,
    offensive_rebound_pct,
    defensive_rebound_pct) {
  
  rebound_score <- p3s6_num(
    rebound_score
  )
  
  offensive_rebound_pct <- p3s6_num(
    offensive_rebound_pct
  )
  
  defensive_rebound_pct <- p3s6_num(
    defensive_rebound_pct
  )
  
  if (
    !is.na(offensive_rebound_pct) &&
    offensive_rebound_pct >= 85
  ) {
    return("OFFENSIVE GLASS IMPACT")
  }
  
  if (
    !is.na(defensive_rebound_pct) &&
    defensive_rebound_pct >= 85
  ) {
    return("DEFENSIVE GLASS IMPACT")
  }
  
  if (
    !is.na(rebound_score) &&
    rebound_score >= 75
  ) {
    return("PLUS REBOUNDER")
  }
  
  if (
    !is.na(rebound_score) &&
    rebound_score >= 50
  ) {
    return("FUNCTIONAL REBOUNDER")
  }
  
  if (
    !is.na(rebound_score)
  ) {
    return("LIMITED REBOUNDING IMPACT")
  }
  
  "UNRATED"
}


# ------------------------------------------------------------
# Create table
# ------------------------------------------------------------

create_player_season_defense_rebounding_table <- function(
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
    CREATE TABLE IF NOT EXISTS player_season_defense_rebounding (

      player_id INTEGER NOT NULL,
      team_id INTEGER NOT NULL,
      season TEXT NOT NULL,

      games_played INTEGER,
      minutes DOUBLE,

      steals DOUBLE,
      blocks DOUBLE,
      rebounds DOUBLE,
      offensive_rebounds DOUBLE,
      defensive_rebounds DOUBLE,

      steals_per_game DOUBLE,
      blocks_per_game DOUBLE,
      rebounds_per_game DOUBLE,

      steals_per_100 DOUBLE,
      blocks_per_100 DOUBLE,
      rebounds_per_100 DOUBLE,
      offensive_rebounds_per_100 DOUBLE,
      defensive_rebounds_per_100 DOUBLE,

      steal_pct DOUBLE,
      block_pct DOUBLE,
      rebound_pct DOUBLE,
      offensive_rebound_pct DOUBLE,
      defensive_rebound_pct DOUBLE,

      defensive_rating DOUBLE,
      defensive_box_plus_minus DOUBLE,

      steal_impact_percentile DOUBLE,
      block_impact_percentile DOUBLE,
      rebound_impact_percentile DOUBLE,
      defensive_rating_percentile DOUBLE,
      defensive_bpm_percentile DOUBLE,

      defense_proxy_score DOUBLE,
      rebounding_score DOUBLE,
      interior_impact_score DOUBLE,
      disruption_score DOUBLE,

      defensive_role TEXT,
      rebounding_role TEXT,
      defense_confidence TEXT,

      components_used INTEGER,

      source_name TEXT,
      metric_version TEXT DEFAULT 'P3S6_v1',

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
    CREATE INDEX IF NOT EXISTS idx_player_defreb_player
    ON player_season_defense_rebounding(player_id)
    ",
    "
    CREATE INDEX IF NOT EXISTS idx_player_defreb_team
    ON player_season_defense_rebounding(team_id)
    ",
    "
    CREATE INDEX IF NOT EXISTS idx_player_defreb_season
    ON player_season_defense_rebounding(season)
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

get_phase3_defense_rebounding_source <- function(
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
      adv.steals_per_100,
      adv.blocks_per_100,
      adv.rebounds_per_100,
      adv.offensive_rebounds_per_100,
      adv.defensive_rebounds_per_100,
      adv.steal_pct,
      adv.block_pct,
      adv.rebound_pct,
      adv.offensive_rebound_pct,
      adv.defensive_rebound_pct,
      adv.defensive_rating,
      adv.defensive_box_plus_minus,
    "
  } else {
    "
      NULL AS steals_per_100,
      NULL AS blocks_per_100,
      NULL AS rebounds_per_100,
      NULL AS offensive_rebounds_per_100,
      NULL AS defensive_rebounds_per_100,
      NULL AS steal_pct,
      NULL AS block_pct,
      NULL AS rebound_pct,
      NULL AS offensive_rebound_pct,
      NULL AS defensive_rebound_pct,
      NULL AS defensive_rating,
      NULL AS defensive_box_plus_minus,
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
      base.steals,
      base.blocks,
      base.rebounds,
      base.offensive_rebounds,
      base.defensive_rebounds,
      base.steals_per_game,
      base.blocks_per_game,
      base.rebounds_per_game,
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
# Derive intelligence
# ------------------------------------------------------------

derive_phase3_defense_rebounding_intelligence <- function(
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
  
  d$steal_impact_percentile <-
    p3s6_percentile(
      d$steals_per_100
    )
  
  d$block_impact_percentile <-
    p3s6_percentile(
      d$blocks_per_100
    )
  
  d$rebound_impact_percentile <-
    p3s6_percentile(
      d$rebounds_per_100
    )
  
  d$defensive_rating_percentile <-
    p3s6_percentile(
      d$defensive_rating,
      higher_is_better = FALSE
    )
  
  d$defensive_bpm_percentile <-
    p3s6_percentile(
      d$defensive_box_plus_minus
    )
  
  orb_pctile <-
    p3s6_percentile(
      d$offensive_rebounds_per_100
    )
  
  drb_pctile <-
    p3s6_percentile(
      d$defensive_rebounds_per_100
    )
  
  n <- nrow(d)
  
  d$defense_proxy_score <-
    rep(
      NA_real_,
      n
    )
  
  d$rebounding_score <-
    rep(
      NA_real_,
      n
    )
  
  d$interior_impact_score <-
    rep(
      NA_real_,
      n
    )
  
  d$disruption_score <-
    rep(
      NA_real_,
      n
    )
  
  d$components_used <-
    rep(
      0L,
      n
    )
  
  for (
    i in seq_len(n)
  ) {
    
    defensive_components <- c(
      steal =
        d$steal_impact_percentile[[i]],
      block =
        d$block_impact_percentile[[i]],
      drtg =
        d$defensive_rating_percentile[[i]],
      dbpm =
        d$defensive_bpm_percentile[[i]],
      rebound =
        d$rebound_impact_percentile[[i]]
    )
    
    valid <- is.finite(
      defensive_components
    )
    
    d$components_used[[i]] <-
      sum(valid)
    
    d$defense_proxy_score[[i]] <-
      p3s6_weighted_mean(
        defensive_components,
        c(
          0.20,
          0.22,
          0.24,
          0.22,
          0.12
        )
      )
    
    d$rebounding_score[[i]] <-
      p3s6_weighted_mean(
        c(
          d$rebound_impact_percentile[[i]],
          orb_pctile[[i]],
          drb_pctile[[i]]
        ),
        c(
          0.50,
          0.20,
          0.30
        )
      )
    
    d$interior_impact_score[[i]] <-
      p3s6_weighted_mean(
        c(
          d$block_impact_percentile[[i]],
          drb_pctile[[i]],
          d$rebound_impact_percentile[[i]]
        ),
        c(
          0.48,
          0.27,
          0.25
        )
      )
    
    d$disruption_score[[i]] <-
      p3s6_weighted_mean(
        c(
          d$steal_impact_percentile[[i]],
          d$block_impact_percentile[[i]]
        ),
        c(
          0.58,
          0.42
        )
      )
  }
  
  d$defense_confidence <- vapply(
    seq_len(n),
    function(i) {
      p3s6_confidence_label(
        games =
          d$games_played[[i]],
        minutes =
          d$minutes[[i]],
        components_used =
          d$components_used[[i]]
      )
    },
    character(1)
  )
  
  d$defensive_role <- vapply(
    seq_len(n),
    function(i) {
      p3s6_defensive_role(
        defense_score =
          d$defense_proxy_score[[i]],
        block_pct =
          d$block_impact_percentile[[i]],
        steal_pct =
          d$steal_impact_percentile[[i]],
        rebound_score =
          d$rebounding_score[[i]]
      )
    },
    character(1)
  )
  
  d$rebounding_role <- vapply(
    seq_len(n),
    function(i) {
      p3s6_rebounding_role(
        rebound_score =
          d$rebounding_score[[i]],
        offensive_rebound_pct =
          orb_pctile[[i]],
        defensive_rebound_pct =
          drb_pctile[[i]]
      )
    },
    character(1)
  )
  
  d$metric_version <-
    "P3S6_v1"
  
  d$updated_at <-
    Sys.time()
  
  keep <- c(
    "player_id",
    "team_id",
    "season",
    "games_played",
    "minutes",
    "steals",
    "blocks",
    "rebounds",
    "offensive_rebounds",
    "defensive_rebounds",
    "steals_per_game",
    "blocks_per_game",
    "rebounds_per_game",
    "steals_per_100",
    "blocks_per_100",
    "rebounds_per_100",
    "offensive_rebounds_per_100",
    "defensive_rebounds_per_100",
    "steal_pct",
    "block_pct",
    "rebound_pct",
    "offensive_rebound_pct",
    "defensive_rebound_pct",
    "defensive_rating",
    "defensive_box_plus_minus",
    "steal_impact_percentile",
    "block_impact_percentile",
    "rebound_impact_percentile",
    "defensive_rating_percentile",
    "defensive_bpm_percentile",
    "defense_proxy_score",
    "rebounding_score",
    "interior_impact_score",
    "disruption_score",
    "defensive_role",
    "rebounding_role",
    "defense_confidence",
    "components_used",
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

upsert_phase3_defense_rebounding_intelligence <- function(
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
  
  create_player_season_defense_rebounding_table(
    con
  )
  
  temp_table <- paste0(
    "tmp_player_defreb_",
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
          DELETE FROM player_season_defense_rebounding
          WHERE EXISTS (
            SELECT 1
            FROM ",
          temp_q,
          " src
            WHERE src.player_id = player_season_defense_rebounding.player_id
              AND src.team_id = player_season_defense_rebounding.team_id
              AND src.season = player_season_defense_rebounding.season
          )
          "
        )
      )
      
      DBI::dbExecute(
        con,
        paste0(
          "
          INSERT INTO player_season_defense_rebounding (
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

build_phase3_defense_rebounding_intelligence <- function(
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
    get_phase3_defense_rebounding_source(
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
    derive_phase3_defense_rebounding_intelligence(
      source
    )
  
  result <-
    upsert_phase3_defense_rebounding_intelligence(
      derived,
      con = con
    )
  
  invisible(
    c(
      list(
        status =
          "DEFENSE + REBOUNDING INTELLIGENCE BUILT"
      ),
      result
    )
  )
}


# ------------------------------------------------------------
# Read intelligence
# ------------------------------------------------------------

get_phase3_defense_rebounding_intelligence <- function(
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
    !tbi_defense_rebounding_table() %in%
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
      "dr.season = ?"
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
      "dr.player_id = ?"
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
      dr.*,
      p.player_name,
      p.primary_position,
      p.player_age,
      t.team_name,
      t.abbreviation

    FROM player_season_defense_rebounding dr

    LEFT JOIN players p
      ON p.player_id = dr.player_id

    LEFT JOIN teams t
      ON t.team_id = dr.team_id

    ",
    where_sql,
    "

    ORDER BY
      dr.defense_proxy_score DESC,
      dr.rebounding_score DESC,
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
# Attach to BIE roster frame
# ------------------------------------------------------------

attach_phase3_defense_rebounding_to_players <- function(
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
  
  dr <-
    get_phase3_defense_rebounding_intelligence(
      season =
        season,
      con =
        con
    )
  
  if (!nrow(dr)) {
    return(players)
  }
  
  keep <- c(
    "player_id",
    "team_id",
    "defense_proxy_score",
    "rebounding_score",
    "interior_impact_score",
    "disruption_score",
    "defensive_role",
    "rebounding_role",
    "defense_confidence"
  )
  
  dr <- dr[
    ,
    intersect(
      keep,
      names(dr)
    ),
    drop = FALSE
  ]
  
  if (
    "team_id" %in%
    names(players) &&
    "team_id" %in%
    names(dr)
  ) {
    
    return(
      merge(
        players,
        dr,
        by = c(
          "player_id",
          "team_id"
        ),
        all.x = TRUE,
        sort = FALSE
      )
    )
  }
  
  dr <- dr[
    !duplicated(
      dr$player_id
    ),
    ,
    drop = FALSE
  ]
  
  dr$team_id <- NULL
  
  merge(
    players,
    dr,
    by = "player_id",
    all.x = TRUE,
    sort = FALSE
  )
}


# ------------------------------------------------------------
# Step 6 health check
# ------------------------------------------------------------

phase3_step6_healthcheck <- function(
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
    tbi_defense_rebounding_table() %in%
    tables
  
  rows <- if (
    exists_table
  ) {
    DBI::dbGetQuery(
      con,
      "
      SELECT COUNT(*) AS n
      FROM player_season_defense_rebounding
      WHERE season = ?
      ",
      params = list(
        season
      )
    )$n[[1]]
  } else {
    0L
  }
  
  defense_rated <- if (
    exists_table &&
    rows > 0
  ) {
    DBI::dbGetQuery(
      con,
      "
      SELECT COUNT(*) AS n
      FROM player_season_defense_rebounding
      WHERE season = ?
        AND defense_proxy_score IS NOT NULL
      ",
      params = list(
        season
      )
    )$n[[1]]
  } else {
    0L
  }
  
  rebounding_rated <- if (
    exists_table &&
    rows > 0
  ) {
    DBI::dbGetQuery(
      con,
      "
      SELECT COUNT(*) AS n
      FROM player_season_defense_rebounding
      WHERE season = ?
        AND rebounding_score IS NOT NULL
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
      "Step 6 — Defense + Rebounding Intelligence",
    status = if (
      rows > 0 &&
      defense_rated > 0 &&
      rebounding_rated > 0
    ) {
      "READY"
    } else {
      "BUILD REQUIRED"
    },
    season =
      season,
    rows =
      rows,
    defense_rated_rows =
      defense_rated,
    rebounding_rated_rows =
      rebounding_rated,
    defense_method =
      "BOX-SCORE / IMPACT PROXY",
    tracking_status =
      "NOT LOADED — NO MATCHUP OR TRACKING CLAIMS"
  )
}

# ============================================================
# PHASE 3.2 — STEP 6 DEFENSE CALIBRATION V2
#
# Goals:
#   - keep defensive rating lower-is-better
#   - use a qualified rotation-player reference pool
#   - reduce steals/blocks over-credit
#   - give DBPM + defensive rating more weight
#   - stabilize small samples toward league-average
#   - prevent elite defensive labels without possession-level
#     evidence
#
# Persistent table schema is unchanged.
# ============================================================


p3s6_v2_reference_percentile <- function(
    x,
    reference_mask = NULL,
    higher_is_better = TRUE) {
  
  value <- suppressWarnings(
    as.numeric(x)
  )
  
  valid <- is.finite(value)
  
  out <- rep(
    NA_real_,
    length(value)
  )
  
  if (
    is.null(reference_mask)
  ) {
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


p3s6_v2_sample_reliability <- function(
    games,
    minutes) {
  
  games <- p3s6_num(
    games,
    0
  )
  
  minutes <- p3s6_num(
    minutes,
    0
  )
  
  game_factor <-
    min(
      1,
      games / 50
    )
  
  minute_factor <-
    min(
      1,
      minutes / 1200
    )
  
  # 60% sample floor prevents wild collapse while still
  # shrinking small samples toward 50.
  max(
    0.60,
    0.50 * game_factor +
      0.50 * minute_factor
  )
}


p3s6_v2_stabilize <- function(
    score,
    reliability,
    center = 50) {
  
  score <- p3s6_num(
    score
  )
  
  reliability <- p3s6_num(
    reliability,
    0.60
  )
  
  if (is.na(score)) {
    return(NA_real_)
  }
  
  center +
    (
      score -
        center
    ) *
    reliability
}


p3s6_v2_defensive_role <- function(
    defense_score,
    drtg_pct,
    dbpm_pct,
    disruption_score,
    rebound_score) {
  
  defense_score <- p3s6_num(defense_score)
  drtg_pct <- p3s6_num(drtg_pct)
  dbpm_pct <- p3s6_num(dbpm_pct)
  disruption_score <- p3s6_num(disruption_score)
  rebound_score <- p3s6_num(rebound_score)
  
  possession_evidence <- p3s6_weighted_mean(
    c(
      drtg_pct,
      dbpm_pct
    ),
    c(
      0.45,
      0.55
    )
  )
  
  if (
    !is.na(defense_score) &&
    defense_score >= 80 &&
    !is.na(possession_evidence) &&
    possession_evidence >= 72
  ) {
    return(
      "HIGH-END DEFENSIVE IMPACT"
    )
  }
  
  if (
    !is.na(defense_score) &&
    defense_score >= 70 &&
    !is.na(possession_evidence) &&
    possession_evidence >= 60
  ) {
    return(
      "PLUS DEFENSIVE IMPACT"
    )
  }
  
  if (
    !is.na(defense_score) &&
    defense_score >= 55
  ) {
    return(
      "FUNCTIONAL DEFENSIVE IMPACT"
    )
  }
  
  if (
    !is.na(defense_score) &&
    defense_score < 40
  ) {
    return(
      "DEFENSIVE CONCERN"
    )
  }
  
  if (
    !is.na(disruption_score) &&
    disruption_score >= 75
  ) {
    return(
      "DISRUPTIVE / CONTEXT NEEDED"
    )
  }
  
  if (
    !is.na(rebound_score) &&
    rebound_score >= 75
  ) {
    return(
      "DEFENSIVE REBOUNDING VALUE"
    )
  }
  
  "UNRATED / CONTEXT NEEDED"
}


derive_phase3_defense_rebounding_intelligence <- function(
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
  n <- nrow(d)
  
  games_vec <- suppressWarnings(
    as.numeric(
      d$games_played
    )
  )
  
  minutes_vec <- suppressWarnings(
    as.numeric(
      d$minutes
    )
  )
  
  # Qualified NBA rotation reference pool.
  qualified <-
    is.finite(games_vec) &
    is.finite(minutes_vec) &
    games_vec >= 20 &
    minutes_vec >= 300
  
  if (
    sum(
      qualified,
      na.rm = TRUE
    ) < 100
  ) {
    qualified <-
      is.finite(games_vec) &
      is.finite(minutes_vec)
  }
  
  d$steal_impact_percentile <-
    p3s6_v2_reference_percentile(
      d$steals_per_100,
      qualified
    )
  
  d$block_impact_percentile <-
    p3s6_v2_reference_percentile(
      d$blocks_per_100,
      qualified
    )
  
  d$rebound_impact_percentile <-
    p3s6_v2_reference_percentile(
      d$rebounds_per_100,
      qualified
    )
  
  d$defensive_rating_percentile <-
    p3s6_v2_reference_percentile(
      d$defensive_rating,
      qualified,
      higher_is_better = FALSE
    )
  
  d$defensive_bpm_percentile <-
    p3s6_v2_reference_percentile(
      d$defensive_box_plus_minus,
      qualified
    )
  
  orb_pctile <-
    p3s6_v2_reference_percentile(
      d$offensive_rebounds_per_100,
      qualified
    )
  
  drb_pctile <-
    p3s6_v2_reference_percentile(
      d$defensive_rebounds_per_100,
      qualified
    )
  
  d$defense_proxy_score <- rep(
    NA_real_,
    n
  )
  
  d$rebounding_score <- rep(
    NA_real_,
    n
  )
  
  d$interior_impact_score <- rep(
    NA_real_,
    n
  )
  
  d$disruption_score <- rep(
    NA_real_,
    n
  )
  
  d$components_used <- rep(
    0L,
    n
  )
  
  for (
    i in seq_len(n)
  ) {
    
    # Disruption stays descriptive, but is no longer allowed to
    # carry the overall defensive score.
    raw_disruption <-
      p3s6_weighted_mean(
        c(
          d$steal_impact_percentile[[i]],
          d$block_impact_percentile[[i]]
        ),
        c(
          0.55,
          0.45
        )
      )
    
    raw_rebounding <-
      p3s6_weighted_mean(
        c(
          d$rebound_impact_percentile[[i]],
          orb_pctile[[i]],
          drb_pctile[[i]]
        ),
        c(
          0.45,
          0.15,
          0.40
        )
      )
    
    raw_interior <-
      p3s6_weighted_mean(
        c(
          d$block_impact_percentile[[i]],
          drb_pctile[[i]],
          d$defensive_rating_percentile[[i]]
        ),
        c(
          0.34,
          0.26,
          0.40
        )
      )
    
    # V2 overall defensive proxy:
    #   30% DBPM
    #   30% defensive rating
    #   18% disruption
    #   12% rebounding
    #   10% interior context
    #
    # If DBPM is missing, weighted_mean renormalizes remaining
    # evidence rather than fabricating a value.
    defensive_components <- c(
      dbpm =
        d$defensive_bpm_percentile[[i]],
      drtg =
        d$defensive_rating_percentile[[i]],
      disruption =
        raw_disruption,
      rebound =
        raw_rebounding,
      interior =
        raw_interior
    )
    
    valid <- is.finite(
      defensive_components
    )
    
    d$components_used[[i]] <-
      sum(valid)
    
    raw_defense <-
      p3s6_weighted_mean(
        defensive_components,
        c(
          0.30,
          0.30,
          0.18,
          0.12,
          0.10
        )
      )
    
    reliability <-
      p3s6_v2_sample_reliability(
        games =
          d$games_played[[i]],
        minutes =
          d$minutes[[i]]
      )
    
    d$disruption_score[[i]] <-
      p3s6_v2_stabilize(
        raw_disruption,
        reliability
      )
    
    d$rebounding_score[[i]] <-
      p3s6_v2_stabilize(
        raw_rebounding,
        reliability
      )
    
    d$interior_impact_score[[i]] <-
      p3s6_v2_stabilize(
        raw_interior,
        reliability
      )
    
    d$defense_proxy_score[[i]] <-
      p3s6_v2_stabilize(
        raw_defense,
        reliability
      )
    
    # Guardrail: without at least average possession-level
    # evidence, the proxy cannot present as elite.
    possession_evidence <-
      p3s6_weighted_mean(
        c(
          d$defensive_rating_percentile[[i]],
          d$defensive_bpm_percentile[[i]]
        ),
        c(
          0.45,
          0.55
        )
      )
    
    if (
      is.finite(
        d$defense_proxy_score[[i]]
      ) &&
      is.finite(
        possession_evidence
      ) &&
      possession_evidence < 55
    ) {
      d$defense_proxy_score[[i]] <-
        min(
          d$defense_proxy_score[[i]],
          69
        )
    }
    
    if (
      is.finite(
        d$defense_proxy_score[[i]]
      ) &&
      is.finite(
        possession_evidence
      ) &&
      possession_evidence < 45
    ) {
      d$defense_proxy_score[[i]] <-
        min(
          d$defense_proxy_score[[i]],
          59
        )
    }
  }
  
  d$defense_confidence <- vapply(
    seq_len(n),
    function(i) {
      p3s6_confidence_label(
        games =
          d$games_played[[i]],
        minutes =
          d$minutes[[i]],
        components_used =
          d$components_used[[i]]
      )
    },
    character(1)
  )
  
  d$defensive_role <- vapply(
    seq_len(n),
    function(i) {
      p3s6_v2_defensive_role(
        defense_score =
          d$defense_proxy_score[[i]],
        drtg_pct =
          d$defensive_rating_percentile[[i]],
        dbpm_pct =
          d$defensive_bpm_percentile[[i]],
        disruption_score =
          d$disruption_score[[i]],
        rebound_score =
          d$rebounding_score[[i]]
      )
    },
    character(1)
  )
  
  d$rebounding_role <- vapply(
    seq_len(n),
    function(i) {
      p3s6_rebounding_role(
        rebound_score =
          d$rebounding_score[[i]],
        offensive_rebound_pct =
          orb_pctile[[i]],
        defensive_rebound_pct =
          drb_pctile[[i]]
      )
    },
    character(1)
  )
  
  d$metric_version <-
    "P3S6_v2_CALIBRATED"
  
  d$updated_at <-
    Sys.time()
  
  keep <- c(
    "player_id",
    "team_id",
    "season",
    "games_played",
    "minutes",
    "steals",
    "blocks",
    "rebounds",
    "offensive_rebounds",
    "defensive_rebounds",
    "steals_per_game",
    "blocks_per_game",
    "rebounds_per_game",
    "steals_per_100",
    "blocks_per_100",
    "rebounds_per_100",
    "offensive_rebounds_per_100",
    "defensive_rebounds_per_100",
    "steal_pct",
    "block_pct",
    "rebound_pct",
    "offensive_rebound_pct",
    "defensive_rebound_pct",
    "defensive_rating",
    "defensive_box_plus_minus",
    "steal_impact_percentile",
    "block_impact_percentile",
    "rebound_impact_percentile",
    "defensive_rating_percentile",
    "defensive_bpm_percentile",
    "defense_proxy_score",
    "rebounding_score",
    "interior_impact_score",
    "disruption_score",
    "defensive_role",
    "rebounding_role",
    "defense_confidence",
    "components_used",
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


preview_phase3_defense_v2 <- function(
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
  
  source <-
    get_phase3_defense_rebounding_source(
      season =
        season,
      con =
        con
    )
  
  d <-
    derive_phase3_defense_rebounding_intelligence(
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
      "games_played",
      "minutes",
      "defensive_rating",
      "defensive_rating_percentile",
      "defensive_box_plus_minus",
      "defensive_bpm_percentile",
      "steal_impact_percentile",
      "block_impact_percentile",
      "rebound_impact_percentile",
      "disruption_score",
      "rebounding_score",
      "interior_impact_score",
      "defense_proxy_score",
      "defensive_role",
      "defense_confidence",
      "metric_version"
    ),
    names(d)
  )
  
  d[
    order(
      -d$defense_proxy_score
    ),
    keep,
    drop = FALSE
  ]
}


phase3_step6_v2_healthcheck <- function(
    season = "2025-26") {
  
  d <- tryCatch(
    preview_phase3_defense_v2(
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
  
  if (!nrow(d)) {
    return(
      list(
        phase =
          "Phase 3.2",
        step =
          "Step 6 Defense Calibration V2",
        status =
          "REVIEW",
        explanation =
          attr(
            d,
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
      "Step 6 Defense Calibration V2",
    status =
      "READY FOR SPOT CHECK",
    season =
      season,
    preview_rows =
      nrow(d),
    finite_defense_scores =
      sum(
        is.finite(
          d$defense_proxy_score
        )
      ),
    metric_version =
      unique(
        d$metric_version
      )[[1]],
    method =
      "DBPM + DEFENSIVE RATING + STABILIZED DISRUPTION / REBOUNDING / INTERIOR CONTEXT"
  )
}

# ============================================================
# PHASE 3.2 — STEP 6 V3
# Defense + Rebounding — evidence-gated calibration
# ============================================================


derive_phase3_defense_rebounding_intelligence <- function(
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
  n <- nrow(d)
  
  games_vec <- suppressWarnings(
    as.numeric(
      d$games_played
    )
  )
  
  minutes_vec <- suppressWarnings(
    as.numeric(
      d$minutes
    )
  )
  
  qualified <-
    is.finite(games_vec) &
    is.finite(minutes_vec) &
    games_vec >= 20 &
    minutes_vec >= 300
  
  if (
    sum(
      qualified,
      na.rm = TRUE
    ) < 100
  ) {
    qualified <-
      is.finite(games_vec) &
      is.finite(minutes_vec)
  }
  
  d$steal_impact_percentile <-
    p3s6_v2_reference_percentile(
      d$steals_per_100,
      qualified
    )
  
  d$block_impact_percentile <-
    p3s6_v2_reference_percentile(
      d$blocks_per_100,
      qualified
    )
  
  d$rebound_impact_percentile <-
    p3s6_v2_reference_percentile(
      d$rebounds_per_100,
      qualified
    )
  
  d$defensive_rating_percentile <-
    p3s6_v2_reference_percentile(
      d$defensive_rating,
      qualified,
      higher_is_better = FALSE
    )
  
  d$defensive_bpm_percentile <-
    p3s6_v2_reference_percentile(
      d$defensive_box_plus_minus,
      qualified
    )
  
  orb_pctile <-
    p3s6_v2_reference_percentile(
      d$offensive_rebounds_per_100,
      qualified
    )
  
  drb_pctile <-
    p3s6_v2_reference_percentile(
      d$defensive_rebounds_per_100,
      qualified
    )
  
  d$defense_proxy_score <- rep(
    NA_real_,
    n
  )
  
  d$rebounding_score <- rep(
    NA_real_,
    n
  )
  
  d$interior_impact_score <- rep(
    NA_real_,
    n
  )
  
  d$disruption_score <- rep(
    NA_real_,
    n
  )
  
  d$components_used <- rep(
    0L,
    n
  )
  
  advanced_defense_available <- rep(
    FALSE,
    n
  )
  
  for (i in seq_len(n)) {
    
    reliability <-
      p3s6_v2_sample_reliability(
        d$games_played[[i]],
        d$minutes[[i]]
      )
    
    raw_disruption <-
      p3s6_weighted_mean(
        c(
          d$steal_impact_percentile[[i]],
          d$block_impact_percentile[[i]]
        ),
        c(
          0.55,
          0.45
        )
      )
    
    raw_rebounding <-
      p3s6_weighted_mean(
        c(
          d$rebound_impact_percentile[[i]],
          orb_pctile[[i]],
          drb_pctile[[i]]
        ),
        c(
          0.45,
          0.15,
          0.40
        )
      )
    
    raw_interior <-
      p3s6_weighted_mean(
        c(
          d$block_impact_percentile[[i]],
          drb_pctile[[i]]
        ),
        c(
          0.55,
          0.45
        )
      )
    
    has_drtg <-
      is.finite(
        d$defensive_rating_percentile[[i]]
      )
    
    has_dbpm <-
      is.finite(
        d$defensive_bpm_percentile[[i]]
      )
    
    advanced_defense_available[[i]] <-
      has_drtg ||
      has_dbpm
    
    if (
      advanced_defense_available[[i]]
    ) {
      
      raw_defense <-
        p3s6_weighted_mean(
          c(
            d$defensive_bpm_percentile[[i]],
            d$defensive_rating_percentile[[i]],
            raw_disruption,
            raw_rebounding,
            raw_interior
          ),
          c(
            0.32,
            0.30,
            0.16,
            0.12,
            0.10
          )
        )
      
    } else {
      
      # Honest fallback: this is defensive ACTIVITY only.
      raw_defense <-
        p3s6_weighted_mean(
          c(
            raw_disruption,
            raw_rebounding,
            raw_interior
          ),
          c(
            0.45,
            0.30,
            0.25
          )
        )
    }
    
    d$disruption_score[[i]] <-
      p3s6_v2_stabilize(
        raw_disruption,
        reliability
      )
    
    d$rebounding_score[[i]] <-
      p3s6_v2_stabilize(
        raw_rebounding,
        reliability
      )
    
    d$interior_impact_score[[i]] <-
      p3s6_v2_stabilize(
        raw_interior,
        reliability
      )
    
    d$defense_proxy_score[[i]] <-
      p3s6_v2_stabilize(
        raw_defense,
        reliability
      )
    
    # A box-score-only defense proxy may be useful, but it may
    # not present as elite overall defensive impact.
    if (
      !advanced_defense_available[[i]] &&
      is.finite(
        d$defense_proxy_score[[i]]
      )
    ) {
      d$defense_proxy_score[[i]] <-
        min(
          d$defense_proxy_score[[i]],
          69
        )
    }
    
    d$components_used[[i]] <-
      sum(
        is.finite(
          c(
            d$steal_impact_percentile[[i]],
            d$block_impact_percentile[[i]],
            d$rebound_impact_percentile[[i]],
            d$defensive_rating_percentile[[i]],
            d$defensive_bpm_percentile[[i]]
          )
        )
      )
  }
  
  d$defense_confidence <- vapply(
    seq_len(n),
    function(i) {
      
      if (
        !advanced_defense_available[[i]]
      ) {
        return(
          if (
            d$games_played[[i]] >= 25 &&
            d$minutes[[i]] >= 500
          ) {
            "MODERATE — BOX SCORE PROXY"
          } else {
            "LIMITED — BOX SCORE PROXY"
          }
        )
      }
      
      p3s6_confidence_label(
        games =
          d$games_played[[i]],
        minutes =
          d$minutes[[i]],
        components_used =
          d$components_used[[i]]
      )
    },
    character(1)
  )
  
  d$defensive_role <- vapply(
    seq_len(n),
    function(i) {
      
      if (
        !advanced_defense_available[[i]]
      ) {
        if (
          is.finite(
            d$defense_proxy_score[[i]]
          ) &&
          d$defense_proxy_score[[i]] >= 60
        ) {
          return(
            "POSITIVE DEFENSIVE ACTIVITY — CONTEXT NEEDED"
          )
        }
        
        if (
          is.finite(
            d$defense_proxy_score[[i]]
          ) &&
          d$defense_proxy_score[[i]] < 40
        ) {
          return(
            "LOW DEFENSIVE ACTIVITY — CONTEXT NEEDED"
          )
        }
        
        return(
          "BOX-SCORE DEFENSE PROXY"
        )
      }
      
      p3s6_v2_defensive_role(
        defense_score =
          d$defense_proxy_score[[i]],
        drtg_pct =
          d$defensive_rating_percentile[[i]],
        dbpm_pct =
          d$defensive_bpm_percentile[[i]],
        disruption_score =
          d$disruption_score[[i]],
        rebound_score =
          d$rebounding_score[[i]]
      )
    },
    character(1)
  )
  
  d$rebounding_role <- vapply(
    seq_len(n),
    function(i) {
      p3s6_rebounding_role(
        rebound_score =
          d$rebounding_score[[i]],
        offensive_rebound_pct =
          orb_pctile[[i]],
        defensive_rebound_pct =
          drb_pctile[[i]]
      )
    },
    character(1)
  )
  
  d$metric_version <-
    "P3S6_v3_EVIDENCE_GATED"
  
  d$updated_at <-
    Sys.time()
  
  keep <- c(
    "player_id",
    "team_id",
    "season",
    "games_played",
    "minutes",
    "steals",
    "blocks",
    "rebounds",
    "offensive_rebounds",
    "defensive_rebounds",
    "steals_per_game",
    "blocks_per_game",
    "rebounds_per_game",
    "steals_per_100",
    "blocks_per_100",
    "rebounds_per_100",
    "offensive_rebounds_per_100",
    "defensive_rebounds_per_100",
    "steal_pct",
    "block_pct",
    "rebound_pct",
    "offensive_rebound_pct",
    "defensive_rebound_pct",
    "defensive_rating",
    "defensive_box_plus_minus",
    "steal_impact_percentile",
    "block_impact_percentile",
    "rebound_impact_percentile",
    "defensive_rating_percentile",
    "defensive_bpm_percentile",
    "defense_proxy_score",
    "rebounding_score",
    "interior_impact_score",
    "disruption_score",
    "defensive_role",
    "rebounding_role",
    "defense_confidence",
    "components_used",
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


phase3_step6_v3_healthcheck <- function(
    season = "2025-26") {
  
  source <-
    get_phase3_defense_rebounding_source(
      season = season
    )
  
  d <-
    derive_phase3_defense_rebounding_intelligence(
      source
    )
  
  advanced_rows <-
    sum(
      is.finite(
        d$defensive_rating
      ) |
        is.finite(
          d$defensive_box_plus_minus
        ),
      na.rm = TRUE
    )
  
  list(
    phase = "Phase 3.2",
    step =
      "Step 6 — Evidence-Gated Defense V3",
    status = if (
      nrow(d) > 0
    ) {
      "READY FOR REBUILD"
    } else {
      "REVIEW"
    },
    rows = nrow(d),
    advanced_defense_rows =
      advanced_rows,
    box_score_proxy_rows =
      nrow(d) -
      advanced_rows,
    metric_version =
      "P3S6_v3_EVIDENCE_GATED",
    rule =
      "BOX-SCORE-ONLY DEFENSE CANNOT EXCEED 69"
  )
}