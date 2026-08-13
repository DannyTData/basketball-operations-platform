# ============================================================
# TBI — Phase 15B Refined Player Data Completeness Audit
#
# PURPOSE
#   Distinguish legitimate "no prior-season NBA data" cases from
#   likely identity/join problems such as Jimmy Butler / Butler III.
#
# READ-ONLY. This script does not modify the database.
#
# OUTPUTS
#   qa/player_data_phase15b_full_audit.csv
#   qa/player_data_phase15b_identity_suspects.csv
#   qa/player_data_phase15b_no_prior_season_data.csv
#   qa/player_data_phase15b_partial_chain.csv
# ============================================================

library(DBI)
library(RSQLite)

db_path <- file.path(
  "inst",
  "database",
  "tbi.sqlite"
)

if (!file.exists(db_path)) {
  stop(
    "Database not found at inst/database/tbi.sqlite",
    call. = FALSE
  )
}

db <- DBI::dbConnect(
  RSQLite::SQLite(),
  db_path
)

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------

safe_disconnect <- function() {
  if (
    exists("db") &&
    !is.null(db) &&
    DBI::dbIsValid(db)
  ) {
    DBI::dbDisconnect(db)
  }
}

normalize_player_name <- function(x) {
  
  x <- as.character(x)
  
  # Transliterate accents where possible:
  # Şengün -> Sengun, Diabaté -> Diabate, Salaün -> Salaun
  x <- iconv(
    x,
    from = "",
    to = "ASCII//TRANSLIT"
  )
  
  x[is.na(x)] <- ""
  
  x <- tolower(x)
  
  # Standardize apostrophes / punctuation.
  x <- gsub(
    "[^a-z0-9 ]+",
    " ",
    x
  )
  
  # Remove common suffixes so:
  # Jimmy Butler == Jimmy Butler III
  # DaRon Holmes == DaRon Holmes II
  # Tolu Smith == Tolu Smith III
  x <- gsub(
    "\\b(jr|sr|ii|iii|iv|v)\\b",
    " ",
    x
  )
  
  x <- gsub(
    "\\s+",
    " ",
    x
  )
  
  trimws(x)
}

table_exists <- function(tbl) {
  tbl %in% DBI::dbListTables(db)
}

has_player_season_row <- function(tbl,
                                  player_id,
                                  season) {
  
  if (!table_exists(tbl)) {
    return(FALSE)
  }
  
  cols <- DBI::dbListFields(
    db,
    tbl
  )
  
  if (
    !"player_id" %in% cols ||
    !"season" %in% cols
  ) {
    return(FALSE)
  }
  
  DBI::dbGetQuery(
    db,
    paste0(
      "SELECT COUNT(*) AS n ",
      "FROM ",
      tbl,
      " WHERE player_id = ? ",
      "AND season = ?"
    ),
    params = list(
      player_id,
      season
    )
  )$n[[1]] > 0L
}

has_any_player_row <- function(tbl,
                               player_id) {
  
  if (!table_exists(tbl)) {
    return(FALSE)
  }
  
  cols <- DBI::dbListFields(
    db,
    tbl
  )
  
  if (!"player_id" %in% cols) {
    return(FALSE)
  }
  
  DBI::dbGetQuery(
    db,
    paste0(
      "SELECT COUNT(*) AS n ",
      "FROM ",
      tbl,
      " WHERE player_id = ?"
    ),
    params = list(
      player_id
    )
  )$n[[1]] > 0L
}

# ------------------------------------------------------------
# Required tables
# ------------------------------------------------------------

required_tables <- c(
  "players",
  "teams",
  "depth_chart",
  "player_season_stats",
  "player_season_advanced",
  "player_season_shooting",
  "player_season_playmaking",
  "player_season_defense_rebounding",
  "player_season_roles",
  "player_season_impact",
  "player_projection_intelligence"
)

missing_tables <- setdiff(
  required_tables,
  DBI::dbListTables(db)
)

if (length(missing_tables)) {
  safe_disconnect()
  
  stop(
    paste0(
      "Missing required table(s): ",
      paste(
        missing_tables,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

# ------------------------------------------------------------
# Resolve current depth-chart season
# ------------------------------------------------------------

depth_season_info <- DBI::dbGetQuery(
  db,
  "
  SELECT
    season,
    COUNT(DISTINCT team_id) AS team_count
  FROM depth_chart
  GROUP BY season
  ORDER BY season DESC
  "
)

complete_depth_seasons <- depth_season_info[
  depth_season_info$team_count >= 30,
  ,
  drop = FALSE
]

if (!nrow(complete_depth_seasons)) {
  safe_disconnect()
  
  stop(
    "No complete 30-team depth-chart season found.",
    call. = FALSE
  )
}

depth_season <- as.character(
  complete_depth_seasons$season[[1]]
)

performance_season <- DBI::dbGetQuery(
  db,
  "
  SELECT MAX(season) AS season
  FROM player_season_stats
  "
)$season[[1]]

if (
  is.na(performance_season) ||
  !nzchar(performance_season)
) {
  safe_disconnect()
  
  stop(
    "No performance season found.",
    call. = FALSE
  )
}

cat("\n")
cat("============================================================\n")
cat("TBI PHASE 15B — REFINED PLAYER DATA AUDIT\n")
cat("============================================================\n")
cat(
  "Depth-chart season: ",
  depth_season,
  "\n",
  sep = ""
)
cat(
  "Performance season: ",
  performance_season,
  "\n\n",
  sep = ""
)

# ------------------------------------------------------------
# Active/depth-chart player population
# ------------------------------------------------------------

active_players <- DBI::dbGetQuery(
  db,
  "
  SELECT DISTINCT
    dc.player_id,
    p.player_name,
    p.nba_player_id,
    p.player_age,
    p.primary_position,
    dc.team_id,
    t.team_name,
    t.abbreviation
  FROM depth_chart dc
  INNER JOIN players p
    ON p.player_id = dc.player_id
  INNER JOIN teams t
    ON t.team_id = dc.team_id
  WHERE dc.season = ?
  ORDER BY
    t.team_name,
    p.player_name
  ",
  params = list(
    depth_season
  )
)

active_players$normalized_name <-
  normalize_player_name(
    active_players$player_name
  )

# ------------------------------------------------------------
# All canonical players + 2025-26 stats availability
# Used to detect duplicate identities.
# ------------------------------------------------------------

all_players <- DBI::dbGetQuery(
  db,
  "
  SELECT
    player_id,
    player_name,
    nba_player_id,
    player_age,
    primary_position
  FROM players
  ORDER BY player_id
  "
)

all_players$normalized_name <-
  normalize_player_name(
    all_players$player_name
  )

perf_ids <- DBI::dbGetQuery(
  db,
  "
  SELECT DISTINCT player_id
  FROM player_season_stats
  WHERE season = ?
  ",
  params = list(
    performance_season
  )
)$player_id

all_players$has_target_stats <-
  all_players$player_id %in%
  perf_ids

# ------------------------------------------------------------
# Build layer-level audit
# ------------------------------------------------------------

layer_tables <- c(
  stats = "player_season_stats",
  advanced = "player_season_advanced",
  shooting = "player_season_shooting",
  playmaking = "player_season_playmaking",
  defense_rebounding =
    "player_season_defense_rebounding",
  roles = "player_season_roles",
  impact = "player_season_impact"
)

audit <- active_players

for (nm in names(layer_tables)) {
  audit[[nm]] <- FALSE
}

audit$projection <- FALSE
audit$any_historical_stats <- FALSE
audit$identity_candidate_found <- FALSE
audit$identity_candidate_player_id <- NA_integer_
audit$identity_candidate_name <- NA_character_

for (i in seq_len(nrow(audit))) {
  
  pid <- audit$player_id[[i]]
  
  for (nm in names(layer_tables)) {
    
    audit[[nm]][[i]] <-
      has_player_season_row(
        layer_tables[[nm]],
        pid,
        performance_season
      )
  }
  
  audit$projection[[i]] <-
    has_any_player_row(
      "player_projection_intelligence",
      pid
    )
  
  audit$any_historical_stats[[i]] <-
    has_any_player_row(
      "player_season_stats",
      pid
    )
  
  # ----------------------------------------------------------
  # Identity candidate search
  #
  # If current active ID is missing target-season stats, find
  # another canonical player with same normalized name that
  # DOES own target-season stats.
  # ----------------------------------------------------------
  
  if (!audit$stats[[i]]) {
    
    key <- audit$normalized_name[[i]]
    
    candidate <- all_players[
      all_players$normalized_name == key &
        all_players$player_id != pid &
        all_players$has_target_stats,
      ,
      drop = FALSE
    ]
    
    if (nrow(candidate)) {
      
      audit$identity_candidate_found[[i]] <-
        TRUE
      
      audit$identity_candidate_player_id[[i]] <-
        as.integer(
          candidate$player_id[[1]]
        )
      
      audit$identity_candidate_name[[i]] <-
        as.character(
          candidate$player_name[[1]]
        )
    }
  }
}

core_fields <- names(
  layer_tables
)

audit$core_complete <- apply(
  audit[
    ,
    core_fields,
    drop = FALSE
  ],
  1,
  all
)

audit$partial_chain <-
  audit$stats &
  !audit$core_complete

# ------------------------------------------------------------
# Classification
# ------------------------------------------------------------

audit$review_class <- "COMPLETE"

audit$review_class[
  audit$identity_candidate_found
] <- "LIKELY IDENTITY DUPLICATE"

audit$review_class[
  !audit$stats &
    !audit$identity_candidate_found &
    audit$any_historical_stats
] <- "NO TARGET-SEASON ROW — HAS OTHER NBA HISTORY"

audit$review_class[
  !audit$stats &
    !audit$identity_candidate_found &
    !audit$any_historical_stats
] <- "NO NBA PERFORMANCE DATA ON THIS ID"

audit$review_class[
  audit$partial_chain
] <- "PARTIAL ANALYTICS CHAIN"

audit$review_class[
  audit$core_complete &
    !audit$projection
] <- "CORE COMPLETE — PROJECTION MISSING"

audit$all_complete <-
  audit$core_complete &
  audit$projection

# ------------------------------------------------------------
# Summary tables
# ------------------------------------------------------------

identity_suspects <- audit[
  audit$review_class ==
    "LIKELY IDENTITY DUPLICATE",
  ,
  drop = FALSE
]

no_prior_data <- audit[
  audit$review_class %in% c(
    "NO NBA PERFORMANCE DATA ON THIS ID",
    "NO TARGET-SEASON ROW — HAS OTHER NBA HISTORY"
  ),
  ,
  drop = FALSE
]

partial_chain <- audit[
  audit$review_class %in% c(
    "PARTIAL ANALYTICS CHAIN",
    "CORE COMPLETE — PROJECTION MISSING"
  ),
  ,
  drop = FALSE
]

classification_counts <- as.data.frame(
  table(
    audit$review_class
  ),
  stringsAsFactors = FALSE
)

names(classification_counts) <- c(
  "review_class",
  "players"
)

classification_counts <- classification_counts[
  order(
    classification_counts$players,
    decreasing = TRUE
  ),
  ,
  drop = FALSE
]

cat("[1] ACTIVE PLAYERS CHECKED: ")
cat(nrow(audit), "\n")

cat("[2] CORE PERFORMANCE COMPLETE: ")
cat(
  sum(audit$core_complete),
  "/",
  nrow(audit),
  "\n",
  sep = ""
)

cat("[3] PROJECTION PRESENT: ")
cat(
  sum(audit$projection),
  "/",
  nrow(audit),
  "\n",
  sep = ""
)

cat("[4] FULLY COMPLETE: ")
cat(
  sum(audit$all_complete),
  "/",
  nrow(audit),
  "\n\n",
  sep = ""
)

cat("[5] CLASSIFICATION COUNTS\n")
print(
  classification_counts,
  row.names = FALSE
)

cat("\n")

cat("[6] LIKELY IDENTITY DUPLICATES\n")

if (nrow(identity_suspects)) {
  
  print(
    identity_suspects[
      ,
      c(
        "player_id",
        "player_name",
        "team_name",
        "identity_candidate_player_id",
        "identity_candidate_name",
        "review_class"
      ),
      drop = FALSE
    ],
    row.names = FALSE
  )
  
} else {
  
  cat("NONE\n")
}

cat("\n")

cat("[7] PARTIAL ANALYTICS CHAINS\n")

if (nrow(partial_chain)) {
  
  print(
    partial_chain[
      ,
      c(
        "player_id",
        "player_name",
        "team_name",
        "stats",
        "advanced",
        "shooting",
        "playmaking",
        "defense_rebounding",
        "roles",
        "impact",
        "projection",
        "review_class"
      ),
      drop = FALSE
    ],
    row.names = FALSE
  )
  
} else {
  
  cat("NONE\n")
}

# ------------------------------------------------------------
# Write QA files
# ------------------------------------------------------------

dir.create(
  "qa",
  showWarnings = FALSE
)

utils::write.csv(
  audit,
  file.path(
    "qa",
    "player_data_phase15b_full_audit.csv"
  ),
  row.names = FALSE
)

utils::write.csv(
  identity_suspects,
  file.path(
    "qa",
    "player_data_phase15b_identity_suspects.csv"
  ),
  row.names = FALSE
)

utils::write.csv(
  no_prior_data,
  file.path(
    "qa",
    "player_data_phase15b_no_prior_season_data.csv"
  ),
  row.names = FALSE
)

utils::write.csv(
  partial_chain,
  file.path(
    "qa",
    "player_data_phase15b_partial_chain.csv"
  ),
  row.names = FALSE
)

cat("\n")
cat("QA files written to qa/\n\n")

# ------------------------------------------------------------
# Final status
#
# A missing prior-season row is NOT automatically a failure.
# Freeze blockers are:
#   - likely duplicate identity
#   - partial analytics chain
# ------------------------------------------------------------

freeze_blockers <-
  nrow(identity_suspects) +
  nrow(partial_chain)

cat("============================================================\n")

if (freeze_blockers == 0L) {
  
  cat("PHASE 15B STATUS: PASS\n")
  cat(
    "No identity duplicates or partial analytics chains detected.\n"
  )
  cat(
    "Players without ",
    performance_season,
    " NBA data remain explicitly classified rather than fabricated.\n",
    sep = ""
  )
  
} else {
  
  cat("PHASE 15B STATUS: REVIEW REQUIRED\n")
  cat(
    nrow(identity_suspects),
    " likely identity duplicate(s).\n",
    sep = ""
  )
  cat(
    nrow(partial_chain),
    " partial analytics-chain issue(s).\n",
    sep = ""
  )
  cat(
    "Resolve these before NBA v1.0 freeze.\n"
  )
}

cat("============================================================\n")

safe_disconnect()