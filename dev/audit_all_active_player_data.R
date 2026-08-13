# ============================================================
# TBI — League-Wide Active Player Data Completeness Audit
#
# READ-ONLY
# Purpose:
#   Find active/depth-chart players missing data in the
#   current performance chain before NBA v1.0 freeze.
# ============================================================

library(DBI)
library(RSQLite)

db_path <- file.path("inst", "database", "tbi.sqlite")

db <- DBI::dbConnect(
  RSQLite::SQLite(),
  db_path
)


tables <- DBI::dbListTables(db)

required_tables <- c(
  "players",
  "depth_chart",
  "teams",
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
  tables
)

if (length(missing_tables)) {
  stop(
    paste(
      "Missing required tables:",
      paste(missing_tables, collapse = ", ")
    ),
    call. = FALSE
  )
}

# Use latest season represented across all 30 depth-chart teams.
season_info <- DBI::dbGetQuery(
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

complete_seasons <- season_info[
  season_info$team_count >= 30,
  ,
  drop = FALSE
]

if (!nrow(complete_seasons)) {
  stop("No complete 30-team depth-chart season found.", call. = FALSE)
}

depth_season <- as.character(
  complete_seasons$season[[1]]
)

# Most recent performance season in stats.
performance_season <- DBI::dbGetQuery(
  db,
  "
  SELECT MAX(season) AS season
  FROM player_season_stats
  "
)$season[[1]]

cat("\n============================================================\n")
cat("TBI LEAGUE-WIDE PLAYER DATA COMPLETENESS AUDIT\n")
cat("============================================================\n")
cat("Depth-chart season: ", depth_season, "\n", sep = "")
cat("Performance season: ", performance_season, "\n\n", sep = "")

# One row per active depth-chart player.
active_players <- DBI::dbGetQuery(
  db,
  "
  SELECT DISTINCT
    dc.player_id,
    p.player_name,
    p.nba_player_id,
    dc.team_id,
    t.team_name,
    t.abbreviation,
    dc.season AS depth_season
  FROM depth_chart dc
  INNER JOIN players p
    ON p.player_id = dc.player_id
  INNER JOIN teams t
    ON t.team_id = dc.team_id
  WHERE dc.season = ?
  ORDER BY t.team_name, p.player_name
  ",
  params = list(depth_season)
)

# Function to test presence of player rows in performance season.
has_row <- function(tbl, player_id) {
  DBI::dbGetQuery(
    db,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      tbl,
      " WHERE player_id = ? AND season = ?"
    ),
    params = list(
      player_id,
      performance_season
    )
  )$n[[1]] > 0L
}

audit <- active_players

audit$stats <- FALSE
audit$advanced <- FALSE
audit$shooting <- FALSE
audit$playmaking <- FALSE
audit$defense_rebounding <- FALSE
audit$roles <- FALSE
audit$impact <- FALSE
audit$projection <- FALSE

for (i in seq_len(nrow(audit))) {
  
  pid <- audit$player_id[[i]]
  
  audit$stats[[i]] <- has_row(
    "player_season_stats",
    pid
  )
  
  audit$advanced[[i]] <- has_row(
    "player_season_advanced",
    pid
  )
  
  audit$shooting[[i]] <- has_row(
    "player_season_shooting",
    pid
  )
  
  audit$playmaking[[i]] <- has_row(
    "player_season_playmaking",
    pid
  )
  
  audit$defense_rebounding[[i]] <- has_row(
    "player_season_defense_rebounding",
    pid
  )
  
  audit$roles[[i]] <- has_row(
    "player_season_roles",
    pid
  )
  
  audit$impact[[i]] <- has_row(
    "player_season_impact",
    pid
  )
  
  audit$projection[[i]] <- DBI::dbGetQuery(
    db,
    "
    SELECT COUNT(*) AS n
    FROM player_projection_intelligence
    WHERE player_id = ?
    ",
    params = list(pid)
  )$n[[1]] > 0L
}

audit$core_complete <- apply(
  audit[
    ,
    c(
      "stats",
      "advanced",
      "shooting",
      "playmaking",
      "defense_rebounding",
      "roles",
      "impact"
    )
  ],
  1,
  all
)

audit$all_complete <-
  audit$core_complete &
  audit$projection

cat("[1] ACTIVE PLAYERS CHECKED: ", nrow(audit), "\n", sep = "")
cat("[2] CORE PERFORMANCE COMPLETE: ", sum(audit$core_complete), "/", nrow(audit), "\n", sep = "")
cat("[3] PROJECTION PRESENT: ", sum(audit$projection), "/", nrow(audit), "\n", sep = "")
cat("[4] FULLY COMPLETE: ", sum(audit$all_complete), "/", nrow(audit), "\n\n", sep = "")

missing <- audit[
  !audit$all_complete,
  ,
  drop = FALSE
]

dir.create("qa", showWarnings = FALSE)

utils::write.csv(
  audit,
  file.path(
    "qa",
    "player_data_completeness_audit.csv"
  ),
  row.names = FALSE
)

utils::write.csv(
  missing,
  file.path(
    "qa",
    "player_data_missing_or_incomplete.csv"
  ),
  row.names = FALSE
)

if (nrow(missing)) {
  cat("PLAYERS REQUIRING REVIEW:\n\n")
  print(
    missing,
    row.names = FALSE
  )
  
  cat("\n============================================================\n")
  cat("PLAYER DATA COMPLETENESS STATUS: REVIEW REQUIRED\n")
  cat(nrow(missing), " active player(s) have at least one missing layer.\n")
  cat("See qa/player_data_missing_or_incomplete.csv\n")
  cat("============================================================\n")
} else {
  cat("============================================================\n")
  cat("PLAYER DATA COMPLETENESS STATUS: PASS\n")
  cat("Every active depth-chart player has the full data chain.\n")
  cat("============================================================\n")
}

# ------------------------------------------------------------
# Close database connection explicitly
# ------------------------------------------------------------
if (!is.null(db) && DBI::dbIsValid(db)) {
  DBI::dbDisconnect(db)
}