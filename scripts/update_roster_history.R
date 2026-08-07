# ============================================================
# Thompson Basketball Intelligence
# Update Active Roster History
# ============================================================

library(DBI)
library(RSQLite)
library(dplyr)

update_tbi_roster_history <- function(
    db_path = "inst/database/tbi.sqlite",
    season = "2026-27"
) {
  
  if (!file.exists(db_path)) {
    stop("Database not found: ", db_path)
  }
  
  con <- DBI::dbConnect(
    RSQLite::SQLite(),
    dbname = db_path
  )
  
  on.exit(
    {
      if (DBI::dbIsValid(con)) {
        DBI::dbDisconnect(con)
      }
    },
    add = TRUE
  )
  
  cat("Connected to TBI database.\n")
  
  # ----------------------------------------------------------
  # Load SalarySwish data
  # ----------------------------------------------------------
  
  source("R/salaryswish.R")
  source("R/clean_salaryswish.R")
  source("R/import_salaryswish.R")
  
  salaryswish_data <- update_salaryswish_data()
  
  roster_data <- salaryswish_data |>
    dplyr::filter(
      !is.na(team),
      team != "-",
      !is.na(player),
      trimws(player) != ""
    ) |>
    dplyr::distinct(player, .keep_all = TRUE)
  
  cat("Active roster players:", nrow(roster_data), "\n")
  
  # ----------------------------------------------------------
  # Read database lookup tables
  # ----------------------------------------------------------
  
  players <- DBI::dbReadTable(con, "players") |>
    dplyr::select(
      player_id,
      player_name
    )
  
  teams <- DBI::dbReadTable(con, "teams") |>
    dplyr::select(
      team_id,
      abbreviation
    )
  
  # ----------------------------------------------------------
  # Match players and teams
  # ----------------------------------------------------------
  
  roster_to_load <- roster_data |>
    dplyr::left_join(
      players,
      by = c("player" = "player_name")
    ) |>
    dplyr::left_join(
      teams,
      by = c("team" = "abbreviation")
    ) |>
    dplyr::transmute(
      player_id = player_id,
      team_id = team_id,
      season = season,
      roster_status = "Active",
      start_date = as.character(Sys.Date()),
      end_date = NA_character_,
      two_way_flag = 0L,
      jersey_number = NA_character_,
      source_id = NA_integer_
    )
  
  # ----------------------------------------------------------
  # Validation
  # ----------------------------------------------------------
  
  missing_players <- roster_data |>
    dplyr::left_join(
      players,
      by = c("player" = "player_name")
    ) |>
    dplyr::filter(is.na(player_id)) |>
    dplyr::distinct(player)
  
  missing_teams <- roster_data |>
    dplyr::left_join(
      teams,
      by = c("team" = "abbreviation")
    ) |>
    dplyr::filter(is.na(team_id)) |>
    dplyr::distinct(team)
  
  cat("\nValidation\n")
  cat("----------------------------\n")
  cat("Missing player IDs:", nrow(missing_players), "\n")
  cat("Missing team IDs:", nrow(missing_teams), "\n")
  
  if (nrow(missing_players) > 0) {
    cat("\nUnmatched players:\n")
    print(missing_players, n = Inf)
  }
  
  if (nrow(missing_teams) > 0) {
    cat("\nUnmatched teams:\n")
    print(missing_teams, n = Inf)
  }
  
  if (
    nrow(missing_players) > 0 ||
    nrow(missing_teams) > 0
  ) {
    stop("Validation failed. No roster data was inserted.")
  }
  
  duplicate_assignments <- roster_to_load |>
    dplyr::count(player_id, season) |>
    dplyr::filter(n > 1)
  
  if (nrow(duplicate_assignments) > 0) {
    stop(
      "Duplicate player-season assignments were found. ",
      "No roster data was inserted."
    )
  }
  
  # ----------------------------------------------------------
  # Prevent accidental duplicate season loads
  # ----------------------------------------------------------
  
  existing_count <- DBI::dbGetQuery(
    con,
    "
    SELECT COUNT(*) AS n
    FROM roster_history
    WHERE season = ?
    ",
    params = list(season)
  )$n[[1]]
  
  if (existing_count > 0) {
    stop(
      "Roster history already contains ",
      existing_count,
      " rows for ",
      season,
      ". No data was inserted."
    )
  }
  
  # ----------------------------------------------------------
  # Insert inside a transaction
  # ----------------------------------------------------------
  
  DBI::dbWithTransaction(
    con,
    {
      DBI::dbAppendTable(
        con,
        "roster_history",
        roster_to_load
      )
    }
  )
  
  validation <- DBI::dbGetQuery(
    con,
    "
    SELECT
      rh.season,
      COUNT(*) AS roster_rows,
      COUNT(DISTINCT rh.player_id) AS unique_players,
      COUNT(DISTINCT rh.team_id) AS teams_represented
    FROM roster_history rh
    WHERE rh.season = ?
    GROUP BY rh.season
    ",
    params = list(season)
  )
  
  cat(
    "\nInserted",
    nrow(roster_to_load),
    "roster assignments.\n\n"
  )
  
  print(validation)
  
  cat("\nSample roster assignments:\n")
  
  print(
    DBI::dbGetQuery(
      con,
      "
      SELECT
        p.player_name,
        t.abbreviation AS team,
        p.primary_position,
        rh.season,
        rh.roster_status
      FROM roster_history rh
      INNER JOIN players p
        ON rh.player_id = p.player_id
      INNER JOIN teams t
        ON rh.team_id = t.team_id
      WHERE rh.season = ?
      ORDER BY t.abbreviation, p.player_name
      LIMIT 15
      ",
      params = list(season)
    )
  )
  
  invisible(roster_to_load)
}

update_tbi_roster_history()