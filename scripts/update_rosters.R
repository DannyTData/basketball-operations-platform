# ============================================================
# Thompson Basketball Intelligence
# SalarySwish Player Database Update
# ============================================================

library(DBI)
library(RSQLite)
library(dplyr)
library(readr)
library(purrr)
library(httr2)
library(rvest)
library(janitor)
library(stringr)

season <- "2026-27"
db_path <- "inst/database/tbi.sqlite"

# ------------------------------------------------------------
# Convert height values such as 6'7", 6-7, or 6 ft 7
# into total inches.
# ------------------------------------------------------------

parse_height_inches <- function(height_value) {
  
  if (is.na(height_value) || trimws(height_value) == "") {
    return(NA_real_)
  }
  
  numbers <- stringr::str_extract_all(
    as.character(height_value),
    "\\d+"
  )[[1]]
  
  numbers <- as.numeric(numbers)
  
  if (length(numbers) >= 2) {
    return((numbers[1] * 12) + numbers[2])
  }
  
  if (
    length(numbers) == 1 &&
    numbers[1] >= 60 &&
    numbers[1] <= 100
  ) {
    return(numbers[1])
  }
  
  NA_real_
}

update_tbi_players <- function() {
  
  if (!file.exists(db_path)) {
    stop("Database not found: ", db_path)
  }
  
  # Load existing SalarySwish functions
  source("R/salaryswish.R")
  source("R/clean_salaryswish.R")
  source("R/import_salaryswish.R")
  
  # Create a database backup before making changes
  backup_directory <- "inst/database/backups"
  
  if (!dir.exists(backup_directory)) {
    dir.create(
      backup_directory,
      recursive = TRUE
    )
  }
  
  backup_path <- file.path(
    backup_directory,
    paste0(
      "tbi_before_player_update_",
      format(Sys.time(), "%Y%m%d_%H%M%S"),
      ".sqlite"
    )
  )
  
  backup_created <- file.copy(
    from = db_path,
    to = backup_path,
    overwrite = FALSE
  )
  
  if (!backup_created) {
    stop("Database backup could not be created.")
  }
  
  cat("Database backup created:\n")
  cat(backup_path, "\n\n")
  
  con <- DBI::dbConnect(
    RSQLite::SQLite(),
    dbname = db_path
  )
  
  on.exit(
    DBI::dbDisconnect(con),
    add = TRUE
  )
  
  cat("Connected to TBI database.\n")
  cat("Downloading SalarySwish data...\n")
  
  salaryswish_data <- update_salaryswish_data()
  
  cat(
    "SalarySwish rows downloaded:",
    nrow(salaryswish_data),
    "\n"
  )
  
  # One row per player
  players_to_load <- salaryswish_data |>
    dplyr::filter(
      !is.na(player),
      trimws(player) != ""
    ) |>
    dplyr::arrange(
      player,
      dplyr::desc(cap_hit)
    ) |>
    dplyr::distinct(
      player,
      .keep_all = TRUE
    ) |>
    dplyr::transmute(
      player_name = trimws(player),
      birth_date = NA_character_,
      height_inches = purrr::map_dbl(
        height,
        parse_height_inches
      ),
      weight_lbs = readr::parse_number(
        as.character(weight)
      ),
      primary_position = dplyr::na_if(
        trimws(as.character(pos)),
        ""
      ),
      nba_player_id = NA_character_,
      is_active = 1L,
      created_at = format(
        Sys.time(),
        "%Y-%m-%d %H:%M:%S"
      ),
      updated_at = format(
        Sys.time(),
        "%Y-%m-%d %H:%M:%S"
      )
    )
  
  existing_player_count <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) AS player_count FROM players"
  )$player_count[[1]]
  
  if (existing_player_count > 0) {
    stop(
      paste0(
        "The players table already contains ",
        existing_player_count,
        " rows. No data was written to prevent duplicates."
      )
    )
  }
  
  DBI::dbWithTransaction(
    con,
    {
      DBI::dbAppendTable(
        con,
        "players",
        players_to_load
      )
    }
  )
  
  validation <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS total_players,
      SUM(CASE WHEN height_inches IS NOT NULL THEN 1 ELSE 0 END)
        AS players_with_height,
      SUM(CASE WHEN weight_lbs IS NOT NULL THEN 1 ELSE 0 END)
        AS players_with_weight,
      SUM(CASE WHEN primary_position IS NOT NULL THEN 1 ELSE 0 END)
        AS players_with_position
    FROM players
    "
  )
  
  cat("\nPlayer update complete.\n")
  cat("Season:", season, "\n\n")
  
  print(validation)
  
  cat("\nSample players:\n")
  
  print(
    DBI::dbGetQuery(
      con,
      "
      SELECT
        player_id,
        player_name,
        height_inches,
        weight_lbs,
        primary_position,
        is_active
      FROM players
      ORDER BY player_name
      LIMIT 10
      "
    )
  )
  
  invisible(players_to_load)
}

update_tbi_players()