# ============================================================
# Thompson Basketball Intelligence
# SalarySwish Contract Import
#
# Purpose:
#   1. Download and clean SalarySwish active-contract data
#   2. Match players and teams to the TBI SQLite database
#   3. Populate contracts
#   4. Populate the verified 2026-27 contract-year records
#   5. Validate the completed import
# ============================================================


# ------------------------------------------------------------
# Packages
# ------------------------------------------------------------

required_packages <- c(
  "DBI",
  "RSQLite",
  "dplyr",
  "purrr",
  "readr",
  "stringr",
  "tibble",
  "httr2",
  "rvest",
  "janitor"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {
  stop(
    paste(
      "Install the following packages before continuing:",
      paste(missing_packages, collapse = ", ")
    )
  )
}


# ------------------------------------------------------------
# Project settings
# ------------------------------------------------------------

database_path <- file.path(
  "inst",
  "database",
  "tbi.sqlite"
)

backup_folder <- file.path(
  "inst",
  "database",
  "backups"
)

import_season <- "2026-27"

salaryswish_reference <- paste0(
  "https://www.salaryswish.com/ajax/browse/active?",
  "stats-season=2027"
)


# ------------------------------------------------------------
# Confirm required files exist
# ------------------------------------------------------------

required_files <- c(
  "R/salaryswish.R",
  "R/clean_salaryswish.R",
  "R/import_salaryswish.R",
  database_path
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0) {
  stop(
    paste(
      "The following required files were not found:",
      paste(missing_files, collapse = ", ")
    )
  )
}


# ------------------------------------------------------------
# Load existing SalarySwish functions
# ------------------------------------------------------------

source("R/salaryswish.R")
source("R/clean_salaryswish.R")
source("R/import_salaryswish.R")


# ------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------

normalize_name <- function(x) {
  
  x <- as.character(x)
  
  x <- iconv(
    x,
    from = "",
    to = "ASCII//TRANSLIT"
  )
  
  x <- tolower(x)
  
  x <- gsub(
    "[^a-z0-9]",
    "",
    x
  )
  
  x
}


clean_text <- function(x) {
  
  x <- as.character(x)
  
  x <- trimws(x)
  
  x[x %in% c(
    "",
    "-",
    "--",
    "NA",
    "N/A",
    "NULL"
  )] <- NA_character_
  
  x
}


safe_number <- function(x) {
  
  if (is.null(x)) {
    return(numeric())
  }
  
  suppressWarnings(
    readr::parse_number(
      as.character(x),
      na = c(
        "",
        "-",
        "--",
        "NA",
        "N/A"
      )
    )
  )
}


safe_date <- function(x) {
  
  x <- clean_text(x)
  
  parsed <- as.Date(
    rep(NA_character_, length(x))
  )
  
  formats <- c(
    "%b %d, %Y",
    "%B %d, %Y",
    "%Y-%m-%d",
    "%m/%d/%Y",
    "%m-%d-%Y"
  )
  
  for (date_format in formats) {
    
    still_missing <- is.na(parsed) & !is.na(x)
    
    if (!any(still_missing)) {
      break
    }
    
    parsed[still_missing] <- suppressWarnings(
      as.Date(
        x[still_missing],
        format = date_format
      )
    )
  }
  
  parsed
}


season_from_expiry_year <- function(expiry_year) {
  
  expiry_year <- suppressWarnings(
    as.integer(expiry_year)
  )
  
  result <- rep(
    NA_character_,
    length(expiry_year)
  )
  
  valid <- !is.na(expiry_year)
  
  result[valid] <- paste0(
    expiry_year[valid] - 1L,
    "-",
    substr(
      as.character(expiry_year[valid]),
      3,
      4
    )
  )
  
  result
}


build_contract_notes <- function(
    signing_date,
    contract_length,
    signing_status,
    expiry_status,
    is_extension
) {
  
  signing_text <- ifelse(
    is.na(signing_date),
    "Signing date unavailable",
    paste0(
      "Signed ",
      signing_date
    )
  )
  
  length_text <- ifelse(
    is.na(contract_length),
    "Length unavailable",
    paste0(
      "Length displayed: ",
      contract_length
    )
  )
  
  extension_text <- ifelse(
    isTRUE(is_extension) |
      (!is.na(is_extension) & is_extension == 1),
    "Extension",
    "Non-extension or unspecified"
  )
  
  status_text <- paste0(
    "Signing status: ",
    ifelse(
      is.na(signing_status),
      "Unavailable",
      signing_status
    ),
    "; expiry status: ",
    ifelse(
      is.na(expiry_status),
      "Unavailable",
      expiry_status
    )
  )
  
  paste(
    signing_text,
    length_text,
    extension_text,
    status_text,
    "Imported from SalarySwish active-contract table.",
    sep = " | "
  )
}


# ------------------------------------------------------------
# Create database backup
# ------------------------------------------------------------

if (!dir.exists(backup_folder)) {
  dir.create(
    backup_folder,
    recursive = TRUE
  )
}

backup_timestamp <- format(
  Sys.time(),
  "%Y%m%d_%H%M%S"
)

backup_path <- file.path(
  backup_folder,
  paste0(
    "tbi_before_salaryswish_",
    backup_timestamp,
    ".sqlite"
  )
)

backup_success <- file.copy(
  from = database_path,
  to = backup_path,
  overwrite = FALSE
)

if (!backup_success) {
  stop(
    "The database backup could not be created. Import cancelled."
  )
}

cat("\n")
cat("============================================\n")
cat("SALARYSWISH CONTRACT IMPORT\n")
cat("============================================\n")
cat("Database backup created:\n")
cat(backup_path, "\n\n")


# ------------------------------------------------------------
# Download and clean SalarySwish data
# ------------------------------------------------------------

cat("Downloading SalarySwish pages...\n")

salary_raw <- update_salaryswish_data()

if (!is.data.frame(salary_raw)) {
  stop(
    "SalarySwish import did not return a data frame."
  )
}

if (nrow(salary_raw) == 0) {
  stop(
    "SalarySwish import returned zero rows."
  )
}

cat(
  "Downloaded rows:",
  nrow(salary_raw),
  "\n"
)

cat(
  "Downloaded columns:",
  ncol(salary_raw),
  "\n\n"
)


# ------------------------------------------------------------
# Standardize SalarySwish columns
# ------------------------------------------------------------

salary_standardized <- tibble::tibble(
  source_row = seq_len(
    nrow(salary_raw)
  ),
  
  player_name = clean_text(
    first_available_column(
      salary_raw,
      c(
        "player_name",
        "player"
      )
    )
  ),
  
  team_abbreviation = toupper(
    clean_text(
      first_available_column(
        salary_raw,
        c(
          "team_abbreviation",
          "team"
        )
      )
    )
  ),
  
  contract_type = clean_text(
    first_available_column(
      salary_raw,
      c(
        "contract_type",
        "type"
      )
    )
  ),
  
  signing_method = clean_text(
    first_available_column(
      salary_raw,
      c(
        "signing_method"
      )
    )
  ),
  
  signing_date = safe_date(
    first_available_column(
      salary_raw,
      c(
        "signing_date"
      )
    )
  ),
  
  contract_length_display = clean_text(
    first_available_column(
      salary_raw,
      c(
        "contract_length",
        "length"
      )
    )
  ),
  
  signing_status = clean_text(
    first_available_column(
      salary_raw,
      c(
        "signing_status",
        "signing"
      )
    )
  ),
  
  expiry_status = clean_text(
    first_available_column(
      salary_raw,
      c(
        "expiry_status",
        "expiry"
      )
    )
  ),
  
  expiry_year = suppressWarnings(
    as.integer(
      safe_number(
        first_available_column(
          salary_raw,
          c(
            "expiry_year",
            "exp_year",
            "exp_yr"
          )
        )
      )
    )
  ),
  
  cap_hit = safe_number(
    first_available_column(
      salary_raw,
      c(
        "cap_hit",
        "caphit"
      )
    )
  ),
  
  base_salary = safe_number(
    first_available_column(
      salary_raw,
      c(
        "base_salary"
      )
    )
  ),
  
  average_annual_value = safe_number(
    first_available_column(
      salary_raw,
      c(
        "average_annual_value",
        "aav"
      )
    )
  ),
  
  likely_incentives = safe_number(
    first_available_column(
      salary_raw,
      c(
        "likely_incentives",
        "likely_incentive"
      )
    )
  ),
  
  unlikely_incentives = safe_number(
    first_available_column(
      salary_raw,
      c(
        "unlikely_incentives",
        "unlikely_incentive"
      )
    )
  ),
  
  extension_flag = as.integer(
    first_available_column(
      salary_raw,
      c(
        "extension_flag",
        "is_extension",
        "extension"
      ),
      default = FALSE
    ) %in% c(
      TRUE,
      1,
      "1",
      "TRUE",
      "True",
      "true",
      "Yes",
      "YES",
      "✔"
    )
  )
) |>
  dplyr::mutate(
    player_key = normalize_name(
      player_name
    ),
    
    contract_end_season = season_from_expiry_year(
      expiry_year
    ),
    
    notes = mapply(
      FUN = build_contract_notes,
      signing_date = signing_date,
      contract_length = contract_length_display,
      signing_status = signing_status,
      expiry_status = expiry_status,
      is_extension = extension_flag,
      USE.NAMES = FALSE
    )
  )


# ------------------------------------------------------------
# Basic source validation
# ------------------------------------------------------------

missing_player_names <- salary_standardized |>
  dplyr::filter(
    is.na(player_name) |
      player_name == ""
  )

if (nrow(missing_player_names) > 0) {
  stop(
    paste(
      nrow(missing_player_names),
      "SalarySwish rows are missing player names."
    )
  )
}


# SalarySwish may include unsigned/free-agent players whose
# team value is displayed as "-". These rows cannot be assigned
# to an NBA team payroll, so they are logged and excluded from
# the team-based contract import.

unassigned_contract_rows <- salary_standardized |>
  dplyr::filter(
    is.na(team_abbreviation) |
      team_abbreviation == ""
  ) |>
  dplyr::select(
    source_row,
    player_name,
    contract_type,
    signing_date,
    contract_length_display,
    expiry_year,
    cap_hit,
    base_salary
  ) |>
  dplyr::arrange(
    player_name
  )

cat(
  "Unsigned or unassigned rows excluded:",
  nrow(unassigned_contract_rows),
  "\n"
)

if (nrow(unassigned_contract_rows) > 0) {
  print(
    unassigned_contract_rows,
    n = Inf
  )
  cat("\n")
}


salary_standardized <- salary_standardized |>
  dplyr::filter(
    !is.na(team_abbreviation),
    team_abbreviation != ""
  )


if (nrow(salary_standardized) == 0) {
  stop(
    "No team-assigned SalarySwish contract rows remain after filtering."
  )
}

cat(
  "Team-assigned rows continuing to import:",
  nrow(salary_standardized),
  "\n\n"
)


# ------------------------------------------------------------
# Connect to SQLite
# ------------------------------------------------------------

con <- DBI::dbConnect(
  RSQLite::SQLite(),
  database_path
)

if (!DBI::dbIsValid(con)) {
  stop("The SQLite database connection could not be opened.")
}

DBI::dbExecute(
  con,
  "PRAGMA foreign_keys = ON;"
)

cat("SQLite database connection opened successfully.\n\n")


# ------------------------------------------------------------
# Read database lookup tables
# ------------------------------------------------------------

players_lookup <- DBI::dbGetQuery(
  con,
  "
  SELECT
    player_id,
    player_name
  FROM players
  "
) |>
  dplyr::mutate(
    player_key = normalize_name(
      player_name
    )
  )

teams_lookup <- DBI::dbGetQuery(
  con,
  "
  SELECT
    team_id,
    team_name,
    abbreviation
  FROM teams
  "
) |>
  dplyr::mutate(
    abbreviation = toupper(
      abbreviation
    )
  )


# ------------------------------------------------------------
# Validate unique player matching keys
# ------------------------------------------------------------

duplicate_database_players <- players_lookup |>
  dplyr::count(
    player_key,
    name = "database_player_count"
  ) |>
  dplyr::filter(
    database_player_count > 1
  )

if (nrow(duplicate_database_players) > 0) {
  
  duplicate_details <- players_lookup |>
    dplyr::semi_join(
      duplicate_database_players,
      by = "player_key"
    ) |>
    dplyr::arrange(
      player_key,
      player_name
    )
  
  print(
    duplicate_details,
    n = Inf
  )
  
  stop(
    "Duplicate normalized player names exist in the players table."
  )
}


# ------------------------------------------------------------
# Match players and teams
# ------------------------------------------------------------

salary_matched <- salary_standardized |>
  dplyr::left_join(
    players_lookup |>
      dplyr::select(
        player_id,
        database_player_name = player_name,
        player_key
      ),
    by = "player_key"
  ) |>
  dplyr::left_join(
    teams_lookup |>
      dplyr::select(
        team_id,
        team_name,
        abbreviation
      ),
    by = c(
      "team_abbreviation" = "abbreviation"
    )
  )


unmatched_players <- salary_matched |>
  dplyr::filter(
    is.na(player_id)
  ) |>
  dplyr::distinct(
    player_name,
    team_abbreviation
  ) |>
  dplyr::arrange(
    player_name
  )

unmatched_team_rows <- salary_matched |>
  dplyr::filter(
    is.na(team_id)
  ) |>
  dplyr::distinct(
    team_abbreviation
  ) |>
  dplyr::arrange(
    team_abbreviation
  )

cat(
  "Unique SalarySwish players:",
  dplyr::n_distinct(
    salary_matched$player_name
  ),
  "\n"
)

cat(
  "Matched contract rows:",
  sum(
    !is.na(salary_matched$player_id) &
      !is.na(salary_matched$team_id)
  ),
  "\n"
)

cat(
  "Unmatched players:",
  nrow(unmatched_players),
  "\n"
)

cat(
  "Unmatched teams:",
  nrow(unmatched_team_rows),
  "\n\n"
)


if (nrow(unmatched_players) > 0) {
  
  cat("UNMATCHED PLAYERS\n")
  print(
    unmatched_players,
    n = Inf
  )
  cat("\n")
}

if (nrow(unmatched_team_rows) > 0) {
  
  cat("UNMATCHED TEAMS\n")
  print(
    unmatched_team_rows,
    n = Inf
  )
  cat("\n")
}

if (nrow(unmatched_team_rows) > 0) {
  stop(
    paste(
      "Import cancelled before database modification.",
      "Resolve unmatched teams first."
    )
  )
}

if (nrow(unmatched_players) > 0) {
  warning(
    paste(
      nrow(unmatched_players),
      "unmatched player rows will be skipped during this import."
    )
  )
}

salary_matched <- salary_matched |>
  dplyr::filter(
    !is.na(player_id),
    !is.na(team_id)
  )

cat(
  "Matched rows continuing to database import:",
  nrow(salary_matched),
  "\n\n"
)


# ------------------------------------------------------------
# Show overlapping agreements
# ------------------------------------------------------------

overlapping_contracts <- salary_matched |>
  dplyr::count(
    player_id,
    player_name,
    team_id,
    team_abbreviation,
    name = "agreement_count"
  ) |>
  dplyr::filter(
    agreement_count > 1
  ) |>
  dplyr::arrange(
    dplyr::desc(agreement_count),
    player_name
  )

cat(
  "Players with multiple displayed agreements:",
  nrow(overlapping_contracts),
  "\n"
)

if (nrow(overlapping_contracts) > 0) {
  print(
    overlapping_contracts,
    n = Inf
  )
}

cat("\n")


# ------------------------------------------------------------
# Select one active 2026-27 payroll row per player/team
#
# Rule:
#   Keep the agreement with the newest signing date.
#   If signing dates tie or are missing, keep the last source row.
# ------------------------------------------------------------

active_contract_year_rows <- salary_matched |>
  dplyr::arrange(
    player_id,
    team_id,
    dplyr::desc(signing_date),
    dplyr::desc(source_row)
  ) |>
  dplyr::group_by(
    player_id,
    team_id
  ) |>
  dplyr::slice_head(
    n = 1
  ) |>
  dplyr::ungroup()


duplicate_payroll_check <- active_contract_year_rows |>
  dplyr::count(
    player_id,
    team_id,
    name = "payroll_row_count"
  ) |>
  dplyr::filter(
    payroll_row_count > 1
  )

if (nrow(duplicate_payroll_check) > 0) {
  stop(
    "Duplicate player/team payroll records remain after resolution."
  )
}


# ------------------------------------------------------------
# Create or update SalarySwish data source
# ------------------------------------------------------------

checked_at <- format(
  Sys.time(),
  "%Y-%m-%d %H:%M:%S"
)

existing_source <- DBI::dbGetQuery(
  con,
  "
  SELECT source_id
  FROM data_sources
  WHERE source_name = 'SalarySwish'
  LIMIT 1
  "
)

if (nrow(existing_source) == 0) {
  
  DBI::dbExecute(
    con,
    "
    INSERT INTO data_sources (
      source_name,
      source_reference,
      access_method,
      usage_notes,
      checked_at,
      verification_status
    )
    VALUES (?, ?, ?, ?, ?, ?)
    ",
    params = list(
      "SalarySwish",
      salaryswish_reference,
      "Automated web request",
      paste(
        "Active NBA contract data.",
        "Agreement data and displayed 2026-27 salary values."
      ),
      checked_at,
      "Imported"
    )
  )
  
  source_id <- DBI::dbGetQuery(
    con,
    "SELECT last_insert_rowid() AS source_id"
  )$source_id[[1]]
  
} else {
  
  source_id <- existing_source$source_id[[1]]
  
  DBI::dbExecute(
    con,
    "
    UPDATE data_sources
    SET
      source_reference = ?,
      access_method = ?,
      usage_notes = ?,
      checked_at = ?,
      verification_status = ?
    WHERE source_id = ?
    ",
    params = list(
      salaryswish_reference,
      "Automated web request",
      paste(
        "Active NBA contract data.",
        "Agreement data and displayed 2026-27 salary values."
      ),
      checked_at,
      "Imported",
      source_id
    )
  )
}


# ------------------------------------------------------------
# Import contracts and contract years
# ------------------------------------------------------------

DBI::dbBegin(con)

import_success <- FALSE

tryCatch(
  {
    
    # Delete previous SalarySwish contract-year rows first.
    DBI::dbExecute(
      con,
      "
      DELETE FROM contract_years
      WHERE source_id = ?
      ",
      params = list(
        source_id
      )
    )
    
    # Delete previous SalarySwish contracts.
    DBI::dbExecute(
      con,
      "
      DELETE FROM contracts
      WHERE source_id = ?
      ",
      params = list(
        source_id
      )
    )
    
    salary_matched$contract_id <- NA_integer_
    
    for (
      row_number in seq_len(
        nrow(salary_matched)
      )
    ) {
      
      current_row <- salary_matched[
        row_number,
        ,
        drop = FALSE
      ]
      
      DBI::dbExecute(
        con,
        "
        INSERT INTO contracts (
          player_id,
          contract_start_season,
          contract_end_season,
          contract_type,
          total_value,
          guaranteed_value,
          free_agent_year,
          bird_rights,
          trade_bonus_percent,
          notes,
          source_id,
          created_at,
          updated_at
        )
        VALUES (
          ?,
          ?,
          ?,
          ?,
          ?,
          ?,
          ?,
          ?,
          ?,
          ?,
          ?,
          CURRENT_TIMESTAMP,
          CURRENT_TIMESTAMP
        )
        ",
        params = list(
          current_row$player_id[[1]],
          NA_character_,
          current_row$contract_end_season[[1]],
          current_row$contract_type[[1]],
          NA_real_,
          NA_real_,
          current_row$expiry_year[[1]],
          current_row$signing_method[[1]],
          NA_real_,
          current_row$notes[[1]],
          source_id
        )
      )
      
      salary_matched$contract_id[
        row_number
      ] <- DBI::dbGetQuery(
        con,
        "SELECT last_insert_rowid() AS contract_id"
      )$contract_id[[1]]
    }
    
    
    # Attach the inserted contract IDs to the selected
    # active payroll rows.
    active_contract_year_rows <- active_contract_year_rows |>
      dplyr::select(
        -dplyr::any_of(
          "contract_id"
        )
      ) |>
      dplyr::left_join(
        salary_matched |>
          dplyr::select(
            source_row,
            contract_id
          ),
        by = "source_row"
      )
    
    
    if (
      any(
        is.na(
          active_contract_year_rows$contract_id
        )
      )
    ) {
      stop(
        "One or more payroll rows could not be linked to a contract."
      )
    }
    
    
    for (
      row_number in seq_len(
        nrow(active_contract_year_rows)
      )
    ) {
      
      current_row <- active_contract_year_rows[
        row_number,
        ,
        drop = FALSE
      ]
      
      DBI::dbExecute(
        con,
        "
        INSERT INTO contract_years (
          contract_id,
          player_id,
          team_id,
          season,
          base_salary,
          cap_hit,
          guaranteed_amount,
          option_type,
          likely_incentives,
          unlikely_incentives,
          dead_cap,
          source_id,
          verified_at
        )
        VALUES (
          ?,
          ?,
          ?,
          ?,
          ?,
          ?,
          ?,
          ?,
          ?,
          ?,
          ?,
          ?,
          ?
        )
        ",
        params = list(
          current_row$contract_id[[1]],
          current_row$player_id[[1]],
          current_row$team_id[[1]],
          import_season,
          
          dplyr::coalesce(
            current_row$base_salary[[1]],
            0
          ),
          
          dplyr::coalesce(
            current_row$cap_hit[[1]],
            0
          ),
          
          0,
          
          NA_character_,
          
          dplyr::coalesce(
            current_row$likely_incentives[[1]],
            0
          ),
          
          dplyr::coalesce(
            current_row$unlikely_incentives[[1]],
            0
          ),
          
          0,
          source_id,
          checked_at
        )
      )
    }
    
    
    DBI::dbCommit(con)
    
    import_success <- TRUE
  },
  
  error = function(error_condition) {
    
    if (DBI::dbIsValid(con)) {
      DBI::dbRollback(con)
    }
    
    stop(
      paste(
        "SalarySwish import failed.",
        "Database changes were rolled back.",
        conditionMessage(error_condition)
      )
    )
  }
)


# ------------------------------------------------------------
# Post-import validation
# ------------------------------------------------------------

if (!import_success) {
  stop(
    "SalarySwish import did not complete."
  )
}


validation_summary <- DBI::dbGetQuery(
  con,
  "
  SELECT
    (
      SELECT COUNT(*)
      FROM contracts
      WHERE source_id = ?
    ) AS contract_rows,

    (
      SELECT COUNT(*)
      FROM contract_years
      WHERE source_id = ?
        AND season = ?
    ) AS contract_year_rows,

    (
      SELECT COUNT(DISTINCT player_id)
      FROM contract_years
      WHERE source_id = ?
        AND season = ?
    ) AS unique_payroll_players,

    (
      SELECT COUNT(DISTINCT team_id)
      FROM contract_years
      WHERE source_id = ?
        AND season = ?
    ) AS teams_with_contracts
  ",
  params = list(
    source_id,
    source_id,
    import_season,
    source_id,
    import_season,
    source_id,
    import_season
  )
)


duplicate_database_payroll <- DBI::dbGetQuery(
  con,
  "
  SELECT
    player_id,
    team_id,
    season,
    COUNT(*) AS row_count
  FROM contract_years
  WHERE source_id = ?
    AND season = ?
  GROUP BY
    player_id,
    team_id,
    season
  HAVING COUNT(*) > 1
  ",
  params = list(
    source_id,
    import_season
  )
)


team_payroll_summary <- DBI::dbGetQuery(
  con,
  "
  SELECT
    t.abbreviation,
    t.team_name,
    COUNT(cy.contract_year_id) AS contract_rows,
    SUM(cy.base_salary) AS total_base_salary,
    SUM(cy.cap_hit) AS total_cap_hit,
    SUM(cy.likely_incentives) AS likely_incentives,
    SUM(cy.unlikely_incentives) AS unlikely_incentives
  FROM teams t
  LEFT JOIN contract_years cy
    ON t.team_id = cy.team_id
   AND cy.source_id = ?
   AND cy.season = ?
  GROUP BY
    t.team_id,
    t.abbreviation,
    t.team_name
  ORDER BY
    total_cap_hit DESC
  ",
  params = list(
    source_id,
    import_season
  )
)


cat("\n")
cat("============================================\n")
cat("IMPORT COMPLETE\n")
cat("============================================\n")

cat(
  "Contract agreement rows:",
  validation_summary$contract_rows,
  "\n"
)

cat(
  "2026-27 payroll rows:",
  validation_summary$contract_year_rows,
  "\n"
)

cat(
  "Unique payroll players:",
  validation_summary$unique_payroll_players,
  "\n"
)

cat(
  "Teams represented:",
  validation_summary$teams_with_contracts,
  "\n"
)

cat(
  "Duplicate player/team payroll rows:",
  nrow(duplicate_database_payroll),
  "\n"
)

cat(
  "Database backup:",
  backup_path,
  "\n"
)

cat("============================================\n\n")


if (nrow(duplicate_database_payroll) > 0) {
  
  print(
    duplicate_database_payroll,
    row.names = FALSE
  )
  
  warning(
    "Duplicate payroll rows were detected after import."
  )
}


print(
  team_payroll_summary,
  row.names = FALSE
)


# ------------------------------------------------------------
# Clean shutdown
# ------------------------------------------------------------

if (exists("con") && DBI::dbIsValid(con)) {
  DBI::dbDisconnect(con)
}

cat("\nSalarySwish database import finished successfully.\n")