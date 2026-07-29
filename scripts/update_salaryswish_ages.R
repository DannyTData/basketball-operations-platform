library(httr2)
library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(janitor)
library(DBI)
library(RSQLite)

# ------------------------------------------------------------
# Settings
# ------------------------------------------------------------

database_path <- "inst/database/tbi.sqlite"
total_pages <- 13L

# ------------------------------------------------------------
# Download one SalarySwish page
# ------------------------------------------------------------

get_salaryswish_page <- function(pg = 1L) {
  
  url <- paste0(
    "https://www.salaryswish.com/ajax/browse/active?",
    "contract=null&",
    "signing-method=null&",
    "stats-season=2027&",
    "display=weight,height,draft,signing-status,expiry-year,",
    "likely-incentive,unlikely-incentive,caphit-percent,aav,length,",
    "base-salary,type,signing-method,signing-date,extension&",
    "hide=stats&",
    "pg=", pg
  )
  
  raw <- httr2::request(url) |>
    httr2::req_perform() |>
    httr2::resp_body_json(
      simplifyVector = FALSE
    )
  
  page_data <- raw$data$results |>
    rvest::read_html() |>
    rvest::html_element("table") |>
    rvest::html_table(fill = TRUE) |>
    janitor::clean_names() |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        as.character
      )
    )
  
  message(
    "Page ",
    pg,
    " loaded: ",
    nrow(page_data),
    " rows"
  )
  
  page_data
}

# ------------------------------------------------------------
# Download all pages
# ------------------------------------------------------------

salary_raw <- purrr::map_dfr(
  seq_len(total_pages),
  function(page_number) {
    
    page_data <- get_salaryswish_page(page_number)
    
    Sys.sleep(1)
    
    page_data
  }
)

message(
  "Total rows downloaded: ",
  nrow(salary_raw)
)

# ------------------------------------------------------------
# Inspect available columns
# ------------------------------------------------------------

print(names(salary_raw))

# ------------------------------------------------------------
# Confirm required columns exist
# ------------------------------------------------------------

required_columns <- c(
  "player",
  "age"
)

missing_columns <- setdiff(
  required_columns,
  names(salary_raw)
)

if (length(missing_columns) > 0) {
  stop(
    "Missing required SalarySwish columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

# ------------------------------------------------------------
# Clean player names and ages
# ------------------------------------------------------------

salary_clean <- salary_raw |>
  dplyr::transmute(
    player_name = stringr::str_remove(
      player,
      "^\\s*\\d+\\.\\s*"
    ),
    player_name = stringr::str_squish(
      player_name
    ),
    age = suppressWarnings(
      as.integer(age)
    )
  ) |>
  dplyr::filter(
    !is.na(player_name),
    player_name != "",
    !is.na(age)
  ) |>
  dplyr::distinct(
    player_name,
    .keep_all = TRUE
  )

message(
  "Unique players with ages: ",
  nrow(salary_clean)
)

# ------------------------------------------------------------
# Connect to SQLite database
# ------------------------------------------------------------

con <- DBI::dbConnect(
  RSQLite::SQLite(),
  database_path
)

# ------------------------------------------------------------
# Update SQLite players table
# ------------------------------------------------------------

tryCatch(
  {
    
    DBI::dbBegin(con)
    
    DBI::dbExecute(
      con,
      "UPDATE players SET player_age = NULL"
    )
    
    update_statement <- DBI::dbSendStatement(
      con,
      "
      UPDATE players
      SET player_age = ?
      WHERE player_name = ?
      "
    )
    
    tryCatch(
      {
        
        for (i in seq_len(nrow(salary_clean))) {
          
          DBI::dbBind(
            update_statement,
            list(
              salary_clean$age[[i]],
              salary_clean$player_name[[i]]
            )
          )
        }
        
      },
      finally = {
        
        if (DBI::dbIsValid(con)) {
          DBI::dbClearResult(update_statement)
        }
        
      }
    )
    
    DBI::dbCommit(con)
    
    # --------------------------------------------------------
    # Verify result
    # --------------------------------------------------------
    
    verification <- DBI::dbGetQuery(
      con,
      "
      SELECT
          COUNT(*) AS total_players,
          SUM(player_age IS NOT NULL) AS players_with_age
      FROM players
      "
    )
    
    print(verification)
    
    unmatched_players <- DBI::dbGetQuery(
      con,
      "
      SELECT
          player_name
      FROM players
      WHERE player_age IS NULL
      ORDER BY player_name
      "
    )
    
    message(
      "Players still missing ages: ",
      nrow(unmatched_players)
    )
    
    if (nrow(unmatched_players) > 0) {
      print(unmatched_players)
    }
    
    message(
      "SalarySwish age update completed successfully."
    )
    
  },
  error = function(e) {
    
    if (DBI::dbIsValid(con)) {
      try(
        DBI::dbRollback(con),
        silent = TRUE
      )
    }
    
    stop(
      "SalarySwish age update failed: ",
      conditionMessage(e),
      call. = FALSE
    )
    
  },
  finally = {
    
    if (DBI::dbIsValid(con)) {
      DBI::dbDisconnect(con)
    }
    
  }
)