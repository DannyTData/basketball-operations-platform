# ============================================================
# Thompson Basketball Intelligence
# Database Validation
# ============================================================

#' Required tables for the TBI application
#' @noRd
tbi_required_tables <- function() {
  c(
    "teams", "players", "data_sources", "contracts", "contract_years",
    "roster_history", "cap_thresholds", "depth_chart",
    "depth_chart_overrides", "player_positions", "player_season_stats",
    "transactions"
  )
}

#' Recommended database indexes
#' @noRd
tbi_required_indexes <- function() {
  c(
    "idx_contract_years_team_season",
    "idx_contract_years_player_season",
    "idx_contract_years_contract",
    "idx_roster_history_team_season",
    "idx_roster_history_player_season",
    "idx_depth_chart_team_season_position",
    "idx_depth_chart_player_season",
    "idx_depth_chart_overrides_team_season",
    "idx_player_positions_player_rank",
    "idx_player_season_stats_team_season",
    "idx_transactions_season_date",
    "idx_transactions_player"
  )
}

#' Create recommended indexes safely
#'
#' @param con Open DBI connection.
#' @return Invisibly returns TRUE.
#' @noRd
create_tbi_indexes <- function(con) {
  statements <- c(
    "CREATE INDEX IF NOT EXISTS idx_contract_years_team_season ON contract_years(team_id, season)",
    "CREATE INDEX IF NOT EXISTS idx_contract_years_player_season ON contract_years(player_id, season)",
    "CREATE INDEX IF NOT EXISTS idx_contract_years_contract ON contract_years(contract_id)",
    "CREATE INDEX IF NOT EXISTS idx_roster_history_team_season ON roster_history(team_id, season)",
    "CREATE INDEX IF NOT EXISTS idx_roster_history_player_season ON roster_history(player_id, season)",
    "CREATE INDEX IF NOT EXISTS idx_depth_chart_team_season_position ON depth_chart(team_id, season, position, depth_order)",
    "CREATE INDEX IF NOT EXISTS idx_depth_chart_player_season ON depth_chart(player_id, season)",
    "CREATE INDEX IF NOT EXISTS idx_depth_chart_overrides_team_season ON depth_chart_overrides(team_id, season)",
    "CREATE INDEX IF NOT EXISTS idx_player_positions_player_rank ON player_positions(player_id, eligibility_rank)",
    "CREATE INDEX IF NOT EXISTS idx_player_season_stats_team_season ON player_season_stats(team_id, season)",
    "CREATE INDEX IF NOT EXISTS idx_transactions_season_date ON transactions(season, transaction_date)",
    "CREATE INDEX IF NOT EXISTS idx_transactions_player ON transactions(player_id)"
  )

  DBI::dbWithTransaction(con, {
    for (statement in statements) DBI::dbExecute(con, statement)
  })

  invisible(TRUE)
}

#' Validate the TBI database
#'
#' @param db_path Optional explicit database path.
#' @param create_indexes Whether to create missing recommended indexes.
#' @return A list with status, checks, table counts, and diagnostics.
#' @noRd
validate_tbi_database <- function(db_path = NULL, create_indexes = FALSE) {
  con <- connect_db(db_path = db_path, read_only = !isTRUE(create_indexes))
  on.exit(disconnect_db(con), add = TRUE)

  if (isTRUE(create_indexes)) create_tbi_indexes(con)

  tables <- DBI::dbListTables(con)
  required_tables <- tbi_required_tables()
  missing_tables <- setdiff(required_tables, tables)

  integrity <- DBI::dbGetQuery(con, "PRAGMA integrity_check;")[[1]][[1]]
  foreign_keys <- DBI::dbGetQuery(con, "PRAGMA foreign_key_check;")

  index_rows <- DBI::dbGetQuery(
    con,
    "SELECT name FROM sqlite_master WHERE type = 'index' AND name NOT LIKE 'sqlite_autoindex%'"
  )
  existing_indexes <- index_rows$name
  missing_indexes <- setdiff(tbi_required_indexes(), existing_indexes)

  count_tables <- intersect(required_tables, tables)
  row_counts <- data.frame(
    table = count_tables,
    rows = vapply(
      count_tables,
      function(table) DBI::dbGetQuery(
        con,
        paste0("SELECT COUNT(*) AS n FROM ", DBI::dbQuoteIdentifier(con, table))
      )$n[[1]],
      numeric(1)
    ),
    stringsAsFactors = FALSE
  )

  duplicate_checks <- list()
  if (all(c("teams") %in% tables)) {
    duplicate_checks$teams <- DBI::dbGetQuery(
      con,
      "SELECT team_name, COUNT(*) AS n FROM teams GROUP BY team_name HAVING COUNT(*) > 1"
    )
  }
  if (all(c("contract_years") %in% tables)) {
    duplicate_checks$contract_years <- DBI::dbGetQuery(
      con,
      paste(
        "SELECT contract_id, season, COUNT(*) AS n",
        "FROM contract_years GROUP BY contract_id, season HAVING COUNT(*) > 1"
      )
    )
  }
  if (all(c("depth_chart") %in% tables)) {
    duplicate_checks$depth_chart <- DBI::dbGetQuery(
      con,
      paste(
        "SELECT player_id, team_id, season, COUNT(*) AS n",
        "FROM depth_chart GROUP BY player_id, team_id, season HAVING COUNT(*) > 1"
      )
    )
  }

  duplicate_rows <- sum(vapply(duplicate_checks, nrow, integer(1)))
  empty_core_tables <- row_counts$table[
    row_counts$rows == 0 & row_counts$table %in% c("teams", "players", "contracts", "contract_years", "roster_history")
  ]

  checks <- data.frame(
    check = c(
      "SQLite integrity",
      "Foreign-key integrity",
      "Required tables",
      "Core table population",
      "Duplicate business keys",
      "Recommended indexes"
    ),
    passed = c(
      identical(tolower(integrity), "ok"),
      nrow(foreign_keys) == 0,
      length(missing_tables) == 0,
      length(empty_core_tables) == 0,
      duplicate_rows == 0,
      length(missing_indexes) == 0
    ),
    details = c(
      integrity,
      if (nrow(foreign_keys)) paste(nrow(foreign_keys), "violation(s)") else "No violations",
      if (length(missing_tables)) paste(missing_tables, collapse = ", ") else "All required tables present",
      if (length(empty_core_tables)) paste(empty_core_tables, collapse = ", ") else "Core tables populated",
      if (duplicate_rows) paste(duplicate_rows, "duplicate row group(s)") else "No duplicate business keys",
      if (length(missing_indexes)) paste(missing_indexes, collapse = ", ") else "All recommended indexes present"
    ),
    stringsAsFactors = FALSE
  )

  list(
    passed = all(checks$passed),
    checks = checks,
    row_counts = row_counts,
    missing_tables = missing_tables,
    missing_indexes = missing_indexes,
    foreign_key_violations = foreign_keys,
    duplicates = duplicate_checks,
    database_path = resolve_tbi_db_path(db_path)
  )
}

#' Print a readable database validation report
#' @noRd
print_tbi_database_report <- function(report) {
  cat("\nTBI DATABASE VERIFICATION\n")
  cat(strrep("=", 52), "\n", sep = "")
  cat("Database:", report$database_path, "\n\n")
  print(report$checks, row.names = FALSE)
  cat("\nTABLE ROW COUNTS\n")
  print(report$row_counts, row.names = FALSE)
  cat("\nOverall status:", if (isTRUE(report$passed)) "PASS" else "ATTENTION REQUIRED", "\n")
  invisible(report)
}
