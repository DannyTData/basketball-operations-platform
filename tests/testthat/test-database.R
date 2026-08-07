test_that("the packaged TBI database resolves", {
  path <- resolve_tbi_db_path()
  expect_true(file.exists(path))
  expect_match(basename(path), "tbi\\.sqlite")
})

test_that("database connection enables foreign keys", {
  con <- connect_db()
  on.exit(disconnect_db(con), add = TRUE)

  foreign_keys <- DBI::dbGetQuery(con, "PRAGMA foreign_keys;")[[1]][[1]]
  expect_equal(as.integer(foreign_keys), 1L)
})

test_that("required database tables are present", {
  con <- connect_db(read_only = TRUE)
  on.exit(disconnect_db(con), add = TRUE)

  expect_setequal(
    intersect(tbi_required_tables(), DBI::dbListTables(con)),
    tbi_required_tables()
  )
})

test_that("database integrity and foreign keys pass", {
  report <- validate_tbi_database(create_indexes = FALSE)

  expect_true(report$checks$passed[report$checks$check == "SQLite integrity"])
  expect_true(report$checks$passed[report$checks$check == "Foreign-key integrity"])
  expect_length(report$missing_tables, 0L)
})

test_that("core tables contain data", {
  report <- validate_tbi_database(create_indexes = FALSE)
  core <- c("teams", "players", "contracts", "contract_years", "roster_history")
  counts <- report$row_counts[report$row_counts$table %in% core, ]

  expect_setequal(counts$table, core)
  expect_true(all(counts$rows > 0))
})

test_that("recommended indexes exist", {
  report <- validate_tbi_database(create_indexes = FALSE)
  expect_length(report$missing_indexes, 0L)
})

test_that("team lookup returns an NBA organization", {
  team <- get_team("BOS")
  expect_equal(nrow(team), 1L)
  expect_equal(team$team_name[[1]], "Boston Celtics")
})
