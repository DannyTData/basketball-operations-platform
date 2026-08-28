feedback_test_database <- function() {
  path <- tempfile(pattern = "tbi-feedback-source-", fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(con, "CREATE TABLE feedback_probe (value INTEGER NOT NULL)")
  DBI::dbExecute(con, "INSERT INTO feedback_probe (value) VALUES (1)")
  DBI::dbDisconnect(con)
  path
}

feedback_probe_value <- function(path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbGetQuery(con, "SELECT value FROM feedback_probe")$value[[1]]
}

testthat::test_that("feedback launcher requires an explicit future expiration and strict source", {
  source <- feedback_test_database()
  on.exit(unlink(source), add = TRUE)
  missing <- paste0(source, "-missing")

  testthat::expect_error(
    tbi_feedback_config("", source_db = source),
    "expiration must be explicit"
  )
  testthat::expect_error(
    tbi_feedback_config("2020-01-01 00:00:00", source_db = source),
    "must be in the future"
  )
  testthat::expect_error(
    tbi_feedback_config("2099-01-01 00:00:00", source_db = missing),
    "does not exist"
  )

  config <- tbi_feedback_config("2099-01-01 00:00:00", source_db = source)
  testthat::expect_s3_class(config, "tbi_feedback_config")
  testthat::expect_false(config$expiration_disabled)
  testthat::expect_false(config$tpe_test_mode)
  testthat::expect_identical(config$status_label, "FEEDBACK / NON-AUTHORITATIVE")
  testthat::expect_identical(config$source_db, normalizePath(source, winslash = "/"))

  utc_config <- tbi_feedback_config(
    "2099-01-01 00:00:00",
    source_db = source,
    timezone = "UTC"
  )
  testthat::expect_identical(utc_config$timezone, "UTC")

  invalid_timezones <- list(
    NULL,
    NA_character_,
    "",
    "Not/A_Timezone",
    c("UTC", "America/New_York")
  )
  for (timezone in invalid_timezones) {
    testthat::expect_error(
      tbi_feedback_config(
        "2099-01-01 00:00:00",
        source_db = source,
        timezone = timezone
      ),
      "valid feedback timezone",
      fixed = TRUE
    )
  }

  app <- run_tbi_feedback("2099-01-01 00:00:00", source_db = source)
  testthat::expect_s3_class(app, "shiny.appobj")
  launcher <- paste(
    readLines(testthat::test_path("..", "..", "R", "run_tbi_feedback.R"), warn = FALSE),
    collapse = "\n"
  )
  launcher_prose <- gsub(
    "\\s+",
    " ",
    gsub("(?m)^#' ?", "", launcher, perl = TRUE)
  )
  testthat::expect_false(grepl("pkgload::load_all", launcher, fixed = TRUE))
  testthat::expect_match(
    launcher,
    "does not authenticate viewers",
    fixed = TRUE
  )
  testthat::expect_match(
    launcher_prose,
    "platform or reverse-proxy authentication",
    fixed = TRUE
  )
})

testthat::test_that("feedback database routing is unique per session and fail closed", {
  source <- feedback_test_database()
  on.exit(unlink(source), add = TRUE)
  source_hash <- unname(tools::md5sum(source))
  session_a <- shiny::MockShinySession$new()
  session_b <- shiny::MockShinySession$new()
  root_a <- root_b <- NULL
  on.exit({
    if (!is.null(root_a) && dir.exists(root_a)) tbi_cleanup_feedback_database(root_a)
    if (!is.null(root_b) && dir.exists(root_b)) tbi_cleanup_feedback_database(root_b)
  }, add = TRUE)

  path_a <- tbi_prepare_feedback_session(session_a, source)
  path_b <- tbi_prepare_feedback_session(session_b, source)
  root_a <- session_a$userData$tbi_feedback_db_root
  root_b <- session_b$userData$tbi_feedback_db_root
  testthat::expect_false(identical(path_a, path_b))
  testthat::expect_false(identical(path_a, normalizePath(source, winslash = "/")))
  testthat::expect_true(tbi_feedback_owned_root(root_a))
  testthat::expect_true(tbi_feedback_owned_root(root_b))

  resolved_a <- shiny::withReactiveDomain(session_a, {
    resolve_tbi_db_path(file.path("inst", "database", "tbi.sqlite"))
  })
  resolved_b <- shiny::withReactiveDomain(session_b, resolve_tbi_db_path())
  testthat::expect_identical(resolved_a, path_a)
  testthat::expect_identical(resolved_b, path_b)

  shiny::withReactiveDomain(session_a, {
    con <- connect_db()
    DBI::dbExecute(con, "UPDATE feedback_probe SET value = 9")
    disconnect_db(con)
  })
  testthat::expect_identical(feedback_probe_value(path_a), 9L)
  testthat::expect_identical(feedback_probe_value(path_b), 1L)
  testthat::expect_identical(feedback_probe_value(source), 1L)
  testthat::expect_identical(unname(tools::md5sum(source)), source_hash)
  testthat::expect_false(shiny::withReactiveDomain(
    session_a,
    v2_trade_test_mode_enabled("true")
  ))

  broken <- shiny::MockShinySession$new()
  broken$userData$tbi_feedback_mode <- TRUE
  broken$userData$tbi_feedback_db_path <- paste0(source, "-gone")
  testthat::expect_error(
    shiny::withReactiveDomain(broken, resolve_tbi_db_path()),
    "no valid disposable database"
  )
  withr::local_envvar(TBI_FEEDBACK_MODE = "true")
  testthat::expect_error(
    resolve_tbi_db_path(source),
    "active isolated Shiny session"
  )

  testthat::expect_true(tbi_cleanup_feedback_database(root_a))
  testthat::expect_true(tbi_cleanup_feedback_database(root_b))
  testthat::expect_false(dir.exists(root_a))
  testthat::expect_false(dir.exists(root_b))
})

testthat::test_that("feedback cleanup removes sidecars and reports incomplete deletion", {
  make_root <- function() {
    root <- tempfile(pattern = "tbi-feedback-session-", tmpdir = tempdir())
    testthat::expect_true(dir.create(root, recursive = FALSE))
    database <- file.path(root, "tbi.sqlite")
    paths <- c(
      database,
      paste0(database, "-wal"),
      paste0(database, "-shm"),
      paste0(database, "-journal")
    )
    testthat::expect_true(all(file.create(paths)))
    list(root = root, paths = paths)
  }

  removable <- make_root()
  testthat::expect_true(tbi_cleanup_feedback_database(removable$root))
  testthat::expect_false(dir.exists(removable$root))
  testthat::expect_false(any(file.exists(removable$paths)))

  retained <- make_root()
  on.exit({
    if (dir.exists(retained$root)) tbi_cleanup_feedback_database(retained$root)
  }, add = TRUE)
  attempted <- character()
  sidecars <- retained$paths[-1L]
  failing_unlink <- function(path, recursive = FALSE, force = FALSE) {
    attempted <<- c(attempted, as.character(path))
    if (!isTRUE(recursive)) {
      base::unlink(path, recursive = FALSE, force = force)
    }
    1L
  }

  testthat::expect_warning(
    removed <- tbi_cleanup_feedback_database(
      retained$root,
      unlink_fn = failing_unlink
    ),
    "Failed to completely remove disposable feedback database",
    fixed = TRUE
  )
  testthat::expect_false(removed)
  testthat::expect_true(dir.exists(retained$root))
  testthat::expect_false(any(file.exists(sidecars)))
  normalize_test_paths <- function(paths) {
    tolower(normalizePath(paths, winslash = "/", mustWork = FALSE))
  }
  testthat::expect_true(all(
    normalize_test_paths(sidecars) %in% normalize_test_paths(attempted)
  ))
  testthat::expect_true(
    normalizePath(retained$root, winslash = "/", mustWork = TRUE) %in% attempted
  )
})

testthat::test_that("mounted feedback server expires, signals, closes, and cleans its disposable DB", {
  authoritative <- normalizePath(
    testthat::test_path("..", "..", "inst", "database", "tbi.sqlite"),
    winslash = "/",
    mustWork = TRUE
  )
  authoritative_hash <- unname(tools::md5sum(authoritative))
  source <- tempfile(pattern = "tbi-feedback-lifecycle-source-", fileext = ".sqlite")
  testthat::expect_true(file.copy(authoritative, source, overwrite = FALSE))
  on.exit(unlink(source, force = TRUE), add = TRUE)
  source_hash <- unname(tools::md5sum(source))

  start <- as.POSIXct("2035-01-15 12:00:00", tz = "UTC")
  clock <- shiny::reactiveVal(start)
  config <- tbi_feedback_config(
    start + 60,
    source_db = source,
    timezone = "UTC",
    now = start
  )
  root <- database <- NULL
  messages <- list()
  mock_session <- shiny::MockShinySession$new()
  mock_session$sendCustomMessage <- function(type, message) {
    messages[[length(messages) + 1L]] <<- list(
      type = type,
      message = message
    )
    invisible(NULL)
  }

  testthat::local_mocked_bindings(
    app_server = function(input, output, session) {
      session$userData$tbi_test_app_server_mounted <- TRUE
    },
    .package = "basketballops"
  )
  withr::local_envvar(TBI_FEEDBACK_MODE = NA_character_)

  feedback_server <- function(input, output, session) {
    tbi_web_demo_server(
      input,
      output,
      session,
      feedback_config = config,
      time_now = function() clock()
    )
  }

  shiny::testServer(
    feedback_server,
    session = mock_session,
    {
      session$flushReact()
      root <<- session$userData$tbi_feedback_db_root
      database <<- session$userData$tbi_feedback_db_path

      testthat::expect_true(isTRUE(session$userData$tbi_test_app_server_mounted))
      testthat::expect_true(tbi_feedback_owned_root(root))
      testthat::expect_true(file.exists(database))
      testthat::expect_identical(
        shiny::withReactiveDomain(session, resolve_tbi_db_path()),
        database
      )
      testthat::expect_false(isTRUE(session$userData$tbi_demo_expired))
      testthat::expect_identical(unname(tools::md5sum(source)), source_hash)

      clock(start + 61)
      session$flushReact()

      expiration_messages <- Filter(
        function(message) identical(message$type, "tbi-demo-expired"),
        messages
      )
      testthat::expect_true(session$isClosed())
      testthat::expect_true(isTRUE(session$userData$tbi_demo_expired))
      testthat::expect_true(
        isTRUE(session$userData$tbi_demo_expiration_signal_sent)
      )
      testthat::expect_length(expiration_messages, 1L)
      testthat::expect_true(
        isTRUE(expiration_messages[[1L]]$message$expired)
      )
      testthat::expect_true(
        isTRUE(session$userData$tbi_feedback_cleanup_succeeded)
      )
      testthat::expect_false(dir.exists(root))
      testthat::expect_false(file.exists(database))
    }
  )

  testthat::expect_false(dir.exists(root))
  testthat::expect_false(file.exists(database))
  testthat::expect_identical(unname(tools::md5sum(source)), source_hash)
  testthat::expect_identical(unname(tools::md5sum(authoritative)), authoritative_hash)

  ended <- shiny::MockShinySession$new()
  ended$userData$tbi_feedback_mode <- TRUE
  ended$userData$tbi_feedback_db_path <- database
  testthat::expect_error(
    shiny::withReactiveDomain(ended, resolve_tbi_db_path()),
    "no valid disposable database",
    fixed = TRUE
  )
})

testthat::test_that("nominal readers cannot create or migrate a database", {
  path <- tempfile(pattern = "tbi-read-only-", fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(con, "CREATE TABLE teams (team_id INTEGER, team_name TEXT, abbreviation TEXT)")
  DBI::dbDisconnect(con)
  on.exit(unlink(path), add = TRUE)
  before_hash <- unname(tools::md5sum(path))
  before_tables <- {
    con <- DBI::dbConnect(RSQLite::SQLite(), path, flags = RSQLite::SQLITE_RO)
    value <- DBI::dbListTables(con)
    DBI::dbDisconnect(con)
    value
  }

  read_only <- connect_db(path, read_only = TRUE)
  testthat::expect_error(
    DBI::dbExecute(read_only, "CREATE TABLE forbidden_write (id INTEGER)"),
    "readonly|read-only|attempt to write"
  )
  disconnect_db(read_only)
  testthat::expect_error(
    get_depth_chart_records("Boston Celtics", "2026-27", db_path = path)
  )
  testthat::expect_error(
    get_player_eligible_positions(1L, "PG", db_path = path)
  )
  testthat::expect_error(
    get_draft_assets("Boston Celtics", db_path = path),
    "requires existing tables"
  )

  con <- DBI::dbConnect(RSQLite::SQLite(), path, flags = RSQLite::SQLITE_RO)
  after_tables <- DBI::dbListTables(con)
  DBI::dbDisconnect(con)
  testthat::expect_identical(after_tables, before_tables)
  testthat::expect_identical(unname(tools::md5sum(path)), before_hash)
})
