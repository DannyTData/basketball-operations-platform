testthat::test_that("official writes are authorized only from the baseline", {
  baseline <- tbi_authorize_official_write(
    list(active = FALSE),
    operation = "Save depth-chart assignment"
  )
  testthat::expect_true(baseline$allowed)
  testthat::expect_identical(baseline$status, "PASS")

  shared <- tbi_authorize_official_write(
    list(
      active = TRUE,
      scenario_type = "trade",
      scenario_scope = "SHARED_SUPPORTED"
    ),
    operation = "Save depth-chart assignment"
  )
  testthat::expect_false(shared$allowed)
  testthat::expect_identical(shared$status, "BLOCKED")
  testthat::expect_identical(shared$code, "ACTIVE_SCENARIO_WRITE_BLOCKED")
  testthat::expect_match(shared$message, "scenario preview is active", fixed = TRUE)

  trade_local <- tbi_authorize_official_write(
    list(
      active = TRUE,
      scenario_type = "v2_multiteam_trade",
      scenario_scope = "TRADE_LOCAL"
    ),
    operation = "Apply Starting Five"
  )
  testthat::expect_false(trade_local$allowed)
  testthat::expect_identical(trade_local$status, "BLOCKED")
  testthat::expect_identical(trade_local$code, "UNSUPPORTED_ACTIVE_SCENARIO")
  testthat::expect_identical(trade_local$scenario_scope, "TRADE_LOCAL")
  testthat::expect_match(trade_local$message, "REVIEW / BLOCKED", fixed = TRUE)
  testthat::expect_match(trade_local$message, "authoritative", fixed = TRUE)
  testthat::expect_match(trade_local$message, "TRADE-LOCAL", fixed = TRUE)

  unknown <- tbi_authorize_official_write(
    list(active = TRUE, scenario_type = "future_scenario"),
    operation = "Reset depth-chart assignment"
  )
  testthat::expect_false(unknown$allowed)
  testthat::expect_identical(unknown$code, "UNSUPPORTED_ACTIVE_SCENARIO")

  unavailable <- tbi_authorize_official_write(
    list(active = TRUE, state_unavailable = TRUE),
    operation = "Apply Starting Five"
  )
  testthat::expect_false(unavailable$allowed)
  testthat::expect_identical(unavailable$status, "BLOCKED")
  testthat::expect_identical(unavailable$code, "SCENARIO_STATE_UNAVAILABLE")
  testthat::expect_match(unavailable$message, "state is unavailable", fixed = TRUE)

  malformed <- tbi_authorize_official_write(
    "malformed snapshot",
    operation = "Apply Starting Five"
  )
  testthat::expect_false(malformed$allowed)
  testthat::expect_identical(malformed$code, "SCENARIO_STATE_UNAVAILABLE")
})

testthat::test_that("Depth Chart writes only the isolated baseline and blocks scenario persistence", {
  expected_sha256 <-
    "1801B06D076EAF2A3353661BD95A0214C86069BDF2627D973D7FD89B6B0FF4F2"
  authoritative_path <- normalizePath(
    testthat::test_path("..", "..", "inst", "database", "tbi.sqlite"),
    winslash = "/",
    mustWork = TRUE
  )
  authoritative_hash_before <- toupper(
    unname(tools::sha256sum(authoritative_path)[[1]])
  )
  if (!identical(authoritative_hash_before, expected_sha256)) {
    stop(
      "Authoritative database does not match the required known-good SHA-256.",
      call. = FALSE
    )
  }
  authoritative_sidecars <- paste0(
    authoritative_path,
    c("-wal", "-shm", "-journal")
  )
  authoritative_sidecars_before <- file.exists(authoritative_sidecars)

  read_overrides <- function(path) {
    con <- DBI::dbConnect(
      RSQLite::SQLite(),
      dbname = normalizePath(path, winslash = "/", mustWork = TRUE),
      flags = RSQLite::SQLITE_RO
    )
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    DBI::dbGetQuery(
      con,
      "SELECT * FROM depth_chart_overrides ORDER BY team_id, season, player_id"
    )
  }
  authoritative_rows_before <- read_overrides(authoritative_path)

  isolated_directory <- tempfile(
    pattern = "tbi-official-write-guard-",
    tmpdir = tempdir()
  )
  if (!dir.create(isolated_directory)) {
    stop("Could not create the isolated database directory.", call. = FALSE)
  }
  isolated_directory <- normalizePath(
    isolated_directory,
    winslash = "/",
    mustWork = TRUE
  )
  temp_root <- normalizePath(tempdir(), winslash = "/", mustWork = TRUE)
  if (!startsWith(tolower(isolated_directory), paste0(tolower(temp_root), "/"))) {
    stop("Refusing to use an isolated database outside the R temp directory.", call. = FALSE)
  }
  isolated_database_path <- file.path(isolated_directory, "tbi.sqlite")
  if (!file.copy(authoritative_path, isolated_database_path, overwrite = FALSE)) {
    stop("Could not create the isolated database copy.", call. = FALSE)
  }
  isolated_database_path <- normalizePath(
    isolated_database_path,
    winslash = "/",
    mustWork = TRUE
  )
  isolated_hash <- toupper(
    unname(tools::sha256sum(isolated_database_path)[[1]])
  )
  if (!identical(isolated_hash, expected_sha256)) {
    stop("The disposable database copy failed SHA-256 verification.", call. = FALSE)
  }

  previous_demo_mode <- Sys.getenv("TBI_DEMO_MODE", unset = NA_character_)
  previous_db_override <- Sys.getenv("TBI_DB_OVERRIDE", unset = NA_character_)
  on.exit({
    if (is.na(previous_demo_mode)) {
      Sys.unsetenv("TBI_DEMO_MODE")
    } else {
      Sys.setenv(TBI_DEMO_MODE = previous_demo_mode)
    }
    if (is.na(previous_db_override)) {
      Sys.unsetenv("TBI_DB_OVERRIDE")
    } else {
      Sys.setenv(TBI_DB_OVERRIDE = previous_db_override)
    }

    authoritative_hash_after <- toupper(
      unname(tools::sha256sum(authoritative_path)[[1]])
    )
    authoritative_rows_after <- read_overrides(authoritative_path)
    authoritative_sidecars_after <- file.exists(authoritative_sidecars)
    unlink(isolated_directory, recursive = TRUE, force = TRUE)

    if (!identical(authoritative_hash_after, expected_sha256) ||
        !identical(authoritative_rows_after, authoritative_rows_before) ||
        !identical(authoritative_sidecars_after, authoritative_sidecars_before)) {
      stop(
        "Authoritative database invariant failed during isolated guard testing.",
        call. = FALSE
      )
    }
  }, add = TRUE)
  Sys.setenv(
    TBI_DEMO_MODE = "true",
    TBI_DB_OVERRIDE = isolated_database_path
  )

  database_path <- resolve_tbi_db_path()
  same_path <- function(left, right) {
    identical(tolower(normalizePath(left, winslash = "/", mustWork = TRUE)),
              tolower(normalizePath(right, winslash = "/", mustWork = TRUE)))
  }
  if (!same_path(database_path, isolated_database_path) ||
      same_path(database_path, authoritative_path)) {
    stop("Unsafe test database resolution: refusing to run mutation controls.", call. = FALSE)
  }

  hash_before <- toupper(unname(tools::sha256sum(database_path)[[1]]))
  con <- connect_db(database_path, read_only = TRUE)
  opened_database <- DBI::dbGetQuery(con, "PRAGMA database_list")
  opened_main <- normalizePath(
    opened_database$file[opened_database$name == "main"][[1]],
    winslash = "/",
    mustWork = TRUE
  )
  disconnect_db(con)
  con <- NULL
  if (!same_path(opened_main, isolated_database_path) ||
      same_path(opened_main, authoritative_path)) {
    stop("SQLite did not open the disposable database copy.", call. = FALSE)
  }
  rows_before <- read_overrides(database_path)

  team <- shiny::reactiveVal("Boston Celtics")
  season <- shiny::reactiveVal("2026-27")
  scenario_value <- shiny::reactiveVal(list(active = FALSE))
  scenario_error <- shiny::reactiveVal(FALSE)
  transaction_state <- list(
    snapshot = function() {
      if (isTRUE(scenario_error())) stop("scenario snapshot unavailable")
      scenario_value()
    }
  )
  isolated_state <- new.env(parent = emptyenv())

  shiny::testServer(
    mod_depth_chart_server,
    args = list(
      selected_team = team,
      selected_season = season,
      transaction_state = transaction_state
    ),
    {
      session$flushReact()

      baseline_row <- selected_player()
      testthat::expect_false(is.null(baseline_row))
      baseline_player_id <- as.integer(baseline_row$player_id[[1]])
      baseline_team_id <- as.integer(baseline_row$team_id[[1]])
      baseline_position <- as.character(baseline_row$position[[1]])

      session$setInputs(selected_player = baseline_player_id)
      session$flushReact()
      session$setInputs(
        assignment_position = baseline_position,
        assignment_depth = 1,
        assignment_starter = TRUE,
        allow_position_override = TRUE,
        override_reason = "Isolated official-write guard test",
        save_assignment = 1
      )
      session$flushReact()
      baseline_save_message <- save_message_value()
      testthat::expect_true(baseline_save_message$ok)

      rows_after_save <- read_overrides(database_path)
      saved_row <- rows_after_save[
        rows_after_save$player_id == baseline_player_id &
          rows_after_save$team_id == baseline_team_id &
          rows_after_save$season == "2026-27",
        ,
        drop = FALSE
      ]
      testthat::expect_equal(nrow(saved_row), 1L)
      testthat::expect_equal(as.integer(saved_row$is_starter[[1]]), 1L)

      session$setInputs(reset_assignment = 1)
      session$flushReact()
      baseline_reset_message <- save_message_value()
      testthat::expect_true(baseline_reset_message$ok)
      rows_after_reset <- read_overrides(database_path)
      testthat::expect_false(any(
        rows_after_reset$player_id == baseline_player_id &
          rows_after_reset$team_id == baseline_team_id &
          rows_after_reset$season == "2026-27"
      ))

      session$setInputs(apply_scenario = 1)
      session$flushReact()
      baseline_apply_message <- save_message_value()
      testthat::expect_true(baseline_apply_message$ok)
      isolated_state$baseline_rows <- read_overrides(database_path)
      isolated_state$baseline_hash <- toupper(
        unname(tools::sha256sum(database_path)[[1]])
      )
      testthat::expect_false(identical(isolated_state$baseline_rows, rows_before))
      testthat::expect_false(identical(isolated_state$baseline_hash, hash_before))

      scenario_value(list(
        active = TRUE,
        scenario_type = "trade",
        scenario_scope = "SHARED_SUPPORTED",
        team = "Boston Celtics",
        partner_team = "Charlotte Hornets",
        season = "2026-27",
        outgoing_players = data.frame(),
        incoming_players = data.frame(),
        outgoing_salary = 0,
        incoming_salary = 0,
        salary_delta = 0
      ))
      session$flushReact()

      session$setInputs(apply_scenario = 2)
      session$flushReact()
      shared_apply_message <- save_message_value()
      testthat::expect_true(shared_apply_message$ok)
      testthat::expect_match(
        shared_apply_message$message,
        "No official depth-chart records were changed.",
        fixed = TRUE
      )
      testthat::expect_identical(
        read_overrides(database_path),
        isolated_state$baseline_rows
      )

      save_message_value(list(ok = TRUE, message = "sentinel"))
      session$setInputs(save_assignment = 1)
      session$flushReact()
      shared_save_message <- save_message_value()
      testthat::expect_false(shared_save_message$ok)
      testthat::expect_identical(
        shared_save_message$code,
        "ACTIVE_SCENARIO_WRITE_BLOCKED"
      )
      testthat::expect_identical(
        read_overrides(database_path),
        isolated_state$baseline_rows
      )

      save_message_value(list(ok = TRUE, message = "sentinel"))
      session$setInputs(reset_assignment = 1)
      session$flushReact()
      shared_reset_message <- save_message_value()
      testthat::expect_false(shared_reset_message$ok)
      testthat::expect_identical(
        shared_reset_message$code,
        "ACTIVE_SCENARIO_WRITE_BLOCKED"
      )
      testthat::expect_identical(
        read_overrides(database_path),
        isolated_state$baseline_rows
      )

      scenario_value(list(
        active = TRUE,
        scenario_type = "v2_multiteam_trade",
        scenario_scope = "TRADE_LOCAL"
      ))
      session$flushReact()

      for (action in c("apply_scenario", "save_assignment", "reset_assignment")) {
        save_message_value(list(ok = TRUE, message = "sentinel"))
        current_value <- shiny::isolate(input[[action]]) %||% 0
        do.call(
          session$setInputs,
          stats::setNames(list(current_value + 1), action)
        )
        session$flushReact()

        blocked_message <- save_message_value()
        testthat::expect_false(blocked_message$ok)
        testthat::expect_identical(blocked_message$status, "BLOCKED")
        testthat::expect_identical(
          blocked_message$code,
          "UNSUPPORTED_ACTIVE_SCENARIO"
        )
        testthat::expect_identical(
          blocked_message$scenario_scope,
          "TRADE_LOCAL"
        )
        testthat::expect_match(
          blocked_message$message,
          "REVIEW / BLOCKED",
          fixed = TRUE
        )
        testthat::expect_match(
          blocked_message$message,
          "TRADE-LOCAL",
          fixed = TRUE
        )
        testthat::expect_identical(
          read_overrides(database_path),
          isolated_state$baseline_rows
        )
        testthat::expect_identical(
          toupper(unname(tools::sha256sum(database_path)[[1]])),
          isolated_state$baseline_hash
        )
      }

      scenario_error(TRUE)
      session$flushReact()

      for (action in c("apply_scenario", "save_assignment", "reset_assignment")) {
        save_message_value(list(ok = TRUE, message = "sentinel"))
        current_value <- shiny::isolate(input[[action]]) %||% 0
        do.call(
          session$setInputs,
          stats::setNames(list(current_value + 1), action)
        )
        session$flushReact()

        blocked_message <- save_message_value()
        testthat::expect_false(blocked_message$ok)
        testthat::expect_identical(blocked_message$status, "BLOCKED")
        testthat::expect_identical(
          blocked_message$code,
          "SCENARIO_STATE_UNAVAILABLE"
        )
        testthat::expect_identical(
          read_overrides(database_path),
          isolated_state$baseline_rows
        )
        testthat::expect_identical(
          toupper(unname(tools::sha256sum(database_path)[[1]])),
          isolated_state$baseline_hash
        )
      }

      scenario_error(FALSE)
      scenario_value("malformed snapshot")
      session$flushReact()

      for (action in c("apply_scenario", "save_assignment", "reset_assignment")) {
        save_message_value(list(ok = TRUE, message = "sentinel"))
        current_value <- shiny::isolate(input[[action]]) %||% 0
        do.call(
          session$setInputs,
          stats::setNames(list(current_value + 1), action)
        )
        session$flushReact()

        blocked_message <- save_message_value()
        testthat::expect_false(blocked_message$ok)
        testthat::expect_identical(
          blocked_message$code,
          "SCENARIO_STATE_UNAVAILABLE"
        )
        testthat::expect_identical(
          read_overrides(database_path),
          isolated_state$baseline_rows
        )
        testthat::expect_identical(
          toupper(unname(tools::sha256sum(database_path)[[1]])),
          isolated_state$baseline_hash
        )
      }
    }
  )

  testthat::expect_identical(
    read_overrides(database_path),
    isolated_state$baseline_rows
  )
  testthat::expect_identical(
    toupper(unname(tools::sha256sum(database_path)[[1]])),
    isolated_state$baseline_hash
  )
  testthat::expect_identical(
    toupper(unname(tools::sha256sum(authoritative_path)[[1]])),
    expected_sha256
  )
  testthat::expect_identical(
    read_overrides(authoritative_path),
    authoritative_rows_before
  )
  testthat::expect_identical(
    file.exists(authoritative_sidecars),
    authoritative_sidecars_before
  )
})

testthat::test_that("Depth preview requires canonical shared-supported scope", {
  malformed <- list(
    active = TRUE,
    scenario_type = "trade",
    scenario_scope = "TRADE_LOCAL",
    team = "Boston Celtics",
    partner_team = "Charlotte Hornets",
    season = "2026-27"
  )

  testthat::expect_null(depth_chart_active_trade_scenario(
    malformed,
    selected_team = "Boston Celtics",
    selected_season = "2026-27"
  ))
})
