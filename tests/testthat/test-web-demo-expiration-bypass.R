with_demo_environment <- function(values, code) {
  keys <- names(values)
  old <- Sys.getenv(keys, unset = NA_character_)
  on.exit({
    for (key in keys) {
      if (is.na(old[[key]])) Sys.unsetenv(key) else do.call(Sys.setenv, setNames(list(old[[key]]), key))
    }
  }, add = TRUE)
  for (key in keys) {
    value <- values[[key]]
    if (is.na(value)) Sys.unsetenv(key) else do.call(Sys.setenv, setNames(list(value), key))
  }
  force(code)
}

testthat::test_that("demo expiration bypass permits missing and expired timestamps", {
  with_demo_environment(list(
    TBI_DEMO_MODE = "true",
    TBI_DISABLE_DEMO_EXPIRATION = "true",
    TBI_DEMO_EXPIRES_AT = NA_character_
  ), {
    testthat::expect_true(tbi_demo_expiration_disabled())
    testthat::expect_false(tbi_demo_is_expired(as.POSIXct("2026-08-20 12:00:00", tz = "America/New_York")))
  })

  with_demo_environment(list(
    TBI_DEMO_MODE = "true",
    TBI_DISABLE_DEMO_EXPIRATION = "yes",
    TBI_DEMO_EXPIRES_AT = "2020-01-01 00:00:00"
  ), {
    testthat::expect_false(tbi_demo_is_expired(as.POSIXct("2026-08-20 12:00:00", tz = "America/New_York")))
  })
})

testthat::test_that("missing or false bypass preserves existing expiration security", {
  now <- as.POSIXct("2026-08-20 12:00:00", tz = "America/New_York")
  with_demo_environment(list(
    TBI_DEMO_MODE = "true",
    TBI_DISABLE_DEMO_EXPIRATION = NA_character_,
    TBI_DEMO_EXPIRES_AT = NA_character_
  ), {
    testthat::expect_false(tbi_demo_expiration_disabled())
    testthat::expect_true(tbi_demo_is_expired(now))
  })
  with_demo_environment(list(
    TBI_DEMO_MODE = "true",
    TBI_DISABLE_DEMO_EXPIRATION = "false",
    TBI_DEMO_EXPIRES_AT = "2020-01-01 00:00:00"
  ), testthat::expect_true(tbi_demo_is_expired(now)))
  with_demo_environment(list(
    TBI_DEMO_MODE = "true",
    TBI_DISABLE_DEMO_EXPIRATION = "false",
    TBI_DEMO_EXPIRES_AT = "2030-01-01 00:00:00"
  ), testthat::expect_false(tbi_demo_is_expired(now)))
})

testthat::test_that("local app entry defaults security flags off and requires explicit opt-in", {
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/", mustWork = TRUE)
  app <- paste(readLines(file.path(root, "app.R"), warn = FALSE), collapse = "\n")
  testthat::expect_match(app, 'Sys.getenv("TBI_DISABLE_DEMO_EXPIRATION", "")', fixed = TRUE)
  testthat::expect_match(app, 'TBI_DISABLE_DEMO_EXPIRATION = "false"', fixed = TRUE)
  testthat::expect_match(app, 'TBI_ENABLE_TPE_TEST_MODE = "false"', fixed = TRUE)
  testthat::expect_false(grepl('TBI_DISABLE_DEMO_EXPIRATION = "true"', app, fixed = TRUE))
})

testthat::test_that("expired-session termination is enforced server side", {
  fake <- new.env(parent = emptyenv())
  fake$sent <- list()
  fake$closed <- 0L
  fake$sendCustomMessage <- function(type, message) {
    fake$sent <- list(type = type, message = message)
  }
  fake$close <- function() {
    fake$closed <- fake$closed + 1L
  }
  testthat::expect_true(tbi_demo_terminate_expired_session(fake))
  testthat::expect_identical(fake$sent$type, "tbi-demo-expired")
  testthat::expect_true(fake$sent$message$expired)
  testthat::expect_identical(fake$closed, 1L)

  failing_send <- new.env(parent = emptyenv())
  failing_send$closed <- 0L
  failing_send$sendCustomMessage <- function(...) stop("cosmetic send failed")
  failing_send$close <- function() {
    failing_send$closed <- failing_send$closed + 1L
  }
  testthat::expect_true(tbi_demo_terminate_expired_session(failing_send))
  testthat::expect_identical(failing_send$closed, 1L)

  source <- paste(
    readLines(testthat::test_path("..", "..", "R", "web_demo_security.R"), warn = FALSE),
    collapse = "\n"
  )
  testthat::expect_match(source, "tbi_demo_terminate_expired_session(session)", fixed = TRUE)
  testthat::expect_match(source, "session$close()", fixed = TRUE)
})

testthat::test_that("expiration observer wakes at the exact deadline when it is near", {
  now <- as.POSIXct("2026-08-20 12:00:00", tz = "America/New_York")

  testthat::expect_identical(
    tbi_demo_expiration_delay_ms(
      now = now,
      expiration = now + 3
    ),
    3000L
  )
  testthat::expect_identical(
    tbi_demo_expiration_delay_ms(
      now = now,
      expiration = now + 60
    ),
    10000L
  )
  testthat::expect_identical(
    tbi_demo_expiration_delay_ms(
      now = now,
      expiration = now
    ),
    1L
  )
})

testthat::test_that("feedback configuration cannot inherit local expiration or TPE bypass", {
  source_db <- testthat::test_path("..", "..", "inst", "database", "tbi.sqlite")
  with_demo_environment(list(
    TBI_DISABLE_DEMO_EXPIRATION = "true",
    TBI_ENABLE_TPE_TEST_MODE = "true"
  ), {
    config <- tbi_feedback_config(
      "2099-01-01 00:00:00",
      source_db = source_db
    )
    testthat::expect_false(config$expiration_disabled)
    testthat::expect_false(config$tpe_test_mode)
    chrome <- htmltools::renderTags(tbi_demo_chrome(
      expiration_disabled = config$expiration_disabled,
      status_label = config$status_label,
      feedback_mode = TRUE
    ))
    html <- paste(chrome$head, chrome$html, collapse = "\n")
    testthat::expect_match(html, "FEEDBACK / NON-AUTHORITATIVE", fixed = TRUE)
    testthat::expect_match(html, "Scenario exploration only", fixed = TRUE)
    testthat::expect_match(html, "tbi-demo-dock", fixed = TRUE)
    testthat::expect_match(html, "tbi-demo-mode", fixed = TRUE)
    testthat::expect_match(html, "tbi-feedback-mode", fixed = TRUE)
    css <- paste(readLines(
      testthat::test_path("..", "..", "inst", "app", "www", "tbi_demo.css"),
      warn = FALSE
    ), collapse = "\n")
    testthat::expect_match(css, "[.]tbi-demo-mode [.]tbi-page-content")
    testthat::expect_match(css, "[.]tbi-demo-vault\\s*\\{[^}]*bottom:\\s*86px", perl = TRUE)
    testthat::expect_false(grepl(".tbi-feedback-mode .tbi-page-content", css, fixed = TRUE))
    feedback_session <- shiny::MockShinySession$new()
    feedback_session$userData$tbi_feedback_mode <- TRUE
    testthat::expect_false(shiny::withReactiveDomain(
      feedback_session,
      v2_trade_test_mode_enabled("true")
    ))
  })
})

testthat::test_that("demo client honors the server expiration-disabled signal", {
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/", mustWork = TRUE)
  js <- paste(readLines(file.path(root, "inst", "app", "www", "tbi_demo.js"), warn = FALSE), collapse = "\n")
  testthat::expect_match(js, "message.expiration_disabled", fixed = TRUE)
  testthat::expect_match(js, "Expiration disabled for local development", fixed = TRUE)
})

testthat::test_that("bypass UI initializes before external script and omits expiration overlay", {
  with_demo_environment(list(
    TBI_DEMO_MODE = "true",
    TBI_DISABLE_DEMO_EXPIRATION = "true",
    TBI_DEMO_EXPIRES_AT = "2020-01-01 00:00:00"
  ), {
    rendered <- htmltools::renderTags(tbi_demo_chrome())
    html <- paste(rendered$head, rendered$html, collapse = "\n")
    testthat::expect_match(html, "window.TBI_DEMO_EXPIRATION_DISABLED = true", fixed = TRUE)
    testthat::expect_match(html, "tbi-demo-mode", fixed = TRUE)
    testthat::expect_match(html, "tbi_demo.js?v=", fixed = TRUE)
    testthat::expect_false(grepl('id="tbi-demo-expiration-overlay"', html, fixed = TRUE))
  })
})

testthat::test_that("web demo UI starts with an expired timestamp when bypass is enabled", {
  with_demo_environment(list(
    TBI_DEMO_MODE = "true",
    TBI_DISABLE_DEMO_EXPIRATION = "true",
    TBI_DEMO_EXPIRES_AT = "2020-01-01 00:00:00"
  ), {
    rendered <- htmltools::renderTags(tbi_web_demo_ui(NULL))
    html <- paste(rendered$head, rendered$html, collapse = "\n")
    testthat::expect_match(html, "tbi-product-shell", fixed = TRUE)
    testthat::expect_false(grepl("Private Demo Access Expired", html, fixed = TRUE))
    testthat::expect_false(grepl('id="tbi-demo-expiration-overlay"', html, fixed = TRUE))
  })
})

testthat::test_that("session configuration disables expiration timer for the full session", {
  with_demo_environment(list(
    TBI_DEMO_MODE = "true",
    TBI_DISABLE_DEMO_EXPIRATION = "true",
    TBI_DEMO_EXPIRES_AT = "2020-01-01 00:00:00"
  ), {
    config <- tbi_demo_session_config()
    testthat::expect_true(config$expiration_disabled)
    testthat::expect_null(config$expires_at)
    testthat::expect_null(config$remaining_ms)
    testthat::expect_false(tbi_demo_expiration_observer_enabled(config$expiration_disabled))
  })
})

testthat::test_that("browser expiration functions are guarded by session bypass state", {
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/", mustWork = TRUE)
  js <- paste(readLines(file.path(root, "inst", "app", "www", "tbi_demo.js"), warn = FALSE), collapse = "\n")
  testthat::expect_match(js, "expirationDisabled: window.TBI_DEMO_EXPIRATION_DISABLED === true", fixed = TRUE)
  testthat::expect_match(js, "if (state.expirationDisabled) return;", fixed = TRUE)
})

testthat::test_that("expiration-enabled UI and session retain existing behavior", {
  with_demo_environment(list(
    TBI_DEMO_MODE = "true",
    TBI_DISABLE_DEMO_EXPIRATION = "false",
    TBI_DEMO_EXPIRES_AT = "2030-01-01 00:00:00"
  ), {
    rendered <- htmltools::renderTags(tbi_demo_chrome())
    html <- paste(rendered$head, rendered$html, collapse = "\n")
    config <- tbi_demo_session_config()
    testthat::expect_match(html, "window.TBI_DEMO_EXPIRATION_DISABLED = false", fixed = TRUE)
    testthat::expect_match(html, 'id="tbi-demo-expiration-overlay"', fixed = TRUE)
    testthat::expect_false(config$expiration_disabled)
    testthat::expect_true(is.character(config$expires_at))
    testthat::expect_true(is.numeric(config$remaining_ms))
    testthat::expect_true(tbi_demo_expiration_observer_enabled(config$expiration_disabled))
  })
})
