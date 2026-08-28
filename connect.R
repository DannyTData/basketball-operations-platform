# Posit Connect private-review entry point.
pkgload::load_all(
  path = ".",
  quiet = TRUE,
  export_all = TRUE,
  helpers = TRUE
)

source_database <- file.path(
  "inst",
  "database",
  "tbi.sqlite"
)

if (!file.exists(source_database)) {
  stop(
    "TBI source database not found.",
    call. = FALSE
  )
}

review_database <- tempfile(
  pattern = "tbi-connect-review-",
  tmpdir = tempdir(),
  fileext = ".sqlite"
)

copy_ok <- file.copy(
  from = source_database,
  to = review_database,
  overwrite = FALSE
)

if (!isTRUE(copy_ok)) {
  stop(
    "Could not create disposable TBI review database.",
    call. = FALSE
  )
}

Sys.setenv(
  TBI_DEMO_MODE = "true",
  TBI_DB_OVERRIDE = normalizePath(
    review_database,
    winslash = "/",
    mustWork = TRUE
  ),
  TBI_ENABLE_TPE_TEST_MODE = "false",
  TBI_FEEDBACK_MODE = "false"
)

options(
  shiny.sanitize.errors = FALSE,
  shiny.fullstacktrace = TRUE
)

diagnostic_server <- function(input, output, session) {
  message("TBI_CONNECT_SERVER_START")

  shiny::onUnhandledError(
    function(err) {
      message(
        "TBI_CONNECT_UNHANDLED_ERROR: ",
        conditionMessage(err)
      )
      message(
        "TBI_CONNECT_FATAL: ",
        inherits(err, "shiny.error.fatal")
      )
      message(
        "TBI_CONNECT_ERROR_CLASS: ",
        paste(class(err), collapse = ", ")
      )
    },
    session = session
  )

  session$onSessionEnded(
    function() {
      message("TBI_CONNECT_SESSION_ENDED")
    }
  )

  tryCatch(
    {
      app_server(input, output, session)
      message("TBI_CONNECT_SERVER_BOUND")
    },
    error = function(err) {
      message(
        "TBI_CONNECT_SERVER_INIT_ERROR: ",
        conditionMessage(err)
      )
      message(
        "TBI_CONNECT_INIT_CLASS: ",
        paste(class(err), collapse = ", ")
      )
      stop(err)
    }
  )
}

shiny::shinyApp(
  ui = app_ui,
  server = diagnostic_server
)
