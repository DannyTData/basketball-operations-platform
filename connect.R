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

shiny::shinyApp(
  ui = app_ui,
  server = app_server
)
