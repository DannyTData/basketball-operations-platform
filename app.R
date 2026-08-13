# ============================================================
# Thompson's Basketball Intelligence
# Private Web Demo Entry Point
# ============================================================


Sys.setenv(
  TBI_DEMO_MODE = "true"
)


if (!nzchar(Sys.getenv("TBI_DEMO_ID", ""))) {
  Sys.setenv(
    TBI_DEMO_ID = "tbi-private-demo"
  )
}


if (!nzchar(Sys.getenv("TBI_DEMO_TIMEZONE", ""))) {
  Sys.setenv(
    TBI_DEMO_TIMEZONE = "America/New_York"
  )
}


# ------------------------------------------------------------
# Disposable database sandbox
# ------------------------------------------------------------

source_database <- file.path(
  "inst",
  "database",
  "tbi.sqlite"
)


if (!file.exists(source_database)) {
  stop(
    "TBI web source database not found.",
    call. = FALSE
  )
}


demo_database_dir <- file.path(
  tempdir(),
  "tbi-web-demo"
)


dir.create(
  demo_database_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


demo_database <- file.path(
  demo_database_dir,
  paste0(
    "tbi-demo-",
    Sys.getpid(),
    ".sqlite"
  )
)


copy_ok <- file.copy(
  source_database,
  demo_database,
  overwrite = TRUE
)


if (!isTRUE(copy_ok)) {
  stop(
    "Could not create disposable TBI demo database.",
    call. = FALSE
  )
}


Sys.setenv(
  TBI_DB_OVERRIDE = normalizePath(
    demo_database,
    winslash = "/",
    mustWork = TRUE
  )
)


# ------------------------------------------------------------
# Load TBI package source
# ------------------------------------------------------------

pkgload::load_all(
  path = ".",
  quiet = TRUE,
  export_all = TRUE,
  helpers = TRUE
)


# ------------------------------------------------------------
# Return web-demo Shiny application
# ------------------------------------------------------------

shiny::shinyApp(
  ui = tbi_web_demo_ui,
  server = tbi_web_demo_server
)
