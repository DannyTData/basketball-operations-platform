# ============================================================
# Thompson's Basketball Intelligence
# NBA v1.0 Freeze Script
#
# RUN ONLY AFTER:
#   PHASE 15 STATUS: PASS
# ============================================================

if (!requireNamespace(
  "devtools",
  quietly = TRUE
)) {
  stop(
    "devtools is required.",
    call. = FALSE
  )
}

devtools::load_all(
  path = ".",
  quiet = TRUE,
  export_all = TRUE,
  helpers = TRUE
)

qa <- phase15_run_30_team_qa(
  season = NULL,
  rotation_size = 10L,
  write_report = TRUE,
  report_dir = "qa"
)

if (!isTRUE(
  qa$summary$overall_pass
)) {
  stop(
    "Freeze aborted: Phase 15 QA is not a clean PASS.",
    call. = FALSE
  )
}

manifest <-
  phase15_freeze_manifest(
    qa_result = qa,
    version = "NBA-v1.0"
  )

freeze_date <- format(
  Sys.Date(),
  "%Y-%m-%d"
)

lines <- c(
  "# Thompson's Basketball Intelligence — NBA v1.0",
  "",
  "**Status:** FROZEN",
  paste0(
    "**Freeze date:** ",
    freeze_date
  ),
  paste0(
    "**Season validated:** ",
    manifest$season
  ),
  paste0(
    "**Teams verified:** ",
    manifest$teams_verified,
    "/30"
  ),
  "",
  "## Freeze standard",
  "",
  "- Phase 15 final QA: PASS",
  "- All 30 NBA teams: PASS",
  "- Minute allocation invariant: 240 regulation minutes",
  "- Lineup profiles: BALANCED, OFFENSE, DEFENSE, CLOSING",
  "- Core front-office engine/function audit: PASS",
  "- Required database-table audit: PASS",
  "- Required source-file audit: PASS",
  "",
  "## Product state",
  "",
  "This marker freezes the NBA v1.0 feature set.",
  "Future feature development should occur after this version marker rather than modifying the frozen v1.0 definition.",
  "",
  "## QA artifacts",
  "",
  "- qa/phase15_30_team_qa.csv",
  "- qa/phase15_function_audit.csv",
  "- qa/phase15_database_audit.csv",
  "- qa/phase15_source_file_audit.csv",
  "- qa/phase15_summary.txt"
)

writeLines(
  lines,
  con = "NBA_V1_FREEZE.md"
)

writeLines(
  c(
    "NBA-v1.0",
    paste0(
      "freeze_date=",
      freeze_date
    ),
    paste0(
      "season=",
      manifest$season
    ),
    "phase15=PASS",
    "teams=30/30",
    "status=FROZEN"
  ),
  con = "VERSION_NBA_V1.txt"
)

cat("\n")
cat("============================================================\n")
cat("NBA v1.0 FREEZE COMPLETE\n")
cat("NBA_V1_FREEZE.md created.\n")
cat("VERSION_NBA_V1.txt created.\n")
cat("============================================================\n")