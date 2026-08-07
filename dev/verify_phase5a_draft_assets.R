# ============================================================
# Thompson's Basketball Intelligence
# Phase 5A Verification: Draft Assets Engine
# ============================================================

required_packages <- c(
  "devtools",
  "testthat",
  "DBI",
  "RSQLite"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )
]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Install required packages first: ",
      paste(missing_packages, collapse = ", "),
      "\n\nRun:\n",
      "install.packages(c(",
      paste(sprintf('"%s"', missing_packages), collapse = ", "),
      "))"
    ),
    call. = FALSE
  )
}

cat("\n")
cat("============================================================\n")
cat("THOMPSON'S BASKETBALL INTELLIGENCE\n")
cat("PHASE 5A: DRAFT ASSETS ENGINE VERIFICATION\n")
cat("============================================================\n\n")

required_files <- c(
  "DESCRIPTION",
  file.path("R", "database.R"),
  file.path("R", "draft_assets_engine.R"),
  file.path(
    "tests",
    "testthat",
    "test-draft_assets_engine.R"
  )
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0) {
  stop(
    paste0(
      "Required Phase 5A files are missing:\n- ",
      paste(missing_files, collapse = "\n- ")
    ),
    call. = FALSE
  )
}

cat("[1/6] Required files found: PASS\n")

load_result <- tryCatch(
  {
    devtools::load_all(
      path = ".",
      quiet = TRUE,
      export_all = TRUE,
      helpers = TRUE
    )
    
    TRUE
  },
  error = function(e) {
    message(
      "\nPackage load failed:\n",
      conditionMessage(e)
    )
    
    FALSE
  }
)

if (!isTRUE(load_result)) {
  cat("\nPHASE 5A STATUS: FAIL\n")
  
  stop(
    "The project could not be loaded.",
    call. = FALSE
  )
}

cat("[2/6] Project package loaded: PASS\n")

required_functions <- c(
  "draft_text",
  "draft_integer",
  "draft_number",
  "draft_flag",
  "normalize_draft_control_type",
  "normalize_draft_round",
  "classify_draft_protection",
  "ensure_draft_asset_tables",
  "initialize_draft_assets_database",
  "resolve_draft_team",
  "validate_draft_asset",
  "save_draft_asset",
  "update_draft_asset",
  "archive_draft_asset",
  "save_draft_asset_condition",
  "get_draft_assets",
  "get_draft_asset_detail",
  "summarize_draft_assets",
  "evaluate_draft_asset_portfolio"
)

missing_functions <- required_functions[
  !vapply(
    required_functions,
    exists,
    mode = "function",
    inherits = TRUE,
    FUN.VALUE = logical(1)
  )
]

if (length(missing_functions) > 0) {
  cat("\nPHASE 5A STATUS: FAIL\n")
  
  stop(
    paste0(
      "Draft-assets functions were not loaded:\n- ",
      paste(
        missing_functions,
        collapse = "\n- "
      )
    ),
    call. = FALSE
  )
}

cat("[3/6] Draft-assets functions loaded: PASS\n")

temporary_database <- tempfile(
  pattern = "tbi_phase5a_",
  fileext = ".sqlite"
)

cleanup_phase5a_database <- function() {
  if (file.exists(temporary_database)) {
    unlink(temporary_database)
  }
}

on.exit(
  cleanup_phase5a_database(),
  add = TRUE
)

database_setup <- tryCatch(
  {
    con <- DBI::dbConnect(
      RSQLite::SQLite(),
      temporary_database
    )
    
    on.exit(
      {
        if (DBI::dbIsValid(con)) {
          DBI::dbDisconnect(con)
        }
      },
      add = TRUE
    )
    
    DBI::dbExecute(
      con,
      "PRAGMA foreign_keys = ON;"
    )
    
    DBI::dbExecute(
      con,
      "
      CREATE TABLE teams (
        team_id INTEGER PRIMARY KEY,
        team_name TEXT NOT NULL UNIQUE,
        abbreviation TEXT NOT NULL UNIQUE
      );
      "
    )
    
    DBI::dbExecute(
      con,
      "
      INSERT INTO teams (
        team_id,
        team_name,
        abbreviation
      )
      VALUES
        (1, 'Boston Celtics', 'BOS'),
        (2, 'Brooklyn Nets', 'BKN'),
        (3, 'New York Knicks', 'NYK');
      "
    )
    
    ensure_draft_asset_tables(con)
    
    required_tables <- c(
      "draft_assets",
      "draft_asset_conditions",
      "draft_asset_audit"
    )
    
    stopifnot(
      all(
        required_tables %in%
          DBI::dbListTables(con)
      )
    )
    
    TRUE
  },
  error = function(e) {
    message(
      "\nTemporary database setup failed:\n",
      conditionMessage(e)
    )
    
    FALSE
  }
)

if (!isTRUE(database_setup)) {
  cat("\nPHASE 5A STATUS: FAIL\n")
  
  stop(
    "The draft-assets schema could not be created.",
    call. = FALSE
  )
}

cat("[4/6] Draft-assets schema and indexes: PASS\n")

persistence_smoke_test <- tryCatch(
  {
    draft_asset_id <- save_draft_asset(
      asset = list(
        draft_year = 2028,
        round = "First",
        original_team = "Brooklyn Nets",
        current_team = "Boston Celtics",
        control_type = "Incoming",
        counterparty = "Brooklyn Nets",
        protection_text = "Top-10 protected",
        strategic_value = "High",
        internal_value = 90,
        source_name = "Phase 5A verification",
        verification_status = "Verified",
        notes = "Temporary verification record",
        is_active = TRUE
      ),
      db_path = temporary_database,
      changed_by = "Verification Script"
    )
    
    stopifnot(
      is.integer(draft_asset_id),
      draft_asset_id >= 1L
    )
    
    save_draft_asset_condition(
      draft_asset_id = draft_asset_id,
      condition_order = 1L,
      condition_year = 2028L,
      condition_text =
        "Top-10 protected in 2028.",
      outcome_if_not_conveyed =
        "Rolls to 2029.",
      converts_to_year = 2029L,
      converts_to_round = "First",
      db_path = temporary_database
    )
    
    assets <- get_draft_assets(
      team_value = "Boston Celtics",
      db_path = temporary_database
    )
    
    stopifnot(
      nrow(assets) == 1L,
      identical(
        assets$control_type[[1]],
        "Incoming"
      ),
      identical(
        assets$protection_type[[1]],
        "Top-N Protected"
      ),
      assets$condition_count[[1]] == 1L
    )
    
    detail <- get_draft_asset_detail(
      draft_asset_id = draft_asset_id,
      db_path = temporary_database
    )
    
    stopifnot(
      nrow(detail$asset) == 1L,
      nrow(detail$conditions) == 1L,
      nrow(detail$audit) >= 1L
    )
    
    portfolio <- evaluate_draft_asset_portfolio(
      team_value = "Boston Celtics",
      db_path = temporary_database
    )
    
    stopifnot(
      portfolio$summary$total_assets == 1L,
      portfolio$summary$controlled_first_round == 1L
    )
    
    TRUE
  },
  error = function(e) {
    message(
      "\nDraft-assets persistence smoke test failed:\n",
      conditionMessage(e)
    )
    
    FALSE
  }
)

if (!isTRUE(persistence_smoke_test)) {
  cat("\nPHASE 5A STATUS: FAIL\n")
  
  stop(
    "The persistent draft-assets smoke test failed.",
    call. = FALSE
  )
}

cat("[5/6] Persistent draft-assets smoke test: PASS\n")

test_result <- tryCatch(
  {
    results <- testthat::test_file(
      file.path(
        "tests",
        "testthat",
        "test-draft_assets_engine.R"
      ),
      reporter = "summary",
      stop_on_failure = FALSE,
      stop_on_warning = FALSE
    )
    
    failures <- sum(
      vapply(
        results,
        function(x) {
          inherits(
            x,
            "expectation_failure"
          ) ||
            inherits(
              x,
              "expectation_error"
            )
        },
        logical(1)
      )
    )
    
    if (failures > 0) {
      stop(
        paste0(
          failures,
          " draft-assets test expectation(s) failed."
        ),
        call. = FALSE
      )
    }
    
    TRUE
  },
  error = function(e) {
    message(
      "\nDraft-assets tests failed:\n",
      conditionMessage(e)
    )
    
    FALSE
  }
)

if (!isTRUE(test_result)) {
  cat("\nPHASE 5A STATUS: FAIL\n")
  
  stop(
    "One or more Phase 5A tests failed.",
    call. = FALSE
  )
}

cat("[6/6] Automated draft-assets tests: PASS\n\n")

cat("============================================================\n")
cat("PHASE 5A STATUS: PASS\n")
cat("Draft Assets Intelligence Engine verified successfully.\n")
cat("============================================================\n")