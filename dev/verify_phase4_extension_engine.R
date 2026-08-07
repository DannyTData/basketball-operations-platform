# ============================================================
# Thompson's Basketball Intelligence
# Phase 4 Verification: Contract and Extension Engine
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
cat("PHASE 4: CONTRACT AND EXTENSION ENGINE VERIFICATION\n")
cat("============================================================\n\n")

required_files <- c(
  "DESCRIPTION",
  file.path("R", "extension_engine.R"),
  file.path(
    "tests",
    "testthat",
    "test-extension_engine.R"
  )
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    paste0(
      "Required Phase 4 files are missing:\n- ",
      paste(missing_files, collapse = "\n- ")
    ),
    call. = FALSE
  )
}

cat("[1/5] Required files found: PASS\n")

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
  cat("\nPHASE 4 STATUS: FAIL\n")
  
  stop(
    "The project could not be loaded.",
    call. = FALSE
  )
}

cat("[2/5] Project package loaded: PASS\n")

required_functions <- c(
  "extension_number",
  "extension_integer",
  "extension_flag",
  "extension_money",
  "extension_rule_defaults",
  "resolve_extension_rules",
  "maximum_salary_percentage",
  "calculate_maximum_player_salary",
  "normalize_extension_type",
  "screen_extension_eligibility",
  "calculate_extension_starting_salary_limit",
  "maximum_extension_raise_percent",
  "build_extension_schedule",
  "summarize_extension_schedule",
  "evaluate_extension_proposal",
  "get_extension_player_pool",
  "extension_player_from_row"
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
  cat("\nPHASE 4 STATUS: FAIL\n")
  
  stop(
    paste0(
      "Extension-engine functions were not loaded:\n- ",
      paste(missing_functions, collapse = "\n- ")
    ),
    call. = FALSE
  )
}

cat("[3/5] Extension-engine functions loaded: PASS\n")

smoke_test <- tryCatch(
  {
    player <- list(
      player_name = "Verification Player",
      service_years = 6,
      current_salary = 20000000,
      next_season_salary = 21000000,
      remaining_contract_years = 1,
      contract_type = "Veteran",
      has_bird_rights = TRUE,
      timing_window_open = TRUE,
      designated_rookie_qualified = FALSE,
      designated_veteran_qualified = FALSE,
      original_team_requirement_met = FALSE,
      eto_exercised = FALSE,
      contract_allows_extension = TRUE
    )
    
    proposal <- list(
      extension_type = "veteran",
      salary_cap = 140000000,
      starting_salary = 28000000,
      years = 4,
      raise_percent = 0.08,
      guarantee_structure = "Fully guaranteed",
      first_season = "2027-28"
    )
    
    result <- evaluate_extension_proposal(
      player = player,
      proposal = proposal
    )
    
    stopifnot(
      is.list(result),
      isTRUE(result$passes_screen),
      identical(
        result$status,
        "PASS_WITH_REVIEW"
      ),
      identical(
        result$maximum_years,
        4L
      ),
      nrow(result$schedule) == 4L,
      result$schedule_summary$total_value > 0
    )
    
    TRUE
  },
  error = function(e) {
    message(
      "\nExtension-engine smoke test failed:\n",
      conditionMessage(e)
    )
    
    FALSE
  }
)

if (!isTRUE(smoke_test)) {
  cat("\nPHASE 4 STATUS: FAIL\n")
  
  stop(
    "The extension-engine smoke test failed.",
    call. = FALSE
  )
}

cat("[4/5] Extension-engine smoke test: PASS\n")

test_result <- tryCatch(
  {
    results <- testthat::test_file(
      file.path(
        "tests",
        "testthat",
        "test-extension_engine.R"
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
          " extension-engine test expectation(s) failed."
        ),
        call. = FALSE
      )
    }
    
    TRUE
  },
  error = function(e) {
    message(
      "\nExtension-engine tests failed:\n",
      conditionMessage(e)
    )
    
    FALSE
  }
)

if (!isTRUE(test_result)) {
  cat("\nPHASE 4 STATUS: FAIL\n")
  
  stop(
    "One or more Phase 4 tests failed.",
    call. = FALSE
  )
}

cat("[5/5] Automated extension-engine tests: PASS\n\n")

cat("============================================================\n")
cat("PHASE 4 STATUS: PASS\n")
cat("Contract and Extension Engine verified successfully.\n")
cat("============================================================\n")