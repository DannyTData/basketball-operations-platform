# ============================================================
# Thompson's Basketball Intelligence
# Phase 5B Verification: Draft Value Engine
# ============================================================

required_packages <- c(
  "devtools",
  "testthat"
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
cat("PHASE 5B: DRAFT VALUE ENGINE VERIFICATION\n")
cat("============================================================\n\n")

required_files <- c(
  "DESCRIPTION",
  file.path("R", "draft_assets_engine.R"),
  file.path("R", "draft_value_engine.R"),
  file.path(
    "tests",
    "testthat",
    "test-draft_value_engine.R"
  )
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0) {
  stop(
    paste0(
      "Required Phase 5B files are missing:\n- ",
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
  cat("\nPHASE 5B STATUS: FAIL\n")
  
  stop(
    "The project could not be loaded.",
    call. = FALSE
  )
}

cat("[2/5] Project package loaded: PASS\n")

required_functions <- c(
  "draft_value_number",
  "draft_value_integer",
  "draft_value_text",
  "draft_value_flag",
  "draft_value_rule_defaults",
  "resolve_draft_value_rules",
  "normalize_draft_value_round",
  "draft_slot_value",
  "draft_time_discount",
  "draft_protection_multiplier",
  "draft_control_type_multiplier",
  "draft_verification_multiplier",
  "draft_strategic_multiplier",
  "draft_condition_multiplier",
  "validate_draft_value_asset",
  "draft_value_tier",
  "evaluate_draft_asset_value",
  "evaluate_draft_portfolio_values",
  "summarize_draft_portfolio_value",
  "evaluate_team_draft_value",
  "compare_draft_portfolios"
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
  cat("\nPHASE 5B STATUS: FAIL\n")
  
  stop(
    paste0(
      "Draft-value functions were not loaded:\n- ",
      paste(
        missing_functions,
        collapse = "\n- "
      )
    ),
    call. = FALSE
  )
}

cat("[3/5] Draft-value functions loaded: PASS\n")

smoke_test <- tryCatch(
  {
    asset <- list(
      draft_asset_id = 1L,
      draft_year = 2028L,
      round = "First",
      control_type = "Incoming",
      protection_type = "Top-N Protected",
      verification_status = "Verified",
      strategic_value = "High",
      condition_count = 1L,
      expected_slot = 10L,
      internal_value = NA_real_
    )
    
    result <- evaluate_draft_asset_value(
      asset = asset,
      current_year = 2026L
    )
    
    expected_score <-
      60 *
      (1 / 1.08^2) *
      0.78 *
      1.00 *
      1.00 *
      1.10 *
      0.95
    
    stopifnot(
      is.list(result),
      identical(
        result$direction,
        "Asset"
      ),
      isTRUE(
        all.equal(
          result$model_score,
          expected_score
        )
      ),
      isTRUE(
        all.equal(
          result$blended_value_score,
          expected_score
        )
      ),
      !isTRUE(
        result$requires_manual_review
      )
    )
    
    TRUE
  },
  error = function(e) {
    message(
      "\nDraft-value smoke test failed:\n",
      conditionMessage(e)
    )
    
    FALSE
  }
)

if (!isTRUE(smoke_test)) {
  cat("\nPHASE 5B STATUS: FAIL\n")
  
  stop(
    "The draft-value smoke test failed.",
    call. = FALSE
  )
}

cat("[4/5] Draft-value smoke test: PASS\n")

test_result <- tryCatch(
  {
    results <- testthat::test_file(
      file.path(
        "tests",
        "testthat",
        "test-draft_value_engine.R"
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
          " draft-value test expectation(s) failed."
        ),
        call. = FALSE
      )
    }
    
    TRUE
  },
  error = function(e) {
    message(
      "\nDraft-value tests failed:\n",
      conditionMessage(e)
    )
    
    FALSE
  }
)

if (!isTRUE(test_result)) {
  cat("\nPHASE 5B STATUS: FAIL\n")
  
  stop(
    "One or more Phase 5B tests failed.",
    call. = FALSE
  )
}

cat("[5/5] Automated draft-value tests: PASS\n\n")

cat("============================================================\n")
cat("PHASE 5B STATUS: PASS\n")
cat("Draft Value Intelligence Engine verified successfully.\n")
cat("============================================================\n")