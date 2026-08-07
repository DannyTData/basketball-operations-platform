# ============================================================
# Thompson's Basketball Intelligence
# Phase 7 Verification: Executive Experience Helpers
# ============================================================

required_packages <- c(
  "devtools",
  "testthat",
  "shiny"
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
cat("PHASE 7: EXECUTIVE EXPERIENCE HELPERS VERIFICATION\n")
cat("============================================================\n\n")

required_files <- c(
  "DESCRIPTION",
  file.path("R", "basketball_intelligence_engine.R"),
  file.path("R", "executive_experience_helpers.R"),
  file.path(
    "tests",
    "testthat",
    "test-executive_experience_helpers.R"
  )
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0) {
  stop(
    paste0(
      "Required Phase 7 files are missing:\n- ",
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
  cat("\nPHASE 7 STATUS: FAIL\n")
  
  stop(
    "The project could not be loaded.",
    call. = FALSE
  )
}

cat("[2/5] Project package loaded: PASS\n")

required_functions <- c(
  "executive_text",
  "executive_number",
  "executive_flag",
  "executive_score",
  "executive_get",
  "executive_status_severity",
  "executive_severity_class",
  "executive_display_label",
  "executive_status_badge",
  "executive_data_badge",
  "executive_metric_card",
  "executive_empty_state",
  "executive_loading_state",
  "executive_score_severity",
  "executive_recommendation_banner",
  "executive_factor_card",
  "executive_intelligence_scorecard",
  "executive_callout",
  "executive_risk_panel",
  "executive_opportunity_panel",
  "executive_scenario_card",
  "executive_scenario_comparison",
  "executive_data_quality_panel",
  "executive_decision_view"
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
  cat("\nPHASE 7 STATUS: FAIL\n")
  
  stop(
    paste0(
      "Executive Experience functions were not loaded:\n- ",
      paste(
        missing_functions,
        collapse = "\n- "
      )
    ),
    call. = FALSE
  )
}

cat("[3/5] Executive Experience functions loaded: PASS\n")

smoke_test <- tryCatch(
  {
    intelligence <- list(
      recommendation = "Advance with Conditions",
      classification = "Positive",
      score = 71.4,
      executive_summary = "The proposed move is viable with manageable constraints.",
      scope_note = "Decision-support only.",
      key_risks = c(
        "First apron exposure.",
        "One draft asset requires verification."
      ),
      components = list(
        competitive_position = list(
          score = 82,
          explanation = "The team projects as a contender."
        ),
        financial_flexibility = list(
          score = 58,
          explanation = "Flexibility is moderate."
        ),
        roster_control = list(
          score = 66,
          explanation = "The roster retains useful optionality."
        ),
        draft_capital = list(
          score = 72,
          explanation = "Draft capital remains strong."
        ),
        transaction_risk = list(
          score = 63,
          explanation = "Transaction risk is manageable."
        )
      )
    )
    
    ui <- executive_decision_view(
      intelligence_result = intelligence,
      opportunities = c(
        "Retains one future first-round pick.",
        "Preserves optionality for a later move."
      ),
      data_quality = list(
        verified_items = 7,
        assumption_items = 2,
        review_items = 1,
        unavailable_items = 0,
        updated_at = "2026-08-06"
      )
    )
    
    html <- as.character(ui)
    
    stopifnot(
      inherits(ui, "shiny.tag"),
      grepl(
        "Executive Recommendation",
        html,
        fixed = TRUE
      ),
      grepl(
        "Basketball Intelligence Scorecard",
        html,
        fixed = TRUE
      ),
      grepl(
        "Key Risks",
        html,
        fixed = TRUE
      ),
      grepl(
        "Data Confidence",
        html,
        fixed = TRUE
      )
    )
    
    TRUE
  },
  error = function(e) {
    message(
      "\nExecutive Experience smoke test failed:\n",
      conditionMessage(e)
    )
    
    FALSE
  }
)

if (!isTRUE(smoke_test)) {
  cat("\nPHASE 7 STATUS: FAIL\n")
  
  stop(
    "The Executive Experience smoke test failed.",
    call. = FALSE
  )
}

cat("[4/5] Executive Experience smoke test: PASS\n")

test_result <- tryCatch(
  {
    testthat::test_file(
      file.path(
        "tests",
        "testthat",
        "test-executive_experience_helpers.R"
      ),
      reporter = "summary",
      stop_on_failure = TRUE,
      stop_on_warning = FALSE
    )
    
    TRUE
  },
  error = function(e) {
    message(
      "\nExecutive Experience tests failed:\n",
      conditionMessage(e)
    )
    
    FALSE
  }
)

if (!isTRUE(test_result)) {
  cat("\nPHASE 7 STATUS: FAIL\n")
  
  stop(
    "One or more Phase 7 tests failed.",
    call. = FALSE
  )
}

cat("[5/5] Automated Executive Experience tests: PASS\n\n")

cat("============================================================\n")
cat("PHASE 7 STATUS: PASS\n")
cat("Executive Experience Helpers verified successfully.\n")
cat("============================================================\n")