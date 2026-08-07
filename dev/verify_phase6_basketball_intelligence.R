# ============================================================
# Thompson's Basketball Intelligence
# Phase 6 Verification: Basketball Intelligence Engine
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
cat("PHASE 6: BASKETBALL INTELLIGENCE ENGINE VERIFICATION\n")
cat("============================================================\n\n")

required_files <- c(
  "DESCRIPTION",
  file.path("R", "cap_engine.R"),
  file.path("R", "trade_engine.R"),
  file.path("R", "extension_engine.R"),
  file.path("R", "draft_assets_engine.R"),
  file.path("R", "draft_value_engine.R"),
  file.path("R", "draft_simulation_engine.R"),
  file.path("R", "basketball_intelligence_engine.R"),
  file.path(
    "tests",
    "testthat",
    "test-basketball_intelligence_engine.R"
  )
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0) {
  stop(
    paste0(
      "Required Phase 6 files are missing:\n- ",
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
  cat("\nPHASE 6 STATUS: FAIL\n")
  
  stop(
    "The project could not be loaded.",
    call. = FALSE
  )
}

cat("[2/5] Project package loaded: PASS\n")

required_functions <- c(
  "basketball_intel_number",
  "basketball_intel_integer",
  "basketball_intel_text",
  "basketball_intel_flag",
  "basketball_intel_clamp",
  "basketball_intel_get",
  "basketball_intelligence_rule_defaults",
  "resolve_basketball_intelligence_rules",
  "evaluate_competitive_position",
  "evaluate_financial_flexibility",
  "evaluate_roster_control",
  "evaluate_draft_capital",
  "evaluate_transaction_risk",
  "classify_basketball_intelligence_score",
  "basketball_intelligence_recommendation_label",
  "build_basketball_intelligence_rationale",
  "evaluate_basketball_decision",
  "compare_basketball_decisions"
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
  cat("\nPHASE 6 STATUS: FAIL\n")
  
  stop(
    paste0(
      "Basketball Intelligence functions were not loaded:\n- ",
      paste(
        missing_functions,
        collapse = "\n- "
      )
    ),
    call. = FALSE
  )
}

cat("[3/5] Basketball Intelligence functions loaded: PASS\n")

smoke_test <- tryCatch(
  {
    inputs <- list(
      competitive = list(
        competitive_tier = "Contender",
        projected_wins = 52,
        playoff_probability = 0.90,
        championship_probability = 0.12
      ),
      
      cap_result = list(
        operating_status = "Tax Team",
        top_three_salary_concentration = 0.55
      ),
      
      financial = list(
        future_committed_salary_ratio = 0.90
      ),
      
      roster = list(
        guaranteed_roster_spots = 11,
        expiring_contracts = 3,
        team_options = 2,
        player_options = 1,
        two_way_contracts = 3,
        dead_money_ratio = 0.01
      ),
      
      draft_value_result = list(
        summary = list(
          portfolio_grade = "Strong",
          review_required = 0
        )
      ),
      
      draft_simulation_result = list(
        mean_portfolio_value = 160,
        portfolio_value_sd = 16
      ),
      
      trade_result = list(
        status = "PASS",
        crosses_first_apron = FALSE,
        crosses_second_apron = FALSE
      ),
      
      extension_result = list(
        status = "PASS_WITH_REVIEW"
      ),
      
      transaction = list(
        manual_review_items = 1
      )
    )
    
    result <- evaluate_basketball_decision(
      inputs = inputs
    )
    
    stopifnot(
      is.list(result),
      result$score >= 0,
      result$score <= 100,
      result$classification %in%
        c(
          "Aggressive",
          "Positive",
          "Neutral",
          "Caution",
          "Negative"
        ),
      nzchar(result$recommendation),
      length(result$components) == 5L,
      grepl(
        "Composite Basketball Intelligence score",
        result$executive_summary,
        fixed = TRUE
      )
    )
    
    comparison <- compare_basketball_decisions(
      decision_a = result,
      decision_b = list(score = result$score - 10),
      label_a = "Proposed Move",
      label_b = "Status Quo"
    )
    
    stopifnot(
      identical(
        comparison$preferred,
        "Proposed Move"
      ),
      comparison$difference > 0
    )
    
    TRUE
  },
  error = function(e) {
    message(
      "\nBasketball Intelligence smoke test failed:\n",
      conditionMessage(e)
    )
    
    FALSE
  }
)

if (!isTRUE(smoke_test)) {
  cat("\nPHASE 6 STATUS: FAIL\n")
  
  stop(
    "The Basketball Intelligence smoke test failed.",
    call. = FALSE
  )
}

cat("[4/5] Basketball Intelligence smoke test: PASS\n")

test_result <- tryCatch(
  {
    testthat::test_file(
      file.path(
        "tests",
        "testthat",
        "test-basketball_intelligence_engine.R"
      ),
      reporter = "summary",
      stop_on_failure = TRUE,
      stop_on_warning = FALSE
    )
    
    TRUE
  },
  error = function(e) {
    message(
      "\nBasketball Intelligence tests failed:\n",
      conditionMessage(e)
    )
    
    FALSE
  }
)

if (!isTRUE(test_result)) {
  cat("\nPHASE 6 STATUS: FAIL\n")
  
  stop(
    "One or more Phase 6 tests failed.",
    call. = FALSE
  )
}

cat("[5/5] Automated Basketball Intelligence tests: PASS\n\n")

cat("============================================================\n")
cat("PHASE 6 STATUS: PASS\n")
cat("Basketball Intelligence Engine verified successfully.\n")
cat("============================================================\n")