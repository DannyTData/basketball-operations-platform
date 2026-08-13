# ============================================================
# Thompson's Basketball Intelligence
# Phase 10 Verification: Scenario Comparison Engine
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

if (length(missing_packages) > 0L) {
  stop(
    paste0(
      "Install required packages first: ",
      paste(
        missing_packages,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

cat("\n")
cat("============================================================\n")
cat("THOMPSON'S BASKETBALL INTELLIGENCE\n")
cat("PHASE 10: SCENARIO COMPARISON ENGINE VERIFICATION\n")
cat("============================================================\n\n")

required_files <- c(
  "DESCRIPTION",
  file.path(
    "R",
    "minute_allocation_engine.R"
  ),
  file.path(
    "R",
    "lineup_optimization_engine.R"
  ),
  file.path(
    "R",
    "scenario_comparison_engine.R"
  ),
  file.path(
    "tests",
    "testthat",
    "test-scenario_comparison_engine.R"
  )
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0L) {
  stop(
    paste0(
      "Required Phase 10 files are missing:\n- ",
      paste(
        missing_files,
        collapse = "\n- "
      )
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
  cat("\nPHASE 10 STATUS: FAIL\n")
  
  stop(
    "The project could not be loaded.",
    call. = FALSE
  )
}

cat("[2/5] Project package loaded: PASS\n")

required_functions <- c(
  "scenario_number",
  "scenario_integer",
  "scenario_text",
  "scenario_flag",
  "scenario_comparison_rule_defaults",
  "resolve_scenario_comparison_rules",
  "validate_scenario_roster",
  "build_scenario_state",
  "scenario_rotation_quality",
  "scenario_rotation_depth",
  "compare_scenario_minutes",
  "compare_scenario_lineups",
  "scenario_lineup_score",
  "scenario_minutes_signal",
  "scenario_rotation_quality_signal",
  "score_scenario_comparison",
  "scenario_recommendation_label",
  "build_scenario_reasons",
  "build_scenario_comparison",
  "scenario_scorecard_table",
  "scenario_executive_summary"
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

if (length(missing_functions) > 0L) {
  cat("\nPHASE 10 STATUS: FAIL\n")
  
  stop(
    paste0(
      "Scenario Comparison functions were not loaded:\n- ",
      paste(
        missing_functions,
        collapse = "\n- "
      )
    ),
    call. = FALSE
  )
}

cat("[3/5] Scenario Comparison functions loaded: PASS\n")

smoke_test <- tryCatch(
  {
    roster <- data.frame(
      player_id = 1:10,
      player_name = paste(
        "Verification Player",
        1:10
      ),
      position = c(
        "PG", "SG", "SF", "PF", "C",
        "PG", "SG", "SF", "PF", "C"
      ),
      depth_order = c(
        1, 1, 1, 1, 1,
        2, 2, 2, 2, 2
      ),
      is_starter = c(
        rep(TRUE, 5),
        rep(FALSE, 5)
      ),
      availability_status =
        rep("AVAILABLE", 10),
      bie_rating = c(
        90, 84, 86, 80, 82,
        78, 72, 76, 70, 68
      ),
      projected_bie_rating = c(
        91, 85, 87, 81, 83,
        79, 73, 77, 71, 69
      ),
      impact_score = c(
        90, 83, 88, 80, 84,
        78, 72, 77, 71, 69
      ),
      offensive_impact = c(
        92, 90, 82, 86, 72,
        84, 86, 68, 64, 72
      ),
      defensive_impact = c(
        78, 70, 90, 82, 92,
        68, 60, 91, 86, 80
      ),
      creation_score = c(
        96, 84, 76, 64, 42,
        90, 66, 50, 35, 58
      ),
      spacing_score = c(
        88, 94, 82, 92, 45,
        78, 96, 70, 38, 80
      ),
      rebounding_score = c(
        45, 42, 70, 78, 96,
        40, 35, 66, 92, 74
      ),
      primary_role = c(
        "Creator",
        "Scorer",
        "Two-Way",
        "Connector",
        "Rim",
        "Creator",
        "Spacer",
        "Defender",
        "Rebounder",
        "Connector"
      ),
      archetype = "",
      impact_tier = c(
        rep("Starter", 5),
        rep("Rotation", 5)
      ),
      stringsAsFactors = FALSE
    )
    
    scenario <- roster
    
    scenario$bie_rating[[7]] <- 96
    scenario$projected_bie_rating[[7]] <- 97
    scenario$impact_score[[7]] <- 96
    scenario$offensive_impact[[7]] <- 100
    scenario$spacing_score[[7]] <- 100
    
    result <- build_scenario_comparison(
      base_roster = roster,
      scenario_roster = scenario,
      scenario_name = "Verification Scenario",
      rotation_size = 10
    )
    
    stopifnot(
      is.list(result),
      is.data.frame(
        result$minute_comparison
      ),
      is.data.frame(
        result$lineup_comparison
      ),
      is.character(
        result$recommendation
      ),
      identical(
        result$model_label,
        "TBI_SCENARIO_v1_COMPARISON"
      )
    )
    
    TRUE
  },
  error = function(e) {
    message(
      "\nScenario Comparison smoke test failed:\n",
      conditionMessage(e)
    )
    
    FALSE
  }
)

if (!isTRUE(smoke_test)) {
  cat("\nPHASE 10 STATUS: FAIL\n")
  
  stop(
    "The Scenario Comparison smoke test failed.",
    call. = FALSE
  )
}

cat("[4/5] Scenario Comparison smoke test: PASS\n")

test_result <- tryCatch(
  {
    results <- testthat::test_file(
      file.path(
        "tests",
        "testthat",
        "test-scenario_comparison_engine.R"
      ),
      reporter = "summary",
      stop_on_failure = FALSE,
      stop_on_warning = FALSE
    )
    
    failures <- unlist(
      lapply(
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
        }
      ),
      use.names = FALSE
    )
    
    if (any(failures)) {
      stop(
        "One or more Scenario Comparison test expectations failed.",
        call. = FALSE
      )
    }
    
    TRUE
  },
  error = function(e) {
    message(
      "\nScenario Comparison tests failed:\n",
      conditionMessage(e)
    )
    
    FALSE
  }
)

if (!isTRUE(test_result)) {
  cat("\nPHASE 10 STATUS: FAIL\n")
  
  stop(
    "One or more Phase 10 tests failed.",
    call. = FALSE
  )
}

cat("[5/5] Automated Scenario Comparison tests: PASS\n\n")

cat("============================================================\n")
cat("PHASE 10 STATUS: PASS\n")
cat("Scenario Comparison Engine verified successfully.\n")
cat("============================================================\n")