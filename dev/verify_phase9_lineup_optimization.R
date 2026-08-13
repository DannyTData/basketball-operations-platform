# ============================================================
# Thompson's Basketball Intelligence
# Phase 9 Verification: Lineup Optimization Engine
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
cat("PHASE 9: LINEUP OPTIMIZATION ENGINE VERIFICATION\n")
cat("============================================================\n\n")

required_files <- c(
  "DESCRIPTION",
  file.path(
    "R",
    "lineup_optimization_engine.R"
  ),
  file.path(
    "tests",
    "testthat",
    "test-lineup_optimization_engine.R"
  )
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0L) {
  stop(
    paste0(
      "Required Phase 9 files are missing:\n- ",
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
  cat("\nPHASE 9 STATUS: FAIL\n")
  
  stop(
    "The project could not be loaded.",
    call. = FALSE
  )
}

cat("[2/5] Project package loaded: PASS\n")

required_functions <- c(
  "lineup_number",
  "lineup_integer",
  "lineup_text",
  "lineup_flag",
  "lineup_clamp",
  "lineup_optimization_rule_defaults",
  "resolve_lineup_optimization_rules",
  "normalize_lineup_position",
  "position_group",
  "position_balance_evaluation",
  "prepare_lineup_player_pool",
  "get_lineup_candidate_pool",
  "enumerate_lineup_candidates",
  "lineup_metric_mean",
  "lineup_minutes_score",
  "score_lineup",
  "lineup_key",
  "summarize_lineup",
  "optimize_lineup_type",
  "build_lineup_optimization",
  "lineup_optimization_table",
  "compare_lineup_optimization"
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
  cat("\nPHASE 9 STATUS: FAIL\n")
  
  stop(
    paste0(
      "Lineup Optimization functions were not loaded:\n- ",
      paste(
        missing_functions,
        collapse = "\n- "
      )
    ),
    call. = FALSE
  )
}

cat("[3/5] Lineup Optimization functions loaded: PASS\n")

smoke_test <- tryCatch(
  {
    roster <- data.frame(
      player_id = 1:10,
      player_name = paste(
        "Verification Player",
        1:10
      ),
      position = c(
        "PG",
        "SG",
        "SF",
        "PF",
        "C",
        "PG",
        "SG",
        "SF",
        "PF",
        "C"
      ),
      availability_status =
        rep(
          "AVAILABLE",
          10
        ),
      recommended_minutes = c(
        35, 34, 33, 31, 30,
        25, 20, 15, 10, 7
      ),
      bie_rating = c(
        90, 84, 86, 80, 82,
        78, 72, 76, 70, 68
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
      stringsAsFactors = FALSE
    )
    
    result <- build_lineup_optimization(
      roster,
      pool_size = 10
    )
    
    stopifnot(
      is.list(result),
      length(
        result$balanced$players
      ) == 5L,
      length(
        result$offense$players
      ) == 5L,
      length(
        result$defense$players
      ) == 5L,
      length(
        result$closing$players
      ) == 5L,
      identical(
        result$model_label,
        "TBI_LINEUP_v1_OPTIMIZATION"
      )
    )
    
    TRUE
  },
  error = function(e) {
    message(
      "\nLineup Optimization smoke test failed:\n",
      conditionMessage(e)
    )
    
    FALSE
  }
)

if (!isTRUE(smoke_test)) {
  cat("\nPHASE 9 STATUS: FAIL\n")
  
  stop(
    "The Lineup Optimization smoke test failed.",
    call. = FALSE
  )
}

cat("[4/5] Lineup Optimization smoke test: PASS\n")

test_result <- tryCatch(
  {
    results <- testthat::test_file(
      file.path(
        "tests",
        "testthat",
        "test-lineup_optimization_engine.R"
      ),
      reporter = "summary",
      stop_on_failure = FALSE,
      stop_on_warning = FALSE
    )
    
    failures <- unlist(
      lapply(
        results,
        function(x) {
          if (
            inherits(
              x,
              "expectation_failure"
            ) ||
            inherits(
              x,
              "expectation_error"
            )
          ) {
            TRUE
          } else {
            FALSE
          }
        }
      ),
      use.names = FALSE
    )
    
    if (any(failures)) {
      stop(
        "One or more Lineup Optimization test expectations failed.",
        call. = FALSE
      )
    }
    
    TRUE
  },
  error = function(e) {
    message(
      "\nLineup Optimization tests failed:\n",
      conditionMessage(e)
    )
    
    FALSE
  }
)

if (!isTRUE(test_result)) {
  cat("\nPHASE 9 STATUS: FAIL\n")
  
  stop(
    "One or more Phase 9 tests failed.",
    call. = FALSE
  )
}

cat("[5/5] Automated Lineup Optimization tests: PASS\n\n")

cat("============================================================\n")
cat("PHASE 9 STATUS: PASS\n")
cat("Lineup Optimization Engine verified successfully.\n")
cat("============================================================\n")