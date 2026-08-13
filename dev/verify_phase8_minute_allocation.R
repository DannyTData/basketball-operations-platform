# ============================================================
# Thompson's Basketball Intelligence
# Phase 8 Verification: Minute Allocation + Rotation Intelligence
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
      paste(missing_packages, collapse = ", ")
    ),
    call. = FALSE
  )
}

cat("\n")
cat("============================================================\n")
cat("THOMPSON'S BASKETBALL INTELLIGENCE\n")
cat("PHASE 8: MINUTE ALLOCATION + ROTATION INTELLIGENCE\n")
cat("============================================================\n\n")

required_files <- c(
  "DESCRIPTION",
  file.path(
    "R",
    "minute_allocation_engine.R"
  ),
  file.path(
    "tests",
    "testthat",
    "test-minute_allocation_engine.R"
  )
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0) {
  stop(
    paste0(
      "Required Phase 8 files are missing:\n- ",
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
  cat("\nPHASE 8 STATUS: FAIL\n")
  stop(
    "The project could not be loaded.",
    call. = FALSE
  )
}

cat("[2/5] Project package loaded: PASS\n")

required_functions <- c(
  "minute_number",
  "minute_integer",
  "minute_text",
  "minute_flag",
  "minute_clamp",
  "minute_allocation_rule_defaults",
  "resolve_minute_allocation_rules",
  "normalize_availability_status",
  "availability_score",
  "role_priority_score",
  "depth_priority_score",
  "prepare_minute_allocation_roster",
  "calculate_player_minute_priority",
  "score_minute_allocation_roster",
  "select_rotation_players",
  "player_minute_bounds",
  "allocate_minutes_with_bounds",
  "round_minutes_to_team_total",
  "build_minute_allocation",
  "summarize_minute_allocation",
  "compare_minute_allocations"
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
  cat("\nPHASE 8 STATUS: FAIL\n")
  
  stop(
    paste0(
      "Minute-allocation functions were not loaded:\n- ",
      paste(
        missing_functions,
        collapse = "\n- "
      )
    ),
    call. = FALSE
  )
}

cat("[3/5] Minute-allocation functions loaded: PASS\n")

smoke_test <- tryCatch(
  {
    roster <- data.frame(
      player_id = 1:10,
      player_name = paste(
        "Verification Player",
        1:10
      ),
      position = rep(
        c(
          "PG",
          "SG",
          "SF",
          "PF",
          "C"
        ),
        2
      ),
      depth_order = c(
        rep(1, 5),
        rep(2, 5)
      ),
      is_starter = c(
        rep(TRUE, 5),
        rep(FALSE, 5)
      ),
      availability_status =
        rep(
          "Available",
          10
        ),
      bie_rating =
        seq(
          88,
          70,
          length.out = 10
        ),
      projected_bie_rating =
        seq(
          89,
          71,
          length.out = 10
        ),
      impact_score =
        seq(
          87,
          69,
          length.out = 10
        ),
      stringsAsFactors = FALSE
    )
    
    result <- build_minute_allocation(
      roster = roster,
      rotation_size = 10
    )
    
    stopifnot(
      is.list(result),
      is.data.frame(
        result$allocation
      ),
      sum(
        result$allocation$
          recommended_minutes
      ) == 240,
      result$summary$
        rotation_size == 10,
      identical(
        result$model_label,
        "TBI_MINUTES_v1_ROTATION_INTELLIGENCE"
      )
    )
    
    TRUE
  },
  error = function(e) {
    message(
      "\nMinute-allocation smoke test failed:\n",
      conditionMessage(e)
    )
    
    FALSE
  }
)

if (!isTRUE(smoke_test)) {
  cat("\nPHASE 8 STATUS: FAIL\n")
  
  stop(
    "The Minute Allocation smoke test failed.",
    call. = FALSE
  )
}

cat("[4/5] Minute-allocation smoke test: PASS\n")

test_result <- tryCatch(
  {
    results <- testthat::test_file(
      file.path(
        "tests",
        "testthat",
        "test-minute_allocation_engine.R"
      ),
      reporter = "summary",
      stop_on_failure = FALSE,
      stop_on_warning = FALSE
    )
    
    failed <- any(
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
    
    if (failed) {
      stop(
        "One or more Minute Allocation test expectations failed.",
        call. = FALSE
      )
    }
    
    TRUE
  },
  error = function(e) {
    message(
      "\nMinute-allocation tests failed:\n",
      conditionMessage(e)
    )
    
    FALSE
  }
)

if (!isTRUE(test_result)) {
  cat("\nPHASE 8 STATUS: FAIL\n")
  
  stop(
    "One or more Phase 8 tests failed.",
    call. = FALSE
  )
}

cat("[5/5] Automated Minute Allocation tests: PASS\n\n")

cat("============================================================\n")
cat("PHASE 8 STATUS: PASS\n")
cat("Minute Allocation + Rotation Intelligence verified successfully.\n")
cat("============================================================\n")