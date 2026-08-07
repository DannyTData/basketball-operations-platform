# ============================================================
# Thompson's Basketball Intelligence
# Phase 5C Verification: Draft Simulation Engine
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
cat("PHASE 5C: DRAFT SIMULATION ENGINE VERIFICATION\n")
cat("============================================================\n\n")

required_files <- c(
  "DESCRIPTION",
  file.path("R", "draft_assets_engine.R"),
  file.path("R", "draft_value_engine.R"),
  file.path("R", "draft_simulation_engine.R"),
  file.path(
    "tests",
    "testthat",
    "test-draft_simulation_engine.R"
  )
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0) {
  stop(
    paste0(
      "Required Phase 5C files are missing:\n- ",
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
  cat("\nPHASE 5C STATUS: FAIL\n")
  
  stop(
    "The project could not be loaded.",
    call. = FALSE
  )
}

cat("[2/5] Project package loaded: PASS\n")

required_functions <- c(
  "draft_sim_number",
  "draft_sim_integer",
  "draft_sim_text",
  "draft_sim_flag",
  "draft_sim_probability",
  "draft_simulation_rule_defaults",
  "resolve_draft_simulation_rules",
  "validate_draft_simulation_asset",
  "normalize_draft_conditions",
  "simulate_draft_slot",
  "simulate_conveyance",
  "simulate_swap_exercise",
  "resolve_condition_path",
  "simulate_draft_asset_once",
  "simulate_draft_asset",
  "simulate_draft_portfolio",
  "get_draft_simulation_inputs",
  "simulate_team_draft_portfolio"
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
  cat("\nPHASE 5C STATUS: FAIL\n")
  
  stop(
    paste0(
      "Draft-simulation functions were not loaded:\n- ",
      paste(
        missing_functions,
        collapse = "\n- "
      )
    ),
    call. = FALSE
  )
}

cat("[3/5] Draft-simulation functions loaded: PASS\n")

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
      conveyance_probability = 0.75
    )
    
    conditions <- data.frame(
      condition_order = 1L,
      condition_year = 2028L,
      condition_text = "Top-10 protected",
      outcome_if_conveys = "Conveys in 2028",
      outcome_if_not_conveyed = "Rolls to 2029",
      converts_to_year = 2029L,
      converts_to_round = "First",
      is_final_condition = 0L,
      stringsAsFactors = FALSE
    )
    
    result_a <- simulate_draft_asset(
      asset = asset,
      conditions = conditions,
      iterations = 250L,
      current_year = 2026L,
      random_seed = 123L
    )
    
    result_b <- simulate_draft_asset(
      asset = asset,
      conditions = conditions,
      iterations = 250L,
      current_year = 2026L,
      random_seed = 123L
    )
    
    stopifnot(
      is.list(result_a),
      result_a$iterations == 250L,
      nrow(result_a$simulation_results) == 250L,
      isTRUE(
        all.equal(
          result_a$mean_value,
          result_b$mean_value
        )
      ),
      result_a$conveyance_rate >= 0,
      result_a$conveyance_rate <= 1
    )
    
    TRUE
  },
  error = function(e) {
    message(
      "\nDraft-simulation smoke test failed:\n",
      conditionMessage(e)
    )
    
    FALSE
  }
)

if (!isTRUE(smoke_test)) {
  cat("\nPHASE 5C STATUS: FAIL\n")
  
  stop(
    "The draft-simulation smoke test failed.",
    call. = FALSE
  )
}

cat("[4/5] Draft-simulation smoke test: PASS\n")

test_result <- tryCatch(
  {
    results <- testthat::test_file(
      file.path(
        "tests",
        "testthat",
        "test-draft_simulation_engine.R"
      ),
      reporter = "summary",
      stop_on_failure = TRUE,
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
          " draft-simulation test expectation(s) failed."
        ),
        call. = FALSE
      )
    }
    
    TRUE
  },
  error = function(e) {
    message(
      "\nDraft-simulation tests failed:\n",
      conditionMessage(e)
    )
    
    FALSE
  }
)

if (!isTRUE(test_result)) {
  cat("\nPHASE 5C STATUS: FAIL\n")
  
  stop(
    "One or more Phase 5C tests failed.",
    call. = FALSE
  )
}

cat("[5/5] Automated draft-simulation tests: PASS\n\n")

cat("============================================================\n")
cat("PHASE 5C STATUS: PASS\n")
cat("Draft Simulation Intelligence Engine verified successfully.\n")
cat("============================================================\n")