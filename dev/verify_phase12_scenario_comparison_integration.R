# ============================================================
# Thompson's Basketball Intelligence
# Phase 12 Verification: Scenario Comparison UI Integration
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
cat("PHASE 12: SCENARIO COMPARISON UI INTEGRATION\n")
cat("============================================================\n\n")

required_files <- c(
  "DESCRIPTION",
  file.path(
    "R",
    "scenario_comparison_engine.R"
  ),
  file.path(
    "R",
    "scenario_comparison_integration.R"
  ),
  file.path(
    "R",
    "mod_depth_chart.R"
  ),
  file.path(
    "tests",
    "testthat",
    "test-scenario_comparison_integration.R"
  )
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0L) {
  stop(
    paste0(
      "Required Phase 12 files are missing:\n- ",
      paste(
        missing_files,
        collapse = "\n- "
      )
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
  cat("\nPHASE 12 STATUS: FAIL\n")
  stop(
    "The project could not be loaded.",
    call. = FALSE
  )
}

cat("[2/6] Project package loaded: PASS\n")

required_functions <- c(
  "phase12_change_text",
  "phase12_recommendation_class",
  "phase12_metric_card",
  "phase12_top_minute_changes",
  "phase12_lineup_changes",
  "build_phase12_scenario_panel",
  "build_scenario_comparison",
  "mod_depth_chart_ui",
  "mod_depth_chart_server"
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
  cat("\nPHASE 12 STATUS: FAIL\n")
  stop(
    paste0(
      "Phase 12 functions were not loaded:\n- ",
      paste(
        missing_functions,
        collapse = "\n- "
      )
    ),
    call. = FALSE
  )
}

cat("[3/6] Scenario integration functions loaded: PASS\n")

module_text <- paste(
  readLines(
    file.path(
      "R",
      "mod_depth_chart.R"
    ),
    warn = FALSE
  ),
  collapse = "\n"
)

markers <- c(
  'ns("phase12_scenario_comparison_panel")',
  "phase12_scenario_result <- shiny::reactive",
  "build_scenario_comparison(",
  "build_phase12_scenario_panel("
)

missing_markers <- markers[
  !vapply(
    markers,
    function(marker) {
      grepl(
        marker,
        module_text,
        fixed = TRUE
      )
    },
    logical(1)
  )
]

if (length(missing_markers) > 0L) {
  cat("\nPHASE 12 STATUS: FAIL\n")
  stop(
    paste0(
      "Depth-chart Phase 12 marker(s) missing:\n- ",
      paste(
        missing_markers,
        collapse = "\n- "
      )
    ),
    call. = FALSE
  )
}

cat("[4/6] Depth-chart scenario wiring markers: PASS\n")

smoke <- tryCatch(
  {
    roster <- data.frame(
      player_id = 1:10,
      player_name = paste(
        "Verification Player",
        1:10
      ),
      position = c(
        "PG","SG","SF","PF","C",
        "PG","SG","SF","PF","C"
      ),
      depth_order = c(
        rep(1,5),
        rep(2,5)
      ),
      is_starter = c(
        rep(TRUE,5),
        rep(FALSE,5)
      ),
      availability_status =
        rep("AVAILABLE",10),
      bie_rating = seq(
        90,68,length.out=10
      ),
      projected_bie_rating = seq(
        91,69,length.out=10
      ),
      impact_score = seq(
        90,68,length.out=10
      ),
      offensive_impact = seq(
        92,70,length.out=10
      ),
      defensive_impact = seq(
        90,68,length.out=10
      ),
      creation_score = seq(
        94,60,length.out=10
      ),
      spacing_score = seq(
        90,62,length.out=10
      ),
      rebounding_score = seq(
        80,64,length.out=10
      ),
      stringsAsFactors = FALSE
    )
    
    scenario <- roster
    scenario$bie_rating[[7]] <- 96
    scenario$offensive_impact[[7]] <- 100
    
    result <- build_scenario_comparison(
      base_roster = roster,
      scenario_roster = scenario,
      scenario_name = "Verification Trade",
      rotation_size = 9
    )
    
    panel <- build_phase12_scenario_panel(
      result,
      scenario = list(
        partner_team = "Verification Partner"
      )
    )
    
    stopifnot(
      is.list(result),
      is.character(
        result$recommendation
      ),
      inherits(
        panel,
        "shiny.tag"
      )
    )
    
    TRUE
  },
  error = function(e) {
    message(
      "\nPhase 12 smoke test failed:\n",
      conditionMessage(e)
    )
    FALSE
  }
)

if (!isTRUE(smoke)) {
  cat("\nPHASE 12 STATUS: FAIL\n")
  stop(
    "Phase 12 smoke test failed.",
    call. = FALSE
  )
}

cat("[5/6] Scenario comparison UI smoke test: PASS\n")

test_result <- tryCatch(
  {
    results <- testthat::test_file(
      file.path(
        "tests",
        "testthat",
        "test-scenario_comparison_integration.R"
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
        "One or more Phase 12 test expectations failed.",
        call. = FALSE
      )
    }
    
    TRUE
  },
  error = function(e) {
    message(
      "\nPhase 12 tests failed:\n",
      conditionMessage(e)
    )
    FALSE
  }
)

if (!isTRUE(test_result)) {
  cat("\nPHASE 12 STATUS: FAIL\n")
  stop(
    "One or more Phase 12 tests failed.",
    call. = FALSE
  )
}

cat("[6/6] Automated scenario integration tests: PASS\n\n")

cat("============================================================\n")
cat("PHASE 12 STATUS: PASS\n")
cat("Scenario Comparison UI integration verified successfully.\n")
cat("============================================================\n")