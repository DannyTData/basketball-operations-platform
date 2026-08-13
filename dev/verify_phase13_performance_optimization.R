# ============================================================
# Thompson's Basketball Intelligence
# Phase 13 Verification: Performance + Loading Optimization
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
cat("PHASE 13: PERFORMANCE + LOADING OPTIMIZATION\n")
cat("============================================================\n\n")

required_files <- c(
  "DESCRIPTION",
  file.path(
    "R",
    "performance_optimization.R"
  ),
  file.path(
    "R",
    "mod_depth_chart.R"
  ),
  file.path(
    "tests",
    "testthat",
    "test-performance_optimization.R"
  )
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0L) {
  stop(
    paste0(
      "Required Phase 13 files are missing:\n- ",
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
  cat("\nPHASE 13 STATUS: FAIL\n")
  stop(
    "The project could not be loaded.",
    call. = FALSE
  )
}

cat("[2/6] Project package loaded: PASS\n")

required_functions <- c(
  "phase13_roster_signature",
  "phase13_lineup_signature",
  "phase13_trade_signature",
  "phase13_cache_new",
  "phase13_cache_get",
  "phase13_cache_store",
  "phase13_cache_hit",
  "phase13_performance_status",
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
  cat("\nPHASE 13 STATUS: FAIL\n")
  stop(
    paste0(
      "Phase 13 functions were not loaded:\n- ",
      paste(
        missing_functions,
        collapse = "\n- "
      )
    ),
    call. = FALSE
  )
}

cat("[3/6] Performance functions loaded: PASS\n")

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
  "phase11_lineup_cache <- shiny::reactiveVal",
  "phase12_base_roster_cache <- shiny::reactiveVal",
  "phase12_scenario_cache <- shiny::reactiveVal",
  "phase13_roster_signature(",
  "phase13_trade_signature("
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
  cat("\nPHASE 13 STATUS: FAIL\n")
  stop(
    paste0(
      "Depth-chart performance marker(s) missing:\n- ",
      paste(
        missing_markers,
        collapse = "\n- "
      )
    ),
    call. = FALSE
  )
}

cat("[4/6] Depth-chart cache wiring markers: PASS\n")

smoke <- tryCatch(
  {
    d <- data.frame(
      player_id = 1:3,
      player_name = c("A", "B", "C"),
      position = c("PG", "SG", "SF"),
      bie_rating = c(90, 80, 70),
      stringsAsFactors = FALSE
    )
    
    key1 <- phase13_roster_signature(d)
    key2 <- phase13_roster_signature(
      d[c(3, 2, 1), , drop = FALSE]
    )
    
    cache <- phase13_cache_new()
    cache <- phase13_cache_store(
      cache,
      key1,
      d
    )
    
    stopifnot(
      identical(key1, key2),
      is.data.frame(
        phase13_cache_get(
          cache,
          key2
        )
      )
    )
    
    TRUE
  },
  error = function(e) {
    message(
      "\nPhase 13 smoke test failed:\n",
      conditionMessage(e)
    )
    FALSE
  }
)

if (!isTRUE(smoke)) {
  cat("\nPHASE 13 STATUS: FAIL\n")
  stop(
    "Phase 13 smoke test failed.",
    call. = FALSE
  )
}

cat("[5/6] Cache/signature smoke test: PASS\n")

test_result <- tryCatch(
  {
    results <- testthat::test_file(
      file.path(
        "tests",
        "testthat",
        "test-performance_optimization.R"
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
        "One or more Phase 13 test expectations failed.",
        call. = FALSE
      )
    }
    
    TRUE
  },
  error = function(e) {
    message(
      "\nPhase 13 tests failed:\n",
      conditionMessage(e)
    )
    FALSE
  }
)

if (!isTRUE(test_result)) {
  cat("\nPHASE 13 STATUS: FAIL\n")
  stop(
    "One or more Phase 13 tests failed.",
    call. = FALSE
  )
}

cat("[6/6] Automated performance tests: PASS\n\n")

cat("============================================================\n")
cat("PHASE 13 STATUS: PASS\n")
cat("Performance + loading optimization verified successfully.\n")
cat("============================================================\n")