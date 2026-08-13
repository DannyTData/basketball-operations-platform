# ============================================================
# Thompson's Basketball Intelligence
# Phase 14 Verification: Final UI / UX Polish
# STRICT VERIFIER — FAILURES CANNOT PRINT PASS
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
cat("PHASE 14: FINAL UI / UX POLISH VERIFICATION\n")
cat("============================================================\n\n")

required_files <- c(
  "DESCRIPTION",
  file.path(
    "R",
    "app_ui.R"
  ),
  file.path(
    "R",
    "ui_polish_phase14.R"
  ),
  file.path(
    "inst",
    "app",
    "www",
    "tbi_phase14.css"
  ),
  file.path(
    "tests",
    "testthat",
    "test-ui_polish_phase14.R"
  )
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0L) {
  cat("\nPHASE 14 STATUS: FAIL\n")
  
  stop(
    paste0(
      "Required Phase 14 files are missing:\n- ",
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
  cat("\nPHASE 14 STATUS: FAIL\n")
  
  stop(
    "The project could not be loaded.",
    call. = FALSE
  )
}

cat("[2/6] Project package loaded: PASS\n")

required_functions <- c(
  "app_ui",
  "phase14_ui_contract",
  "phase14_required_css_markers",
  "phase14_validate_stylesheet"
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
  cat("\nPHASE 14 STATUS: FAIL\n")
  
  stop(
    paste0(
      "Phase 14 functions were not loaded:\n- ",
      paste(
        missing_functions,
        collapse = "\n- "
      )
    ),
    call. = FALSE
  )
}

cat("[3/6] UI polish functions loaded: PASS\n")

ui_text <- paste(
  readLines(
    file.path(
      "R",
      "app_ui.R"
    ),
    warn = FALSE
  ),
  collapse = "\n"
)

ui_markers <- c(
  'app_sys("app/www/tbi_phase14.css")',
  "tbi-phase14-shell",
  "tbi-v2-page-content"
)

missing_ui_markers <- ui_markers[
  !vapply(
    ui_markers,
    function(marker) {
      grepl(
        marker,
        ui_text,
        fixed = TRUE
      )
    },
    logical(1)
  )
]

if (length(missing_ui_markers) > 0L) {
  cat("\nPHASE 14 STATUS: FAIL\n")
  
  stop(
    paste0(
      "App UI Phase 14 marker(s) missing:\n- ",
      paste(
        missing_ui_markers,
        collapse = "\n- "
      )
    ),
    call. = FALSE
  )
}

cat("[4/6] App-shell Phase 14 wiring: PASS\n")

style_result <-
  phase14_validate_stylesheet(
    file.path(
      "inst",
      "app",
      "www",
      "tbi_phase14.css"
    )
  )

if (!isTRUE(style_result$ok)) {
  cat("\nPHASE 14 STATUS: FAIL\n")
  
  stop(
    paste0(
      "Phase 14 stylesheet marker(s) missing: ",
      paste(
        style_result$missing,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

cat("[5/6] Global responsive/UI-state contract: PASS\n")

test_result <- tryCatch(
  {
    results <- testthat::test_file(
      file.path(
        "tests",
        "testthat",
        "test-ui_polish_phase14.R"
      ),
      reporter = "summary",
      stop_on_failure = FALSE,
      stop_on_warning = FALSE
    )
    
    # testthat result objects expose expectation results in
    # results$results on recent versions. Fall back recursively
    # so a real failure/error can never be silently ignored.
    has_failure <- function(x) {
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
        return(TRUE)
      }
      
      if (is.list(x)) {
        return(
          any(
            vapply(
              x,
              has_failure,
              logical(1)
            )
          )
        )
      }
      
      FALSE
    }
    
    failed <- has_failure(
      results
    )
    
    if (isTRUE(failed)) {
      stop(
        "One or more Phase 14 test expectations failed.",
        call. = FALSE
      )
    }
    
    # Additional strict counters when available.
    if (
      !is.null(results$failed) &&
      is.numeric(results$failed) &&
      results$failed > 0
    ) {
      stop(
        "Phase 14 testthat reported failed expectations.",
        call. = FALSE
      )
    }
    
    if (
      !is.null(results$error) &&
      is.numeric(results$error) &&
      results$error > 0
    ) {
      stop(
        "Phase 14 testthat reported errors.",
        call. = FALSE
      )
    }
    
    TRUE
  },
  error = function(e) {
    message(
      "\nPhase 14 automated tests failed:\n",
      conditionMessage(e)
    )
    
    FALSE
  }
)

if (!isTRUE(test_result)) {
  cat("\nPHASE 14 STATUS: FAIL\n")
  
  stop(
    "One or more Phase 14 tests failed.",
    call. = FALSE
  )
}

cat("[6/6] Automated UI polish tests: PASS\n\n")

cat("============================================================\n")
cat("PHASE 14 STATUS: PASS\n")
cat("Final UI / UX polish verified successfully.\n")
cat("============================================================\n")