# ============================================================
# Thompson's Basketball Intelligence
# Phase 3 Verification: Trade Intelligence Engine
# ============================================================

required_packages <- c(
  "devtools",
  "testthat",
  "DBI",
  "RSQLite"
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
cat("PHASE 3: TRADE ENGINE VERIFICATION\n")
cat("============================================================\n\n")

required_files <- c(
  "DESCRIPTION",
  file.path("R", "cap_engine.R"),
  file.path("R", "trade_engine.R"),
  file.path("tests", "testthat", "test-trade_engine.R")
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    paste0(
      "Required Phase 3 files are missing:\n- ",
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
    message("\nPackage load failed:\n", conditionMessage(e))
    FALSE
  }
)

if (!isTRUE(load_result)) {
  cat("\nPHASE 3 STATUS: FAIL\n")
  stop("The project could not be loaded.", call. = FALSE)
}

cat("[2/5] Project package loaded: PASS\n")

required_functions <- c(
  "trade_number",
  "trade_salary_sum",
  "trade_money",
  "resolve_trade_team_status",
  "calculate_maximum_incoming_salary",
  "evaluate_team_trade",
  "trade_side_salary",
  "evaluate_trade_side",
  "evaluate_two_team_trade"
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
  cat("\nPHASE 3 STATUS: FAIL\n")
  stop(
    paste0(
      "Trade engine functions were not loaded:\n- ",
      paste(missing_functions, collapse = "\n- ")
    ),
    call. = FALSE
  )
}

cat("[3/5] Trade engine functions loaded: PASS\n")

thresholds <- data.frame(
  salary_cap = 140000000,
  luxury_tax = 170000000,
  first_apron = 178000000,
  second_apron = 189000000
)

smoke_test <- tryCatch(
  {
    result <- evaluate_team_trade(
      outgoing_salary = 20000000,
      incoming_salary = 25000000,
      team_salary = 150000000,
      thresholds = thresholds
    )
    
    stopifnot(
      is.list(result),
      isTRUE(result$is_salary_match),
      identical(result$status, "PASS"),
      identical(result$post_trade_salary, 155000000)
    )
    
    TRUE
  },
  error = function(e) {
    message("\nTrade-engine smoke test failed:\n", conditionMessage(e))
    FALSE
  }
)

if (!isTRUE(smoke_test)) {
  cat("\nPHASE 3 STATUS: FAIL\n")
  stop("The trade-engine smoke test failed.", call. = FALSE)
}

cat("[4/5] Trade engine smoke test: PASS\n")

test_result <- tryCatch(
  {
    results <- testthat::test_file(
      file.path("tests", "testthat", "test-trade_engine.R"),
      reporter = "summary",
      stop_on_failure = FALSE,
      stop_on_warning = FALSE
    )
    
    failures <- sum(
      vapply(
        results,
        function(x) {
          inherits(x, "expectation_failure") ||
            inherits(x, "expectation_error")
        },
        logical(1)
      )
    )
    
    if (failures > 0) {
      stop(
        paste0(
          failures,
          " trade-engine test expectation(s) failed."
        ),
        call. = FALSE
      )
    }
    
    TRUE
  },
  error = function(e) {
    message("\nTrade-engine tests failed:\n", conditionMessage(e))
    FALSE
  }
)

if (!isTRUE(test_result)) {
  cat("\nPHASE 3 STATUS: FAIL\n")
  stop("One or more Phase 3 tests failed.", call. = FALSE)
}

cat("[5/5] Automated trade-engine tests: PASS\n\n")

cat("============================================================\n")
cat("PHASE 3 STATUS: PASS\n")
cat("Trade Intelligence Engine verified successfully.\n")
cat("============================================================\n")