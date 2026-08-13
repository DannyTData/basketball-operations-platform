# ============================================================
# Thompson's Basketball Intelligence
# Phase 15 Verification
# FINAL 30-TEAM QA — STRICT PASS/FAIL
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
cat("PHASE 15: FINAL 30-TEAM QA + NBA v1.0 FREEZE READINESS\n")
cat("============================================================\n\n")

required_files <- c(
  "DESCRIPTION",
  file.path(
    "R",
    "final_qa_phase15.R"
  ),
  file.path(
    "tests",
    "testthat",
    "test-final_qa_phase15.R"
  )
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0L) {
  cat("\nPHASE 15 STATUS: FAIL\n")
  
  stop(
    paste0(
      "Required Phase 15 files are missing:\n- ",
      paste(
        missing_files,
        collapse = "\n- "
      )
    ),
    call. = FALSE
  )
}

cat("[1/7] Required Phase 15 files found: PASS\n")

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
  cat("\nPHASE 15 STATUS: FAIL\n")
  
  stop(
    "The project could not be loaded.",
    call. = FALSE
  )
}

cat("[2/7] Project package loaded: PASS\n")

required_phase15_functions <- c(
  "phase15_latest_depth_season",
  "phase15_active_teams",
  "phase15_function_audit",
  "phase15_database_audit",
  "phase15_source_file_audit",
  "phase15_run_team_qa",
  "phase15_run_30_team_qa",
  "phase15_freeze_manifest"
)

missing_phase15_functions <-
  required_phase15_functions[
    !vapply(
      required_phase15_functions,
      exists,
      mode = "function",
      inherits = TRUE,
      FUN.VALUE = logical(1)
    )
  ]

if (length(missing_phase15_functions)) {
  cat("\nPHASE 15 STATUS: FAIL\n")
  
  stop(
    paste0(
      "Phase 15 functions missing:\n- ",
      paste(
        missing_phase15_functions,
        collapse = "\n- "
      )
    ),
    call. = FALSE
  )
}

cat("[3/7] Phase 15 QA functions loaded: PASS\n")

# ------------------------------------------------------------
# Strict automated tests first
# ------------------------------------------------------------

test_result <- tryCatch(
  {
    results <- testthat::test_file(
      file.path(
        "tests",
        "testthat",
        "test-final_qa_phase15.R"
      ),
      reporter = "summary",
      stop_on_failure = FALSE,
      stop_on_warning = FALSE
    )
    
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
    
    if (has_failure(results)) {
      stop(
        "One or more Phase 15 automated expectations failed.",
        call. = FALSE
      )
    }
    
    TRUE
  },
  error = function(e) {
    message(
      "\nPhase 15 automated tests failed:\n",
      conditionMessage(e)
    )
    
    FALSE
  }
)

if (!isTRUE(test_result)) {
  cat("\nPHASE 15 STATUS: FAIL\n")
  
  stop(
    "Phase 15 automated tests failed.",
    call. = FALSE
  )
}

cat("[4/7] Automated Phase 15 tests: PASS\n")

# ------------------------------------------------------------
# Run actual 30-team production QA
# ------------------------------------------------------------

cat("\nRunning 30-team production QA...\n")

qa <- tryCatch(
  phase15_run_30_team_qa(
    season = NULL,
    rotation_size = 10L,
    write_report = TRUE,
    report_dir = "qa"
  ),
  error = function(e) {
    message(
      "\n30-team QA execution failed:\n",
      conditionMessage(e)
    )
    
    NULL
  }
)

if (is.null(qa)) {
  cat("\nPHASE 15 STATUS: FAIL\n")
  
  stop(
    "30-team QA could not be completed.",
    call. = FALSE
  )
}

cat(
  "[5/7] 30-team basketball QA: ",
  qa$summary$teams_passed,
  "/",
  qa$summary$teams_tested,
  " PASS\n",
  sep = ""
)

if (!isTRUE(qa$summary$all_teams_pass)) {
  failed <- qa$teams[
    !qa$teams$overall_pass,
    c(
      "team_name",
      "abbreviation",
      "error"
    ),
    drop = FALSE
  ]
  
  print(
    failed,
    row.names = FALSE
  )
  
  cat("\nPHASE 15 STATUS: FAIL\n")
  
  stop(
    paste0(
      qa$summary$teams_failed,
      " team(s) failed production QA. See qa/phase15_30_team_qa.csv."
    ),
    call. = FALSE
  )
}

# ------------------------------------------------------------
# Architecture audits
# ------------------------------------------------------------

if (!isTRUE(qa$summary$functions_pass)) {
  missing <- qa$functions[
    !qa$functions$loaded,
    ,
    drop = FALSE
  ]
  
  print(
    missing,
    row.names = FALSE
  )
  
  cat("\nPHASE 15 STATUS: FAIL\n")
  
  stop(
    "Required production functions are missing.",
    call. = FALSE
  )
}

cat("[6/7] Required engine/function audit: PASS\n")

if (
  !isTRUE(
    qa$summary$database_pass
  ) ||
  !isTRUE(
    qa$summary$source_files_pass
  )
) {
  if (!qa$summary$database_pass) {
    cat("\nMissing required database tables:\n")
    
    print(
      qa$database[
        !qa$database$present,
        ,
        drop = FALSE
      ],
      row.names = FALSE
    )
  }
  
  if (!qa$summary$source_files_pass) {
    cat("\nMissing required source files:\n")
    
    print(
      qa$source_files[
        !qa$source_files$present,
        ,
        drop = FALSE
      ],
      row.names = FALSE
    )
  }
  
  cat("\nPHASE 15 STATUS: FAIL\n")
  
  stop(
    "Database/source-file freeze audit failed.",
    call. = FALSE
  )
}

cat("[7/7] Database + source freeze audit: PASS\n\n")

if (!isTRUE(qa$summary$overall_pass)) {
  cat("\nPHASE 15 STATUS: FAIL\n")
  
  stop(
    "Phase 15 did not satisfy the complete freeze standard.",
    call. = FALSE
  )
}

cat("QA reports written to: qa/\n\n")

cat("============================================================\n")
cat("PHASE 15 STATUS: PASS\n")
cat("30/30 teams verified. NBA v1.0 is READY TO FREEZE.\n")
cat("============================================================\n")