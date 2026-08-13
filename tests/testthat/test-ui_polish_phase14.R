# ============================================================
# Phase 14 Tests: Final UI / UX Polish
# ============================================================

phase14_test_project_root <- function() {
  candidates <- c(
    ".",
    "..",
    "../..",
    "../../.."
  )
  
  for (candidate in candidates) {
    marker <- file.path(
      candidate,
      "DESCRIPTION"
    )
    
    if (file.exists(marker)) {
      return(
        normalizePath(
          candidate,
          winslash = "/",
          mustWork = TRUE
        )
      )
    }
  }
  
  stop(
    "Could not locate project root from Phase 14 tests.",
    call. = FALSE
  )
}


testthat::test_that(
  "Phase 14 contract is stable",
  {
    contract <- phase14_ui_contract()
    
    testthat::expect_equal(
      contract$stylesheet,
      "tbi_phase14.css"
    )
    
    testthat::expect_equal(
      contract$shell_class,
      "tbi-phase14-shell"
    )
    
    testthat::expect_equal(
      contract$version,
      "TBI_UI_PHASE14_v1"
    )
  }
)


testthat::test_that(
  "Phase 14 stylesheet contains required UI states",
  {
    root <- phase14_test_project_root()
    
    path <- file.path(
      root,
      "inst",
      "app",
      "www",
      "tbi_phase14.css"
    )
    
    result <-
      phase14_validate_stylesheet(
        path
      )
    
    testthat::expect_true(
      result$ok,
      info = paste(
        result$missing,
        collapse = ", "
      )
    )
  }
)


testthat::test_that(
  "Phase 14 includes responsive and accessibility markers",
  {
    markers <-
      phase14_required_css_markers()
    
    testthat::expect_true(
      any(
        grepl(
          "1440",
          markers,
          fixed = TRUE
        )
      )
    )
    
    testthat::expect_true(
      any(
        grepl(
          "reduced-motion",
          markers,
          fixed = TRUE
        )
      )
    )
  }
)