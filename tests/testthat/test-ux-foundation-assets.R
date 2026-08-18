ux_foundation_project_root <- function() {
  candidates <- c(".", "..", "../..", "../../..")

  for (candidate in candidates) {
    if (file.exists(file.path(candidate, "DESCRIPTION"))) {
      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }
  }

  stop("Could not locate project root from UX foundation tests.", call. = FALSE)
}

testthat::test_that("UX foundation uses one shared DOM observer", {
  root <- ux_foundation_project_root()
  javascript <- readLines(
    file.path(root, "inst", "app", "www", "tbi_ux_foundation.js"),
    warn = FALSE
  )

  testthat::expect_equal(
    sum(grepl("new MutationObserver", javascript, fixed = TRUE)),
    1L
  )
  testthat::expect_true(
    any(grepl("window.TBIUX.register", javascript, fixed = TRUE))
  )
})

testthat::test_that("UX foundation script is deferred", {
  root <- ux_foundation_project_root()
  app_ui_source <- readLines(file.path(root, "R", "app_ui.R"), warn = FALSE)
  source_text <- paste(app_ui_source, collapse = "\n")

  testthat::expect_match(
    source_text,
    'src = "tbi-assets/tbi_ux_foundation[.]js",\\s+defer = NA'
  )
  testthat::expect_match(
    source_text,
    'includeCSS\\(\\s+app_sys\\("app/www/tbi_ux_foundation[.]css"\\)'
  )
})
