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

testthat::test_that("UX foundation script is deferred and content-versioned", {
  root <- ux_foundation_project_root()
  ui <- app_ui(NULL)
  rendered <- htmltools::renderTags(ui)
  html <- paste(rendered$head, rendered$html, sep = "\n")
  script_path <- file.path(
    root,
    "inst",
    "app",
    "www",
    "tbi_ux_foundation.js"
  )
  script_version <- unname(tools::md5sum(script_path))
  app_ui_source <- paste(
    readLines(file.path(root, "R", "app_ui.R"), warn = FALSE),
    collapse = "\n"
  )

  testthat::expect_true(
    grepl(
      paste0(
        'src="tbi-assets/tbi_ux_foundation.js?v=',
        script_version,
        '"'
      ),
      html,
      fixed = TRUE
    )
  )
  testthat::expect_true(
    grepl("defer", html, fixed = TRUE)
  )
  testthat::expect_match(
    app_ui_source,
    'includeCSS\\(\\s+app_sys\\("app/www/tbi_ux_foundation[.]css"\\)'
  )
})
