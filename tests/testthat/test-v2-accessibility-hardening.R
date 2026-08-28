testthat::test_that("global team and season selectors have accessible labels", {
  html <- htmltools::renderTags(app_ui())$html

  testthat::expect_match(html, '<label[^>]*for="selected_team"', perl = TRUE)
  testthat::expect_match(html, '<label[^>]*for="selected_season"', perl = TRUE)
  testthat::expect_match(html, ">ORGANIZATION</label>", fixed = TRUE)
  testthat::expect_match(html, ">SEASON</label>", fixed = TRUE)
})

testthat::test_that("hardened tab systems expose keyboard and ARIA contracts", {
  root <- normalizePath(testthat::test_path("..", ".."), winslash = "/", mustWork = TRUE)
  js <- paste(readLines(file.path(root, "inst", "app", "www", "tbi_ux_foundation.js"), warn = FALSE), collapse = "\n")

  command_start <- regexpr("// >>> TBI_COMMAND_CENTER_TABS_START >>>", js, fixed = TRUE)[[1]]
  command_end <- regexpr("// <<< TBI_COMMAND_CENTER_TABS_END <<<", js, fixed = TRUE)[[1]]
  extension_start <- regexpr("// >>> TBI_EXTENSION_SIMULATOR_TABS_START >>>", js, fixed = TRUE)[[1]]
  extension_end <- regexpr("// <<< TBI_EXTENSION_SIMULATOR_TABS_END <<<", js, fixed = TRUE)[[1]]
  testthat::expect_gt(command_start, 0L)
  testthat::expect_gt(extension_start, 0L)

  command <- substr(js, command_start, command_end)
  extension <- substr(js, extension_start, extension_end)
  for (block in list(command, extension)) {
    testthat::expect_match(block, "role', 'tablist", fixed = TRUE)
    testthat::expect_match(block, "role', 'tab", fixed = TRUE)
    testthat::expect_match(block, "role', 'tabpanel", fixed = TRUE)
    testthat::expect_match(block, "aria-selected", fixed = TRUE)
    testthat::expect_match(block, "aria-controls", fixed = TRUE)
    testthat::expect_match(block, "aria-labelledby", fixed = TRUE)
    testthat::expect_match(block, "ArrowRight", fixed = TRUE)
    testthat::expect_match(block, "ArrowLeft", fixed = TRUE)
    testthat::expect_match(block, "Home", fixed = TRUE)
    testthat::expect_match(block, "End", fixed = TRUE)
  }
  testthat::expect_match(command, "Command Center sections", fixed = TRUE)
  testthat::expect_match(extension, "Extension Simulator sections", fixed = TRUE)
})

testthat::test_that("mobile tabs and decision evidence meet bounded legibility floors", {
  root <- normalizePath(testthat::test_path("..", ".."), winslash = "/", mustWork = TRUE)
  foundation_css <- paste(readLines(file.path(root, "inst", "app", "www", "tbi_ux_foundation.css"), warn = FALSE), collapse = "\n")
  app_css <- paste(readLines(file.path(root, "inst", "app", "www", "tbi_v2.css"), warn = FALSE), collapse = "\n")
  phase3_css <- paste(readLines(file.path(root, "inst", "app", "www", "tbi_phase3.css"), warn = FALSE), collapse = "\n")

  testthat::expect_match(foundation_css, ".tbi-command-subtab:focus-visible", fixed = TRUE)
  testthat::expect_match(foundation_css, ".tbi-extension-subtab:focus-visible", fixed = TRUE)
  testthat::expect_match(foundation_css, "min-height:44px !important", fixed = TRUE)
  testthat::expect_match(app_css, ".tbi-v2-header .tbi-v2-filter-label", fixed = TRUE)
  testthat::expect_match(app_css, "min-height: 44px !important", fixed = TRUE)
  testthat::expect_match(phase3_css, ".tbi-p3-evidence summary", fixed = TRUE)
  testthat::expect_match(phase3_css, "font-size:.6875rem", fixed = TRUE)
  testthat::expect_match(phase3_css, "font-size:.625rem", fixed = TRUE)
})
