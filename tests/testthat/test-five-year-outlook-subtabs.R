five_year_outlook_project_root <- function() {
  candidates <- c(".", "..", "../..", "../../..")

  for (candidate in candidates) {
    if (file.exists(file.path(candidate, "DESCRIPTION"))) {
      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }
  }

  stop("Could not locate project root from Five-Year Outlook tests.", call. = FALSE)
}

testthat::test_that("Five-Year Outlook exposes the approved sub-tabs", {
  root <- five_year_outlook_project_root()
  javascript <- paste(
    readLines(
      file.path(root, "inst", "app", "www", "tbi_ux_foundation.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  tabs <- c(
    overview = "Overview",
    flexibility = "Flexibility",
    timeline = "Timeline",
    `contracts-free-agency` = "Contracts & Free Agency",
    `draft-optionality` = "Draft & Optionality",
    recommendation = "Recommendation"
  )

  for (tab in names(tabs)) {
    definition <- sprintf("['%s', '%s']", tab, tabs[[tab]])
    testthat::expect_true(
      grepl(definition, javascript, fixed = TRUE),
      info = paste("Missing Five-Year Outlook tab:", tabs[[tab]])
    )
  }

  testthat::expect_true(
    grepl("window.TBIUX.register(schedule);", javascript, fixed = TRUE)
  )
})

testthat::test_that("every Five-Year Outlook section is assigned exactly once", {
  root <- five_year_outlook_project_root()
  module_source <- paste(
    readLines(file.path(root, "R", "mod_five_year_outlook.R"), warn = FALSE),
    collapse = "\n"
  )

  pattern <- paste0(
    "`data-tbi-outlook-section`\\s*=\\s*\"([^\"]+)\"\\s*,\\s*",
    "`data-tbi-outlook-tab`\\s*=\\s*\"([^\"]+)\""
  )
  locations <- gregexpr(pattern, module_source, perl = TRUE)[[1]]
  matches <- regmatches(module_source, list(locations))[[1]]

  sections <- sub(pattern, "\\1", matches, perl = TRUE)
  assigned_tabs <- sub(pattern, "\\2", matches, perl = TRUE)
  actual <- stats::setNames(assigned_tabs, sections)

  expected <- c(
    `long-range-snapshot` = "overview",
    `decision-and-scorecard` = "flexibility",
    `organizational-timeline` = "timeline",
    `outlook-headlines` = "overview",
    `long-range-risks` = "draft-optionality",
    `flexibility-opportunities` = "draft-optionality",
    `contract-runway` = "contracts-free-agency",
    `front-office-readout` = "recommendation",
    `executive-recommendation` = "recommendation"
  )

  testthat::expect_length(matches, length(expected))
  testthat::expect_false(anyDuplicated(sections) > 0L)
  testthat::expect_setequal(names(actual), names(expected))
  testthat::expect_equal(actual[names(expected)], expected)
})

testthat::test_that("Five-Year Outlook keeps every existing Shiny output ID", {
  ui <- mod_five_year_outlook_ui("five_year_test")
  html <- htmltools::renderTags(ui)$html

  expected_ids <- c(
    "outlook_trade_scenario_banner",
    "snapshot_current_payroll",
    "snapshot_year3_payroll",
    "snapshot_near_term_fa",
    "snapshot_team_options",
    "snapshot_draft_value",
    "snapshot_flexibility",
    "flexibility_score",
    "outlook_decision",
    "strategy_scorecard",
    "five_year_timeline",
    "outlook_headlines",
    "outlook_risks",
    "outlook_opportunities",
    "contract_runway_table",
    "outlook_readout",
    "outlook_recommendation"
  )

  for (output_id in expected_ids) {
    namespaced_id <- sprintf('id="five_year_test-%s"', output_id)
    testthat::expect_true(
      grepl(namespaced_id, html, fixed = TRUE),
      info = paste("Missing Five-Year Outlook Shiny ID:", output_id)
    )
  }
})
