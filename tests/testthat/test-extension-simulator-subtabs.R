extension_subtabs_project_root <- function() {
  candidates <- c(".", "..", "../..", "../../..")

  for (candidate in candidates) {
    if (file.exists(file.path(candidate, "DESCRIPTION"))) {
      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }
  }

  stop("Could not locate project root from Extension Simulator tests.", call. = FALSE)
}

extension_subtabs_javascript <- function(root) {
  paste(
    readLines(
      file.path(root, "inst", "app", "www", "tbi_ux_foundation.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )
}

extension_subtabs_block <- function(javascript) {
  block <- regmatches(
    javascript,
    regexpr(
      paste0(
        "(?s)",
        "// >>> TBI_EXTENSION_SIMULATOR_TABS_START >>>",
        ".*",
        "// <<< TBI_EXTENSION_SIMULATOR_TABS_END <<<"
      ),
      javascript,
      perl = TRUE
    )
  )

  if (!length(block)) {
    return("")
  }

  block
}

testthat::test_that("Extension Simulator exposes the approved sub-tabs", {
  root <- extension_subtabs_project_root()
  javascript <- extension_subtabs_javascript(root)

  tabs <- c(
    proposal = "Proposal",
    `cba-screen` = "CBA Screen",
    `financial-impact` = "Financial Impact",
    recommendation = "Recommendation"
  )

  for (tab in names(tabs)) {
    definition <- sprintf("['%s', '%s']", tab, tabs[[tab]])
    testthat::expect_true(
      grepl(definition, javascript, fixed = TRUE),
      info = paste("Missing Extension Simulator tab:", tabs[[tab]])
    )
  }

  extension_block <- extension_subtabs_block(javascript)

  testthat::expect_true(nzchar(extension_block))
  testthat::expect_match(
    extension_block,
    "var selected = 'proposal';",
    fixed = TRUE
  )
  testthat::expect_match(
    extension_block,
    "window.TBIUX.register(schedule);",
    fixed = TRUE
  )
})

testthat::test_that("every Extension Simulator section is assigned exactly once", {
  root <- extension_subtabs_project_root()
  module_source <- paste(
    readLines(file.path(root, "R", "mod_extension_simulator.R"), warn = FALSE),
    collapse = "\n"
  )

  pattern <- paste0(
    "`data-tbi-extension-section`\\s*=\\s*\"([^\"]+)\"\\s*,\\s*",
    "`data-tbi-extension-tab`\\s*=\\s*\"([^\"]+)\""
  )
  locations <- gregexpr(pattern, module_source, perl = TRUE)[[1]]
  matches <- regmatches(module_source, list(locations))[[1]]

  sections <- sub(pattern, "\\1", matches, perl = TRUE)
  assigned_tabs <- sub(pattern, "\\2", matches, perl = TRUE)
  actual <- stats::setNames(assigned_tabs, sections)

  expected <- c(
    `player-eligibility` = "proposal",
    `proposal-builder` = "proposal",
    `extension-decision` = "cba-screen",
    `cba-extension-scorecard` = "cba-screen",
    `cba-review-items` = "cba-screen",
    `proposed-extension-schedule` = "financial-impact",
    `front-office-readout` = "financial-impact",
    `contract-risks` = "recommendation",
    `negotiation-opportunities` = "recommendation",
    `bie-extension-value-timeline` = "recommendation",
    `recommended-contract-action` = "recommendation"
  )

  testthat::expect_length(matches, length(expected))
  testthat::expect_false(anyDuplicated(sections) > 0L)
  testthat::expect_setequal(names(actual), names(expected))
  testthat::expect_equal(actual[names(expected)], expected)
})

testthat::test_that("Extension Snapshot remains outside all tab targets", {
  ui <- mod_extension_simulator_ui("extension_snapshot_test")
  html <- htmltools::renderTags(ui)$html
  snapshot_opening_tag <- regmatches(
    html,
    regexpr(
      '<section class="tbi-v2-exec-snapshot"[^>]*>',
      html,
      perl = TRUE
    )
  )

  testthat::expect_length(snapshot_opening_tag, 1L)
  testthat::expect_false(
    grepl("data-tbi-extension-tab", snapshot_opening_tag, fixed = TRUE)
  )
})

testthat::test_that("Extension Simulator keeps every existing Shiny ID", {
  ui <- mod_extension_simulator_ui("extension_ids_test")
  html <- htmltools::renderTags(ui)$html

  expected_ids <- c(
    "snapshot_player",
    "snapshot_current_salary",
    "snapshot_extension_type",
    "snapshot_starting_salary",
    "snapshot_total_value",
    "snapshot_result",
    "player_id",
    "player_card",
    "service_years",
    "remaining_years",
    "is_first_round_pick",
    "rookie_options_exercised",
    "timing_window_open",
    "designated_rookie_qualified",
    "designated_veteran_qualified",
    "original_team_requirement_met",
    "extension_type",
    "guarantee_structure",
    "starting_salary_m",
    "years",
    "raise_pct",
    "first_season",
    "builder_max_start",
    "builder_max_raise",
    "builder_max_years",
    "builder_room_below_max",
    "extension_decision",
    "scorecard_result",
    "extension_scorecard",
    "extension_schedule",
    "contract_readout",
    "extension_alerts",
    "extension_risks",
    "extension_opportunities",
    "bie_extension_value",
    "executive_recommendation"
  )

  for (shiny_id in expected_ids) {
    namespaced_id <- sprintf('id="extension_ids_test-%s"', shiny_id)
    testthat::expect_true(
      grepl(namespaced_id, html, fixed = TRUE),
      info = paste("Missing Extension Simulator Shiny ID:", shiny_id)
    )
  }
})

testthat::test_that("Extension tab switching keeps Proposal controls mounted", {
  root <- extension_subtabs_project_root()
  javascript <- extension_subtabs_javascript(root)
  extension_block <- extension_subtabs_block(javascript)

  testthat::expect_match(
    extension_block,
    "target.classList.toggle('tbi-extension-hidden', !isActive);",
    fixed = TRUE
  )
  testthat::expect_false(grepl("removeChild", extension_block, fixed = TRUE))
  testthat::expect_false(grepl("replaceChildren", extension_block, fixed = TRUE))
  testthat::expect_false(grepl(".remove()", extension_block, fixed = TRUE))
  testthat::expect_false(
    grepl("innerHTML", extension_block, fixed = TRUE)
  )
})

testthat::test_that("Extension tabs reuse the observer and resize hidden reactable", {
  root <- extension_subtabs_project_root()
  javascript <- extension_subtabs_javascript(root)
  extension_block <- extension_subtabs_block(javascript)

  testthat::expect_equal(
    lengths(regmatches(javascript, gregexpr("new MutationObserver", javascript, fixed = TRUE))),
    1L
  )
  testthat::expect_match(
    extension_block,
    "window.dispatchEvent(new Event('resize'));",
    fixed = TRUE
  )
})
