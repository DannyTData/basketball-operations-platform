command_center_html <- function(value) {
  if (is.list(value) && !is.null(value$html)) value$html else paste(as.character(value), collapse = "")
}

testthat::test_that("Command Center assigns one distinct purpose to every workspace", {
  html <- htmltools::renderTags(mod_executive_dashboard_ui("command"))$html
  for (tab in c("decision", "risks", "context")) {
    testthat::expect_match(html, paste0('data-tbi-command-tab="', tab, '"'), fixed = TRUE)
  }
  testthat::expect_match(html, "command-v2-basketball", fixed = TRUE)
  testthat::expect_match(html, "command-bie-priorities", fixed = TRUE)
  testthat::expect_match(html, "command-scenario-delta", fixed = TRUE)
  testthat::expect_match(html, "command-team-context", fixed = TRUE)
  testthat::expect_match(html, "View standings", fixed = TRUE)
  testthat::expect_false(grepl("<details[^>]*open", html))

  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/", mustWork = TRUE)
  js <- paste(readLines(file.path(root, "inst", "app", "www", "tbi_ux_foundation.js"), warn = FALSE), collapse = "\n")
  command_js <- sub("^[\\s\\S]*TBI_COMMAND_CENTER_TABS_START >>>", "", js, perl = TRUE)
  command_js <- sub("<<< TBI_COMMAND_CENTER_TABS_END <<<[\\s\\S]*$", "", command_js, perl = TRUE)
  for (label in c("Executive Home", "Decision Scorecard", "Executive Priorities", "Team Context", "Decision Evidence")) {
    testthat::expect_match(command_js, label, fixed = TRUE)
  }
  testthat::expect_match(command_js, "aria-selected", fixed = TRUE)
  testthat::expect_match(command_js, "aria-controls", fixed = TRUE)
  testthat::expect_match(command_js, "ArrowRight", fixed = TRUE)
  changed_index <- regexpr("var changed", command_js, fixed = TRUE)[[1]]
  mutation_index <- regexpr("data-tbi-command-active-tab', tabName", command_js, fixed = TRUE)[[1]]
  testthat::expect_gt(changed_index, 0L)
  testthat::expect_gt(mutation_index, changed_index)
  testthat::expect_match(command_js, "if (changed)", fixed = TRUE)

  css <- paste(readLines(file.path(root, "inst", "app", "www", "tbi_ux_foundation.css"), warn = FALSE), collapse = "\n")
  polish_start <- "/* >>> TBI_COMMAND_CENTER_PRE_DEMO_POLISH_START >>> */"
  polish_end <- "/* <<< TBI_COMMAND_CENTER_PRE_DEMO_POLISH_END <<< */"
  testthat::expect_match(css, polish_start, fixed = TRUE)
  testthat::expect_match(css, polish_end, fixed = TRUE)
  polish <- sub(paste0("^[\\s\\S]*", polish_start), "", css, perl = TRUE)
  polish <- sub(paste0(polish_end, "[\\s\\S]*$"), "", polish, perl = TRUE)
  testthat::expect_match(polish, ".executive-status-item", fixed = TRUE)
  testthat::expect_match(polish, "grid-template-columns:repeat(4,minmax(0,1fr))", fixed = TRUE)
  testthat::expect_match(polish, "grid-template-columns:repeat(2,minmax(0,1fr))", fixed = TRUE)
  testthat::expect_match(polish, "grid-template-columns:minmax(0,1fr)", fixed = TRUE)
  testthat::expect_match(polish, ".command-v2-basketball .tbi-p3-panel", fixed = TRUE)
  testthat::expect_match(polish, "min-height:0", fixed = TRUE)
})

testthat::test_that("Command Center refreshes executive outputs across four real teams", {
  selected_team <- shiny::reactiveVal("Boston Celtics")
  selected_season <- shiny::reactiveVal("2026-27")

  shiny::testServer(
    mod_executive_dashboard_server,
    args = list(selected_team = selected_team, selected_season = selected_season),
    {
      for (tab in c("scorecard", "risks", "context", "confidence")) session$setInputs(active_subtab = tab)

      seen <- character()
      for (team in c("Boston Celtics", "Oklahoma City Thunder", "Charlotte Hornets", "Cleveland Cavaliers")) {
        selected_team(team)
        session$flushReact()
        testthat::expect_match(output$dashboard_title, team, fixed = TRUE)
        testthat::expect_true(nzchar(command_center_html(output$executive_decision)))
        testthat::expect_match(command_center_html(output$executive_decision), "Decision Evidence", fixed = TRUE)
        testthat::expect_match(command_center_html(output$executive_decision), "Shows how much of this executive recommendation is supported", fixed = TRUE)
        testthat::expect_match(command_center_html(output$executive_decision), "Loaded facts with verified/current evidence.", fixed = TRUE)
        testthat::expect_match(command_center_html(output$executive_decision), "Outputs that depend on explicit modeled assumptions.", fixed = TRUE)
        testthat::expect_match(command_center_html(output$executive_decision), "Loaded evidence that requires source or rule verification.", fixed = TRUE)
        testthat::expect_match(command_center_html(output$executive_decision), "Required information not currently loaded.", fixed = TRUE)
        testthat::expect_match(command_center_html(output$executive_decision), "not a probability or a scouting-confidence score", fixed = TRUE)
        testthat::expect_match(command_center_html(output$command_context_summary), "Record", fixed = TRUE)
        testthat::expect_match(command_center_html(output$command_context_summary), "Competitive tier", fixed = TRUE)
        testthat::expect_true(nzchar(output$outlook_summary))
        seen <- c(seen, paste(team, output$conference_rank, output$team_payroll, sep = ":"))
      }
      testthat::expect_length(unique(seen), 4L)
    }
  )
})

testthat::test_that("Command Center scenario reset restores the baseline", {
  state <- tbi_transaction_state()
  outgoing <- get_roster("Boston Celtics", "2026-27")[1, , drop = FALSE]
  incoming <- get_roster("Atlanta Hawks", "2026-27")[1, , drop = FALSE]
  shiny::isolate(state$publish_trade(
    team = "Boston Celtics",
    partner_team = "Atlanta Hawks",
    season = "2026-27",
    outgoing_players = outgoing,
    incoming_players = incoming
  ))

  shiny::testServer(
    mod_executive_dashboard_server,
    args = list(
      selected_team = shiny::reactive("Boston Celtics"),
      selected_season = shiny::reactive("2026-27"),
      transaction_state = state
    ),
    {
      scenario_html <- command_center_html(output$executive_scenario)
      testthat::expect_match(scenario_html, "SCENARIO", fixed = TRUE)
      state$clear()
      session$flushReact()
      testthat::expect_true(is.null(active_trade_scenario()))
      testthat::expect_true(is.null(output$executive_scenario) || !nzchar(command_center_html(output$executive_scenario)))
      testthat::expect_identical(output$dashboard_title, "Boston Celtics Executive Dashboard")
    }
  )
})
