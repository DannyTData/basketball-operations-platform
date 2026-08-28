team_overview_rendered_html <- function(value) {
  if (is.list(value) && !is.null(value$html)) value$html else as.character(value)
}

testthat::test_that("Team Overview exposes six purposeful compact workspaces", {
  html <- htmltools::renderTags(mod_team_overview_ui("team_v2"))$html

  for (tab in c("overview", "decision", "profile", "risk", "personnel", "recommendation")) {
    testthat::expect_match(html, paste0('data-tbi-team-tab="', tab, '"'), fixed = TRUE)
  }

  testthat::expect_match(html, "team-v2-overview-grid", fixed = TRUE)
  testthat::expect_match(html, "View full conference standings", fixed = TRUE)
  testthat::expect_match(html, "team-v2-rec-brief", fixed = TRUE)

  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/", mustWork = TRUE)
  css <- paste(readLines(file.path(root, "inst", "app", "www", "tbi_ux_foundation.css"), warn = FALSE), collapse = "\n")
  javascript <- paste(readLines(file.path(root, "inst", "app", "www", "tbi_ux_foundation.js"), warn = FALSE), collapse = "\n")
  team_css <- sub("^[\\s\\S]*TBI_TEAM_OVERVIEW_TABS_START >>>", "", css, perl = TRUE)
  team_css <- sub("<<< TBI_TEAM_OVERVIEW_TABS_END <<<[\\s\\S]*$", "", team_css, perl = TRUE)
  team_js <- sub("^[\\s\\S]*TBI_TEAM_OVERVIEW_TABS_START >>>", "", javascript, perl = TRUE)
  team_js <- sub("<<< TBI_TEAM_OVERVIEW_TABS_END <<<[\\s\\S]*$", "", team_js, perl = TRUE)

  testthat::expect_match(team_css, 'data-tbi-team-active-tab="overview"', fixed = TRUE)
  testthat::expect_match(team_css, "repeat(3,minmax(0,1fr))", fixed = TRUE)
  testthat::expect_match(team_js, "data-tbi-team-active-tab", fixed = TRUE)
  testthat::expect_match(team_js, "aria-selected", fixed = TRUE)
  testthat::expect_match(team_js, "tabindex", fixed = TRUE)
  testthat::expect_match(team_js, "aria-controls", fixed = TRUE)
  testthat::expect_match(team_js, "ArrowRight", fixed = TRUE)
})

testthat::test_that("Team Overview refreshes every workspace across four real teams", {
  selected_team <- shiny::reactiveVal("Boston Celtics")
  selected_season <- shiny::reactiveVal("2026-27")

  shiny::testServer(
    mod_team_overview_server,
    args = list(selected_team = selected_team, selected_season = selected_season),
    {
      for (tab in c("decision", "profile", "risk", "personnel", "recommendation")) {
        session$setInputs(active_subtab = tab)
      }

      seen <- character()
      for (team in c("Boston Celtics", "Oklahoma City Thunder", "Charlotte Hornets", "Cleveland Cavaliers")) {
        selected_team(team)
        session$flushReact()

        testthat::expect_identical(output$team_name, team)
        testthat::expect_true(nzchar(team_overview_rendered_html(output$organization_readout)))
        testthat::expect_equal(length(gregexpr("tbi-v2-score-row", team_overview_rendered_html(output$organization_scorecard), fixed = TRUE)[[1L]]), 6L)
        testthat::expect_true(nzchar(output$profile_win_pct))
        testthat::expect_true(nzchar(team_overview_rendered_html(output$team_risks)))
        testthat::expect_true(nzchar(team_overview_rendered_html(output$team_opportunities)))
        testthat::expect_match(team_overview_rendered_html(output$team_context_summary), "Record", fixed = TRUE)
        testthat::expect_match(team_overview_rendered_html(output$team_recommendation), team, fixed = TRUE)
        seen <- c(seen, paste(team, output$snapshot_record, output$organization_score, sep = ":"))
      }

      testthat::expect_length(unique(seen), 4L)
    }
  )
})

testthat::test_that("Team Overview scenario reset restores authoritative state", {
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
    mod_team_overview_server,
    args = list(
      selected_team = shiny::reactive("Boston Celtics"),
      selected_season = shiny::reactive("2026-27"),
      transaction_state = state
    ),
    {
      testthat::expect_false(is.null(active_trade_scenario()))
      testthat::expect_match(team_overview_rendered_html(output$team_trade_scenario_banner), "TRADE SCENARIO", fixed = TRUE)
      state$clear()
      session$flushReact()
      testthat::expect_true(is.null(active_trade_scenario()))
      testthat::expect_true(is.null(output$team_trade_scenario_banner) || !nzchar(team_overview_rendered_html(output$team_trade_scenario_banner)))
      testthat::expect_setequal(roster_contracts()$player_id, base_roster_contracts()$player_id)
    }
  )
})
