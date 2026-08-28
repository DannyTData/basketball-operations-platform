player_management_rendered_html <- function(value) {
  if (is.list(value) && !is.null(value$html)) {
    return(value$html)
  }

  htmltools::renderTags(value)$html
}

testthat::test_that("Player Management exposes five explicit compact workspaces", {
  html <- htmltools::renderTags(mod_player_manager_ui("player"))$html

  for (tab in c("overview", "value", "development", "contract", "recommendation")) {
    testthat::expect_match(html, paste0('data-tbi-pm-tab="', tab, '"'), fixed = TRUE)
  }

  for (class_name in c(
    "pi-overview-performance", "pi-overview-role", "pi-overview-summary",
    "pi-value-workspace", "pi-development-current", "pi-development-timeline",
    "pi-development-insights", "pi-contract-timeline", "pi-contract-cba",
    "pi-recommendation-panel", "pi-recommendation-decision",
    "pi-recommendation-columns", "pi-recommendation-value",
    "pi-recommendation-control", "pi-recommendation-risk"
  )) {
    testthat::expect_match(html, class_name, fixed = TRUE)
  }

  testthat::expect_match(html, "CURRENT", fixed = TRUE)
  testthat::expect_match(html, 'id="player-projection_current"', fixed = TRUE)
  testthat::expect_match(html, 'id="player-recommendation_evidence_state"', fixed = TRUE)
  testthat::expect_match(html, 'id="player-recommendation_value_brief"', fixed = TRUE)
  testthat::expect_match(html, 'id="player-recommendation_control_brief"', fixed = TRUE)

  normalized_css <- gsub("\\s+", " ", html)
  testthat::expect_match(
    normalized_css,
    "data-tbi-pm-active-tab='overview'[^}]+grid-template-columns:[^}]+1[.]08fr[^}]+1[.]08fr",
    perl = TRUE
  )
  testthat::expect_match(
    normalized_css,
    "data-tbi-pm-active-tab='contract'[^}]+grid-template-columns:[^}]+1[.]08fr[^}]+[.]92fr",
    perl = TRUE
  )
  testthat::expect_match(normalized_css, "@media (max-width: 1000px)", fixed = TRUE)
  testthat::expect_match(normalized_css, "@media (max-width: 680px)", fixed = TRUE)
  testthat::expect_match(
    normalized_css,
    "@media \\(max-width: 1100px\\) \\{.*?data-tbi-pm-active-tab='development'.*?grid-template-columns:minmax\\(0,1fr\\)",
    perl = TRUE
  )
  testthat::expect_match(
    normalized_css,
    "@media \\(max-width: 1000px\\) \\{.*?[.]pi-recommendation-columns \\{ grid-template-columns:repeat\\(2,minmax\\(0,1fr\\)\\)",
    perl = TRUE
  )
  testthat::expect_match(
    normalized_css,
    "@media \\(max-width: 680px\\) \\{.*?[.]pi-recommendation-columns \\{ grid-template-columns:1fr",
    perl = TRUE
  )
  testthat::expect_match(
    normalized_css,
    "pi-projection-grid { grid-template-columns:repeat(2,minmax(0,1fr))",
    fixed = TRUE
  )
  testthat::expect_match(
    normalized_css,
    "[.]pi-recommendation-columns \\{[^}]+grid-template-columns: repeat\\(3,minmax\\(0,1fr\\)\\)",
    perl = TRUE
  )
  testthat::expect_match(
    normalized_css,
    "[.]pi-page \\{[^}]+gap: 8px !important",
    perl = TRUE
  )
})

testthat::test_that("Player Management browser controller owns stable tab state", {
  js_path <- testthat::test_path("..", "..", "inst", "app", "www", "tbi_ux_foundation.js")
  js <- paste(readLines(js_path, warn = FALSE), collapse = "\n")

  testthat::expect_match(js, "TBI_PM_VERSION = '2.0.0'", fixed = TRUE)
  testthat::expect_match(js, "data-tbi-pm-active-tab", fixed = TRUE)
  testthat::expect_match(js, "panel.getAttribute('data-tbi-pm-tab')", fixed = TRUE)
  testthat::expect_match(js, "aria-selected", fixed = TRUE)
})

testthat::test_that("Player Management updates every decision view across players and teams", {
  team <- shiny::reactiveVal("Boston Celtics")
  season <- shiny::reactiveVal("2026-27")
  captured <- new.env(parent = emptyenv())
  captured$seen <- character()
  captured$briefs <- character()

  shiny::testServer(
    mod_player_manager_server,
    args = list(
      selected_team = team,
      selected_season = season,
      transaction_state = NULL
    ),
    {
      for (tab in c("value", "development", "contract", "recommendation")) {
        session$setInputs(active_subtab = tab)
      }

      for (next_team in c(
        "Boston Celtics",
        "Brooklyn Nets",
        "Cleveland Cavaliers",
        "Boston Celtics"
      )) {
        team(next_team)
        session$flushReact()
        pool <- player_pool()
        testthat::expect_gte(nrow(pool), 3L)

        player_indexes <- if (identical(next_team, "Boston Celtics") && !length(captured$seen)) 1:3 else 1L
        for (i in player_indexes) {
          session$setInputs(selected_player = as.character(pool$player_id[[i]]))

          testthat::expect_identical(output$player_name, pool$player_name[[i]])
          testthat::expect_false(grepl("#", output$player_subtitle, fixed = TRUE))
          testthat::expect_identical(output$jersey_number, "UNKNOWN")
          testthat::expect_true(nzchar(output$current_role))
          testthat::expect_true(nzchar(output$summary_position))
          testthat::expect_true(nzchar(output$bie_player_score))
          testthat::expect_true(nzchar(output$projection_current))
          testthat::expect_true(nzchar(output$projection_1y))
          testthat::expect_true(nzchar(output$projection_3y))
          testthat::expect_true(nzchar(output$trajectory))
          cba_html <- player_management_rendered_html(output$cba_flags)
          testthat::expect_true(nzchar(cba_html))
          if (is.na(pool$contract_type[[i]]) || !nzchar(trimws(pool$contract_type[[i]]))) {
            testthat::expect_false(grepl("Veteran Extension", cba_html, fixed = TRUE))
            testthat::expect_identical(
              output$contract_type,
              "UNKNOWN / REQUIRES SOURCE VERIFICATION"
            )
            testthat::expect_match(
              output$recommendation_evidence_state,
              "REVIEW",
              fixed = TRUE
            )
          }
          if (is.na(pool$option_type[[i]]) || !nzchar(trimws(pool$option_type[[i]]))) {
            testthat::expect_match(cba_html, "Unknown", fixed = TRUE)
            testthat::expect_identical(
              output$option_type,
              "UNKNOWN / REQUIRES SOURCE VERIFICATION"
            )
          }
          if (is.na(pool$free_agent_year[[i]])) {
            testthat::expect_identical(
              output$free_agent_year,
              "UNKNOWN / REQUIRES SOURCE VERIFICATION"
            )
            testthat::expect_identical(
              output$years_remaining,
              "UNKNOWN / REQUIRES SOURCE VERIFICATION"
            )
          }
          testthat::expect_true(nzchar(output$recommendation))
          recommendation_html <- player_management_rendered_html(output$recommendation_rationale)
          testthat::expect_true(nzchar(recommendation_html))
          evidence_state <- output$recommendation_evidence_state
          value_brief <- player_management_rendered_html(output$recommendation_value_brief)
          control_brief <- player_management_rendered_html(output$recommendation_control_brief)
          testthat::expect_true(nzchar(evidence_state))
          testthat::expect_match(value_brief, output$current_role, fixed = TRUE)
          testthat::expect_match(value_brief, output$bie_player_score, fixed = TRUE)
          testthat::expect_match(
            value_brief,
            sub(" .*", "", output$performance_season_label),
            fixed = TRUE
          )
          if (grepl("LATEST AVAILABLE", output$performance_season_label, fixed = TRUE)) {
            testthat::expect_match(value_brief, "LATEST AVAILABLE", fixed = TRUE)
          }
          testthat::expect_match(control_brief, output$trajectory, fixed = TRUE)
          if (
            is.na(pool$contract_type[[i]]) ||
              !nzchar(trimws(pool$contract_type[[i]])) ||
              is.na(pool$free_agent_year[[i]])
          ) {
            testthat::expect_match(
              control_brief,
              "REQUIRES SOURCE VERIFICATION",
              fixed = TRUE
            )
          }
          contract_watch <- output$recommendation_contract_watch
          testthat::expect_match(contract_watch, "Option:", fixed = TRUE)
          testthat::expect_match(contract_watch, "Free agency:", fixed = TRUE)
          captured$seen <- c(
            captured$seen,
            paste(
              next_team,
              output$player_name,
              output$recommendation,
              recommendation_html,
              value_brief,
              control_brief,
              sep = ":"
            )
          )
          captured$briefs <- c(
            captured$briefs,
            paste(
              output$recommendation,
              evidence_state,
              value_brief,
              control_brief,
              sep = ":"
            )
          )
        }
      }
    }
  )

  testthat::expect_gte(length(unique(captured$seen)), 5L)
  testthat::expect_gte(length(unique(captured$briefs)), 2L)
})

testthat::test_that("Player Management keeps missing contract evidence UNKNOWN and REVIEW", {
  production_db_get_query <- get("dbGetQuery", envir = asNamespace("DBI"))

  testthat::local_mocked_bindings(
    dbGetQuery = function(...) {
      result <- production_db_get_query(...)

      if (
        is.data.frame(result) &&
          nrow(result) &&
          "contract_type" %in% names(result)
      ) {
        result$contract_type[[1L]] <- NA_character_
      }

      result
    },
    .package = "DBI"
  )

  team <- shiny::reactiveVal("Boston Celtics")
  season <- shiny::reactiveVal("2026-27")

  shiny::testServer(
    mod_player_manager_server,
    args = list(
      selected_team = team,
      selected_season = season,
      transaction_state = NULL
    ),
    {
      session$setInputs(active_subtab = "recommendation")
      roster <- player_pool()
      session$setInputs(selected_player = as.character(roster$player_id[[1L]]))

      testthat::expect_identical(
        output$contract_type,
        "UNKNOWN / REQUIRES SOURCE VERIFICATION"
      )
      testthat::expect_match(output$recommendation_evidence_state, "REVIEW", fixed = TRUE)
      testthat::expect_match(
        player_management_rendered_html(output$recommendation_control_brief),
        "REQUIRES SOURCE VERIFICATION",
        fixed = TRUE
      )
    }
  )
})

testthat::test_that("Player Management scenario reset restores the authoritative player pool", {
  team <- shiny::reactiveVal("Boston Celtics")
  season <- shiny::reactiveVal("2026-27")
  state <- tbi_transaction_state()

  shiny::testServer(
    mod_player_manager_server,
    args = list(
      selected_team = team,
      selected_season = season,
      transaction_state = state
    ),
    {
      session$flushReact()
      baseline <- player_pool()
      outgoing <- baseline[1L, , drop = FALSE]
      incoming <- get_depth_chart_records("Cleveland Cavaliers", "2026-27")[1L, , drop = FALSE]
      session$setInputs(
        active_subtab = "recommendation",
        selected_player = as.character(outgoing$player_id[[1]])
      )
      baseline_name <- output$player_name
      baseline_recommendation <- paste(
        output$recommendation,
        output$recommendation_evidence_state,
        player_management_rendered_html(output$recommendation_value_brief),
        player_management_rendered_html(output$recommendation_control_brief),
        output$key_risk,
        output$key_opportunity,
        player_management_rendered_html(output$recommended_actions),
        sep = "\n"
      )

      shiny::isolate(state$publish_trade(
        team = "Boston Celtics",
        partner_team = "Cleveland Cavaliers",
        season = "2026-27",
        outgoing_players = outgoing,
        incoming_players = incoming
      ))
      session$flushReact()

      preview <- player_pool()
      testthat::expect_false(outgoing$player_id[[1]] %in% preview$player_id)
      testthat::expect_true(incoming$player_id[[1]] %in% preview$player_id)
      testthat::expect_match(
        player_management_rendered_html(output$player_trade_scenario_banner),
        "TRADE SCENARIO",
        fixed = TRUE
      )

      team("Oklahoma City Thunder")
      session$flushReact()
      testthat::expect_null(active_trade_scenario())
      testthat::expect_false(grepl(
        "TRADE SCENARIO",
        player_management_rendered_html(output$player_trade_scenario_banner),
        fixed = TRUE
      ))

      team("Boston Celtics")
      session$flushReact()
      testthat::expect_true(is.list(active_trade_scenario()))

      state$clear()
      session$flushReact()
      restored <- player_pool()
      testthat::expect_identical(sort(restored$player_id), sort(baseline$player_id))
      testthat::expect_identical(output$player_name, baseline_name)
      testthat::expect_identical(
        paste(
          output$recommendation,
          output$recommendation_evidence_state,
          player_management_rendered_html(output$recommendation_value_brief),
          player_management_rendered_html(output$recommendation_control_brief),
          output$key_risk,
          output$key_opportunity,
          player_management_rendered_html(output$recommended_actions),
          sep = "\n"
        ),
        baseline_recommendation
      )
      testthat::expect_null(active_trade_scenario())
      testthat::expect_false(grepl(
        "TRADE SCENARIO",
        player_management_rendered_html(output$player_trade_scenario_banner),
        fixed = TRUE
      ))
    }
  )
})
