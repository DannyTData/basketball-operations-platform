cap_market_fixture <- function() {
  data.frame(
    player_id = 1:5,
    player_name = c("Center One", "Guard Two", "Center Three", "Wing Four", "Center Five"),
    primary_position = c("C", "PG", "PF, C", "SF", "C"),
    player_age = c(27, 25, 30, 23, 28),
    team_name = c("Boston Celtics", "Cleveland Cavaliers", "Charlotte Hornets", "Boston Celtics", "Oklahoma City Thunder"),
    cap_hit = c(5000000, 9000000, 13000000, 4000000, 12000000),
    contract_type = c("Standard", "Standard", "Standard", "Standard", "Standard"),
    contract_end_season = c("2026-27", "2027-28", "2026-27", NA, "2026-27"),
    free_agent_year = c(2027, 2028, 2027, 2029, 2027),
    free_agent_status = c("UFA", "RFA", "UFA", NA, "UFA"),
    option_type = c(NA, NA, "Player Option", NA, NA),
    verified_at = c("2026-08-01", "2026-08-01", "2026-08-01", NA, "2026-08-01"),
    bie = c(71, 82, 65, NA, 76),
    bie_grade = c("B", "A", "C", "UNKNOWN", "B"),
    bie_season = c("2025-26", "2025-26", "2025-26", NA, "2025-26"),
    bie_confidence = c("HIGH", "HIGH", "MEDIUM", NA, "HIGH"),
    stringsAsFactors = FALSE
  )
}

test_that("Cap market defaults to the nearest loaded upcoming year", {
  expect_identical(cap_market_default_year(c(2030, 2028, 2027), "2026-27"), "2027")
  expect_identical(cap_market_default_year(c(2024, 2025), "2026-27"), "2025")
})

test_that("Cap market derives a governed comparison season and one decision vocabulary", {
  expect_identical(cap_market_performance_season("2026-27"), "2025-26")
  expect_true(is.na(cap_market_performance_season("UNKNOWN")))
  expect_identical(cap_financial_decision_state("Above Second Apron")$decision, "RESTRICTED")
  expect_identical(cap_financial_decision_state("Below Cap")$decision, "FLEXIBLE")
})

test_that("Cap TPE presentation never turns missing authority into a zero", {
  expect_identical(cap_tpe_display_value(NA_integer_), "TPE data not loaded")
  expect_identical(cap_tpe_display_value(0L), "0")
  expect_identical(cap_tpe_display_value(2L), "2")
})

test_that("Cap feedback consumers never present an unverified TPE count", {
  withr::local_envvar(TBI_ENABLE_TPE_TEST_MODE = "true")
  teams <- shiny::reactiveVal("Boston Celtics")
  season <- shiny::reactiveVal("2026-27")
  source_db <- normalizePath(
    testthat::test_path("..", "..", "inst", "database", "tbi.sqlite"),
    winslash = "/",
    mustWork = TRUE
  )

  shiny::testServer(
    mod_salary_cap_server,
    args = list(selected_team = teams, selected_season = season),
    {
      rendered <- function(value) paste(unlist(value), collapse = " ")
      disposable_db <- tbi_prepare_feedback_session(session, source_db)
      stopifnot(
        file.exists(disposable_db),
        !identical(disposable_db, source_db),
        identical(resolve_tbi_db_path(), disposable_db)
      )
      session$flushReact()

      expect_identical(rendered(output$active_tpes), "TPE data not loaded")
      expect_match(rendered(output$overview_flexibility), "TPE data not loaded", fixed = TRUE)
      expect_match(rendered(output$cap_signals), "TPE data not loaded", fixed = TRUE)

      count_claims <- paste(
        rendered(output$cba_alerts),
        rendered(output$financial_opportunities)
      )
      expect_false(grepl("active trade exception", count_claims, fixed = TRUE))
      expect_false(grepl("0 TPE", count_claims, fixed = TRUE))
    }
  )
})

test_that("Cap market filters and deterministic sorts change visible rows", {
  market <- cap_market_prepare(cap_market_fixture())

  centers <- cap_market_filter(market, year = "2027", position = "C")
  expect_identical(sort(centers$player_name), c("Center Five", "Center One", "Center Three"))
  expect_true(all(roster_position_matches(centers$primary_position, "C")))

  ufa <- cap_market_filter(market, free_agent_type = "UFA")
  expect_true(all(ufa$free_agent_status == "UFA"))

  searched <- cap_market_filter(market, search = "guard")
  expect_identical(searched$player_name, "Guard Two")

  team <- cap_market_filter(market, current_team = "Boston Celtics")
  expect_true(all(team$team_name == "Boston Celtics"))

  combined <- cap_market_filter(
    market,
    year = "2027",
    position = "C",
    free_agent_type = "UFA"
  )
  expect_equal(nrow(combined), 3L)

  bie <- cap_market_filter(market, sort_by = "BIE_DESC")
  expect_equal(bie$bie[!is.na(bie$bie)], sort(bie$bie[!is.na(bie$bie)], decreasing = TRUE))

  salary <- cap_market_filter(market, sort_by = "SALARY_DESC")
  expect_equal(salary$cap_hit, sort(salary$cap_hit, decreasing = TRUE))

  cleared <- cap_market_filter(market)
  expect_equal(nrow(cleared), nrow(market))
})

test_that("Cap market preserves unresolved contract evidence as review", {
  market <- cap_market_prepare(cap_market_fixture())
  option <- market[market$player_name == "Center Three", , drop = FALSE]
  incomplete <- market[market$player_name == "Wing Four", , drop = FALSE]

  expect_identical(option$data_quality, "REQUIRES SOURCE VERIFICATION")
  expect_identical(option$market_window_status, "REVIEW")
  expect_identical(incomplete$data_quality, "UNKNOWN")
  expect_identical(incomplete$market_window_status, "REVIEW")
  expect_true(market$loaded_window_fields[market$player_name == "Center One"])
})

test_that("Cap market maps canonical evidence to approved display-only terminology", {
  canonical <- c(
    "CURRENT",
    "CURRENT CONTRACT WINDOW",
    "REVIEW",
    "REQUIRES REVIEW",
    "REQUIRES SOURCE VERIFICATION",
    "UNKNOWN",
    "STALE",
    "CONFLICT",
    "UNEXPECTED",
    NA_character_
  )
  expected <- c(
    "CURRENT / VERIFIED",
    "CURRENT / VERIFIED",
    "REQUIRES REVIEW",
    "REQUIRES REVIEW",
    "REQUIRES REVIEW",
    "UNKNOWN",
    "STALE",
    "CONFLICT",
    "UNKNOWN",
    "UNKNOWN"
  )

  expect_identical(cap_market_display_status(canonical), expected)
})

test_that("Cap UI exposes six distinct mounted workspaces and market controls", {
  html <- as.character(mod_salary_cap_ui("cap"))
  expect_match(html, 'data-tbi-cap-tab="overview"', fixed = TRUE)
  expect_match(html, 'data-tbi-cap-tab="decision"', fixed = TRUE)
  expect_match(html, 'data-tbi-cap-tab="contracts"', fixed = TRUE)
  expect_match(html, 'data-tbi-cap-tab="market"', fixed = TRUE)
  expect_match(html, 'data-tbi-cap-tab="risk"', fixed = TRUE)
  expect_match(html, 'data-tbi-cap-tab="recommendation"', fixed = TRUE)
  expect_match(html, 'cap-market_year', fixed = TRUE)
  expect_match(html, 'cap-market_position', fixed = TRUE)
  expect_match(html, 'cap-market_search', fixed = TRUE)
  expect_match(html, 'cap-market_table', fixed = TRUE)
  expect_match(html, 'cap-clear_market_filters', fixed = TRUE)
  expect_match(html, "REQUIRES REVIEW means contract dates, option status, or source reconciliation", fixed = TRUE)
  expect_false(grepl("Option-dependent, unreconciled, or incomplete rows remain REVIEW.", html, fixed = TRUE))

  js <- paste(readLines(testthat::test_path("..", "..", "inst", "app", "www", "tbi_ux_foundation.js"), warn = FALSE), collapse = "\n")
  expect_match(js, "['market', 'Free Agent Market']", fixed = TRUE)
  expect_match(js, "['recommendation', 'Recommendation']", fixed = TRUE)
  expect_match(js, "data-tbi-cap-active-tab", fixed = TRUE)
  expect_match(js, "ArrowRight", fixed = TRUE)
  expect_match(js, "aria-controls", fixed = TRUE)

  css <- paste(readLines(testthat::test_path("..", "..", "inst", "app", "www", "tbi_ux_foundation.css"), warn = FALSE), collapse = "\n")
  expect_match(css, ".cap-v2-market-toolbar", fixed = TRUE)
  expect_match(css, "grid-template-columns\\s*:\\s*repeat\\(2,\\s*minmax\\(0,\\s*1fr\\)\\)")
})

test_that("Cap market uses loaded league data and is independent of selected team", {
  teams <- shiny::reactiveVal("Boston Celtics")
  season <- shiny::reactiveVal("2026-27")

  shiny::testServer(
    mod_salary_cap_server,
    args = list(selected_team = teams, selected_season = season),
    {
      rendered_html <- function(value) {
        if (is.list(value) && !is.null(value$html)) value$html else as.character(value)
      }
      first_market <- market_data()
      expect_gt(nrow(first_market), 0L)
      expect_equal(nrow(first_market), length(unique(first_market$player_id)))
      expect_gt(length(unique(first_market$team_name)), 20L)
      expect_true(all(is.na(first_market$bie) | is.finite(first_market$bie)))
      expect_true(all(is.na(first_market$bie_season) | first_market$bie_season == "2025-26"))
      expect_true(any(!is.na(first_market$option_type)))
      expect_true(all(!first_market$loaded_window_fields[!is.na(first_market$option_type)]))
      expect_match(rendered_html(output$overview_position), "Payroll")
      expect_match(rendered_html(output$market_summary), "loaded market rows")
      expect_match(rendered_html(output$cap_recommendation_posture), "Boston Celtics")
      expect_match(rendered_html(output$cap_recommendation_money), "Operating band")
      expect_match(rendered_html(output$cap_recommendation_watch), "Free-agent window")
      expect_false(is.null(output$market_table))
      expect_false(is.null(output$salary_table))

      available_years <- sort(unique(first_market$free_agent_year[!is.na(first_market$free_agent_year)]))
      expect_gte(length(available_years), 2L)
      session$setInputs(market_year = as.character(available_years[[1L]]), market_position = "C", market_type = "", market_grade = "", market_team = "", market_search = "", market_sort = "BIE_DESC")
      centers <- filtered_market()
      expect_true(all(roster_position_matches(centers$primary_position, "C")))
      expect_true(all(as.character(centers$free_agent_year) == as.character(available_years[[1L]])))
      expect_equal(centers$bie[!is.na(centers$bie)], sort(centers$bie[!is.na(centers$bie)], decreasing = TRUE))

      session$setInputs(market_year = as.character(available_years[[2L]]), market_position = "", market_search = "")
      expect_true(all(as.character(filtered_market()$free_agent_year) == as.character(available_years[[2L]])))

      preserved_type <- first_market$free_agent_status[which(!is.na(first_market$free_agent_status))[[1L]]]
      session$setInputs(market_type = preserved_type, active_subtab = "decision")
      session$setInputs(active_subtab = "market")
      expect_identical(input$market_type, preserved_type)
      expect_identical(input$market_year, as.character(available_years[[2L]]))

      for (team in c("Oklahoma City Thunder", "Charlotte Hornets", "Cleveland Cavaliers")) {
        teams(team)
        session$flushReact()
        expect_identical(selected_team(), team)
        expect_equal(nrow(market_data()), nrow(first_market))
        expect_identical(sort(market_data()$player_id), sort(first_market$player_id))
        expect_gt(nrow(salary_data()), 0L)
        expect_match(rendered_html(output$cap_recommendation_posture), team, fixed = TRUE)
      }
    }
  )
})
