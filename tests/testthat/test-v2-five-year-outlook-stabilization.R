five_year_rendered_html <- function(value) {
  if (is.list(value) && !is.null(value$html)) value$html else as.character(value)
}

five_year_contract_fixture <- function() {
  data.frame(
    player_id = 1:4,
    player_name = c("Alpha Guard", "Bravo Wing", "Charlie Big", "Delta Guard"),
    primary_position = c("PG", "SF", "C", "SG"),
    player_age = c(25, 28, 30, 23),
    cap_hit = c(5000000, 12000000, 18000000, 3500000),
    contract_type = c("Veteran Contract", "Veteran Contract", "Two-Way Contract", NA),
    contract_end_season = c("2027-28", "2028-29", "2026-27", NA),
    option_type = c("Team Option", "Player Option", NA, NA),
    free_agent_year = c(2028, 2029, 2027, NA),
    bird_rights = c("Full Bird", "Early Bird", NA, NA),
    stringsAsFactors = FALSE
  )
}

testthat::test_that("Five-Year contract filters compose and clear without changing facts", {
  contracts <- five_year_contract_fixture()

  by_search <- filter_five_year_contracts(contracts, list(search = "alpha"))
  testthat::expect_identical(by_search$player_name, "Alpha Guard")

  combined <- filter_five_year_contracts(
    contracts,
    list(contract_type = "Veteran Contract", option_status = "Player Option")
  )
  testthat::expect_identical(combined$player_name, "Bravo Wing")

  by_year <- filter_five_year_contracts(contracts, list(free_agent_year = "2027"))
  testthat::expect_identical(by_year$player_name, "Charlie Big")

  cleared <- filter_five_year_contracts(
    contracts,
    list(search = "", contract_type = "", option_status = "", free_agent_year = "")
  )
  testthat::expect_identical(cleared$player_id, contracts$player_id)
})

testthat::test_that("Five-Year contract display preserves sortable salary facts and unknowns", {
  display <- five_year_contract_runway_data(five_year_contract_fixture(), 2026L)

  testthat::expect_true(is.numeric(display$`Current Cap Hit`))
  testthat::expect_true(is.na(display$`Contract Type`[[4]]))
  testthat::expect_true(is.na(display$`FA Year`[[4]]))
  testthat::expect_true(is.na(display$`Years Control`[[4]]))
  testthat::expect_identical(
    display$Player[order(display$`Current Cap Hit`, decreasing = TRUE)],
    c("Charlie Big", "Bravo Wing", "Alpha Guard", "Delta Guard")
  )
  token_html <- htmltools::renderTags(five_year_contract_token_cell(NA_character_))$html
  testthat::expect_match(token_html, "UNKNOWN", fixed = TRUE)
})

testthat::test_that("Five-Year draft summary never turns missing facts into zeroes", {
  partial <- five_year_draft_summary_values(list(portfolio_grade = "B"))
  testthat::expect_identical(partial$portfolio, "B")
  testthat::expect_identical(partial$net_value, "UNKNOWN")
  testthat::expect_identical(partial$obligations, "UNKNOWN")
  testthat::expect_identical(partial$verification, "REQUIRES SOURCE VERIFICATION")

  unrated <- five_year_draft_summary_values(list(
    portfolio_grade = "Unrated",
    net_portfolio_value = 0,
    obligations = 0,
    review_required = 0
  ))
  testthat::expect_identical(unrated$net_value, "Not loaded")
  testthat::expect_identical(unrated$obligations, "UNKNOWN")
  testthat::expect_identical(unrated$verification, "REQUIRES SOURCE VERIFICATION")

  loaded <- five_year_draft_summary_values(list(
    portfolio_grade = "A",
    net_portfolio_value = 0,
    obligations = 0,
    review_required = 0
  ))
  testthat::expect_identical(loaded$net_value, "0.0")
  testthat::expect_identical(loaded$obligations, "0")
  testthat::expect_identical(loaded$verification, "LOADED")
})

testthat::test_that("Five-Year UI exposes governed filters and compact page workspaces", {
  html <- htmltools::renderTags(mod_five_year_outlook_ui("outlook"))$html

  for (id in c(
    "contract_search", "contract_type_filter", "contract_fa_year_filter",
    "contract_option_filter", "clear_contract_filters", "contract_runway_summary",
    "outlook_draft_summary"
  )) {
    testthat::expect_match(html, paste0('id="outlook-', id, '"'), fixed = TRUE)
  }

  for (class_name in c(
    "outlook-v2-overview-grid", "outlook-v2-flexibility-grid",
    "outlook-v2-contract-workspace", "outlook-v2-draft-grid",
    "outlook-v2-recommendation-workspace"
  )) {
    testthat::expect_match(html, class_name, fixed = TRUE)
  }
})

testthat::test_that("Five-Year filters operate against real loaded team contracts", {
  selected_team_value <- shiny::reactiveVal("Boston Celtics")
  selected_season_value <- shiny::reactiveVal("2026-27")

  shiny::testServer(
    mod_five_year_outlook_server,
    args = list(
      selected_team = selected_team_value,
      selected_season = selected_season_value,
      transaction_state = NULL
    ),
    {
      session$flushReact()
      full <- current_roster_contracts()
      testthat::expect_gt(nrow(full), 1L)

      query <- substr(full$player_name[[1]], 1L, 5L)
      session$setInputs(contract_search = query)
      session$flushReact()
      searched <- filtered_current_contracts()
      testthat::expect_gt(nrow(searched), 0L)
      testthat::expect_true(all(grepl(tolower(query), tolower(searched$player_name), fixed = TRUE)))

      type <- full$contract_type[!is.na(full$contract_type) & nzchar(full$contract_type)][[1]]
      session$setInputs(contract_search = "", contract_type_filter = type)
      session$flushReact()
      typed <- filtered_current_contracts()
      testthat::expect_gt(nrow(typed), 0L)
      testthat::expect_true(all(typed$contract_type == type))

      combined_query <- substr(typed$player_name[[1]], 1L, 5L)
      session$setInputs(contract_search = combined_query)
      session$flushReact()
      combined <- filtered_current_contracts()
      testthat::expect_gt(nrow(combined), 0L)
      testthat::expect_true(all(combined$contract_type == type))
      testthat::expect_true(all(grepl(tolower(combined_query), tolower(combined$player_name), fixed = TRUE)))

      reset_messages <- list()
      root_session <- session$rootScope()
      root_session$sendInputMessage <- function(inputId, message) {
        reset_messages[[length(reset_messages) + 1L]] <<- list(inputId = inputId, message = message)
        invisible()
      }

      selected_team_value("Oklahoma City Thunder")
      session$flushReact()
      switched <- current_roster_contracts()
      testthat::expect_gt(nrow(switched), 0L)
      testthat::expect_false(any(switched$player_id %in% full$player_id) && identical(switched$player_name, full$player_name))
      reset_ids <- vapply(reset_messages, `[[`, character(1), "inputId")
      testthat::expect_setequal(
        reset_ids,
        c("contract_search", "contract_type_filter", "contract_fa_year_filter", "contract_option_filter")
      )
      select_resets <- reset_messages[reset_ids != "contract_search"]
      select_values <- lapply(select_resets, function(item) item$message$value %||% item$message$selected)
      testthat::expect_true(all(vapply(select_values, identical, logical(1), "")))
    }
  )
})

testthat::test_that("Five-Year outputs update through required team sequence", {
  selected_team_value <- shiny::reactiveVal("Boston Celtics")
  selected_season_value <- shiny::reactiveVal("2026-27")

  shiny::testServer(
    mod_five_year_outlook_server,
    args = list(
      selected_team = selected_team_value,
      selected_season = selected_season_value,
      transaction_state = NULL
    ),
    {
      seen <- character()
      for (team in c(
        "Boston Celtics", "Oklahoma City Thunder",
        "Charlotte Hornets", "Cleveland Cavaliers"
      )) {
        selected_team_value(team)
        session$flushReact()
        roster <- current_roster_contracts()
        years <- year_summary()
        testthat::expect_gt(nrow(roster), 0L)
        testthat::expect_identical(years$season, c("2026-27", "2027-28", "2028-29", "2029-30", "2030-31"))
        testthat::expect_true(nzchar(output$snapshot_current_payroll))
        testthat::expect_true(nzchar(five_year_rendered_html(output$outlook_decision)))
        testthat::expect_true(nzchar(five_year_rendered_html(output$outlook_headlines)))
        testthat::expect_true(nzchar(five_year_rendered_html(output$strategy_scorecard)))
        testthat::expect_true(nzchar(five_year_rendered_html(output$outlook_readout)))
        testthat::expect_match(five_year_rendered_html(output$five_year_timeline), "2026-27", fixed = TRUE)
        testthat::expect_true(nzchar(five_year_rendered_html(output$contract_runway_summary)))
        testthat::expect_false(is.null(output$contract_runway_table))
        testthat::expect_true(nzchar(five_year_rendered_html(output$outlook_draft_summary)))
        testthat::expect_true(nzchar(five_year_rendered_html(output$outlook_risks)))
        testthat::expect_true(nzchar(five_year_rendered_html(output$outlook_opportunities)))
        testthat::expect_true(nzchar(five_year_rendered_html(output$outlook_recommendation)))
        seen <- c(seen, paste(team, roster$player_name[[1]], sep = ":"))
      }
      testthat::expect_length(unique(seen), 4L)
    }
  )
})

testthat::test_that("Five-Year scenario reset restores the authoritative baseline", {
  state <- tbi_transaction_state()
  outgoing <- get_roster("Boston Celtics", "2026-27")[1, , drop = FALSE]
  incoming <- get_roster("Atlanta Hawks", "2026-27")[1, , drop = FALSE]
  shiny::isolate(
    state$publish_trade(
      team = "Boston Celtics",
      partner_team = "Atlanta Hawks",
      season = "2026-27",
      outgoing_players = outgoing,
      incoming_players = incoming
    )
  )

  shiny::testServer(
    mod_five_year_outlook_server,
    args = list(
      selected_team = shiny::reactive("Boston Celtics"),
      selected_season = shiny::reactive("2026-27"),
      transaction_state = state
    ),
    {
      session$flushReact()
      testthat::expect_false(is.null(active_trade_scenario()))
      testthat::expect_match(five_year_rendered_html(output$outlook_trade_scenario_banner), "TRADE SCENARIO", fixed = TRUE)

      reset_messages <- list()
      root_session <- session$rootScope()
      root_session$sendInputMessage <- function(inputId, message) {
        reset_messages[[length(reset_messages) + 1L]] <<- list(inputId = inputId, message = message)
        invisible()
      }

      state$clear()
      session$flushReact()
      testthat::expect_true(is.null(active_trade_scenario()))
      testthat::expect_setequal(current_roster_contracts()$player_id, base_current_roster_contracts()$player_id)
      testthat::expect_true(is.null(output$outlook_trade_scenario_banner) || !nzchar(five_year_rendered_html(output$outlook_trade_scenario_banner)))
      reset_ids <- vapply(reset_messages, `[[`, character(1), "inputId")
      testthat::expect_setequal(
        reset_ids,
        c("contract_search", "contract_type_filter", "contract_fa_year_filter", "contract_option_filter")
      )
      reset_values <- lapply(reset_messages, function(item) item$message$value %||% item$message$selected)
      testthat::expect_true(all(vapply(reset_values, identical, logical(1), "")))
    }
  )
})

testthat::test_that("Five-Year responsive rules remove known vertical and mobile defects", {
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/", mustWork = TRUE)
  module_source <- paste(readLines(file.path(root, "R", "mod_five_year_outlook.R"), warn = FALSE), collapse = "\n")
  css <- paste(readLines(file.path(root, "inst", "app", "www", "tbi_ux_foundation.css"), warn = FALSE), collapse = "\n")
  javascript <- paste(readLines(file.path(root, "inst", "app", "www", "tbi_ux_foundation.js"), warn = FALSE), collapse = "\n")

  testthat::expect_false(grepl("max-height:430px", module_source, fixed = TRUE))
  testthat::expect_match(module_source, "grid-template-columns:repeat(6,minmax(0,1fr))", fixed = TRUE)
  testthat::expect_match(module_source, ".outlook-v2-year-card:nth-last-child(-n+2)", fixed = TRUE)
  testthat::expect_match(css, ".tbi-v2-outlook-page .outlook-v2-draft-findings", fixed = TRUE)
  testthat::expect_match(css, ".tbi-v2-outlook-page:not([data-tbi-outlook-tabs-ready])", fixed = TRUE)
  testthat::expect_match(javascript, "REQUIRED_SECTIONS.every", fixed = TRUE)
  testthat::expect_match(javascript, "data-tbi-outlook-tabs-ready", fixed = TRUE)
  testthat::expect_match(
    javascript,
    "var VERSION = '2.0.0';\n\n  var VALID = [",
    fixed = TRUE
  )
  testthat::expect_false(grepl("targets.length !== 9", javascript, fixed = TRUE))
  testthat::expect_match(javascript, "if (previousNav) previousNav.remove();", fixed = TRUE)
  for (behavior in c("role', 'tabpanel", "aria-controls", "aria-labelledby", "ArrowRight", "tabindex")) {
    testthat::expect_match(javascript, behavior, fixed = TRUE)
  }
})
