manual_audit_root <- function() {
  normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/", mustWork = TRUE)
}

testthat::test_that("application startup uses a neutral team choice", {
  source <- paste(readLines(file.path(manual_audit_root(), "R", "app_ui.R"), warn = FALSE), collapse = "\n")
  testthat::expect_false(grepl('default_team <- if ("Boston Celtics"', source, fixed = TRUE))
  testthat::expect_match(source, '"Select Team" = ""', fixed = TRUE)
  testthat::expect_match(source, 'selected = ""', fixed = TRUE)
  javascript <- paste(readLines(file.path(manual_audit_root(), "inst", "app", "www", "tbi_ux_foundation.js"), warn = FALSE), collapse = "\n")
  testthat::expect_match(javascript, "tbi-v2-selected-team-v1", fixed = TRUE)
})

testthat::test_that("complete roster retains numeric sortable fields", {
  roster <- data.frame(
    player_name = c("Low", "High"), primary_position = c("PG", "C"),
    player_age = c(22, 31), cap_hit = c(950000, 32000000),
    contract_id = c(1, 2), contract_type = c("Two-Way Contract", "Veteran Contract"),
    stringsAsFactors = FALSE
  )
  display <- roster_complete_table_data(roster)
  widget <- roster_complete_reactable(display)
  attributes <- widget$x$tag$attribs
  payload <- jsonlite::fromJSON(as.character(attributes$data), simplifyVector = TRUE)

  testthat::expect_true(is.numeric(payload$`Cap Hit`))
  testthat::expect_false(is.character(payload$`Cap Hit`))
  testthat::expect_identical(display$Player[order(display$`Cap Hit`, decreasing = TRUE)], c("High", "Low"))
  testthat::expect_true(isTRUE(attributes$defaultSortDesc))
  testthat::expect_identical(attributes$defaultSorted[[1]]$id, "Cap Hit")
})

testthat::test_that("draft and five-year ledgers provide working filters", {
  draft <- htmltools::renderTags(mod_draft_assets_ui("draft"))$html
  outlook <- paste(readLines(file.path(manual_audit_root(), "R", "mod_five_year_outlook.R"), warn = FALSE), collapse = "\n")
  testthat::expect_match(draft, 'id="draft-draft_search"', fixed = TRUE)
  testthat::expect_match(draft, 'id="draft-draft_year_filter"', fixed = TRUE)
  testthat::expect_match(draft, 'id="draft-draft_verification_filter"', fixed = TRUE)
  testthat::expect_match(draft, 'id="draft-clear_draft_filters"', fixed = TRUE)
  testthat::expect_match(draft, "draft-v2-year-group", fixed = TRUE)
  for (control in c(
    "contract_search", "contract_type_filter", "contract_fa_year_filter",
    "contract_option_filter", "clear_contract_filters"
  )) {
    testthat::expect_match(outlook, paste0('ns("', control, '")'), fixed = TRUE)
  }
})

testthat::test_that("Trade Intelligence exposes multi-team and exception controls", {
  ui <- v2_ui_transaction_foundation_banner("trade_analyzer")
  html <- htmltools::renderTags(ui)$html
  for (id in c("v2_team_count", "v2_additional_teams", "v2_route_editor", "v2_exception_inventory", "v2_exception_controls", "v2_evaluate_transaction", "v2_transaction_result")) {
    testthat::expect_match(html, paste0('id="trade_analyzer-', id, '"'), fixed = TRUE)
  }
  testthat::expect_match(html, "2 teams", fixed = TRUE)
  testthat::expect_match(html, "4 teams", fixed = TRUE)
})

testthat::test_that("legacy TPE control fails closed without an incoming salary route", {
  ledger <- new_v2_team_exception_ledger(data.frame(
    team_id = "Boston Celtics", season = "2026-27", exception_id = "verified-tpe",
    exception_type = "TRADED_PLAYER_EXCEPTION", original_amount = 12000000,
    remaining_amount = 12000000, creation_transaction = "verified-fixture",
    creation_date = "2026-07-01", expiration_date = "2027-07-01", status = "ACTIVE",
    source = "verified test fixture", source_version = "1", verification_status = "VERIFIED",
    use_restrictions = "UNKNOWN", stringsAsFactors = FALSE
  ))
  state <- tbi_transaction_state()
  shiny::testServer(v2_transaction_workspace_server, args = list(
    selected_team = shiny::reactive("Boston Celtics"), transaction_state = state,
    authoritative_exception_ledger = ledger
  ), {
    session$setInputs(v2_team_count = "2", partner_team = "Atlanta Hawks",
      v2_exception_id = "verified-tpe", v2_exception_amount = 5000000,
      v2_evaluate_transaction = 1)
    session$flushReact()
    snapshot <- state$snapshot()
    testthat::expect_identical(snapshot$v2_exception_scenario$status, "FAIL")
    testthat::expect_true(snapshot$v2_exception_scenario$is_blocked)
    testthat::expect_equal(snapshot$v2_exception_scenario$scenario_ledger$entries$remaining_amount, 12000000)
    testthat::expect_equal(nrow(snapshot$v2_exception_scenario$scenario_ledger$usage_history), 0L)
    testthat::expect_equal(ledger$entries$remaining_amount, 12000000)
  })
})

testthat::test_that("V2 transaction workspace evaluates two to four teams without authority mutation", {
  graph <- v2_build_ui_transaction_graph(
    transaction_id = "manual-audit-3-team",
    teams = c("A", "B", "C"),
    route_rows = list(list(kind = "PLAYER", identity = "p1", from = "A", to = "B"))
  )
  testthat::expect_identical(length(graph$teams), 3L)
  evaluation <- evaluate_multiteam_transaction(graph)
  testthat::expect_identical(evaluation$status, "REVIEW")
  testthat::expect_false(evaluation$is_blocked)
})

testthat::test_that("multi-team workspace publishes and resets a session scenario", {
  state <- tbi_transaction_state()
  shiny::testServer(
    v2_transaction_workspace_server,
    args = list(selected_team = shiny::reactive("Boston Celtics"), transaction_state = state),
    {
      session$setInputs(
        partner_team = "Brooklyn Nets",
        v2_team_count = "3",
        v2_team_3 = "Charlotte Hornets",
        v2_route_kind_1 = "PLAYER",
        v2_route_identity_1 = "player-1",
        v2_route_from_1 = "Boston Celtics",
        v2_route_to_1 = "Brooklyn Nets",
        v2_evaluate_transaction = 1
      )
      session$flushReact()
      snapshot <- state$snapshot()
      testthat::expect_true(snapshot$active)
      testthat::expect_identical(snapshot$scenario_type, "v2_multiteam_trade")
      testthat::expect_length(snapshot$v2_transaction_graph$teams, 3L)
      testthat::expect_identical(snapshot$v2_transaction_evaluation$status, "REVIEW")
      testthat::expect_identical(snapshot$v2_exception_scenario$scenario_ledger$contract_type, "tbi-v2-scenario-exception-ledger")

      session$setInputs(v2_reset_transaction = 1)
      session$flushReact()
      testthat::expect_false(state$snapshot()$active)
    }
  )
})

testthat::test_that("shared correction CSS protects density and floating utilities", {
  css <- paste(readLines(file.path(manual_audit_root(), "inst", "app", "www", "tbi_phase3.css"), warn = FALSE), collapse = "\n")
  demo <- paste(readLines(file.path(manual_audit_root(), "inst", "app", "www", "tbi_demo.css"), warn = FALSE), collapse = "\n")
  testthat::expect_match(css, "FINAL MANUAL-AUDIT CORRECTIONS", fixed = TRUE)
  testthat::expect_match(css, "grid-auto-rows:max-content", fixed = TRUE)
  testthat::expect_match(demo, ".tbi-demo-dock", fixed = TRUE)
  testthat::expect_match(demo, "max-width: calc(100vw - 32px)", fixed = TRUE)
  testthat::expect_match(demo, "max-width: 430px", fixed = TRUE)
})

testthat::test_that("Depth Chart navigation separates decisions without a player-intelligence tab", {
  js <- paste(readLines(file.path(manual_audit_root(), "inst", "app", "www", "tbi_ux_foundation.js"), warn = FALSE), collapse = "\n")
  for (definition in c(
    "['depth', 'Depth Chart']",
    "['rotation', 'Rotation']",
    "['lineup', 'Lineups']",
    "['staggering', 'Staggering']",
    "['gameplan', 'Game Plan']"
  )) {
    testthat::expect_match(js, definition, fixed = TRUE)
  }
  testthat::expect_false(grepl("['player', 'Player Intelligence']", js, fixed = TRUE))
})

testthat::test_that("unsupported current facts remain visible as governed unknowns", {
  roster <- paste(readLines(file.path(manual_audit_root(), "R", "mod_roster_contracts.R"), warn = FALSE), collapse = "\n")
  player <- paste(readLines(file.path(manual_audit_root(), "R", "mod_player_manager.R"), warn = FALSE), collapse = "\n")
  testthat::expect_match(roster, "Current-data status: Requires Source Verification", fixed = TRUE)
  testthat::expect_match(player, 'metric_box("JERSEY", "jersey_number")', fixed = TRUE)
  testthat::expect_match(player, 'output$jersey_number', fixed = TRUE)
})
