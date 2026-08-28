trade_test_players <- function(team, season = "2026-27") {
  pool <- get_trade_player_pool(team, season)
  testthat::expect_gt(nrow(pool), 0L)
  pool
}

testthat::test_that("multi-team mode accepts only the canonical 3/4 server contract", {
  testthat::expect_identical(v2_trade_multi_mode(3L), 3L)
  testthat::expect_identical(v2_trade_multi_mode(4), 4L)
  testthat::expect_identical(v2_trade_multi_mode("3"), 3L)
  testthat::expect_identical(v2_trade_multi_mode("4"), 4L)

  invalid <- list(
    NULL,
    character(),
    NA_character_,
    "",
    2L,
    5L,
    -1L,
    3.9,
    Inf,
    TRUE,
    "3.0",
    "4 teams",
    "3; injected",
    c("3", "4"),
    list(3),
    structure("3", class = "forged_mode")
  )
  for (value in invalid) {
    testthat::expect_identical(v2_trade_multi_mode(value), NA_integer_)
  }
})

testthat::test_that("invalid multi-team mode fails closed before routes are built", {
  state <- tbi_transaction_state()
  rendered <- function(value) paste(unlist(value), collapse = " ")

  shiny::testServer(
    v2_trade_intelligence_server,
    args = list(
      selected_team = shiny::reactive("Atlanta Hawks"),
      selected_season = shiny::reactive("2026-27"),
      transaction_state = state
    ),
    {
      testthat::expect_true(is.na(multi_team_mode()))
      testthat::expect_length(selected_multi_teams(), 0L)

      invalid <- list("2", "5", "-1", "3.0", "3; injected", NA_character_)
      for (value in invalid) {
        session$setInputs(multi_mode = value)
        session$flushReact()

        testthat::expect_true(is.na(multi_team_mode()))
        testthat::expect_length(selected_multi_teams(), 0L)
        testthat::expect_length(multi_player_pools(), 0L)
        testthat::expect_length(multi_asset_pools(), 0L)
        testthat::expect_error(
          evaluate_multi(),
          "Transaction size must be 3 or 4 teams",
          fixed = TRUE
        )
        testthat::expect_match(
          rendered(output$multi_team_selectors),
          "Transaction size must be 3 or 4 teams",
          fixed = TRUE
        )
        testthat::expect_match(
          rendered(output$multi_team_workspaces),
          "Transaction size must be 3 or 4 teams",
          fixed = TRUE
        )
        testthat::expect_false(state$snapshot()$active)
      }

      session$setInputs(
        multi_mode = "3",
        multi_team_1 = "Atlanta Hawks",
        multi_team_2 = "Boston Celtics",
        multi_team_3 = "Brooklyn Nets"
      )
      session$flushReact()
      testthat::expect_identical(multi_team_mode(), 3L)
      testthat::expect_identical(
        selected_multi_teams(),
        c("Atlanta Hawks", "Boston Celtics", "Brooklyn Nets")
      )

      session$setInputs(multi_mode = "4")
      session$flushReact()
      testthat::expect_identical(multi_team_mode(), 4L)
    }
  )
})

testthat::test_that("Trade workbench has five purposeful subtabs", {
  html <- as.character(v2_trade_intelligence_ui("trade", shiny::div(id = "legacy-builder")))
  for (label in c("Trade Summary", "2-Team Trade", "Multi-Team Trade", "Evaluation", "Recommendation")) {
    testthat::expect_match(html, label, fixed = TRUE)
  }
  testthat::expect_match(html, "Clear Trade", fixed = TRUE)
})

testthat::test_that("2-Team Trade mounts only the protected builder workspace", {
  html <- htmltools::renderTags(mod_trade_analyzer_ui("trade", builder_only = TRUE))$html
  testthat::expect_match(html, "tbi-trade-workspace-grid", fixed = TRUE)
  testthat::expect_false(grepl("SCENARIO SNAPSHOT", html, fixed = TRUE))
  testthat::expect_false(grepl("TRANSACTION DECISION", html, fixed = TRUE))
  testthat::expect_false(grepl("EXECUTIVE RECOMMENDATION", html, fixed = TRUE))
  testthat::expect_match(html, 'id="trade-evaluate_trade"', fixed = TRUE)
  testthat::expect_match(html, "Evaluate Trade", fixed = TRUE)
})

testthat::test_that("2-Team Evaluate owns one current result and edits invalidate it", {
  team_names <- c("Cleveland Cavaliers", "Atlanta Hawks")
  outgoing <- trade_test_players(team_names[[1]])
  incoming <- trade_test_players(team_names[[2]])
  testthat::expect_gte(nrow(outgoing), 2L)
  testthat::expect_gte(nrow(incoming), 1L)
  state <- tbi_transaction_state()

  shiny::testServer(
    mod_trade_analyzer_server,
    args = list(
      selected_team = shiny::reactive(team_names[[1]]),
      selected_season = shiny::reactive("2026-27"),
      transaction_state = state,
      builder_only = TRUE
    ),
    {
      session$flushReact()
      session$setInputs(partner_team = team_names[[2]])
      session$flushReact()
      session$setInputs(
        outgoing_players = as.character(outgoing$player_id[[1L]]),
        incoming_players = as.character(incoming$player_id[[1L]])
      )
      session$flushReact()
      session$setInputs(evaluate_trade = 1)
      session$flushReact()

      first <- state$snapshot()
      testthat::expect_true(first$active)
      testthat::expect_false(is.null(first$evaluation))
      testthat::expect_true(nzchar(first$transaction_signature))
      testthat::expect_identical(first$transaction_signature, first$evaluation_signature)

      session$setInputs(outgoing_players = as.character(outgoing$player_id[[2L]]))
      session$flushReact()
      testthat::expect_false(state$snapshot()$active)

      session$setInputs(evaluate_trade = 2)
      session$flushReact()
      second <- state$snapshot()
      testthat::expect_true(second$active)
      testthat::expect_false(identical(second$scenario_id, first$scenario_id))
      testthat::expect_false(identical(second$transaction_signature, first$transaction_signature))

      session$setInputs(outgoing_players = character())
      session$flushReact()
      testthat::expect_false(state$snapshot()$active)
    }
  )
})

testthat::test_that("organization switch preserves a trade until the next builder edit", {
  team_names <- c("Cleveland Cavaliers", "Atlanta Hawks", "Boston Celtics")
  outgoing <- trade_test_players(team_names[[1]])
  incoming <- trade_test_players(team_names[[2]])
  switched_outgoing <- trade_test_players(team_names[[3]])
  selected_team <- shiny::reactiveVal(team_names[[1]])
  state <- tbi_transaction_state()

  shiny::testServer(
    mod_trade_analyzer_server,
    args = list(
      selected_team = selected_team,
      selected_season = shiny::reactive("2026-27"),
      transaction_state = state,
      builder_only = TRUE
    ),
    {
      session$flushReact()
      session$setInputs(
        partner_team = team_names[[2]],
        outgoing_players = as.character(outgoing$player_id[[1L]]),
        incoming_players = as.character(incoming$player_id[[1L]])
      )
      session$flushReact()
      session$setInputs(evaluate_trade = 1)
      session$flushReact()

      evaluated <- state$snapshot()
      testthat::expect_true(evaluated$active)

      selected_team(team_names[[3]])
      session$flushReact()
      switched <- state$snapshot()
      testthat::expect_true(switched$active)
      testthat::expect_identical(switched$scenario_id, evaluated$scenario_id)

      session$setInputs(
        partner_team = team_names[[1]],
        outgoing_players = character(),
        incoming_players = character(),
        outgoing_draft_assets = character(),
        incoming_draft_assets = character()
      )
      session$flushReact()
      rebound <- state$snapshot()
      testthat::expect_true(rebound$active)
      testthat::expect_identical(rebound$scenario_id, evaluated$scenario_id)

      session$setInputs(
        outgoing_players = as.character(switched_outgoing$player_id[[1L]])
      )
      session$flushReact()
      testthat::expect_false(state$snapshot()$active)
      testthat::expect_null(v2_trade_canonical_result(state$snapshot()))
    }
  )
})

testthat::test_that("Trade Summary Evaluation and Recommendation share the current evaluated trade", {
  teams <- c("Cleveland Cavaliers", "Atlanta Hawks", "Boston Celtics")
  state <- tbi_transaction_state()
  publish <- function(partner) {
    outgoing <- trade_test_players(teams[[1]])[1, , drop = FALSE]
    incoming <- trade_test_players(partner)[1, , drop = FALSE]
    team_a <- build_trade_team_input(teams[[1]], "2026-27", outgoing$player_id, incoming)
    team_b <- build_trade_team_input(partner, "2026-27", incoming$player_id, outgoing)
    evaluation <- evaluate_two_team_trade(team_a, team_b, get_cap_thresholds("2026-27"))
    shiny::isolate(state$publish_trade(
      teams[[1]], partner, "2026-27", outgoing, incoming,
      evaluation = evaluation
    ))
  }
  publish(teams[[2]])

  shiny::testServer(
    v2_trade_intelligence_server,
    args = list(
      selected_team = shiny::reactive(teams[[1]]),
      selected_season = shiny::reactive("2026-27"),
      transaction_state = state
    ),
    {
      rendered <- function(value) paste(unlist(value), collapse = " ")
      surfaces <- function() c(
        rendered(output$scenario_summary),
        rendered(output$evaluation_view),
        rendered(output$recommendation_view)
      )

      first <- state$snapshot()
      first_surfaces <- surfaces()
      testthat::expect_true(all(grepl(teams[[2]], first_surfaces, fixed = TRUE)))

      publish(teams[[3]])
      session$flushReact()
      second <- state$snapshot()
      second_surfaces <- surfaces()
      testthat::expect_false(identical(second$scenario_id, first$scenario_id))
      testthat::expect_true(all(grepl(teams[[3]], second_surfaces, fixed = TRUE)))
      testthat::expect_false(any(grepl(teams[[2]], second_surfaces, fixed = TRUE)))

      state$clear()
      session$flushReact()
      testthat::expect_true(all(grepl("Not evaluated yet", surfaces(), fixed = TRUE)))
    }
  )
})

testthat::test_that("failed multi-team Evaluate removes prior two-team evidence", {
  teams <- c("Atlanta Hawks", "Boston Celtics", "Brooklyn Nets")
  outgoing <- trade_test_players(teams[[1]])[1, , drop = FALSE]
  incoming <- trade_test_players(teams[[2]])[1, , drop = FALSE]
  team_a <- build_trade_team_input(teams[[1]], "2026-27", outgoing$player_id, incoming)
  team_b <- build_trade_team_input(teams[[2]], "2026-27", incoming$player_id, outgoing)
  evaluation <- evaluate_two_team_trade(team_a, team_b, get_cap_thresholds("2026-27"))
  state <- tbi_transaction_state()
  shiny::isolate(state$publish_trade(
    teams[[1]], teams[[2]], "2026-27", outgoing, incoming,
    evaluation = evaluation
  ))

  shiny::testServer(v2_trade_intelligence_server, args = list(
    selected_team = shiny::reactive(teams[[1]]),
    selected_season = shiny::reactive("2026-27"),
    transaction_state = state
  ), {
    session$setInputs(
      multi_mode = "3",
      multi_team_1 = teams[[1]],
      multi_team_2 = teams[[2]],
      multi_team_3 = teams[[3]],
      evaluate_multi = 1
    )
    session$flushReact()

    testthat::expect_false(state$snapshot()$active)
    testthat::expect_null(v2_trade_canonical_result(state$snapshot()))
    surfaces <- c(
      paste(unlist(output$scenario_summary), collapse = " "),
      paste(unlist(output$evaluation_view), collapse = " "),
      paste(unlist(output$recommendation_view), collapse = " ")
    )
    testthat::expect_true(all(grepl("Not evaluated yet", surfaces, fixed = TRUE)))
    testthat::expect_false(any(grepl(teams[[2]], surfaces, fixed = TRUE)))
  })
})

testthat::test_that("TPE test mode is explicit and defaults off outside local opt-in", {
  testthat::expect_true(v2_trade_test_mode_enabled("true"))
  testthat::expect_true(v2_trade_test_mode_enabled("ON"))
  testthat::expect_false(v2_trade_test_mode_enabled("false"))
  testthat::expect_false(v2_trade_test_mode_enabled(""))
  app_source <- paste(readLines(testthat::test_path("..", "..", "app.R"), warn = FALSE), collapse = "\n")
  testthat::expect_match(app_source, 'TBI_ENABLE_TPE_TEST_MODE = "false"', fixed = TRUE)
})

testthat::test_that("TPE controls are mounted only for explicit local QA", {
  withr::local_envvar(TBI_ENABLE_TPE_TEST_MODE = "false")
  shiny::testServer(v2_trade_intelligence_server, args = list(
    selected_team = shiny::reactive("Atlanta Hawks"), selected_season = shiny::reactive("2026-27"),
    transaction_state = tbi_transaction_state()
  ), {
    session$flushReact()
    rendered <- paste(unlist(output$development_tpe_workbench), collapse = " ")
    testthat::expect_false(grepl("Development TPE Test Mode", rendered, fixed = TRUE))
    tpe_workspace <- paste(unlist(output$tpe_workspace), collapse = " ")
    testthat::expect_false(grepl("Verified TPE inventory", tpe_workspace, fixed = TRUE))
  })

  withr::local_envvar(TBI_ENABLE_TPE_TEST_MODE = "true")
  shiny::testServer(v2_trade_intelligence_server, args = list(
    selected_team = shiny::reactive("Atlanta Hawks"), selected_season = shiny::reactive("2026-27"),
    transaction_state = tbi_transaction_state()
  ), {
    session$flushReact()
    local_workspace <- paste(unlist(output$tpe_workspace), collapse = " ")
    testthat::expect_match(local_workspace, "Verified TPE inventory", fixed = TRUE)
  })

  shiny::testServer(v2_trade_intelligence_server, args = list(
    selected_team = shiny::reactive("Atlanta Hawks"), selected_season = shiny::reactive("2026-27"),
    transaction_state = tbi_transaction_state()
  ), {
    session$userData$tbi_feedback_mode <- TRUE
    session$flushReact()
    feedback_workspace <- paste(unlist(output$tpe_workspace), collapse = " ")
    testthat::expect_false(grepl("Verified TPE inventory", feedback_workspace, fixed = TRUE))
  })
})

testthat::test_that("scenario exception ledger stays local-QA only", {
  withr::local_envvar(TBI_ENABLE_TPE_TEST_MODE = "true")
  team <- "Atlanta Hawks"
  partner <- "Boston Celtics"
  state <- tbi_transaction_state()
  outgoing <- data.frame(
    player_id = "local-outgoing",
    player_name = "Local Outgoing",
    cap_hit = 1000000,
    stringsAsFactors = FALSE
  )
  incoming <- data.frame(
    player_id = "local-incoming",
    player_name = "Local Incoming",
    cap_hit = 1000000,
    stringsAsFactors = FALSE
  )
  exception <- list(
    status = "PASS",
    is_blocked = FALSE,
    scenario_ledger = list(
      entries = data.frame(
        exception_id = "local-test-tpe",
        team_id = team,
        original_amount = 1000000,
        remaining_amount = 1000000,
        use_restrictions = "TEST ONLY",
        stringsAsFactors = FALSE
      ),
      usage_history = data.frame()
    )
  )
  shiny::isolate({
    state$publish_trade(
      team,
      partner,
      "2026-27",
      outgoing,
      incoming,
      evaluation = list(
        team_a_name = team,
        team_b_name = partner,
        status = "PASS",
        is_blocked = FALSE,
        requires_manual_review = FALSE,
        team_a = list(is_screen_pass = TRUE),
        team_b = list(is_screen_pass = TRUE)
      )
    )
    state$set_v2_exception_scenario(exception)
  })

  shiny::testServer(v2_trade_intelligence_server, args = list(
    selected_team = shiny::reactive(team),
    selected_season = shiny::reactive("2026-27"),
    transaction_state = state
  ), {
    local_summary <- paste(unlist(output$scenario_summary), collapse = " ")
    testthat::expect_match(local_summary, "Scenario Exception Ledger", fixed = TRUE)
    testthat::expect_match(local_summary, "TEST ONLY", fixed = TRUE)
  })

  shiny::testServer(v2_trade_intelligence_server, args = list(
    selected_team = shiny::reactive(team),
    selected_season = shiny::reactive("2026-27"),
    transaction_state = state
  ), {
    session$userData$tbi_feedback_mode <- TRUE
    session$flushReact()
    feedback_summary <- paste(unlist(output$scenario_summary), collapse = " ")
    testthat::expect_false(grepl("Scenario Exception Ledger", feedback_summary, fixed = TRUE))
    testthat::expect_false(grepl("TEST ONLY", feedback_summary, fixed = TRUE))
  })
})

testthat::test_that("each team can route five distinct roster players and clear one slot safely", {
  teams <- c("Cleveland Cavaliers", "Atlanta Hawks", "Boston Celtics")
  fourth_team <- "Brooklyn Nets"
  cleveland <- get_trade_player_pool(teams[[1]], "2026-27")
  atlanta <- get_trade_player_pool(teams[[2]], "2026-27")
  testthat::expect_gte(nrow(cleveland), 5L)
  testthat::expect_gte(nrow(atlanta), 5L)
  testthat::expect_identical(v2_trade_player_slot_indices(1L), 1:5)
  testthat::expect_identical(v2_trade_player_slot_indices(2L), 6:10)
  testthat::expect_identical(v2_trade_player_slot_indices(4L), 16:20)
  testthat::expect_identical(v2_trade_destination_choices(teams[[1]], teams), teams[-1L])
  state <- tbi_transaction_state()
  shiny::testServer(v2_trade_intelligence_server, args = list(
    selected_team = shiny::reactive(teams[[1]]), selected_season = shiny::reactive("2026-27"),
    transaction_state = state
  ), {
    session$setInputs(multi_mode = "3", multi_team_1 = teams[[1]], multi_team_2 = teams[[2]], multi_team_3 = teams[[3]])
    session$flushReact()
    workspace <- paste(unlist(output$multi_team_workspaces), collapse = " ")
    testthat::expect_match(workspace, "player_route_3_clear", fixed = TRUE)
    testthat::expect_equal(lengths(regmatches(workspace, gregexpr("player_route_[0-9]+_identity_ui", workspace)))[[1]], 15L)

    for (i in 1:5) {
      rendered <- paste(unlist(output[[paste0("player_route_", i, "_identity_ui")]]), collapse = " ")
      testthat::expect_match(rendered, paste("Player", i), fixed = TRUE)
      testthat::expect_match(rendered, cleveland$player_name[[i]], fixed = TRUE)
      testthat::expect_match(rendered, v2_trade_money(cleveland$cap_hit[[i]]), fixed = TRUE)
      values <- stats::setNames(list(as.character(cleveland$player_id[[i]]), teams[[if (i %% 2L) 2L else 3L]]),
        c(paste0("player_route_", i, "_id"), paste0("player_route_", i, "_to")))
      do.call(session$setInputs, values)
      session$flushReact()
      if (i == 1L) {
        second <- paste(unlist(output$player_route_2_identity_ui), collapse = " ")
        testthat::expect_false(grepl(cleveland$player_name[[1]], second, fixed = TRUE))
      }
    }
    routes <- build_routes("player", v2_trade_max_player_slots, teams)
    testthat::expect_equal(nrow(routes), 5L)
    testthat::expect_identical(as.character(routes$player_id), as.character(cleveland$player_id[1:5]))
    testthat::expect_true(all(routes$to_team_id != routes$from_team_id))

    session$setInputs(player_route_5_id = as.character(cleveland$player_id[[1]]))
    testthat::expect_error(build_routes("player", v2_trade_max_player_slots, teams), "cannot be selected")
    session$setInputs(player_route_5_id = as.character(cleveland$player_id[[5]]), player_route_3_id = "")
    remaining <- build_routes("player", v2_trade_max_player_slots, teams)
    testthat::expect_identical(as.character(remaining$player_id), as.character(cleveland$player_id[c(1, 2, 4, 5)]))

    clear_cleveland <- stats::setNames(as.list(rep("", 5L)), paste0("player_route_", 1:5, "_id"))
    do.call(session$setInputs, clear_cleveland)
    for (i in 1:5) {
      slot <- i + 5L
      values <- stats::setNames(list(as.character(atlanta$player_id[[i]]), teams[[if (i %% 2L) 1L else 3L]]),
        c(paste0("player_route_", slot, "_id"), paste0("player_route_", slot, "_to")))
      do.call(session$setInputs, values)
    }
    session$flushReact()
    routes <- build_routes("player", v2_trade_max_player_slots, teams)
    testthat::expect_equal(nrow(routes), 5L)
    testthat::expect_identical(as.character(routes$player_id), as.character(atlanta$player_id[1:5]))
    testthat::expect_true(all(routes$from_team_id == teams[[2]]))

    session$setInputs(player_route_10_to = teams[[2]])
    testthat::expect_equal(nrow(build_routes("player", v2_trade_max_player_slots, teams)), 4L)
    session$setInputs(player_route_10_to = teams[[1]])

    session$setInputs(multi_mode = "4", multi_team_1 = teams[[1]], multi_team_2 = teams[[2]],
      multi_team_3 = teams[[3]], multi_team_4 = fourth_team)
    session$flushReact()
    workspace <- paste(unlist(output$multi_team_workspaces), collapse = " ")
    testthat::expect_equal(lengths(regmatches(workspace, gregexpr("player_route_[0-9]+_identity_ui", workspace)))[[1]], 20L)
    testthat::expect_identical(vapply(c(1L, 6L, 11L, 16L), function(i) {
      v2_trade_route_source("player", i, c(teams, fourth_team))
    }, character(1)), c(teams, fourth_team))
    for (i in c(1L, 6L, 11L, 16L)) {
      rendered <- paste(unlist(output[[paste0("player_route_", i, "_identity_ui")]]), collapse = " ")
      testthat::expect_match(rendered, "Player", fixed = TRUE)
      testthat::expect_match(rendered, "$", fixed = TRUE)
    }

    stale_id <- as.character(cleveland$player_id[[1]])
    session$setInputs(player_route_1_id = stale_id, player_route_1_to = teams[[2]], multi_team_1 = "Charlotte Hornets")
    session$flushReact()
    changed_teams <- c("Charlotte Hornets", teams[[2]], teams[[3]], fourth_team)
    changed <- paste(unlist(output$player_route_1_identity_ui), collapse = " ")
    testthat::expect_false(grepl(cleveland$player_name[[1]], changed, fixed = TRUE))
    stale_routes <- build_routes("player", v2_trade_max_player_slots, changed_teams)
    testthat::expect_false(stale_id %in% as.character(stale_routes$player_id))
  })
})

trade_test_exception <- function(amount = 10, expiration = "2027-07-01", id = "tpe") {
  data.frame(team_id = "A", season = "2026-27", exception_id = id,
    exception_type = "TRADED_PLAYER_EXCEPTION", original_amount = amount, remaining_amount = amount,
    creation_transaction = "verified-reference", creation_date = "2026-07-01",
    expiration_date = expiration, status = "ACTIVE", source = "verified synthetic reference fixture",
    source_version = "1", verification_status = "VERIFIED", use_restrictions = "fixture only",
    stringsAsFactors = FALSE)
}

testthat::test_that("protected Draft BLOCK controls every Trade decision view", {
  side <- function(name) list(
    is_screen_pass = TRUE,
    screen_status = "PASS",
    status = "PASS",
    requires_manual_review = FALSE,
    matching_rule = "Synthetic salary PASS fixture",
    explanation = "Salary matching passes.",
    incoming_salary = 5000000,
    maximum_incoming_salary = 10000000,
    second_apron_aggregation_violation = FALSE,
    restriction_flags = character(),
    team_name = name
  )
  evaluation <- list(
    status = "PASS",
    overall_status = "PASS",
    is_blocked = FALSE,
    requires_manual_review = FALSE,
    team_a_name = "Atlanta Hawks",
    team_b_name = "Boston Celtics",
    team_a = side("Atlanta Hawks"),
    team_b = side("Boston Celtics")
  )
  draft_block <- list(
    status = "BLOCK",
    blocked = TRUE,
    requires_manual_review = FALSE,
    summary = "Protected pick cannot legally be conveyed."
  )
  outgoing <- data.frame(
    player_id = 1L,
    player_name = "Outgoing Fixture",
    cap_hit = 5000000,
    stringsAsFactors = FALSE
  )
  incoming <- data.frame(
    player_id = 2L,
    player_name = "Incoming Fixture",
    cap_hit = 5000000,
    stringsAsFactors = FALSE
  )
  state <- tbi_transaction_state()
  shiny::isolate(state$publish_trade(
    team = "Atlanta Hawks",
    partner_team = "Boston Celtics",
    season = "2026-27",
    outgoing_players = outgoing,
    incoming_players = incoming,
    outgoing_draft_assets = data.frame(asset_id = "protected-pick"),
    draft_evaluation = draft_block,
    evaluation = evaluation
  ))
  snapshot <- shiny::isolate(state$snapshot())

  decision <- v2_trade_controlling_decision(snapshot)
  testthat::expect_identical(decision$status, "FAIL")
  testthat::expect_identical(decision$code, "PROTECTED_DRAFT_BLOCK")
  testthat::expect_identical(decision$decision, "DO NOT PROCEED")
  testthat::expect_match(decision$basis, "salary or basketball PASS", fixed = TRUE)
  testthat::expect_identical(
    v2_trade_recommendation_model(snapshot)$decision,
    "DO NOT PROCEED"
  )

  cba_failure <- snapshot
  cba_failure$evaluation$status <- "FAIL"
  cba_failure$evaluation$overall_status <- "FAIL"
  testthat::expect_identical(
    v2_trade_controlling_decision(cba_failure)$code,
    "CBA_FAIL"
  )

  draft_review <- snapshot
  draft_review$draft_evaluation$status <- "REVIEW"
  draft_review$draft_evaluation$blocked <- FALSE
  draft_review$draft_screen_status <- "REVIEW"
  testthat::expect_identical(
    v2_trade_controlling_decision(draft_review)$decision,
    "REVIEW REQUIRED"
  )

  no_draft <- snapshot
  no_draft$outgoing_draft_assets <- data.frame()
  no_draft$incoming_draft_assets <- data.frame()
  no_draft$draft_evaluation <- list(status = "NOT USED", blocked = FALSE)
  no_draft$draft_screen_status <- "NOT USED"
  testthat::expect_identical(
    v2_trade_controlling_decision(no_draft)$decision,
    "PROCEED"
  )

  missing_evaluation <- no_draft
  missing_evaluation$evaluation <- NULL
  testthat::expect_identical(
    v2_trade_controlling_decision(missing_evaluation)$decision,
    "REVIEW REQUIRED"
  )

  shiny::testServer(v2_trade_intelligence_server, args = list(
    selected_team = shiny::reactive("Atlanta Hawks"),
    selected_season = shiny::reactive("2026-27"),
    transaction_state = state
  ), {
    session$flushReact()
    for (view in c("scenario_summary", "evaluation_view", "recommendation_view")) {
      rendered <- paste(unlist(output[[view]]), collapse = " ")
      testthat::expect_match(rendered, "DO NOT PROCEED", fixed = TRUE)
    }
  })
})

testthat::test_that("protected two-team builder publishes actual selected players and resets", {
  teams <- c("Atlanta Hawks", "Boston Celtics")
  pools <- lapply(teams, trade_test_players)
  state <- tbi_transaction_state()
  outgoing <- pools[[1]][1, , drop = FALSE]
  incoming <- pools[[2]][1, , drop = FALSE]
  team_a <- build_trade_team_input(teams[[1]], "2026-27", outgoing$player_id, incoming)
  team_b <- build_trade_team_input(teams[[2]], "2026-27", incoming$player_id, outgoing)
  thresholds <- get_cap_thresholds("2026-27")
  evaluation <- evaluate_two_team_trade(team_a, team_b, thresholds)
  shiny::isolate(state$publish_trade(teams[[1]], teams[[2]], "2026-27", outgoing, incoming, evaluation = evaluation))
  snapshot <- shiny::isolate(state$snapshot())
  testthat::expect_true(snapshot$active)
  testthat::expect_identical(snapshot$scenario_scope, "SHARED_SUPPORTED")
  testthat::expect_true(shiny::isolate(tbi_scenario_active(state)))
  testthat::expect_equal(nrow(snapshot$outgoing_players), 1L)
  testthat::expect_equal(nrow(snapshot$incoming_players), 1L)
  testthat::expect_equal(snapshot$outgoing_salary, outgoing$cap_hit[[1]])
  testthat::expect_equal(snapshot$incoming_salary, incoming$cap_hit[[1]])
  state$clear()
  testthat::expect_false(shiny::isolate(state$snapshot())$active)
})

testthat::test_that("organization switches preserve the active shared trade scenario", {
  team_names <- c("Atlanta Hawks", "Boston Celtics")
  testthat::local_mocked_bindings(
    get_teams = function() data.frame(
      team_name = team_names,
      stringsAsFactors = FALSE
    ),
    .package = "basketballops"
  )
  outgoing <- trade_test_players(team_names[[1]])[1, , drop = FALSE]
  incoming <- trade_test_players(team_names[[2]])[1, , drop = FALSE]
  state <- tbi_transaction_state()
  team <- shiny::reactiveVal(team_names[[1]])
  season <- shiny::reactiveVal("2026-27")

  shiny::isolate(state$publish_trade(
    team_names[[1]],
    team_names[[2]],
    season(),
    outgoing,
    incoming
  ))
  before <- shiny::isolate(state$snapshot())

  shiny::testServer(
    mod_trade_analyzer_server,
    args = list(
      selected_team = team,
      selected_season = season,
      transaction_state = state
    ),
    {
      session$flushReact()
      team(team_names[[2]])
      session$flushReact()
      team(team_names[[1]])
      session$flushReact()

      after <- shiny::isolate(state$snapshot())
      testthat::expect_true(after$active)
      testthat::expect_identical(after$scenario_scope, "SHARED_SUPPORTED")
      testthat::expect_identical(after$scenario_id, before$scenario_id)
      testthat::expect_identical(after$team, before$team)
      testthat::expect_identical(after$partner_team, before$partner_team)
      testthat::expect_identical(after$season, before$season)
      testthat::expect_identical(after$outgoing_players, before$outgoing_players)
      testthat::expect_identical(after$incoming_players, before$incoming_players)
    }
  )
})

testthat::test_that("Recommendation uses the successful evaluated two-team trade and scenario TPE", {
  teams <- c("Cleveland Cavaliers", "Atlanta Hawks")
  pools <- lapply(teams, trade_test_players)
  outgoing <- pools[[1]][pools[[1]]$player_name == "Craig Porter Jr.", , drop = FALSE]
  incoming <- pools[[2]][pools[[2]]$player_name == "Mouhamed Gueye", , drop = FALSE]
  testthat::expect_equal(nrow(outgoing), 1L)
  testthat::expect_equal(nrow(incoming), 1L)
  team_a <- build_trade_team_input(teams[[1]], "2026-27", outgoing$player_id, incoming)
  team_b <- build_trade_team_input(teams[[2]], "2026-27", incoming$player_id, outgoing)
  evaluation <- evaluate_two_team_trade(team_a, team_b, get_cap_thresholds("2026-27"))
  testthat::expect_identical(evaluation$status, "PASS")

  authority_entry <- trade_test_exception(amount = 12000000, id = "recommendation-tpe")
  authority_entry$team_id <- teams[[1]]
  tpe_amount <- as.numeric(incoming$cap_hit[[1]])
  testthat::expect_true(is.finite(tpe_amount) && tpe_amount > 0 && tpe_amount <= 12000000)
  state <- tbi_transaction_state()
  shiny::isolate({
    state$publish_trade(teams[[1]], teams[[2]], "2026-27", outgoing, incoming, evaluation = evaluation)
    graph <- v2_trade_two_team_scenario_graph(state$snapshot())
    exception <- apply_v2_scenario_exceptions(
      new_v2_team_exception_ledger(authority_entry),
      data.frame(
        route_id = "two-team-incoming-1",
        team_id = teams[[1]],
        exception_id = "recommendation-tpe",
        amount = tpe_amount
      ),
      "2026-08-20",
      transaction_graph = graph
    )
    state$set_v2_exception_scenario(exception)
  })

  shiny::testServer(v2_trade_intelligence_server, args = list(
    selected_team = shiny::reactive(teams[[1]]), selected_season = shiny::reactive("2026-27"),
    transaction_state = state
  ), {
    rendered <- paste(unlist(output$recommendation_view), collapse = " ")
    testthat::expect_match(rendered, "PROCEED", fixed = TRUE)
    testthat::expect_match(rendered, paste(teams[[1]], "salary matching"), fixed = TRUE)
    testthat::expect_match(rendered, evaluation$team_a$matching_rule, fixed = TRUE)
    testthat::expect_match(rendered, paste("Salary sent", v2_trade_money(outgoing$cap_hit)), fixed = TRUE)
    testthat::expect_match(rendered, "recommendation-tpe", fixed = TRUE)
    testthat::expect_match(rendered, paste(v2_trade_money(tpe_amount), "used"), fixed = TRUE)
    testthat::expect_match(rendered, paste(v2_trade_money(12000000 - tpe_amount), "remaining"), fixed = TRUE)
    testthat::expect_match(rendered, "No routed draft assets", fixed = TRUE)
    testthat::expect_match(rendered, "Basketball recalculation is unavailable", fixed = TRUE)
    testthat::expect_match(rendered, "RULE RESULT", fixed = TRUE)
    testthat::expect_match(rendered, "FACT", fixed = TRUE)
    testthat::expect_false(grepl("Verify salary matching, apron, aggregation", rendered, fixed = TRUE))

    session$setInputs(reset_two_team = 1)
    session$flushReact()
    testthat::expect_match(paste(unlist(output$recommendation_view), collapse = " "), "Not evaluated yet", fixed = TRUE)
  })
})

testthat::test_that("Recommendation exposes the exact failing two-team salary rule", {
  teams <- c("Atlanta Hawks", "Boston Celtics")
  pools <- lapply(teams, trade_test_players)
  positive_salary <- which(is.finite(pools[[1]]$cap_hit) & pools[[1]]$cap_hit > 0)
  outgoing <- pools[[1]][positive_salary[[which.min(pools[[1]]$cap_hit[positive_salary])]], , drop = FALSE]
  incoming <- pools[[2]][which.max(pools[[2]]$cap_hit), , drop = FALSE]
  team_a <- build_trade_team_input(teams[[1]], "2026-27", outgoing$player_id, incoming)
  team_b <- build_trade_team_input(teams[[2]], "2026-27", incoming$player_id, outgoing)
  evaluation <- evaluate_two_team_trade(team_a, team_b, get_cap_thresholds("2026-27"))
  testthat::expect_identical(evaluation$status, "FAIL")
  state <- tbi_transaction_state()
  shiny::isolate(state$publish_trade(teams[[1]], teams[[2]], "2026-27", outgoing, incoming, evaluation = evaluation))

  shiny::testServer(v2_trade_intelligence_server, args = list(
    selected_team = shiny::reactive(teams[[1]]), selected_season = shiny::reactive("2026-27"),
    transaction_state = state
  ), {
    rendered <- paste(unlist(output$recommendation_view), collapse = " ")
    testthat::expect_match(rendered, "DO NOT PROCEED", fixed = TRUE)
    testthat::expect_match(rendered, evaluation$team_a$matching_rule, fixed = TRUE)
    testthat::expect_match(rendered, evaluation$team_a$explanation, fixed = TRUE)
    testthat::expect_match(rendered, paste(teams[[1]], "- FAIL"), fixed = TRUE)
    testthat::expect_match(rendered, "FAIL - BLOCKING", fixed = TRUE)
    testthat::expect_match(rendered, "Reduce incoming salary", fixed = TRUE)
    testthat::expect_match(rendered, "No TPE used", fixed = TRUE)
    testthat::expect_false(grepl("Modify / Review", rendered, fixed = TRUE))
  })
})

testthat::test_that("three-team UI routes actual players and clears downstream state", {
  teams <- c("Atlanta Hawks", "Boston Celtics", "Brooklyn Nets")
  pools <- lapply(teams, trade_test_players)
  state <- tbi_transaction_state()
  shiny::testServer(v2_trade_intelligence_server, args = list(
    selected_team = shiny::reactive(teams[[1]]), selected_season = shiny::reactive("2026-27"),
    transaction_state = state
  ), {
    session$setInputs(multi_mode = "3", multi_team_1 = teams[[1]], multi_team_2 = teams[[2]], multi_team_3 = teams[[3]],
      player_route_1_to = teams[[2]], player_route_1_id = as.character(pools[[1]]$player_id[[1]]),
      player_route_6_to = teams[[3]], player_route_6_id = as.character(pools[[2]]$player_id[[1]]),
      player_route_11_to = teams[[1]], player_route_11_id = as.character(pools[[3]]$player_id[[1]]),
      evaluate_multi = 1)
    session$flushReact()
    workspace <- paste(unlist(output$multi_team_workspaces), collapse = " ")
    for (team in teams) testthat::expect_match(workspace, team, fixed = TRUE)
    testthat::expect_match(workspace, "Players being sent", fixed = TRUE)
    testthat::expect_match(workspace, "Draft assets being sent", fixed = TRUE)
    snapshot <- state$snapshot()
    testthat::expect_true(snapshot$active)
    testthat::expect_identical(snapshot$scenario_scope, "TRADE_LOCAL")
    testthat::expect_false(shiny::isolate(tbi_scenario_active(state)))
    testthat::expect_equal(nrow(snapshot$v2_transaction_graph$player_routes), 3L)
    testthat::expect_setequal(names(snapshot$v2_transaction_evaluation$team_results), teams)
    testthat::expect_true(all(is.finite(snapshot$v2_transaction_graph$player_routes$salary)))
    recommendation <- paste(unlist(output$recommendation_view), collapse = " ")
    testthat::expect_match(recommendation, "REVIEW REQUIRED", fixed = TRUE)
    testthat::expect_match(recommendation, "Rule evidence is not available", fixed = TRUE)
    testthat::expect_match(recommendation, "salary_matching", fixed = TRUE)
    testthat::expect_match(recommendation, "Basketball recalculation is unavailable", fixed = TRUE)
    summary <- paste(unlist(output$scenario_summary), collapse = " ")
    testthat::expect_match(summary, "TRADE-LOCAL", fixed = TRUE)
    testthat::expect_match(summary, "Not propagated to downstream pages", fixed = TRUE)
    session$setInputs(reset_multi = 1)
    session$flushReact()
    cleared <- state$snapshot()
    testthat::expect_false(cleared$active)
    testthat::expect_null(cleared$v2_transaction_graph)
    testthat::expect_null(cleared$v2_organizational_impact)
  })
})

testthat::test_that("four-team UI routes players and a controlled draft asset", {
  teams <- c("Atlanta Hawks", "Boston Celtics", "Brooklyn Nets", "Charlotte Hornets")
  pools <- lapply(teams, trade_test_players)
  asset <- tbi_trade_selectable_draft_assets(teams[[1]])
  testthat::expect_gt(nrow(asset), 0L)
  state <- tbi_transaction_state()
  shiny::testServer(v2_trade_intelligence_server, args = list(
    selected_team = shiny::reactive(teams[[1]]), selected_season = shiny::reactive("2026-27"),
    transaction_state = state
  ), {
    values <- list(multi_mode = "4", multi_team_1 = teams[[1]], multi_team_2 = teams[[2]], multi_team_3 = teams[[3]], multi_team_4 = teams[[4]])
    slots <- c(1L, 6L, 11L, 16L)
    for (i in seq_along(teams)) {
      slot <- slots[[i]]
      values[[paste0("player_route_", slot, "_to")]] <- teams[[if (i == 4L) 1L else i + 1L]]
      values[[paste0("player_route_", slot, "_id")]] <- as.character(pools[[i]]$player_id[[1]])
    }
    values$asset_route_1_from <- teams[[1]]
    values$asset_route_1_to <- teams[[3]]
    values$asset_route_1_id <- as.character(asset$draft_asset_id[[1]])
    values$evaluate_multi <- 1
    do.call(session$setInputs, values)
    session$flushReact()
    workspace <- paste(unlist(output$multi_team_workspaces), collapse = " ")
    for (team in teams) testthat::expect_match(workspace, team, fixed = TRUE)
    testthat::expect_match(workspace, "Destination", fixed = TRUE)
    asset_choices <- paste(unlist(output$asset_route_1_identity_ui), collapse = " ")
    testthat::expect_match(asset_choices, as.character(asset$draft_asset_id[[1]]), fixed = TRUE)
    snapshot <- state$snapshot()
    testthat::expect_true(snapshot$active)
    testthat::expect_identical(snapshot$scenario_scope, "TRADE_LOCAL")
    testthat::expect_false(shiny::isolate(tbi_scenario_active(state)))
    testthat::expect_equal(length(snapshot$v2_transaction_graph$teams), 4L)
    testthat::expect_equal(nrow(snapshot$v2_transaction_graph$player_routes), 4L)
    testthat::expect_equal(nrow(snapshot$v2_transaction_graph$asset_routes), 1L)
    testthat::expect_identical(snapshot$v2_transaction_evaluation$status, "REVIEW")
  })
})

testthat::test_that("changing a participant invalidates an evaluated scenario", {
  teams <- c("Atlanta Hawks", "Boston Celtics", "Brooklyn Nets", "Charlotte Hornets")
  pool <- trade_test_players(teams[[1]])
  state <- tbi_transaction_state()
  shiny::testServer(v2_trade_intelligence_server, args = list(
    selected_team = shiny::reactive(teams[[1]]), selected_season = shiny::reactive("2026-27"),
    transaction_state = state
  ), {
    session$setInputs(multi_mode = "3", multi_team_1 = teams[[1]], multi_team_2 = teams[[2]], multi_team_3 = teams[[3]],
      player_route_1_from = teams[[1]], player_route_1_to = teams[[2]], player_route_1_id = as.character(pool$player_id[[1]]), evaluate_multi = 1)
    session$flushReact()
    testthat::expect_true(state$snapshot()$active)
    session$setInputs(multi_team_3 = teams[[4]])
    session$flushReact()
    testthat::expect_false(state$snapshot()$active)
    testthat::expect_null(state$snapshot()$v2_exception_scenario)
    testthat::expect_identical(input$multi_team_3, teams[[4]])
    testthat::expect_identical(input$multi_team_1, teams[[1]])
    testthat::expect_identical(input$multi_team_2, teams[[2]])
    testthat::expect_identical(input$player_route_1_id, as.character(pool$player_id[[1]]))
    testthat::expect_identical(input$player_route_1_to, teams[[2]])
  })
})

testthat::test_that("multi-team builder edits invalidate results without resetting compatible inputs", {
  teams <- c("Atlanta Hawks", "Boston Celtics", "Brooklyn Nets", "Charlotte Hornets")
  pool <- trade_test_players(teams[[1]])
  eligible <- which(is.finite(pool$cap_hit) & pool$cap_hit > 0 & pool$cap_hit <= 12000000)
  testthat::expect_gte(length(eligible), 2L)
  players <- pool[eligible[1:2], , drop = FALSE]
  asset <- tbi_trade_selectable_draft_assets(teams[[1]])
  testthat::expect_gt(nrow(asset), 0L)
  entry <- trade_test_exception(amount = 12000000, id = "lifecycle-tpe")
  entry$team_id <- teams[[2]]
  authority <- new_v2_team_exception_ledger(entry)
  state <- tbi_transaction_state()

  shiny::testServer(v2_trade_intelligence_server, args = list(
    selected_team = shiny::reactive(teams[[1]]),
    selected_season = shiny::reactive("2026-27"),
    transaction_state = state,
    authoritative_exception_ledger = authority
  ), {
    evaluate_count <- 1L
    evaluate_current <- function() {
      values <- list(evaluate_multi = evaluate_count)
      evaluate_count <<- evaluate_count + 1L
      do.call(session$setInputs, values)
      session$flushReact()
      testthat::expect_true(state$snapshot()$active)
    }

    session$setInputs(
      multi_mode = "3",
      multi_team_1 = teams[[1]],
      multi_team_2 = teams[[2]],
      multi_team_3 = teams[[3]],
      player_route_1_to = teams[[2]],
      player_route_1_id = as.character(players$player_id[[1]]),
      asset_route_1_to = teams[[3]],
      asset_route_1_id = as.character(asset$draft_asset_id[[1]]),
      tpe_use_id = "lifecycle-tpe",
      tpe_use_amount = players$cap_hit[[1]]
    )
    evaluate_current()

    session$setInputs(asset_route_1_to = teams[[2]])
    session$flushReact()
    testthat::expect_false(state$snapshot()$active)
    testthat::expect_identical(input$asset_route_1_id, as.character(asset$draft_asset_id[[1]]))
    testthat::expect_identical(input$asset_route_1_to, teams[[2]])
    evaluate_current()

    session$setInputs(
      player_route_1_id = as.character(players$player_id[[2]]),
      tpe_use_amount = players$cap_hit[[2]]
    )
    session$flushReact()
    testthat::expect_false(state$snapshot()$active)
    testthat::expect_identical(input$player_route_1_id, as.character(players$player_id[[2]]))
    evaluate_current()

    session$setInputs(player_route_1_to = teams[[3]])
    session$flushReact()
    testthat::expect_false(state$snapshot()$active)
    testthat::expect_identical(input$player_route_1_to, teams[[3]])
    session$setInputs(player_route_1_to = teams[[2]])
    evaluate_current()

    session$setInputs(tpe_use_amount = players$cap_hit[[2]] + 1)
    session$flushReact()
    testthat::expect_false(state$snapshot()$active)
    testthat::expect_equal(input$tpe_use_amount, players$cap_hit[[2]] + 1)
    session$setInputs(tpe_use_amount = players$cap_hit[[2]])
    evaluate_current()

    session$setInputs(multi_mode = "4")
    session$flushReact()
    testthat::expect_false(state$snapshot()$active)
    testthat::expect_identical(input$multi_mode, "4")
    testthat::expect_identical(input$multi_team_1, teams[[1]])
    testthat::expect_identical(input$multi_team_2, teams[[2]])
    testthat::expect_identical(input$multi_team_3, teams[[3]])
    testthat::expect_identical(input$player_route_1_id, as.character(players$player_id[[2]]))
    testthat::expect_identical(input$asset_route_1_id, as.character(asset$draft_asset_id[[1]]))
  })
})

testthat::test_that("multi-team preview renders only the canonical transaction identity", {
  teams <- c("Atlanta Hawks", "Boston Celtics", "Brooklyn Nets")
  state <- tbi_transaction_state()
  publish_multi <- function(graph) {
    evaluation <- evaluate_multiteam_transaction(graph)
    impact <- build_v2_organizational_impact(graph, evaluation)
    shiny::isolate(state$publish_v2_transaction(graph, evaluation, impact))
  }
  first <- normalize_transaction_graph(
    "same-second-id",
    teams,
    data.frame(
      route_id = "p1",
      player_id = "first-player",
      from_team_id = teams[[1]],
      to_team_id = teams[[2]],
      salary = 1,
      stringsAsFactors = FALSE
    )
  )
  second <- normalize_transaction_graph(
    "same-second-id",
    teams,
    data.frame(
      route_id = "p2",
      player_id = "replacement-player",
      from_team_id = teams[[2]],
      to_team_id = teams[[3]],
      salary = 2,
      stringsAsFactors = FALSE
    )
  )
  publish_multi(first)

  shiny::testServer(v2_trade_intelligence_server, args = list(
    selected_team = shiny::reactive(teams[[1]]),
    selected_season = shiny::reactive("2026-27"),
    transaction_state = state
  ), {
    session$flushReact()
    testthat::expect_identical(canonical_multi_result()$graph$signature, first$signature)
    testthat::expect_match(
      paste(unlist(output$multi_team_preview), collapse = " "),
      teams[[1]],
      fixed = TRUE
    )

    shiny::isolate(state$publish_trade(
      teams[[1]],
      teams[[2]],
      "2026-27",
      data.frame(player_id = "out", player_name = "Out", cap_hit = 1),
      data.frame(player_id = "in", player_name = "In", cap_hit = 1),
      evaluation = NULL
    ))
    session$flushReact()
    testthat::expect_null(canonical_multi_result())
    testthat::expect_match(
      paste(unlist(output$multi_team_preview), collapse = " "),
      "No multi-team result",
      fixed = TRUE
    )
    testthat::expect_match(
      paste(unlist(output$scenario_summary), collapse = " "),
      "Not evaluated yet",
      fixed = TRUE
    )

    publish_multi(second)
    session$flushReact()
    testthat::expect_identical(canonical_multi_result()$graph$signature, second$signature)
    testthat::expect_false(identical(first$signature, second$signature))
  })
})

testthat::test_that("verified TPE scenarios cover use, failure, expiry, creation, and isolation", {
  authority <- new_v2_team_exception_ledger(trade_test_exception(amount = 12, id = "repair-tpe"))
  request <- function(amount) data.frame(route_id = "route", team_id = "A", exception_id = "repair-tpe", amount = amount)
  graph <- function(amount) normalize_transaction_graph(
    paste0("repair-tpe-", amount),
    c("A", "B"),
    data.frame(route_id = "route", player_id = "incoming", from_team_id = "B",
      to_team_id = "A", salary = amount, stringsAsFactors = FALSE),
    season = "2026-27"
  )
  partial <- apply_v2_scenario_exceptions(authority, request(5), "2026-08-20", transaction_graph = graph(5))
  full <- apply_v2_scenario_exceptions(authority, request(12), "2026-08-20", transaction_graph = graph(12))
  insufficient <- apply_v2_scenario_exceptions(authority, request(13), "2026-08-20", transaction_graph = graph(13))
  expired_authority <- new_v2_team_exception_ledger(trade_test_exception(amount = 12, expiration = "2026-08-01", id = "repair-tpe"))
  expired <- apply_v2_scenario_exceptions(expired_authority, request(1), "2026-08-20", transaction_graph = graph(1))
  created <- apply_v2_scenario_exceptions(authority, data.frame(), "2026-08-20", creation_facts = trade_test_exception(amount = 6, id = "created-tpe"))
  testthat::expect_equal(partial$scenario_ledger$entries$remaining_amount, 7)
  testthat::expect_identical(full$scenario_ledger$entries$status, "CONSUMED")
  testthat::expect_identical(insufficient$status, "FAIL")
  testthat::expect_identical(expired$status, "FAIL")
  testthat::expect_true("created-tpe" %in% created$scenario_ledger$entries$exception_id)
  testthat::expect_equal(authority$entries$remaining_amount, 12)
  testthat::expect_false(created$scenario_ledger$authoritative)
})

testthat::test_that("multi-team UI applies verified TPE use without mutating authority", {
  teams <- c("Atlanta Hawks", "Boston Celtics", "Brooklyn Nets")
  pool <- trade_test_players(teams[[1]])
  eligible <- which(is.finite(pool$cap_hit) & pool$cap_hit > 0 & pool$cap_hit <= 12000000)
  testthat::expect_gt(length(eligible), 0L)
  player <- pool[eligible[[1]], , drop = FALSE]
  authority_entry <- trade_test_exception(amount = 12000000, id = "ui-tpe")
  authority_entry$team_id <- teams[[2]]
  authority <- new_v2_team_exception_ledger(authority_entry)
  state <- tbi_transaction_state()
  shiny::testServer(v2_trade_intelligence_server, args = list(
    selected_team = shiny::reactive(teams[[1]]), selected_season = shiny::reactive("2026-27"),
    transaction_state = state, authoritative_exception_ledger = authority
  ), {
    session$setInputs(multi_mode = "3", multi_team_1 = teams[[1]], multi_team_2 = teams[[2]], multi_team_3 = teams[[3]],
      player_route_1_from = teams[[1]], player_route_1_to = teams[[2]], player_route_1_id = as.character(player$player_id[[1]]),
      tpe_use_id = "ui-tpe", tpe_use_amount = player$cap_hit[[1]], evaluate_multi = 1)
    session$flushReact()
    testthat::expect_match(paste(unlist(output$tpe_inventory), collapse = " "), "ui-tpe", fixed = TRUE)
    scenario <- state$snapshot()$v2_exception_scenario
    testthat::expect_equal(scenario$scenario_ledger$entries$remaining_amount, 12000000 - player$cap_hit[[1]])
    testthat::expect_equal(scenario$scenario_ledger$usage_history$amount_used, player$cap_hit[[1]])
    testthat::expect_identical(
      scenario$scenario_ledger$transaction_signature,
      state$snapshot()$v2_transaction_graph$signature
    )
    testthat::expect_equal(authority$entries$remaining_amount, 12000000)
  })
})

testthat::test_that("actual development UI exercises use, expiry, creation, and reset end to end", {
  withr::local_envvar(TBI_ENABLE_TPE_TEST_MODE = "true")
  team <- "Atlanta Hawks"
  partner <- "Boston Celtics"
  state <- tbi_transaction_state()
  reset_called <- shiny::reactiveVal(FALSE)
  outgoing <- data.frame(
    player_id = "development-outgoing",
    player_name = "Development Outgoing",
    cap_hit = 4000000,
    stringsAsFactors = FALSE
  )
  incoming <- data.frame(
    player_id = paste0("development-incoming-", 1:4),
    player_name = paste("Development Incoming", 1:4),
    cap_hit = c(5000000, 12000000, 13000000, 1000000),
    stringsAsFactors = FALSE
  )
  shiny::isolate(state$publish_trade(team, partner, "2026-27", outgoing, incoming))
  shiny::testServer(v2_trade_intelligence_server, args = list(
    selected_team = shiny::reactive(team), selected_season = shiny::reactive("2026-27"),
    transaction_state = state, reset_two_team_builder = function() reset_called(TRUE)
  ), {
    session$setInputs(development_tpe_team = team, development_tpe_state = "ACTIVE", development_tpe_amount = 5000000,
      development_tpe_apply = 1)
    session$flushReact()
    partial <- development_tpe_scenario()
    testthat::expect_identical(partial$status, "PASS")
    testthat::expect_equal(partial$scenario_ledger$entries$remaining_amount, 7000000)
    testthat::expect_equal(development_authority()$entries$remaining_amount, 12000000)
    testthat::expect_identical(state$snapshot()$v2_exception_scenario$status, "PASS")
    testthat::expect_match(paste(unlist(output$development_tpe_ledger), collapse = " "), "Scenario after use", fixed = TRUE)
    testthat::expect_match(paste(unlist(output$scenario_summary), collapse = " "), "Not evaluated yet", fixed = TRUE)
    missing_evaluation <- paste(unlist(output$recommendation_view), collapse = " ")
    testthat::expect_match(missing_evaluation, "Not evaluated yet", fixed = TRUE)

    session$setInputs(development_tpe_reset = 1)
    session$flushReact()
    testthat::expect_null(development_tpe_scenario())

    session$setInputs(development_tpe_state = "ACTIVE", development_tpe_amount = 12000000, development_tpe_apply = 2)
    session$flushReact()
    full <- development_tpe_scenario()
    testthat::expect_identical(full$status, "PASS")
    testthat::expect_identical(full$scenario_ledger$entries$status, "CONSUMED")
    testthat::expect_equal(development_authority()$entries$remaining_amount, 12000000)

    session$setInputs(development_tpe_reset = 2)
    session$setInputs(development_tpe_state = "ACTIVE", development_tpe_amount = 13000000, development_tpe_apply = 3)
    session$flushReact()
    testthat::expect_identical(development_tpe_scenario()$status, "FAIL")
    testthat::expect_equal(development_tpe_scenario()$scenario_ledger$entries$remaining_amount, 12000000)

    session$setInputs(development_tpe_reset = 3)
    session$setInputs(development_tpe_state = "EXPIRED", development_tpe_amount = 1000000, development_tpe_apply = 4)
    session$flushReact()
    testthat::expect_identical(development_tpe_scenario()$status, "FAIL")
    testthat::expect_match(paste(unlist(output$development_tpe_ledger), collapse = " "), "expired or exceeds", fixed = TRUE)

    session$setInputs(development_tpe_reset = 4)
    session$setInputs(development_tpe_state = "ACTIVE", development_tpe_create = 1)
    session$flushReact()
    created <- development_tpe_scenario()
    testthat::expect_identical(created$status, "PASS")
    testthat::expect_true("development-created-tpe-6m" %in% created$scenario_ledger$entries$exception_id)
    testthat::expect_equal(development_authority()$entries$remaining_amount, 12000000)
    testthat::expect_match(paste(unlist(output$development_tpe_ledger), collapse = " "), "DEVELOPMENT / SCENARIO ONLY", fixed = TRUE)
    testthat::expect_match(paste(unlist(output$development_tpe_ledger), collapse = " "), "Created TPE", fixed = TRUE)
    testthat::expect_match(paste(unlist(output$evaluation_view), collapse = " "), "Not evaluated yet", fixed = TRUE)

    session$setInputs(reset_two_team = 1)
    session$flushReact()
    testthat::expect_null(development_tpe_scenario())
    testthat::expect_false(state$snapshot()$active)
    testthat::expect_true(reset_called())
    testthat::expect_null(state$snapshot()$v2_exception_scenario)
    testthat::expect_match(paste(unlist(output$scenario_summary), collapse = " "), "Not evaluated yet", fixed = TRUE)
    testthat::expect_match(paste(unlist(output$evaluation_view), collapse = " "), "Not evaluated yet", fixed = TRUE)
    testthat::expect_match(paste(unlist(output$recommendation_view), collapse = " "), "Not evaluated yet", fixed = TRUE)
  })
})
