testthat::test_that("Depth Chart immediately returns to baseline after the canonical trade reset", {
  baseline <- data.frame(
    player_id = c(1L, 2L),
    player_name = c("Baseline Guard", "Baseline Wing"),
    cap_hit = c(10000000, 8000000),
    stringsAsFactors = FALSE
  )
  outgoing <- baseline[1L, , drop = FALSE]
  incoming <- data.frame(
    player_id = 3L,
    player_name = "Incoming Guard",
    cap_hit = 9000000,
    stringsAsFactors = FALSE
  )
  state <- tbi_transaction_state()

  shiny::isolate(state$publish_trade(
    team = "Boston Celtics",
    partner_team = "Charlotte Hornets",
    season = "2025-26",
    outgoing_players = outgoing,
    incoming_players = incoming
  ))

  active <- shiny::isolate(depth_chart_active_trade_scenario(
    state$snapshot(), "Boston Celtics", "2025-26"
  ))
  preview <- shiny::isolate(tbi_apply_trade_scenario_to_roster(
    baseline, state, "Boston Celtics"
  ))
  testthat::expect_true(is.list(active))
  testthat::expect_identical(preview$player_id, c(2L, 3L))

  partner <- shiny::isolate(depth_chart_active_trade_scenario(
    state$snapshot(), "Charlotte Hornets", "2025-26"
  ))
  testthat::expect_identical(partner$team, "Charlotte Hornets")
  testthat::expect_identical(partner$partner_team, "Boston Celtics")
  testthat::expect_identical(partner$outgoing_players$player_id, incoming$player_id)
  testthat::expect_identical(partner$incoming_players$player_id, outgoing$player_id)
  testthat::expect_equal(partner$salary_delta, -active$salary_delta)
  testthat::expect_null(shiny::isolate(depth_chart_active_trade_scenario(
    state$snapshot(), "Cleveland Cavaliers", "2025-26"
  )))
  testthat::expect_null(shiny::isolate(depth_chart_active_trade_scenario(
    state$snapshot(), "Boston Celtics", "2026-27"
  )))

  state$clear()

  cleared <- shiny::isolate(depth_chart_active_trade_scenario(
    state$snapshot(), "Boston Celtics", "2025-26"
  ))
  restored <- shiny::isolate(tbi_apply_trade_scenario_to_roster(
    baseline, state, "Boston Celtics"
  ))
  testthat::expect_null(cleared)
  testthat::expect_identical(restored, baseline)
})

depth_chart_stabilization_shadow <- function() {
  players <- data.frame(
    player_id = 1:10,
    player_name = paste("Rotation Player", 1:10),
    is_starter = c(rep(TRUE, 5), rep(FALSE, 5)),
    rotation_role = c(rep("STARTER", 5), "SIXTH_MAN", rep("BENCH", 4)),
    stringsAsFactors = FALSE
  )
  minutes <- transform(
    players,
    availability_status = c(rep("AVAILABLE", 8), rep("UNKNOWN", 2)),
    assigned_minutes = c(32L, 31L, 30L, 29L, 28L, 22L, 20L, 18L, 16L, 14L)
  )
  segments <- data.frame(
    period = 1:4,
    start_clock = rep("12:00", 4),
    end_clock = rep("00:00", 4),
    duration = rep(12L, 4),
    starter_count = rep(5L, 4),
    creator_coverage_status = rep("UNKNOWN", 4),
    big_center_coverage_status = rep("UNKNOWN", 4),
    stringsAsFactors = FALSE
  )
  segments$player_ids <- I(rep(list(1:5), 4))
  substitutions <- data.frame(
    event_id = paste0("SUB-", 1:4),
    period = 1:4,
    clock = rep("06:00", 4),
    player_out = rep(5L, 4),
    player_in = rep(6L, 4),
    stringsAsFactors = FALSE
  )
  lineups <- data.frame(
    lineup_type = c("BASE", "OFFENSE", "CLOSING"),
    legality_status = "PASS",
    explanation = c("Approved starting group.", "Legal offense group.", "Legal closing group."),
    reliability = "INDIVIDUAL_EVIDENCE_INCOMPLETE",
    stringsAsFactors = FALSE
  )
  lineups$player_ids <- I(list(1:5, c(1:4, 6L), c(1:4, 6L)))
  list(
    execution_status = "COMPLETED",
    phase2_diagnostics = list(status = "REVIEW", is_blocked = FALSE),
    rotation_10 = list(members = players),
    minute_ledger = list(
      ledger = minutes,
      total_assigned_minutes = 240L,
      status = "REVIEW",
      is_blocked = FALSE
    ),
    stagger_plan = list(
      segments = segments,
      substitution_events = substitutions,
      total_player_minutes = 240L,
      status = "REVIEW"
    ),
    lineup_portfolio = list(lineups = lineups, status = "REVIEW", is_blocked = FALSE),
    validation_findings = list(list(status = "REVIEW", code = "ROLE_COVERAGE_UNKNOWN"))
  )
}

testthat::test_that("Rotation is a populated, distinct 240-minute decision view", {
  html <- htmltools::renderTags(
    v2_ui_depth_intelligence(depth_chart_stabilization_shadow())
  )$html

  testthat::expect_match(html, 'data-tbi-v2-depth-view="rotation"', fixed = TRUE)
  testthat::expect_match(html, "Starting Five", fixed = TRUE)
  testthat::expect_match(html, "Bench Rotation", fixed = TRUE)
  testthat::expect_match(html, "Rotation Player 1", fixed = TRUE)
  testthat::expect_match(html, "Rotation Player 10", fixed = TRUE)
  testthat::expect_match(html, "240 / 240", fixed = TRUE)
  testthat::expect_match(html, "Sixth man", fixed = TRUE)
  testthat::expect_match(html, "AVAILABLE", fixed = TRUE)
})

testthat::test_that("selected-player detail is contained and can be cleared", {
  ui <- htmltools::renderTags(mod_depth_chart_ui("depth_chart"))$html
  css <- paste(readLines(
    file.path(testthat::test_path(), "..", "..", "inst", "app", "www", "tbi_phase3.css"),
    warn = FALSE
  ), collapse = "\n")

  testthat::expect_match(ui, 'id="depth_chart-clear_player_detail"', fixed = TRUE)
  testthat::expect_match(ui, "Clear player detail", fixed = TRUE)
  testthat::expect_match(css, ".depth-v21-rail", fixed = TRUE)
  testthat::expect_match(ui, 'class="depth-v21-right-workspace"', fixed = TRUE)
  testthat::expect_false(grepl("isolation:isolate", css, fixed = TRUE))
  testthat::expect_false(grepl("z-index:999", css, fixed = TRUE))
})

testthat::test_that("Lineups renders three primary groups and one situational detail", {
  html <- htmltools::renderTags(
    v2_ui_depth_lineups(
      depth_chart_stabilization_shadow(),
      lineup_working_ui = shiny::div(
        class = "tbi-p3-working-lineup-card",
        "Working lineup fixture"
      )
    )
  )$html

  testthat::expect_match(html, 'data-tbi-v2-depth-view="lineup"', fixed = TRUE)
  testthat::expect_match(html, 'class="tbi-p3-lineup-row tbi-p3-lineup-row-top"', fixed = TRUE)
  testthat::expect_match(html, 'class="tbi-p3-situational-lineups"', fixed = TRUE)
  testthat::expect_match(html, 'data-tbi-situational-active="OFFENSE"', fixed = TRUE)
  testthat::expect_match(html, 'class="tbi-p3-situational-controls"', fixed = TRUE)
  testthat::expect_match(html, 'class="tbi-p3-situational-detail"', fixed = TRUE)
  testthat::expect_match(html, "Starting Lineup", fixed = TRUE)
  testthat::expect_match(html, "Closing Lineup", fixed = TRUE)
  testthat::expect_match(html, "Offense Lineup", fixed = TRUE)
  testthat::expect_match(html, "Defense Lineup", fixed = TRUE)
  testthat::expect_match(html, "Small-Ball Lineup", fixed = TRUE)
  testthat::expect_match(html, "Bench Bridge", fixed = TRUE)
  testthat::expect_match(html, "Working lineup fixture", fixed = TRUE)
  testthat::expect_match(html, "Rotation Player 6", fixed = TRUE)
  testthat::expect_match(html, "Approved starting group", fixed = TRUE)
  testthat::expect_match(html, "View details", fixed = TRUE)
  testthat::expect_match(html, "INDIVIDUAL_EVIDENCE_INCOMPLETE", fixed = TRUE)
  testthat::expect_false(grepl('class="tbi-p3-metric-grid"', html, fixed = TRUE))
})

testthat::test_that("Lineups keeps the working-five editor mounted behind disclosure", {
  ui <- htmltools::renderTags(mod_depth_chart_ui("depth_chart"))$html

  testthat::expect_match(ui, 'class="depth-lineup-editor-disclosure"', fixed = TRUE)
  testthat::expect_match(ui, "Edit Working Lineup", fixed = TRUE)
  testthat::expect_match(ui, 'id="depth_chart-view_working_lineup"', fixed = TRUE)
  testthat::expect_match(ui, 'id="depth_chart-view_bie_lineup"', fixed = TRUE)
  testthat::expect_match(ui, 'id="depth_chart-use_bie_lineup"', fixed = TRUE)
  testthat::expect_match(ui, 'id="depth_chart-starting_five_court"', fixed = TRUE)
})

testthat::test_that("Staggering and Game Plan are separate existing-output views", {
  html <- htmltools::renderTags(
    v2_ui_depth_intelligence(depth_chart_stabilization_shadow())
  )$html

  testthat::expect_match(html, 'data-tbi-v2-depth-view="staggering"', fixed = TRUE)
  testthat::expect_match(html, 'data-tbi-v2-depth-view="gameplan"', fixed = TRUE)
  for (quarter in paste0("Q", 1:4)) {
    testthat::expect_match(html, paste0(">", quarter, "<"), fixed = TRUE)
  }
  testthat::expect_match(html, "48 minutes", fixed = TRUE)
  testthat::expect_match(html, "Rotation Player 1", fixed = TRUE)
  testthat::expect_match(html, "Starter overlap", fixed = TRUE)
  testthat::expect_match(html, "Substitution sequence", fixed = TRUE)
  testthat::expect_match(html, "Rotation Player 5 out / Rotation Player 6 in", fixed = TRUE)
  testthat::expect_match(html, "Creator coverage", fixed = TRUE)
  testthat::expect_match(html, "Center coverage", fixed = TRUE)
  testthat::expect_match(html, "Key rest windows", fixed = TRUE)
  testthat::expect_match(html, "Rotation flow", fixed = TRUE)
})

testthat::test_that("Depth Chart uses compact rows, inspector fields, and current-five court", {
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/", mustWork = TRUE)
  ui <- htmltools::renderTags(mod_depth_chart_ui("depth_chart"))$html
  module_source <- paste(readLines(file.path(root, "R", "mod_depth_chart.R"), warn = FALSE), collapse = "\n")
  css <- paste(readLines(file.path(root, "inst", "app", "www", "tbi_phase3.css"), warn = FALSE), collapse = "\n")

  testthat::expect_match(module_source, 'class = "depth-v21-player-depth"', fixed = TRUE)
  testthat::expect_false(grepl('class = "depth-v21-player-age"', module_source, fixed = TRUE))
  testthat::expect_match(ui, "Current Starting Five", fixed = TRUE)
  testthat::expect_match(module_source, 'class = "depth-v21-assignment-fields"', fixed = TRUE)
  testthat::expect_match(ui, "depth-v21-contract-output", fixed = TRUE)
  testthat::expect_match(css, "grid-template-columns:repeat(5,minmax(0,1fr))", fixed = TRUE)
  testthat::expect_match(css, "height:190px !important", fixed = TRUE)
  testthat::expect_false(grepl("overflow-y:auto", css, fixed = TRUE))
})

testthat::test_that("actual Depth Chart runtime switches teams without player or plan leakage", {
  team <- shiny::reactiveVal("Boston Celtics")
  season <- shiny::reactiveVal("2026-27")
  state <- tbi_transaction_state()
  expected_teams <- c(
    "Charlotte Hornets",
    "Portland Trail Blazers",
    "Boston Celtics",
    "Brooklyn Nets",
    "Cleveland Cavaliers"
  )
  seen_player_sets <- list()

  shiny::testServer(
    mod_depth_chart_server,
    args = list(
      selected_team = team,
      selected_season = season,
      transaction_state = state,
      rotation_route = tbi_rotation_route("v2_shadow")
    ),
    {
      session$flushReact()
      baseline_roster <- depth_data()
      baseline_shadow <- v2_shadow_diagnostic()
      incoming_pool <- get_depth_chart_records("Charlotte Hornets", "2026-27")
      outgoing <- baseline_roster[1L, , drop = FALSE]
      incoming <- incoming_pool[1L, , drop = FALSE]
      shiny::isolate(state$publish_trade(
        team = "Boston Celtics",
        partner_team = "Charlotte Hornets",
        season = "2026-27",
        outgoing_players = outgoing,
        incoming_players = incoming
      ))
      session$flushReact()
      scenario_roster <- depth_data()
      scenario_shadow <- v2_shadow_diagnostic()
      testthat::expect_true(is.list(active_trade_scenario()))
      testthat::expect_false(outgoing$player_id[[1]] %in% scenario_roster$player_id)
      testthat::expect_true(incoming$player_id[[1]] %in% scenario_roster$player_id)
      testthat::expect_length(unique(as.character(scenario_roster$team_id)), 1L)
      testthat::expect_identical(scenario_shadow$execution_status, "COMPLETED")
      banner_html <- htmltools::renderTags(output$shared_trade_scenario_banner)$html
      testthat::expect_match(banner_html, "TRADE PREVIEW", fixed = TRUE)

      state$clear()
      session$flushReact()
      restored_roster <- depth_data()
      testthat::expect_null(active_trade_scenario())
      testthat::expect_identical(sort(restored_roster$player_id), sort(baseline_roster$player_id))
      cleared_banner_html <- htmltools::renderTags(output$shared_trade_scenario_banner)$html
      testthat::expect_false(grepl("TRADE PREVIEW", cleared_banner_html, fixed = TRUE))
      restored_shadow <- v2_shadow_diagnostic()
      testthat::expect_identical(
        restored_shadow$rotation_10$members$player_id,
        baseline_shadow$rotation_10$members$player_id
      )
      testthat::expect_identical(
        restored_shadow$minute_ledger$ledger,
        baseline_shadow$minute_ledger$ledger
      )
      testthat::expect_identical(
        restored_shadow$lineup_portfolio$lineups,
        baseline_shadow$lineup_portfolio$lineups
      )
      testthat::expect_identical(
        restored_shadow$stagger_plan$segments,
        baseline_shadow$stagger_plan$segments
      )

      for (team_name in expected_teams) {
        team(team_name)
        session$flushReact()

        roster <- depth_data()
        shadow <- v2_shadow_diagnostic()
        selected <- selected_player_id()
        html <- htmltools::renderTags(v2_ui_depth_intelligence(shadow))$html

        testthat::expect_gt(nrow(roster), 10L)
        testthat::expect_true(selected %in% roster$player_id)
        testthat::expect_identical(shadow$execution_status, "COMPLETED")
        testthat::expect_equal(nrow(shadow$rotation_10$members), 10L)
        testthat::expect_identical(shadow$minute_ledger$total_assigned_minutes, 240L)
        testthat::expect_gt(nrow(shadow$lineup_portfolio$lineups), 0L)
        testthat::expect_setequal(unique(shadow$stagger_plan$segments$period), 1:4)
        testthat::expect_match(html, shadow$rotation_10$members$player_name[[1]], fixed = TRUE)
        testthat::expect_true(all(shadow$rotation_10$members$player_id %in% roster$player_id))
        testthat::expect_true(all(unlist(shadow$lineup_portfolio$lineups$player_ids) %in% roster$player_id))
        testthat::expect_true(all(unlist(shadow$stagger_plan$segments$player_ids) %in% roster$player_id))

        segments <- shadow$stagger_plan$segments
        rotation_ids <- as.character(shadow$rotation_10$members$player_id)
        player_minutes <- stats::setNames(rep(0, length(rotation_ids)), rotation_ids)
        for (i in seq_len(nrow(segments))) {
          segment_ids <- as.character(segments$player_ids[[i]])
          testthat::expect_length(segment_ids, 5L)
          testthat::expect_length(unique(segment_ids), 5L)
          testthat::expect_true(all(segment_ids %in% rotation_ids))
          player_minutes[segment_ids] <- player_minutes[segment_ids] + as.numeric(segments$duration[[i]])
        }
        expected_minutes <- stats::setNames(
          as.numeric(shadow$minute_ledger$ledger$assigned_minutes),
          as.character(shadow$minute_ledger$ledger$player_id)
        )
        testthat::expect_equal(player_minutes[names(expected_minutes)], expected_minutes)
        testthat::expect_equal(
          sum(as.numeric(segments$duration) * lengths(segments$player_ids)),
          240
        )
        seen_player_sets[[team_name]] <- sort(as.character(roster$player_id))
      }

      testthat::expect_gt(length(unique(seen_player_sets)), 1L)

      compact_lineup_html <- htmltools::renderTags(
        output$compact_working_lineup_card
      )$html
      testthat::expect_match(
        compact_lineup_html,
        "BIE Recommended / Working Lineup",
        fixed = TRUE
      )
      testthat::expect_match(
        compact_lineup_html,
        "Showing Working Lineup",
        fixed = TRUE
      )
      testthat::expect_match(compact_lineup_html, "PASS", fixed = TRUE)

      recommendation <- bie_recommended_result()
      if (is.list(recommendation) && identical(recommendation$status, "OK")) {
        session$setInputs(view_bie_lineup = 1)
        session$flushReact()
        recommended_html <- htmltools::renderTags(
          output$compact_working_lineup_card
        )$html
        testthat::expect_match(
          recommended_html,
          "Showing BIE Recommended",
          fixed = TRUE
        )

        session$setInputs(view_working_lineup = 1)
        session$flushReact()
      }

      current_roster <- depth_data()
      selected_rows <- c(2L, 4L, 6L)
      for (selected_row in selected_rows) {
        player_id <- current_roster$player_id[[selected_row]]
        session$setInputs(selected_player = player_id)
        session$flushReact()
        testthat::expect_identical(selected_player_id(), as.integer(player_id))
        selected_header <- htmltools::renderTags(output$player_profile_header)$html
        testthat::expect_match(
          selected_header,
          current_roster$player_name[[selected_row]],
          fixed = TRUE
        )
      }
      session$setInputs(clear_player_detail = 1)
      session$flushReact()
      testthat::expect_null(selected_player_id())
      cleared_header <- htmltools::renderTags(output$player_profile_header)$html
      testthat::expect_match(
        cleared_header,
        "Select a player to inspect or edit their depth-chart assignment.",
        fixed = TRUE
      )
    }
  )
})

testthat::test_that("true unavailable states explain the exact failed dependency", {
  failed <- list(
    execution_status = "ERROR",
    phase2_diagnostics = list(
      status = "ERROR",
      is_blocked = TRUE,
      error = list(message = "Starter state blocked by an unavailable approved lock.")
    )
  )
  html <- htmltools::renderTags(v2_ui_depth_intelligence(failed))$html
  testthat::expect_match(html, "V2 rotation could not be built", fixed = TRUE)
  testthat::expect_match(html, "Starter state blocked by an unavailable approved lock", fixed = TRUE)
  testthat::expect_match(html, 'class="tbi-p3-status fail"', fixed = TRUE)
  testthat::expect_match(html, "BLOCKED", fixed = TRUE)

  no_lineups <- depth_chart_stabilization_shadow()
  no_lineups$lineup_portfolio$lineups <- data.frame()
  lineup_html <- htmltools::renderTags(v2_ui_depth_intelligence(no_lineups))$html
  testthat::expect_match(lineup_html, "Lineup portfolio is unavailable", fixed = TRUE)
  testthat::expect_match(lineup_html, "verified position-eligibility findings", fixed = TRUE)

  no_stagger <- depth_chart_stabilization_shadow()
  no_stagger$stagger_plan$segments <- data.frame()
  stagger_html <- htmltools::renderTags(v2_ui_depth_intelligence(no_stagger))$html
  testthat::expect_match(stagger_html, "Staggering plan is unavailable", fixed = TRUE)
  testthat::expect_match(stagger_html, "exact-minute and lineup-feasibility findings", fixed = TRUE)

  partial <- depth_chart_stabilization_shadow()
  partial$phase2_diagnostics <- list(
    status = "ERROR",
    is_blocked = TRUE,
    error = list(message = "Lineup portfolio builder failed after minute reconciliation.")
  )
  partial$lineup_portfolio <- NULL
  partial_html <- htmltools::renderTags(v2_ui_depth_intelligence(partial))$html
  testthat::expect_match(partial_html, "Who Plays and How Much?", fixed = TRUE)
  testthat::expect_match(partial_html, "240 / 240", fixed = TRUE)
  testthat::expect_match(partial_html, "Lineup portfolio is unavailable", fixed = TRUE)
  testthat::expect_match(partial_html, "Lineup portfolio builder failed", fixed = TRUE)
})

testthat::test_that("Depth Chart responsive contracts contain all five views", {
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/", mustWork = TRUE)
  css <- paste(readLines(file.path(root, "inst", "app", "www", "tbi_phase3.css"), warn = FALSE), collapse = "\n")
  javascript <- paste(readLines(file.path(root, "inst", "app", "www", "tbi_ux_foundation.js"), warn = FALSE), collapse = "\n")

  testthat::expect_match(css, '@media(max-width:900px)', fixed = TRUE)
  testthat::expect_match(css, '@media(max-width:520px)', fixed = TRUE)
  testthat::expect_match(css, ".tbi-p3-quarter-grid { grid-template-columns:1fr; }", fixed = TRUE)
  testthat::expect_match(css, ".tbi-p3-lineup-row-top { grid-template-columns:minmax(0,1fr); }", fixed = TRUE)
  testthat::expect_match(css, ".tbi-p3-situational-controls", fixed = TRUE)
  testthat::expect_match(css, "data-tbi-situational-active=\"BENCH_BRIDGE\"", fixed = TRUE)
  testthat::expect_match(css, ".tbi-p3-game-segment-players { white-space:normal; }", fixed = TRUE)
  testthat::expect_match(css, ".tbi-product-workspace { overflow-x:hidden; }", fixed = TRUE)
  testthat::expect_match(javascript, "shell.parentNode.insertBefore(workspace, shell)", fixed = TRUE)
  testthat::expect_match(javascript, "editorDisclosure.removeAttribute('open')", fixed = TRUE)
  for (definition in c("['depth', 'Depth Chart']", "['rotation', 'Rotation']", "['lineup', 'Lineups']", "['staggering', 'Staggering']", "['gameplan', 'Game Plan']")) {
    testthat::expect_match(javascript, definition, fixed = TRUE)
  }
})

testthat::test_that("Depth Chart layout keeps the decision board, detail, and court together", {
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/", mustWork = TRUE)
  ui <- htmltools::renderTags(mod_depth_chart_ui("depth_chart"))$html
  module_source <- paste(readLines(file.path(root, "R", "mod_depth_chart.R"), warn = FALSE), collapse = "\n")
  css <- paste(readLines(file.path(root, "inst", "app", "www", "tbi_phase3.css"), warn = FALSE), collapse = "\n")
  javascript <- paste(readLines(file.path(root, "inst", "app", "www", "tbi_ux_foundation.js"), warn = FALSE), collapse = "\n")

  testthat::expect_match(ui, 'class="depth-v21-right-workspace"', fixed = TRUE)
  testthat::expect_match(
    module_source,
    "Select a player to inspect or edit their depth-chart assignment.",
    fixed = TRUE
  )
  testthat::expect_gt(
    regexpr('id="depth_chart-v2_development_intelligence"', ui, fixed = TRUE)[[1]],
    regexpr('class="depth-v21-shell"', ui, fixed = TRUE)[[1]]
  )
  testthat::expect_match(javascript, "court.setAttribute(", fixed = TRUE)
  testthat::expect_match(javascript, "'depth lineup'", fixed = TRUE)
  testthat::expect_match(javascript, "insertAdjacentElement('afterend', nav)", fixed = TRUE)
  testthat::expect_match(css, 'grid-template-columns:minmax(760px,1.75fr) minmax(500px,1fr)', fixed = TRUE)
  testthat::expect_match(css, '.depth-v21-right-workspace', fixed = TRUE)
  testthat::expect_match(css, '@media(max-width:1500px)', fixed = TRUE)
  testthat::expect_match(css, 'grid-template-columns:minmax(0,1.65fr) minmax(390px,1fr)', fixed = TRUE)
  testthat::expect_match(css, '@media(max-width:900px)', fixed = TRUE)
})

testthat::test_that("Depth Chart desktop compositions fit 1920 and 1440 workspaces without nested scrolling", {
  desktop_available <- function(viewport, sidebar = 235, gutter = min(24, max(12, viewport * .0125))) {
    viewport - sidebar - (2 * gutter)
  }

  testthat::expect_gte(desktop_available(1920), 760 + 500 + 10)
  testthat::expect_gte(desktop_available(1440), 390 + 700 + 10)

  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/", mustWork = TRUE)
  css <- paste(readLines(file.path(root, "inst", "app", "www", "tbi_phase3.css"), warn = FALSE), collapse = "\n")
  testthat::expect_false(grepl("overflow-y:auto", css, fixed = TRUE))
  testthat::expect_false(grepl("max-height:", css, fixed = TRUE))
})

testthat::test_that("Game Plan explains its governed 48-minute composition", {
  html <- htmltools::renderTags(
    v2_ui_depth_intelligence(depth_chart_stabilization_shadow())
  )$html

  testthat::expect_match(html, "48-Minute Game Plan", fixed = TRUE)
  testthat::expect_match(html, "on-court groups", fixed = TRUE)
  testthat::expect_match(html, "starter overlap", fixed = TRUE)
  testthat::expect_match(html, "coverage", fixed = TRUE)
})
