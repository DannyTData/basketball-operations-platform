v2_phase2_availability <- function(player_id) {
  data.frame(
    player_id = player_id, team_id = "TEST", season = "2026-27",
    availability_status = "AVAILABLE", evidence_class = "AUTHORITATIVE_FACT",
    source = "MANUAL_VERIFIED", source_version = "test-source-1",
    verification_status = "VERIFIED", verified_by = "Test Basketball Operations",
    reason = "Explicit test availability evidence", effective_date = "2026-10-20",
    expiration_date = "2026-10-21", minute_restriction = NA_real_,
    evidence_version = "1.0.0", stringsAsFactors = FALSE
  )
}


v2_phase2_shadow_input <- function() {
  roster <- data.frame(
    player_id = 1:12,
    player_name = sprintf("Player %02d", 1:12),
    team_id = "TEST",
    season = "2026-27",
    position = c("PG", "SG", "SF", "PF", "C", "PG", "SG", "SF", "PF", "C", "G", "F"),
    primary_position = c("PG", "SG", "SF", "PF", "C", "PG", "SG", "SF", "PF", "C", "G", "F"),
    depth_order = c(rep(1L, 5L), rep(2L, 5L), 3L, 3L),
    is_starter = c(rep(1L, 5L), rep(0L, 7L)),
    availability_status = "AVAILABLE",
    bie_rating = seq(90, 60, length.out = 12L),
    bie_offense_score = seq(88, 58, length.out = 12L),
    bie_defense_score = seq(86, 56, length.out = 12L),
    projected_bie_rating = seq(91, 61, length.out = 12L),
    impact_score = seq(89, 59, length.out = 12L),
    is_preseason_rookie = FALSE,
    tbi_performance_available = TRUE,
    stringsAsFactors = FALSE
  )
  list(
    roster = roster,
    lineup = stats::setNames(1:5, c("PG", "SG", "SF", "PF", "C")),
    availability = do.call(rbind, lapply(1:12, v2_phase2_availability))
  )
}


testthat::test_that("shadow compares Phase 2 on rotation_10 while preserving both rotations", {
  input <- v2_phase2_shadow_input()
  result <- run_v2_rotation_shadow(
    "v2_shadow", "TEST", "2026-27", input$roster, input$lineup,
    manual_availability_evidence = input$availability,
    availability_as_of_date = "2026-10-20"
  )

  testthat::expect_identical(result$execution_status, "COMPLETED")
  testthat::expect_identical(result$phase2_rotation_size, 10L)
  testthat::expect_type(result$minute_ledger, "list")
  testthat::expect_identical(result$minute_ledger$contract_type, "tbi-v2-minute-ledger")
  testthat::expect_identical(result$minute_ledger$total_assigned_minutes, 240L)
  testthat::expect_identical(result$stagger_plan$contract_type, "tbi-v2-stagger-plan")
  testthat::expect_identical(result$stagger_plan$total_player_minutes, 240L)
  testthat::expect_identical(result$lineup_portfolio$contract_type, "tbi-v2-lineup-portfolio")
  testthat::expect_false(is.null(result$rotation_10))
  testthat::expect_false(is.null(result$rotation_11))
  testthat::expect_true(all(c(
    "phase2_minutes_seconds", "phase2_stagger_seconds", "phase2_lineups_seconds"
  ) %in% names(result$execution_timing)))
  testthat::expect_true(all(vapply(result$execution_timing[c(
    "phase2_minutes_seconds", "phase2_stagger_seconds", "phase2_lineups_seconds"
  )], is.numeric, logical(1))))
})


testthat::test_that("shadow passes only frozen prepared-roster evidence to Phase 2C", {
  input <- v2_phase2_shadow_input()
  captured <- NULL
  portfolio_builder <- function(rotation_state, minute_ledger, stagger_plan,
                                role_ledger, player_evidence, ...) {
    captured <<- player_evidence
    build_v2_lineup_portfolio(
      rotation_state, minute_ledger, stagger_plan, role_ledger, player_evidence
    )
  }
  result <- run_v2_rotation_shadow(
    "v2_shadow", "TEST", "2026-27", input$roster, input$lineup,
    manual_availability_evidence = input$availability,
    availability_as_of_date = "2026-10-20",
    lineup_portfolio_builder = portfolio_builder
  )

  testthat::expect_identical(result$execution_status, "COMPLETED")
  testthat::expect_true(all(c(
    "player_id", "bie_rating", "offensive_impact", "defensive_impact",
    "evidence_source"
  ) %in% names(captured)))
  testthat::expect_true(all(captured$evidence_source == "FROZEN_BIE"))
  testthat::expect_equal(captured$offensive_impact, input$roster$bie_offense_score[match(captured$player_id, input$roster$player_id)])
  testthat::expect_equal(captured$defensive_impact, input$roster$bie_defense_score[match(captured$player_id, input$roster$player_id)])
})


testthat::test_that("Phase 2 failures are isolated without disturbing V1 comparison state", {
  input <- v2_phase2_shadow_input()
  v1 <- list(id = "protected-v1")
  result <- run_v2_rotation_shadow(
    "v2_shadow", "TEST", "2026-27", input$roster, input$lineup,
    v1_reference = v1,
    manual_availability_evidence = input$availability,
    availability_as_of_date = "2026-10-20",
    minute_builder = function(...) stop("phase2 exploded")
  )

  testthat::expect_identical(result$execution_status, "COMPLETED")
  testthat::expect_identical(result$v1_reference, v1)
  testthat::expect_false(is.null(result$rotation_10))
  testthat::expect_false(is.null(result$rotation_11))
  testthat::expect_null(result$minute_ledger)
  testthat::expect_null(result$stagger_plan)
  testthat::expect_null(result$lineup_portfolio)
  testthat::expect_identical(result$phase2_diagnostics$status, "ERROR")
  testthat::expect_match(result$phase2_diagnostics$error$message, "phase2 exploded")
})


testthat::test_that("Phase 2 shadow output is deterministic", {
  input <- v2_phase2_shadow_input()
  first <- run_v2_rotation_shadow(
    "v2_shadow", "TEST", "2026-27", input$roster, input$lineup,
    manual_availability_evidence = input$availability,
    availability_as_of_date = "2026-10-20"
  )
  second <- run_v2_rotation_shadow(
    "v2_shadow", "TEST", "2026-27", input$roster, input$lineup,
    manual_availability_evidence = input$availability,
    availability_as_of_date = "2026-10-20"
  )

  testthat::expect_identical(first$minute_ledger, second$minute_ledger)
  testthat::expect_identical(first$stagger_plan, second$stagger_plan)
  testthat::expect_identical(first$lineup_portfolio, second$lineup_portfolio)
})


testthat::test_that("30-team diagnostics expose Phase 2 reconciliation and legality", {
  input <- v2_phase2_shadow_input()
  shadow <- run_v2_rotation_shadow(
    "v2_shadow", "TEST", "2026-27", input$roster, input$lineup,
    manual_availability_evidence = input$availability,
    availability_as_of_date = "2026-10-20"
  )
  teams <- data.frame(
    team_name = sprintf("Team %02d", 1:30),
    abbreviation = sprintf("T%02d", 1:30),
    stringsAsFactors = FALSE
  )
  roster <- input$roster
  roster$team_id <- NULL
  roster$season <- NULL
  roster$team_abbreviation <- NULL

  testthat::local_mocked_bindings(
    phase15_latest_depth_season = function() "2026-27",
    phase15_active_teams = function() teams,
    v2_role_authoritative_roster_snapshot = function(...) roster,
    depth_chart_batched_bie_enrich_roster = function(value, ...) value,
    evaluate_bie_players = function(value, ...) value,
    run_v2_rotation_shadow = function(...) shadow,
    .package = "basketballops"
  )
  results <- v2_shadow_validate_30_teams("2026-27")

  testthat::expect_equal(nrow(results), 30L)
  testthat::expect_true(all(c(
    "phase2_status", "phase2_blocked", "phase2_exact_240",
    "phase2_segments_reconcile", "phase2_lineups_legal", "phase2_closing_legal",
    "phase2_minutes_seconds", "phase2_stagger_seconds", "phase2_lineups_seconds"
  ) %in% names(results)))
  testthat::expect_true(all(results$phase2_exact_240))
  testthat::expect_true(all(results$phase2_segments_reconcile))
  testthat::expect_true(all(results$phase2_lineups_legal))
  testthat::expect_true(all(results$phase2_closing_legal))
  testthat::expect_true(all(results$deterministic))
})
