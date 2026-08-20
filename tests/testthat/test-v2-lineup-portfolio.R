lineup_portfolio_fixture <- function() {
  members <- data.frame(
    player_id = 1:10,
    player_name = paste("Player", 1:10),
    is_starter = c(rep(TRUE, 5), rep(FALSE, 5)),
    rotation_role = c(paste0("STARTER_", c("PG", "SG", "SF", "PF", "C")), paste0("BENCH_", 1:5)),
    position = c("PG", "SG", "SF", "PF", "C", "PG", "SG", "SF", "PF", "C"),
    is_preseason_rookie = c(TRUE, rep(FALSE, 9)),
    stringsAsFactors = FALSE
  )
  rotation <- list(
    contract_type = "tbi-v2-rotation-state", contract_version = "1.0.0",
    team_id = "TST", season = "2026-27", status = "PASS", is_blocked = FALSE,
    members = members, input_signature = v2_input_signature(members)
  )
  ledger_rows <- transform(members,
    assigned_minutes = c(rep(28L, 5), rep(20L, 5)),
    maximum_minutes = 40L,
    availability_status = "AVAILABLE"
  )
  ledger_rows$reason_codes <- I(rep(list("AVAILABILITY_AVAILABLE"), 10))
  minute_ledger <- list(
    contract_type = "tbi-v2-minute-ledger", contract_version = "1.0.0",
    status = "PASS", is_blocked = FALSE, ledger = ledger_rows,
    total_assigned_minutes = 240L, input_signature = v2_input_signature(ledger_rows)
  )
  roles <- unlist(lapply(seq_len(nrow(members)), function(i) {
    lapply(c("PG", "SG", "SF", "PF", "C"), function(position) {
      new_v2_role_eligibility(
        player_id = members$player_id[[i]], team_id = "TST", season = "2026-27",
        role = paste0("POSITION_", position),
        eligibility = if (members$position[[i]] == position) "ELIGIBLE" else "UNKNOWN",
        evidence_status = if (members$position[[i]] == position) "VERIFIED" else "UNKNOWN",
        evidence_source = if (members$position[[i]] == position) "OFFICIAL_PRIMARY_POSITION" else "UNKNOWN",
        evidence_class = if (members$position[[i]] == position) "AUTHORITATIVE_FACT" else "UNKNOWN",
        source_field = "fixture.position", source_version = "fixture-v1",
        verification_status = if (members$position[[i]] == position) "VERIFIED" else "MISSING",
        evidence_fields = if (members$position[[i]] == position) "position" else character(),
        missing_fields = if (members$position[[i]] == position) character() else "verified_position_eligibility"
      )
    })
  }), recursive = FALSE)
  role_ledger <- list(
    contract_type = "tbi-v2-role-eligibility-ledger", contract_version = "1.0.0",
    team_id = "TST", season = "2026-27", records = roles,
    input_signature = v2_input_signature(roles)
  )
  stagger <- build_v2_stagger_plan(minute_ledger, role_ledger)
  evidence <- data.frame(
    player_id = 1:10,
    bie_rating = c(90, 86, 84, 82, 80, 79, 78, 77, 76, 75),
    offensive_impact = c(90, 91, 85, 82, 78, 86, 84, 80, 77, 72),
    defensive_impact = c(82, 78, 90, 88, 92, 75, 79, 87, 86, 89),
    evidence_source = "FROZEN_BIE", reliability = "FOUNDATION",
    stringsAsFactors = FALSE
  )
  list(rotation = rotation, minutes = minute_ledger, stagger = stagger, roles = role_ledger, evidence = evidence)
}

portfolio_lineup <- function(portfolio, type) {
  index <- which(portfolio$lineups$lineup_type == type)
  if (!length(index)) return(NULL)
  portfolio$lineups[index[[1]], , drop = FALSE]
}

testthat::test_that("Phase 2C constructs a deterministic legal portfolio", {
  fixture <- lineup_portfolio_fixture()
  first <- build_v2_lineup_portfolio(fixture$rotation, fixture$minutes, fixture$stagger, fixture$roles, fixture$evidence)
  second <- build_v2_lineup_portfolio(fixture$rotation, fixture$minutes, fixture$stagger, fixture$roles, fixture$evidence)
  testthat::expect_identical(first$contract_type, "tbi-v2-lineup-portfolio")
  testthat::expect_identical(first$contract_version, "1.0.0")
  testthat::expect_identical(first$model_version, "2.0.0-phase2c")
  testthat::expect_true(all(c("BASE", "BENCH_BRIDGE", "OFFENSE", "DEFENSE", "CLOSING") %in% first$lineups$lineup_type))
  testthat::expect_true(all(first$lineups$legality_status == "PASS"))
  testthat::expect_true(all(lengths(first$lineups$player_ids) == 5L))
  testthat::expect_true(all(vapply(first$lineups$player_ids, function(ids) !anyDuplicated(ids), logical(1))))
  testthat::expect_true(all(vapply(first$lineups$assigned_positions, function(x) setequal(x, c("PG", "SG", "SF", "PF", "C")), logical(1))))
  testthat::expect_identical(first$input_signature, second$input_signature)
  testthat::expect_identical(first$lineups$lineup_id, second$lineups$lineup_id)
  testthat::expect_true(portfolio_lineup(first, "BASE")$player_ids[[1]][[1]] %in% 1:5)
  testthat::expect_true(1L %in% portfolio_lineup(first, "BASE")$player_ids[[1]])
})

testthat::test_that("portfolio IDs reconcile stagger exposure and input changes alter signature", {
  fixture <- lineup_portfolio_fixture()
  result <- build_v2_lineup_portfolio(fixture$rotation, fixture$minutes, fixture$stagger, fixture$roles, fixture$evidence)
  expected <- vapply(seq_len(nrow(result$lineups)), function(i) {
    sum(fixture$stagger$segments$duration[fixture$stagger$segments$lineup_id == result$lineups$lineup_id[[i]]])
  }, integer(1))
  testthat::expect_identical(result$lineups$actual_exposure, expected)
  testthat::expect_true(all(result$lineups$actual_exposure <= result$lineups$target_exposure))
  changed <- fixture
  changed$evidence$offensive_impact[[10]] <- 100
  changed_result <- build_v2_lineup_portfolio(changed$rotation, changed$minutes, changed$stagger, changed$roles, changed$evidence)
  testthat::expect_false(identical(result$input_signature, changed_result$input_signature))
})

testthat::test_that("small-ball requires verified alternate center eligibility", {
  fixture <- lineup_portfolio_fixture()
  unknown <- build_v2_lineup_portfolio(fixture$rotation, fixture$minutes, fixture$stagger, fixture$roles, fixture$evidence)
  testthat::expect_false("SMALL_BALL" %in% unknown$lineups$lineup_type)
  testthat::expect_true("SMALL_BALL_ELIGIBILITY_UNKNOWN" %in% vapply(unknown$validation$findings, `[[`, character(1), "code"))

  alternate <- new_v2_role_eligibility(
    player_id = 9L, team_id = "TST", season = "2026-27", role = "POSITION_C",
    eligibility = "ELIGIBLE", evidence_status = "VERIFIED", evidence_source = "MANUAL_VERIFIED",
    evidence_class = "AUTHORITATIVE_FACT", source_field = "manual_evidence.eligibility",
    source_version = "fixture-v1", verification_status = "VERIFIED",
    evidence_fields = "eligibility"
  )
  key <- vapply(fixture$roles$records, function(x) paste(x$player_id, x$role), character(1))
  fixture$roles$records[[which(key == "9 POSITION_C")]] <- alternate
  fixture$roles$input_signature <- v2_input_signature(fixture$roles$records)
  eligible <- build_v2_lineup_portfolio(fixture$rotation, fixture$minutes, fixture$stagger, fixture$roles, fixture$evidence)
  testthat::expect_true("SMALL_BALL" %in% eligible$lineups$lineup_type)
  testthat::expect_false(5L %in% portfolio_lineup(eligible, "SMALL_BALL")$player_ids[[1]])
})

testthat::test_that("unavailable players and malformed inputs cannot enter portfolios", {
  fixture <- lineup_portfolio_fixture()
  fixture$minutes$ledger$reason_codes[[6]] <- "AVAILABILITY_OUT"
  fixture$minutes$ledger$availability_status[[6]] <- "OUT"
  fixture$minutes$ledger$assigned_minutes[[6]] <- 0L
  fixture$minutes$ledger$assigned_minutes[[10]] <- 40L
  fixture$minutes$ledger$maximum_minutes[[10]] <- 40L
  fixture$minutes$input_signature <- v2_input_signature(fixture$minutes$ledger)
  fixture$stagger <- build_v2_stagger_plan(fixture$minutes, fixture$roles)
  result <- build_v2_lineup_portfolio(fixture$rotation, fixture$minutes, fixture$stagger, fixture$roles, fixture$evidence)
  testthat::expect_false(any(vapply(result$lineups$player_ids, function(ids) 6L %in% ids, logical(1))))

  duplicate <- fixture$stagger
  duplicate$segments$player_ids[[1]][[5]] <- duplicate$segments$player_ids[[1]][[1]]
  testthat::expect_error(build_v2_lineup_portfolio(fixture$rotation, fixture$minutes, duplicate, fixture$roles, fixture$evidence), "unique")
  mismatch <- fixture$stagger
  mismatch$total_player_minutes <- 239L
  testthat::expect_error(build_v2_lineup_portfolio(fixture$rotation, fixture$minutes, mismatch, fixture$roles, fixture$evidence), "240")
})

testthat::test_that("missing synergy and clutch evidence remain explicit review data", {
  fixture <- lineup_portfolio_fixture()
  result <- build_v2_lineup_portfolio(fixture$rotation, fixture$minutes, fixture$stagger, fixture$roles, fixture$evidence)
  testthat::expect_identical(result$status, "REVIEW")
  testthat::expect_true(all(result$lineups$reliability == "INDIVIDUAL_EVIDENCE_ONLY"))
  testthat::expect_true(all(vapply(result$lineups$missing_fields, function(x) all(c("verified_lineup_synergy", "verified_clutch_performance") %in% x), logical(1))))
  testthat::expect_true(all(!grepl("clutch", result$lineups$explanation, ignore.case = TRUE)))
})

testthat::test_that("exact objective ties use stable player-ID ordering", {
  fixture <- lineup_portfolio_fixture()
  fixture$evidence[c("bie_rating", "offensive_impact", "defensive_impact")] <- 50
  result <- build_v2_lineup_portfolio(fixture$rotation, fixture$minutes, fixture$stagger, fixture$roles, fixture$evidence)
  testthat::expect_identical(sort(portfolio_lineup(result, "OFFENSE")$player_ids[[1]]), 1:5)
  testthat::expect_identical(sort(portfolio_lineup(result, "DEFENSE")$player_ids[[1]]), 1:5)
  testthat::expect_identical(sort(portfolio_lineup(result, "CLOSING")$player_ids[[1]]), 1:5)
})

testthat::test_that("illegal position coverage is omitted and rookie starter is preserved", {
  fixture <- lineup_portfolio_fixture()
  fixture$roles$records <- Filter(function(x) !(x$player_id == 5L && x$role == "POSITION_C"), fixture$roles$records)
  fixture$roles$input_signature <- v2_input_signature(fixture$roles$records)
  result <- build_v2_lineup_portfolio(fixture$rotation, fixture$minutes, fixture$stagger, fixture$roles, fixture$evidence)
  testthat::expect_false("BASE" %in% result$lineups$lineup_type)
  testthat::expect_true("BASE_LINEUP_ILLEGAL" %in% vapply(result$validation$findings, `[[`, character(1), "code"))
  testthat::expect_true(any(vapply(result$lineups$player_ids, function(ids) 1L %in% ids, logical(1))))
})
