v2_feedback_player_route <- function(id, player, from, to, salary = 0) {
  data.frame(route_id = id, player_id = player, from_team_id = from,
    to_team_id = to, salary = salary, stringsAsFactors = FALSE)
}

v2_feedback_exception <- function(amount = 10, remaining = amount,
                                  status = "ACTIVE", verification = "VERIFIED",
                                  expiration = "2027-07-01", id = "tpe-fixture") {
  data.frame(
    team_id = "A", season = "2026-27", exception_id = id,
    exception_type = "TRADED_PLAYER_EXCEPTION", original_amount = amount,
    remaining_amount = remaining, creation_transaction = "synthetic-reference-transaction",
    creation_date = "2026-07-01", expiration_date = expiration, status = status,
    source = "verified synthetic reference fixture", source_version = "1",
    verification_status = verification, use_restrictions = "fixture only",
    stringsAsFactors = FALSE
  )
}

testthat::test_that("feedback workflow: two-team scenario is isolated and review-safe", {
  graph <- normalize_transaction_graph(
    "feedback-2", c("A", "B"),
    rbind(v2_feedback_player_route("r1", "1", "A", "B", 8),
      v2_feedback_player_route("r2", "2", "B", "A", 7))
  )
  result <- evaluate_multiteam_transaction(graph)
  impact <- build_v2_organizational_impact(graph, result)
  testthat::expect_identical(result$status, "REVIEW")
  testthat::expect_identical(impact$executive_recommendation, "PROCEED WITH REVIEW")
  testthat::expect_equal(impact$team_impacts$A$payroll_delta, -1)
})

testthat::test_that("feedback workflow: verified TPE partial, full, insufficient, and expiration", {
  ledger <- new_v2_team_exception_ledger(v2_feedback_exception())
  request <- function(amount) data.frame(
    route_id = "r1", team_id = "A", exception_id = "tpe-fixture", amount = amount
  )
  graph <- function(amount) normalize_transaction_graph(
    paste0("feedback-tpe-", amount),
    c("A", "B"),
    v2_feedback_player_route("r1", "incoming-player", "B", "A", amount),
    season = "2026-27"
  )
  testthat::expect_equal(
    apply_v2_scenario_exceptions(
      ledger, request(4), "2026-08-20", transaction_graph = graph(4)
    )$scenario_ledger$entries$remaining_amount,
    6
  )
  testthat::expect_identical(
    apply_v2_scenario_exceptions(
      ledger, request(10), "2026-08-20", transaction_graph = graph(10)
    )$scenario_ledger$entries$status,
    "CONSUMED"
  )
  testthat::expect_identical(
    apply_v2_scenario_exceptions(
      ledger, request(11), "2026-08-20", transaction_graph = graph(11)
    )$status,
    "FAIL"
  )
  expired <- new_v2_team_exception_ledger(v2_feedback_exception(expiration = "2026-08-01"))
  testthat::expect_identical(
    apply_v2_scenario_exceptions(
      expired, request(1), "2026-08-20", transaction_graph = graph(1)
    )$status,
    "FAIL"
  )
})

testthat::test_that("feedback workflow: verified synthetic TPE creation never mutates authority", {
  authority <- new_v2_team_exception_ledger(v2_feedback_exception(id = "old"))
  created <- v2_feedback_exception(amount = 6, id = "new")
  result <- apply_v2_scenario_exceptions(
    authority, data.frame(), "2026-08-20", creation_facts = created
  )
  testthat::expect_identical(result$status, "PASS")
  testthat::expect_setequal(result$scenario_ledger$entries$exception_id, c("old", "new"))
  testthat::expect_identical(authority$entries$exception_id, "old")
})

testthat::test_that("feedback workflow: three-team chain and circle are deterministic", {
  chain <- normalize_transaction_graph(
    "feedback-chain", c("A", "B", "C"),
    rbind(v2_feedback_player_route("r1", "1", "A", "B"),
      v2_feedback_player_route("r2", "2", "B", "C"))
  )
  circle_routes <- rbind(v2_feedback_player_route("r1", "1", "A", "B"),
    v2_feedback_player_route("r2", "2", "B", "C"),
    v2_feedback_player_route("r3", "3", "C", "A"))
  circle <- normalize_transaction_graph("feedback-circle", c("A", "B", "C"), circle_routes)
  reversed <- normalize_transaction_graph("feedback-circle", c("C", "B", "A"), circle_routes[3:1, ])
  testthat::expect_identical(validate_transaction_routes(chain)$status, "PASS")
  testthat::expect_identical(validate_transaction_routes(circle)$status, "PASS")
  testthat::expect_identical(circle$signature, reversed$signature)
})

testthat::test_that("feedback workflow: four-team graph reports every organization", {
  graph <- normalize_transaction_graph(
    "feedback-4", LETTERS[1:4],
    rbind(v2_feedback_player_route("r1", "1", "A", "B"),
      v2_feedback_player_route("r2", "2", "B", "C"),
      v2_feedback_player_route("r3", "3", "C", "D"),
      v2_feedback_player_route("r4", "4", "D", "A"))
  )
  result <- evaluate_multiteam_transaction(graph)
  testthat::expect_identical(names(result$team_results), LETTERS[1:4])
  testthat::expect_identical(result$status, "REVIEW")
})

testthat::test_that("feedback workflow: duplicate pick and invalid ownership block", {
  assets <- data.frame(
    route_id = c("a1", "a2"), asset_id = c("pick-1", "pick-1"),
    from_team_id = c("A", "A"), to_team_id = c("B", "C"), stringsAsFactors = FALSE
  )
  graph <- normalize_transaction_graph("feedback-asset", c("A", "B", "C"), asset_routes = assets)
  testthat::expect_identical(validate_transaction_routes(graph)$status, "FAIL")

  player_graph <- normalize_transaction_graph(
    "feedback-owner", c("A", "B"), v2_feedback_player_route("r1", "1", "A", "B")
  )
  ownership <- data.frame(player_id = "1", team_id = "B")
  testthat::expect_true(validate_transaction_routes(player_graph, ownership)$is_blocked)
})

testthat::test_that("feedback workflow: one-team CBA FAIL dominates every positive domain", {
  graph <- normalize_transaction_graph(
    "feedback-fail", c("A", "B"), v2_feedback_player_route("r1", "1", "A", "B")
  )
  facts <- list(A = list(salary_matching = list(
    status = "FAIL", is_blocked = TRUE, source = "verified synthetic fixture",
    source_reference = "salary-fail", explanation = "Fixture fails salary matching."
  )))
  result <- evaluate_multiteam_transaction(graph, facts)
  impact <- build_v2_organizational_impact(graph, result)
  testthat::expect_identical(result$status, "FAIL")
  testthat::expect_identical(impact$executive_recommendation, "DO NOT PROCEED")
})

testthat::test_that("feedback workflow: reset and replacement scenarios do not leak state", {
  build <- function(id, teams) {
    graph <- normalize_transaction_graph(id, teams,
      v2_feedback_player_route("r1", "1", teams[[1]], teams[[2]]))
    evaluation <- evaluate_multiteam_transaction(graph)
    list(graph, evaluation, build_v2_organizational_impact(graph, evaluation))
  }
  state <- tbi_transaction_state()
  first <- build("first", c("A", "B"))
  shiny::isolate(state$publish_v2_transaction(first[[1]], first[[2]], first[[3]]))
  state$clear()
  second <- build("second", c("C", "D"))
  shiny::isolate(state$publish_v2_transaction(second[[1]], second[[2]], second[[3]]))
  snapshot <- shiny::isolate(state$snapshot())
  testthat::expect_identical(snapshot$scenario_id, "second")
  testthat::expect_identical(snapshot$v2_transaction_graph$teams, c("C", "D"))
  testthat::expect_null(snapshot$v2_exception_scenario)
})
