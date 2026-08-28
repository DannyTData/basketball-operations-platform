make_v2_route <- function(id, player, from, to, salary = 0) {
  data.frame(
    route_id = id, player_id = player, from_team_id = from,
    to_team_id = to, salary = salary, stringsAsFactors = FALSE
  )
}

make_v2_tpe_graph <- function(amount = 5,
                              owner = "A",
                              from = "B",
                               teams = c("A", "B"),
                               route_id = "r1",
                               salary_available = TRUE,
                               asset_only = FALSE,
                               season = "2026-27") {
  if (isTRUE(asset_only)) {
    return(normalize_transaction_graph(
      paste0("tpe-asset-", route_id),
      teams,
      asset_routes = data.frame(
        route_id = route_id,
        asset_id = "pick-1",
        from_team_id = from,
        to_team_id = owner,
        stringsAsFactors = FALSE
      ),
      season = season
    ))
  }
  route <- make_v2_route(route_id, "player-1", from, owner, amount)
  if (!isTRUE(salary_available)) route$salary <- NULL
  normalize_transaction_graph(
    paste0("tpe-player-", route_id),
    teams,
    player_routes = route,
    season = season
  )
}

testthat::test_that("transaction events are versioned, deterministic, and staged only", {
  event <- new_v2_transaction_event(
    event_id = "evt-1", effective_date = "2026-08-20",
    source = "verified fixture", source_version = "1",
    verification_status = "VERIFIED", teams = c("A", "B"),
    player_routes = make_v2_route("p1", "10", "A", "B")
  )
  testthat::expect_identical(event$contract_type, "tbi-v2-transaction-event")
  testthat::expect_identical(event$event_version, "1.0.0")
  testthat::expect_match(event$signature, "^v2sig1-")

  roster <- data.frame(player_id = c("10", "20"), team_id = c("A", "B"))
  staged <- stage_v2_transaction_refresh(event, roster)
  testthat::expect_identical(staged$state, "READY TO APPLY")
  testthat::expect_identical(staged$mode, "DRY RUN")
  testthat::expect_identical(staged$roster_after$team_id, c("B", "B"))
  testthat::expect_identical(roster$team_id, c("A", "B"))
})

testthat::test_that("unverified refresh facts require review", {
  event <- new_v2_transaction_event(
    "evt-2", "2026-08-20", "fixture", "1", "UNVERIFIED",
    teams = c("A", "B"), player_routes = make_v2_route("p1", "10", "A", "B")
  )
  staged <- stage_v2_transaction_refresh(
    event, data.frame(player_id = "10", team_id = "A")
  )
  testthat::expect_identical(staged$state, "REQUIRES REVIEW")
  testthat::expect_false(staged$authoritative_apply)
  testthat::expect_error(
    new_v2_transaction_event(
      "evt-invalid", "2026-08-20", "fixture", "1", "VERIFIED",
      teams = c("A", "B"), player_routes = make_v2_route("p1", "10", "A", "C")
    ),
    "participating teams"
  )
})

testthat::test_that("exception scenarios consume copies without mutating authority", {
  ledger <- new_v2_team_exception_ledger(data.frame(
    team_id = "A", season = "2026-27", exception_id = "tpe-1",
    exception_type = "TRADED_PLAYER_EXCEPTION", original_amount = 12,
    remaining_amount = 12, creation_transaction = "verified-tx",
    creation_date = "2026-07-01", expiration_date = "2027-07-01",
    status = "ACTIVE", source = "verified fixture", source_version = "1",
    verification_status = "VERIFIED", use_restrictions = "NONE",
    stringsAsFactors = FALSE
  ))
  partial <- apply_v2_scenario_exceptions(
    ledger, data.frame(route_id = "r1", team_id = "A", exception_id = "tpe-1", amount = 5),
    as_of_date = "2026-08-20",
    transaction_graph = make_v2_tpe_graph(5, route_id = "r1")
  )
  testthat::expect_identical(partial$status, "PASS")
  testthat::expect_equal(partial$scenario_ledger$entries$remaining_amount, 7)
  testthat::expect_equal(ledger$entries$remaining_amount, 12)

  full <- apply_v2_scenario_exceptions(
    ledger, data.frame(route_id = "r2", team_id = "A", exception_id = "tpe-1", amount = 12),
    as_of_date = "2026-08-20",
    transaction_graph = make_v2_tpe_graph(12, route_id = "r2")
  )
  testthat::expect_identical(full$scenario_ledger$entries$status, "CONSUMED")

  failed <- apply_v2_scenario_exceptions(
    ledger, data.frame(route_id = "r3", team_id = "A", exception_id = "tpe-1", amount = 13),
    as_of_date = "2026-08-20",
    transaction_graph = make_v2_tpe_graph(13, route_id = "r3")
  )
  testthat::expect_identical(failed$status, "FAIL")
  testthat::expect_true(failed$is_blocked)
  testthat::expect_identical(
    partial$scenario_ledger$transaction_signature,
    make_v2_tpe_graph(5, route_id = "r1")$signature
  )
})

testthat::test_that("TPE use is bound to participant-owned incoming salary routes", {
  entry <- data.frame(
    team_id = "A", season = "2026-27", exception_id = "bound-tpe",
    exception_type = "TRADED_PLAYER_EXCEPTION", original_amount = 12,
    remaining_amount = 12, creation_transaction = "verified-tx",
    creation_date = "2026-07-01", expiration_date = "2027-07-01",
    status = "ACTIVE", source = "verified fixture", source_version = "1",
    verification_status = "VERIFIED", use_restrictions = "NONE",
    stringsAsFactors = FALSE
  )
  ledger <- new_v2_team_exception_ledger(entry)
  request <- function(route_id = "r1", team = "A", amount = 5) data.frame(
    route_id = route_id,
    team_id = team,
    exception_id = "bound-tpe",
    amount = amount,
    stringsAsFactors = FALSE
  )
  run <- function(graph, usage = request(), authority = ledger) {
    apply_v2_scenario_exceptions(
      authority,
      usage,
      "2026-08-20",
      transaction_graph = graph
    )
  }
  first_code <- function(value) value$validation$findings[[1]]$code

  no_graph <- apply_v2_scenario_exceptions(
    ledger,
    request(),
    "2026-08-20"
  )
  testthat::expect_identical(first_code(no_graph), "TPE_TRANSACTION_REQUIRED")

  nonparticipant_entry <- entry
  nonparticipant_entry$team_id <- "C"
  nonparticipant <- run(
    make_v2_tpe_graph(5),
    request(team = "C"),
    new_v2_team_exception_ledger(nonparticipant_entry)
  )
  testthat::expect_identical(first_code(nonparticipant), "TPE_OWNER_NOT_PARTICIPANT")

  asset_only <- run(
    make_v2_tpe_graph(5, asset_only = TRUE),
    request()
  )
  testthat::expect_identical(first_code(asset_only), "TPE_ASSET_ONLY_USE")

  wrong_destination <- run(
    make_v2_tpe_graph(5, owner = "B", from = "A"),
    request()
  )
  testthat::expect_identical(first_code(wrong_destination), "TPE_RECEIVER_MISMATCH")

  missing_salary <- run(
    make_v2_tpe_graph(5, salary_available = FALSE),
    request()
  )
  testthat::expect_identical(first_code(missing_salary), "TPE_INCOMING_SALARY_MISSING")

  amount_mismatch <- run(
    make_v2_tpe_graph(5),
    request(amount = 4)
  )
  testthat::expect_identical(first_code(amount_mismatch), "TPE_AMOUNT_ROUTE_MISMATCH")

  season_mismatch <- run(
    make_v2_tpe_graph(5, season = "2027-28"),
    request()
  )
  testthat::expect_identical(first_code(season_mismatch), "TPE_SEASON_MISMATCH")

  expired_entry <- entry
  expired_entry$expiration_date <- "2026-08-01"
  expired <- run(
    make_v2_tpe_graph(5),
    authority = new_v2_team_exception_ledger(expired_entry)
  )
  testthat::expect_identical(first_code(expired), "TPE_EXPIRED")

  unknown_entry <- entry
  unknown_entry$use_restrictions <- "UNKNOWN"
  unknown_authority <- new_v2_team_exception_ledger(unknown_entry)
  unknown <- run(
    make_v2_tpe_graph(5),
    authority = unknown_authority
  )
  testthat::expect_identical(unknown$status, "REVIEW")
  testthat::expect_false(unknown$is_blocked)
  testthat::expect_identical(first_code(unknown), "TPE_RESTRICTIONS_UNKNOWN")
  testthat::expect_equal(unknown$scenario_ledger$entries$remaining_amount, 7)

  testthat::expect_identical(ledger$entries, new_v2_team_exception_ledger(entry)$entries)
  testthat::expect_equal(ledger$entries$remaining_amount, 12)
})

testthat::test_that("an empty authoritative exception ledger is explicit and safe", {
  ledger <- new_v2_team_exception_ledger()
  testthat::expect_equal(nrow(ledger$entries), 0L)
  testthat::expect_true(ledger$authoritative)
  testthat::expect_identical(ledger$contract_version, "1.0.0")
})

testthat::test_that("scenario exception route and creation identities are unique", {
  ledger <- new_v2_team_exception_ledger(data.frame(
    team_id = "A", season = "2026-27", exception_id = "tpe-1",
    exception_type = "TRADED_PLAYER_EXCEPTION", original_amount = 12,
    remaining_amount = 12, creation_transaction = "verified-tx",
    creation_date = "2026-07-01", expiration_date = "2027-07-01",
    status = "ACTIVE", source = "verified fixture", source_version = "1",
    verification_status = "VERIFIED", use_restrictions = "UNKNOWN"
  ))
  duplicate_routes <- data.frame(
    route_id = c("r1", "r1"), team_id = "A", exception_id = "tpe-1", amount = c(2, 2)
  )
  testthat::expect_error(
    apply_v2_scenario_exceptions(ledger, duplicate_routes, "2026-08-20"),
    "unique route_id"
  )
  duplicate_creation <- ledger$entries
  result <- apply_v2_scenario_exceptions(
    ledger, data.frame(), "2026-08-20", creation_facts = duplicate_creation
  )
  testthat::expect_identical(result$status, "FAIL")
  testthat::expect_true(result$is_blocked)
})

testthat::test_that("transaction graph supports stable two through four team routes", {
  three <- normalize_transaction_graph(
    "tx-3", c("A", "B", "C"),
    rbind(
      make_v2_route("r2", "20", "B", "C", 4),
      make_v2_route("r1", "10", "A", "B", 3),
      make_v2_route("r3", "30", "C", "A", 5)
    )
  )
  reordered <- normalize_transaction_graph(
    "tx-3", c("C", "A", "B"), three$player_routes[3:1, ]
  )
  testthat::expect_identical(three$signature, reordered$signature)
  testthat::expect_identical(validate_transaction_routes(three)$status, "PASS")

  four <- normalize_transaction_graph(
    "tx-4", LETTERS[1:4],
    rbind(make_v2_route("r1", "1", "A", "B"), make_v2_route("r2", "2", "C", "D"))
  )
  testthat::expect_identical(validate_transaction_routes(four)$status, "PASS")
})

testthat::test_that("transaction seasons participate in signatures and shared state", {
  route <- make_v2_route("r1", "10", "A", "B", 3)
  current <- normalize_transaction_graph(
    "tx-season", c("A", "B"), route, season = "2026-27"
  )
  future <- normalize_transaction_graph(
    "tx-season", c("A", "B"), route, season = "2027-28"
  )
  testthat::expect_identical(current$season, "2026-27")
  testthat::expect_false(identical(current$signature, future$signature))

  evaluation <- evaluate_multiteam_transaction(current)
  impact <- build_v2_organizational_impact(current, evaluation)
  state <- tbi_transaction_state()
  shiny::isolate(state$publish_v2_transaction(current, evaluation, impact))
  snapshot <- shiny::isolate(state$snapshot())
  testthat::expect_identical(snapshot$season, "2026-27")
  testthat::expect_true(tbi_scenario_matches_season(snapshot, "2026-27"))
  testthat::expect_false(tbi_scenario_matches_season(snapshot, "2027-28"))
  testthat::expect_false(tbi_scenario_matches_season(
    list(active = TRUE, scenario_type = "future", season = NULL),
    "2026-27"
  ))
})

testthat::test_that("application season lifecycle observes selected_season", {
  source <- paste(
    readLines(testthat::test_path("..", "..", "R", "app_server.R"), warn = FALSE),
    collapse = "\n"
  )
  testthat::expect_false(grepl("input$global_season", source, fixed = TRUE))
  testthat::expect_match(source, "selected_season()", fixed = TRUE)
  testthat::expect_match(source, "tbi_scenario_matches_season", fixed = TRUE)
})

testthat::test_that("duplicate and invalid routes block the transaction", {
  duplicate <- normalize_transaction_graph(
    "tx-dup", c("A", "B", "C"),
    rbind(make_v2_route("r1", "10", "A", "B"), make_v2_route("r2", "10", "A", "C"))
  )
  result <- validate_transaction_routes(duplicate)
  testthat::expect_identical(result$status, "FAIL")
  testthat::expect_true(result$is_blocked)

  invalid <- normalize_transaction_graph(
    "tx-invalid", c("A", "B"), make_v2_route("r1", "10", "A", "C")
  )
  testthat::expect_identical(validate_transaction_routes(invalid)$status, "FAIL")
})

testthat::test_that("unsupported rules remain REVIEW and a team FAIL controls overall", {
  graph <- normalize_transaction_graph(
    "tx-rules", c("A", "B"), make_v2_route("r1", "10", "A", "B", 3)
  )
  review <- evaluate_multiteam_transaction(graph)
  testthat::expect_identical(review$status, "REVIEW")
  testthat::expect_false(review$is_blocked)

  failed <- evaluate_multiteam_transaction(
    graph,
    rule_facts = list(A = list(salary_matching = list(status = "FAIL", is_blocked = TRUE,
      source = "verified fixture", source_reference = "fixture-1", explanation = "Salary match fails.")))
  )
  testthat::expect_identical(failed$status, "FAIL")
  testthat::expect_true(failed$is_blocked)
  impact <- build_v2_organizational_impact(graph, failed)
  testthat::expect_identical(impact$executive_recommendation, "DO NOT PROCEED")
})

testthat::test_that("reconciliation reports stale, conflict, unknown, and current without writes", {
  authoritative <- data.frame(player_id = c("1", "2", "3"), team_id = c("A", "A", NA))
  external <- data.frame(player_id = c("1", "2", "4"), team_id = c("A", "B", "C"))
  result <- reconcile_v2_current_rosters(authoritative, external, "verified fixture", "1")
  testthat::expect_setequal(result$rows$status, c("CURRENT", "CONFLICT", "STALE", "UNKNOWN"))
  testthat::expect_false(result$authoritative_apply)
})

testthat::test_that("governed media registry never guesses missing player imagery", {
  team <- v2_team_media_registry()
  testthat::expect_equal(nrow(team), 30L)
  testthat::expect_true(all(team$verification_status == "REQUIRES SOURCE VERIFICATION"))
  testthat::expect_null(v2_player_headshot_record("missing-player"))
})

testthat::test_that("shared scenario state publishes and resets V2 contracts without leakage", {
  graph <- normalize_transaction_graph(
    "tx-state", c("A", "B"), make_v2_route("r1", "10", "A", "B")
  )
  evaluation <- evaluate_multiteam_transaction(graph)
  impact <- build_v2_organizational_impact(graph, evaluation)
  state <- tbi_transaction_state()
  shiny::isolate(state$publish_v2_transaction(graph, evaluation, impact))
  snapshot <- shiny::isolate(state$snapshot())
  testthat::expect_identical(snapshot$scenario_type, "v2_multiteam_trade")
  testthat::expect_identical(snapshot$scenario_scope, "TRADE_LOCAL")
  testthat::expect_false(shiny::isolate(tbi_scenario_active(state)))
  testthat::expect_identical(
    snapshot$v2_organizational_impact$executive_recommendation,
    "PROCEED WITH REVIEW"
  )
  state$clear()
  cleared <- shiny::isolate(state$snapshot())
  testthat::expect_false(cleared$active)
  testthat::expect_null(cleared$scenario_scope)
  testthat::expect_null(cleared$v2_transaction_graph)
})

testthat::test_that("shared state rejects cross-wired V2 graph evaluation and impact contracts", {
  teams <- c("A", "B", "C")
  graph_a <- normalize_transaction_graph(
    "same-id", teams,
    make_v2_route("route-a", "player-a", "A", "B", 1)
  )
  graph_b <- normalize_transaction_graph(
    "same-id", teams,
    make_v2_route("route-b", "player-b", "B", "C", 2)
  )
  evaluation_a <- evaluate_multiteam_transaction(graph_a)
  impact_a <- build_v2_organizational_impact(graph_a, evaluation_a)
  state <- tbi_transaction_state()

  testthat::expect_error(
    shiny::isolate(state$publish_v2_transaction(graph_b, evaluation_a, impact_a)),
    "signatures"
  )
  testthat::expect_false(shiny::isolate(state$snapshot())$active)
})

testthat::test_that("sequential evaluated two-team inputs receive coupled identities", {
  state <- tbi_transaction_state()
  player <- function(id, salary) data.frame(
    player_id = id,
    player_name = id,
    cap_hit = salary,
    stringsAsFactors = FALSE
  )
  evaluation <- function(team, partner) list(
    team_a_name = team,
    team_b_name = partner,
    status = "PASS"
  )

  shiny::isolate(state$publish_trade(
    "A", "B", "2026-27", player("out-a", 1), player("in-a", 2),
    evaluation = evaluation("A", "B")
  ))
  first <- shiny::isolate(state$snapshot())
  shiny::isolate(state$publish_trade(
    "A", "B", "2026-27", player("out-b", 3), player("in-b", 4),
    evaluation = evaluation("A", "B")
  ))
  second <- shiny::isolate(state$snapshot())

  testthat::expect_false(identical(first$scenario_id, second$scenario_id))
  testthat::expect_false(identical(first$transaction_signature, second$transaction_signature))
  testthat::expect_identical(first$transaction_signature, first$evaluation_signature)
  testthat::expect_identical(second$transaction_signature, second$evaluation_signature)
})

testthat::test_that("unknown active scenario types do not fail open as shared-supported", {
  testthat::expect_null(tbi_scenario_scope_value(list(
    active = TRUE,
    scenario_type = "future_unsupported_scenario"
  )))
  testthat::expect_false(tbi_scenario_is_shared_supported(list(
    active = TRUE,
    scenario_type = "future_unsupported_scenario"
  )))
})

testthat::test_that("shared roster and payroll adapters reject Trade-local envelopes", {
  roster <- data.frame(
    player_id = c("out", "keep"),
    player_name = c("Outgoing", "Keeper"),
    stringsAsFactors = FALSE
  )
  scenario <- list(
    active = TRUE,
    scenario_type = "trade",
    scenario_scope = "TRADE_LOCAL",
    team = "A",
    partner_team = "B",
    outgoing_players = roster[1, , drop = FALSE],
    incoming_players = data.frame(
      player_id = "in",
      player_name = "Incoming",
      stringsAsFactors = FALSE
    ),
    salary_delta = 5
  )
  state <- list(snapshot = function() scenario)

  testthat::expect_identical(
    tbi_apply_trade_scenario_to_roster(roster, state, "A"),
    roster
  )
  testthat::expect_identical(
    tbi_apply_trade_scenario_to_payroll(100, state, "A"),
    100
  )

  scenario$scenario_scope <- "SHARED_SUPPORTED"
  shared_roster <- tbi_apply_trade_scenario_to_roster(roster, state, "A")
  testthat::expect_setequal(shared_roster$player_id, c("keep", "in"))
  testthat::expect_identical(
    tbi_apply_trade_scenario_to_payroll(100, state, "A"),
    105
  )
})
