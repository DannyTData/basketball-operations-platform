minute_rotation_fixture <- function(size = 10L) {
  stopifnot(size %in% c(10L, 11L))
  data.frame(
    player_id = seq_len(size),
    player_name = paste("Player", seq_len(size)),
    is_starter = c(rep(TRUE, 5), rep(FALSE, size - 5L)),
    starter_position = c(c("PG", "SG", "SF", "PF", "C"), rep(NA_character_, size - 5L)),
    bench_order = c(rep(NA_integer_, 5), seq_len(size - 5L)),
    rotation_role = c(rep("STARTER", 5), "SIXTH_MAN", rep("ROTATION", size - 6L)),
    ranking_evidence_status = "COMPLETE",
    is_designated_rookie_starter = c(TRUE, rep(FALSE, size - 1L)),
    stringsAsFactors = FALSE
  )
}

minute_availability_fixture <- function(members, status = "AVAILABLE", cap = NA_real_) {
  records <- lapply(seq_len(nrow(members)), function(i) {
    list(
      contract_type = "tbi-v2-availability-evidence",
      contract_version = "1.0.0",
      player_id = members$player_id[[i]],
      availability_status = status[[min(i, length(status))]],
      verification_status = if (status[[min(i, length(status))]] == "UNKNOWN") "MISSING" else "VERIFIED",
      minute_restriction = cap[[min(i, length(cap))]],
      evidence_fields = if (status[[min(i, length(status))]] == "UNKNOWN") character() else "manual_availability",
      missing_fields = if (status[[min(i, length(status))]] == "UNKNOWN") "verified_availability_status" else character(),
      reason_codes = if (status[[min(i, length(status))]] == "UNKNOWN") "AVAILABILITY_EVIDENCE_UNKNOWN" else paste0("VERIFIED_AVAILABILITY_", status[[min(i, length(status))]])
    )
  })
  list(contract_type = "tbi-v2-availability-evidence-ledger", contract_version = "1.0.0", records = records, input_signature = v2_input_signature(records))
}

testthat::test_that("Phase 2A produces deterministic exact integer ledgers for 10 and 11 players", {
  for (size in c(10L, 11L)) {
    members <- minute_rotation_fixture(size)
    availability <- minute_availability_fixture(members)
    first <- allocate_v2_minutes(members, availability)
    second <- allocate_v2_minutes(members, availability)
    testthat::expect_identical(first$contract_type, "tbi-v2-minute-ledger")
    testthat::expect_identical(first$contract_version, "1.0.0")
    testthat::expect_identical(first$model_version, "2.0.0-phase2a")
    testthat::expect_identical(first$status, "PASS")
    testthat::expect_equal(sum(first$ledger$assigned_minutes), 240L)
    testthat::expect_true(all(first$ledger$assigned_minutes == as.integer(first$ledger$assigned_minutes)))
    testthat::expect_identical(anyDuplicated(first$ledger$player_id), 0L)
    testthat::expect_true(all(first$ledger$assigned_minutes[first$ledger$is_starter] >= first$ledger$minimum_minutes[first$ledger$is_starter]))
    testthat::expect_true(all(first$ledger$assigned_minutes[!first$ledger$is_starter] <= first$ledger$maximum_minutes[!first$ledger$is_starter]))
    testthat::expect_identical(first$input_signature, second$input_signature)
    testthat::expect_identical(first$ledger, second$ledger)
  }
})

testthat::test_that("Phase 2A availability is governed conservatively", {
  members <- minute_rotation_fixture(10L)
  out <- minute_availability_fixture(members, c("OUT", rep("AVAILABLE", 9)), rep(NA_real_, 10))
  out_result <- allocate_v2_minutes(members, out)
  testthat::expect_equal(out_result$ledger$assigned_minutes[[1]], 0L)
  testthat::expect_identical(out_result$status, "FAIL")
  testthat::expect_true(out_result$is_blocked)

  limited <- minute_availability_fixture(members, c("LIMITED", rep("AVAILABLE", 9)), c(20, rep(NA_real_, 9)))
  limited_result <- allocate_v2_minutes(members, limited)
  testthat::expect_lte(limited_result$ledger$assigned_minutes[[1]], 20L)
  testthat::expect_equal(sum(limited_result$ledger$assigned_minutes), 240L)

  unknown <- minute_availability_fixture(members, c("UNKNOWN", rep("AVAILABLE", 9)), rep(NA_real_, 10))
  unknown_result <- allocate_v2_minutes(members, unknown)
  testthat::expect_identical(unknown_result$status, "REVIEW")
  testthat::expect_gt(unknown_result$ledger$assigned_minutes[[1]], 0L)
  testthat::expect_true("AVAILABILITY_UNKNOWN" %in% unknown_result$ledger$reason_codes[[1]])
})

testthat::test_that("Phase 2A validates manual overrides and infeasible capacity", {
  members <- minute_rotation_fixture(10L)
  availability <- minute_availability_fixture(members)
  override <- data.frame(player_id = 1L, minutes = 38L, source = "COACH", reason = "Approved plan", provenance = "session:test", validation = "VERIFIED")
  result <- allocate_v2_minutes(members, availability, manual_overrides = override)
  testthat::expect_equal(result$ledger$assigned_minutes[[1]], 38L)
  testthat::expect_identical(result$ledger$override_source[[1]], "COACH")
  testthat::expect_false(identical(result$input_signature, allocate_v2_minutes(members, availability)$input_signature))

  testthat::expect_error(allocate_v2_minutes(members, availability, manual_overrides = rbind(override, transform(override, minutes = 37L))), "conflicting")
  testthat::expect_error(allocate_v2_minutes(members, availability, manual_overrides = transform(override, player_id = 99L)), "outside")
  testthat::expect_error(allocate_v2_minutes(members, availability, manual_overrides = transform(override, minutes = -1L)), "negative")
  impossible <- allocate_v2_minutes(members, availability, manual_overrides = transform(override, minutes = 49L))
  testthat::expect_identical(impossible$status, "FAIL")
  testthat::expect_true(any(vapply(impossible$validation$findings, `[[`, character(1), "code") == "MANUAL_OVERRIDE_OUTSIDE_BOUNDS"))

  capped <- minute_availability_fixture(members, rep("LIMITED", 10), rep(20, 10))
  infeasible <- allocate_v2_minutes(members, capped)
  testthat::expect_false(identical(infeasible$status, "PASS"))
  testthat::expect_true(any(vapply(infeasible$validation$findings, `[[`, character(1), "code") == "MINUTE_CAPACITY_INSUFFICIENT"))
})

testthat::test_that("Phase 2A preserves rookie starters and surfaces ranking review", {
  members <- minute_rotation_fixture(10L)
  availability <- minute_availability_fixture(members)
  rookie <- allocate_v2_minutes(members, availability)
  testthat::expect_gt(rookie$ledger$assigned_minutes[[1]], 0L)
  members$ranking_evidence_status[[2]] <- "INCOMPLETE"
  review <- allocate_v2_minutes(members, availability)
  testthat::expect_identical(review$status, "REVIEW")
  testthat::expect_true("RANKING_EVIDENCE_INCOMPLETE" %in% review$ledger$reason_codes[[2]])
})

testthat::test_that("Phase 2A rejects malformed rotations and leaves V1 allocation equivalent", {
  members <- minute_rotation_fixture(10L)
  availability <- minute_availability_fixture(members)
  duplicate <- members
  duplicate$player_id[[10]] <- duplicate$player_id[[9]]
  testthat::expect_error(allocate_v2_minutes(duplicate, availability), "unique")
  blocked <- list(
    contract_type = "tbi-v2-rotation-state", status = "FAIL", is_blocked = TRUE,
    members = members
  )
  testthat::expect_error(allocate_v2_minutes(blocked, availability), "non-blocked")

  v1_roster <- data.frame(
    player_id = 101:110, player_name = paste("V1", 1:10), position = rep(c("PG", "SG", "SF", "PF", "C"), 2),
    depth_order = rep(1:2, each = 5), is_starter = c(rep(TRUE, 5), rep(FALSE, 5)), availability_status = "Available",
    bie_rating = seq(88, 70, length.out = 10), projected_bie_rating = seq(89, 71, length.out = 10), impact_score = seq(87, 69, length.out = 10),
    tbi_prior_nba_games = rep(10, 10), stringsAsFactors = FALSE
  )
  before <- build_minute_allocation(v1_roster, rotation_size = 10)$allocation
  allocate_v2_minutes(members, availability)
  after <- build_minute_allocation(v1_roster, rotation_size = 10)$allocation
  testthat::expect_identical(after, before)
})
