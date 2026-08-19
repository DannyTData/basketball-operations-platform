# ============================================================
# V2 rotation contract foundation
# ============================================================

testthat::test_that("V2 metadata and policy are versioned and conservative", {
  metadata <- v2_rotation_metadata()
  policy <- v2_rotation_policy()

  testthat::expect_identical(metadata$model_name, "tbi-rotation")
  testthat::expect_identical(metadata$model_version, "2.0.0-phase1")
  testthat::expect_identical(policy$canonical_statuses, c("PASS", "REVIEW", "FAIL"))
  testthat::expect_identical(policy$rotation_sizes, c(10L, 11L))
  testthat::expect_true(policy$unknown_role_coverage_requires_review)
  testthat::expect_true(policy$approved_lock_conflicts_are_blocking)
  weakened <- policy
  weakened$unknown_role_coverage_requires_review <- FALSE
  testthat::expect_error(
    validate_v2_rotation_policy(weakened),
    "weakens required V2 safety behavior"
  )
})


testthat::test_that("findings aggregate canonical status and blocking independently", {
  review <- new_v2_validation_finding(
    code = "ROLE_EVIDENCE_UNKNOWN",
    status = "REVIEW",
    message = "Backup point guard evidence is unknown."
  )
  blocked <- new_v2_validation_finding(
    code = "LOCKED_STARTER_UNAVAILABLE",
    status = "FAIL",
    message = "An approved starter is unavailable.",
    is_blocking = TRUE
  )

  review_result <- aggregate_v2_validation(list(review))
  blocked_result <- aggregate_v2_validation(list(review, blocked))

  testthat::expect_identical(review_result$status, "REVIEW")
  testthat::expect_false(review_result$is_blocked)
  testthat::expect_identical(blocked_result$status, "FAIL")
  testthat::expect_true(blocked_result$is_blocked)
  testthat::expect_error(
    new_v2_validation_finding("BAD", "BLOCKED", "Not canonical."),
    "PASS, REVIEW, or FAIL"
  )
})


testthat::test_that("role eligibility never promotes unknown evidence", {
  unknown <- new_v2_role_eligibility(
    player_id = 10L,
    role = "BACKUP_PG"
  )
  verified <- new_v2_role_eligibility(
    player_id = 11L,
    role = "BACKUP_C",
    eligibility = "ELIGIBLE",
    evidence_status = "VERIFIED",
    evidence_source = "approved-role-ledger",
    evidence_fields = "backup_center_eligible"
  )

  testthat::expect_identical(unknown$eligibility, "UNKNOWN")
  testthat::expect_identical(unknown$evidence_status, "UNKNOWN")
  testthat::expect_identical(unknown$validation$status, "REVIEW")
  testthat::expect_identical(unknown$status, "REVIEW")
  testthat::expect_false(unknown$validation$is_blocked)
  testthat::expect_identical(verified$validation$status, "PASS")
  testthat::expect_error(
    new_v2_role_eligibility(12L, "BACKUP_PG", "ELIGIBLE", "UNKNOWN"),
    "verified evidence"
  )
})


testthat::test_that("starter contracts preserve locks and block invalid conditions", {
  slots <- data.frame(
    position = c("PG", "SG", "SF", "PF", "C"),
    player_id = 1:5,
    player_name = paste("Starter", 1:5),
    lock_status = "LOCKED",
    lock_source = "APPROVED",
    availability_status = c("OUT", rep("AVAILABLE", 4)),
    position_eligibility = "PASS",
    stringsAsFactors = FALSE
  )
  state <- new_v2_starter_state(
    team_id = "BOS",
    season = "2026-27",
    slots = slots,
    roster_signature = "roster-signature"
  )

  testthat::expect_identical(state$contract_type, "tbi-v2-starter-state")
  testthat::expect_identical(state$contract_version, "1.0.0")
  testthat::expect_identical(state$model_name, "tbi-rotation")
  testthat::expect_identical(state$model_version, "2.0.0-phase1")
  testthat::expect_identical(state$validation$status, "FAIL")
  testthat::expect_identical(state$status, "FAIL")
  testthat::expect_true(state$validation$is_blocked)
  testthat::expect_true(state$is_blocked)
  testthat::expect_identical(state$slots$player_id, 1:5)
  testthat::expect_match(state$starter_state_id, "^starter-")
})


testthat::test_that("verified role eligibility requires explicit provenance", {
  testthat::expect_error(
    new_v2_role_eligibility(
      player_id = 11L,
      role = "BACKUP_C",
      eligibility = "ELIGIBLE",
      evidence_status = "VERIFIED"
    ),
    "evidence source and evidence fields"
  )
  testthat::expect_error(
    new_v2_role_eligibility(NA_integer_, "BACKUP_PG"),
    "player_id must be one known integer ID"
  )
})


testthat::test_that("starter contracts reject invalid lock and availability enums", {
  slots <- data.frame(
    position = c("PG", "SG", "SF", "PF", "C"),
    player_id = 1:5,
    player_name = paste("Starter", 1:5),
    lock_status = "LOCKED",
    lock_source = c("APPROVED", "MANUAL", "V1_BASELINE", "MODEL_PROPOSAL", "BANANA"),
    availability_status = c(rep("AVAILABLE", 4), "BANANA"),
    position_eligibility = "PASS",
    stringsAsFactors = FALSE
  )
  state <- new_v2_starter_state("BOS", "2026-27", slots, "roster")

  testthat::expect_identical(state$status, "FAIL")
  testthat::expect_true(state$is_blocked)
})


testthat::test_that("rotation state is a pure envelope and does not select candidates", {
  members <- data.frame(
    player_id = 1:10,
    player_name = paste("Player", 1:10),
    is_starter = c(rep(TRUE, 5), rep(FALSE, 5)),
    starter_position = c("PG", "SG", "SF", "PF", "C", rep(NA, 5)),
    bench_order = c(rep(NA_integer_, 5), 1:5),
    rotation_role = c(rep("STARTER", 5), "SIXTH_MAN", rep("BENCH", 4)),
    stringsAsFactors = FALSE
  )
  state <- new_v2_rotation_state(
    team_id = "BOS",
    season = "2026-27",
    starter_state_id = "starter-123",
    requested_rotation_size = 10L,
    members = members,
    roster_signature = "roster-signature",
    policy_signature = v2_input_signature(v2_rotation_policy()),
    role_eligibility = list(
      new_v2_role_eligibility(6L, "BACKUP_PG"),
      new_v2_role_eligibility(7L, "BACKUP_C")
    )
  )

  testthat::expect_identical(state$contract_type, "tbi-v2-rotation-state")
  testthat::expect_identical(state$contract_version, "1.0.0")
  testthat::expect_identical(state$actual_rotation_size, 10L)
  testthat::expect_identical(state$validation$status, "REVIEW")
  testthat::expect_identical(state$status, "REVIEW")
  testthat::expect_false(state$validation$is_blocked)
  testthat::expect_false(state$is_blocked)
  testthat::expect_match(state$rotation_state_id, "^rotation-")
})


testthat::test_that("rotation coverage cannot pass without eligible role evidence", {
  members <- data.frame(
    player_id = 1:10,
    player_name = paste("Player", 1:10),
    is_starter = c(rep(TRUE, 5), rep(FALSE, 5)),
    starter_position = c("PG", "SG", "SF", "PF", "C", rep(NA, 5)),
    bench_order = c(rep(NA_integer_, 5), 1:5),
    rotation_role = c(rep("STARTER", 5), "SIXTH_MAN", rep("BENCH", 4)),
    stringsAsFactors = FALSE
  )
  state <- new_v2_rotation_state(
    team_id = "BOS",
    season = "2026-27",
    starter_state_id = "starter-123",
    requested_rotation_size = 10L,
    members = members,
    roster_signature = "roster-signature",
    policy_signature = v2_input_signature(v2_rotation_policy()),
    role_eligibility = list()
  )

  testthat::expect_identical(state$status, "REVIEW")
  testthat::expect_false(state$is_blocked)
  testthat::expect_true(all(
    c("BACKUP_PG_COVERAGE_UNKNOWN", "BACKUP_C_COVERAGE_UNKNOWN") %in%
      vapply(state$validation$findings, `[[`, character(1), "code")
  ))
})


testthat::test_that("role evidence outside the rotation is blocking", {
  members <- data.frame(
    player_id = 1:10,
    player_name = paste("Player", 1:10),
    is_starter = c(rep(TRUE, 5), rep(FALSE, 5)),
    starter_position = c("PG", "SG", "SF", "PF", "C", rep(NA, 5)),
    bench_order = c(rep(NA_integer_, 5), 1:5),
    rotation_role = c(rep("STARTER", 5), "SIXTH_MAN", rep("BENCH", 4)),
    stringsAsFactors = FALSE
  )
  state <- new_v2_rotation_state(
    "BOS", "2026-27", "starter-123", 10L, members, "roster", "policy",
    role_eligibility = list(
      new_v2_role_eligibility(
        99L, "BACKUP_PG", "ELIGIBLE", "VERIFIED", "ledger", "backup_pg"
      )
    )
  )

  testthat::expect_identical(state$status, "FAIL")
  testthat::expect_true(state$is_blocked)
})


testthat::test_that("signatures and IDs are deterministic and order-aware", {
  first <- list(team_id = "BOS", season = "2026-27", players = c(1L, 2L, 3L))
  same <- list(players = c(1L, 2L, 3L), season = "2026-27", team_id = "BOS")
  changed <- list(team_id = "BOS", season = "2026-27", players = c(2L, 1L, 3L))

  testthat::expect_identical(v2_input_signature(first), v2_input_signature(same))
  testthat::expect_false(identical(v2_input_signature(first), v2_input_signature(changed)))
  testthat::expect_identical(v2_state_id("rotation", first), v2_state_id("rotation", same))
})
