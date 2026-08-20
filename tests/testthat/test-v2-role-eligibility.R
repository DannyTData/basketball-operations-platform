v2_role_test_roster <- function() {
  data.frame(
    player_id = 1:12,
    player_name = sprintf("Player %02d", 1:12),
    team_id = "TEST",
    season = "2026-27",
    position = rep(c("PG", "SG", "SF", "PF", "C", "G"), 2L),
    primary_position = rep(c("PG", "SG", "SF", "PF", "C", "G"), 2L),
    depth_order = c(rep(1L, 5L), rep(2L, 5L), rep(3L, 2L)),
    is_starter = c(rep(1L, 5L), rep(0L, 7L)),
    is_position_override = 0L,
    position_override_reason = NA_character_,
    availability_status = "AVAILABLE",
    bie_rating = seq(90, 60, length.out = 12L),
    projected_bie_rating = seq(91, 61, length.out = 12L),
    impact_score = seq(89, 59, length.out = 12L),
    is_preseason_rookie = FALSE,
    tbi_performance_available = TRUE,
    stringsAsFactors = FALSE
  )
}


v2_role_manual <- function(player_id, role, eligibility = "ELIGIBLE",
                           team_id = "TEST", season = "2026-27") {
  data.frame(
    player_id = player_id,
    team_id = team_id,
    season = season,
    role = role,
    eligibility = eligibility,
    source = "MANUAL_VERIFIED",
    author_source_note = "Approved by test basketball operations",
    reason = "Explicit Phase 1D fixture evidence",
    stringsAsFactors = FALSE
  )
}


v2_role_manual_availability <- function(player_ids) {
  data.frame(
    player_id = player_ids,
    team_id = "TEST",
    season = "2026-27",
    availability_status = "AVAILABLE",
    evidence_class = "AUTHORITATIVE_FACT",
    source = "MANUAL_VERIFIED",
    source_version = "test-source-1",
    verification_status = "VERIFIED",
    verified_by = "Test Basketball Operations",
    reason = "Explicit availability fixture",
    effective_date = "2026-10-20",
    expiration_date = "2026-10-21",
    minute_restriction = NA_real_,
    evidence_version = "1.0.0",
    stringsAsFactors = FALSE
  )
}


testthat::test_that("role policy documents authoritative precedence", {
  policy <- v2_role_policy()
  testthat::expect_identical(policy$contract_version, "1.0.0")
  testthat::expect_identical(
    policy$supported_roles,
    c("POSITION_PG", "POSITION_SG", "POSITION_SF", "POSITION_PF", "POSITION_C",
      "PRIMARY_CREATOR", "SECONDARY_CREATOR", "BALL_HANDLER", "RIM_PROTECTOR",
      "BACKUP_PG", "BACKUP_C")
  )
  testthat::expect_identical(
    policy$precedence,
    c("MANUAL_VERIFIED", "AUTHORITATIVE_FACT", "DETERMINISTIC_DERIVATION",
      "MODEL_EVIDENCE", "UNKNOWN")
  )
  testthat::expect_false(policy$generic_position_implies_backup_role)
  testthat::expect_false(policy$model_evidence_is_authoritative)
  sources <- v2_role_evidence_sources()
  testthat::expect_false(sources$can_establish_eligible[sources$source == "PLAYER_POSITIONS_TABLE"])
  testthat::expect_false(sources$can_establish_eligible[sources$source == "MODEL_OR_BIE_EVIDENCE"])
  weakened <- policy
  weakened$generic_position_implies_backup_role <- NA
  testthat::expect_error(validate_v2_role_policy(weakened), "weakens")
  wrong_type <- policy
  wrong_type$policy_type <- "other-policy"
  testthat::expect_error(validate_v2_role_policy(wrong_type), "weakens")
})


testthat::test_that("ledger is deterministic and generic positions stay narrow", {
  roster <- v2_role_test_roster()
  first <- build_v2_role_eligibility_ledger(roster, "TEST", "2026-27")
  second <- build_v2_role_eligibility_ledger(roster[sample(nrow(roster)), ], "TEST", "2026-27")

  testthat::expect_identical(first$contract_type, "tbi-v2-role-eligibility-ledger")
  testthat::expect_identical(first$input_signature, second$input_signature)
  testthat::expect_identical(first$records, second$records)
  testthat::expect_length(first$records, nrow(roster) * 11L)

  pg <- v2_role_record(first, 1L, "POSITION_PG")
  sg <- v2_role_record(first, 1L, "POSITION_SG")
  generic_guard <- v2_role_record(first, 6L, "POSITION_PG")
  backup <- v2_role_record(first, 1L, "BACKUP_PG")
  testthat::expect_identical(pg$eligibility, "ELIGIBLE")
  testthat::expect_identical(pg$verification_status, "VERIFIED")
  testthat::expect_identical(sg$eligibility, "UNKNOWN")
  testthat::expect_identical(generic_guard$eligibility, "UNKNOWN")
  testthat::expect_identical(backup$eligibility, "UNKNOWN")
})


testthat::test_that("approved overrides and verified multi-position facts are explicit", {
  roster <- v2_role_test_roster()
  roster$is_position_override[[6]] <- 1L
  roster$position[[6]] <- "PG"
  roster$position_override_reason[[6]] <- "Approved emergency point assignment"
  roster$verified_positions <- I(rep(list(character()), nrow(roster)))
  roster$verified_positions_source <- "TEST_VERIFIED_POSITION_LEDGER"
  roster$verified_positions_source_version <- "1.0.0"
  roster$verified_positions_verification_status <- "VERIFIED"
  roster$position[[7]] <- "G"
  roster$primary_position[[7]] <- "G"
  roster$verified_positions[[7]] <- c("SG", "SF")

  ledger <- build_v2_role_eligibility_ledger(roster, "TEST", "2026-27")
  override <- v2_role_record(ledger, 6L, "POSITION_PG")
  multi_sg <- v2_role_record(ledger, 7L, "POSITION_SG")
  multi_sf <- v2_role_record(ledger, 7L, "POSITION_SF")
  multi_pg <- v2_role_record(ledger, 7L, "POSITION_PG")

  testthat::expect_identical(override$evidence_class, "AUTHORITATIVE_FACT")
  testthat::expect_identical(override$source, "APPROVED_POSITION_OVERRIDE")
  testthat::expect_identical(multi_sg$eligibility, "ELIGIBLE")
  testthat::expect_identical(multi_sf$eligibility, "ELIGIBLE")
  testthat::expect_identical(multi_pg$eligibility, "UNKNOWN")
})


testthat::test_that("unverified multi-position rows remain unknown", {
  roster <- v2_role_test_roster()
  roster$position[[7]] <- "G"
  roster$primary_position[[7]] <- "G"
  roster$verified_positions <- I(rep(list(character()), nrow(roster)))
  roster$verified_positions[[7]] <- c("PG", "SG")
  ledger <- build_v2_role_eligibility_ledger(roster, "TEST", "2026-27")
  testthat::expect_identical(v2_role_record(ledger, 7L, "POSITION_PG")$eligibility, "UNKNOWN")
  testthat::expect_identical(v2_role_record(ledger, 7L, "POSITION_SG")$eligibility, "UNKNOWN")
})


testthat::test_that("manual verified evidence supports eligible and not eligible", {
  roster <- v2_role_test_roster()
  manual <- rbind(
    v2_role_manual(6L, "BACKUP_PG", "ELIGIBLE"),
    v2_role_manual(10L, "BACKUP_C", "NOT_ELIGIBLE")
  )
  ledger <- build_v2_role_eligibility_ledger(roster, "TEST", "2026-27", manual)

  pg <- v2_role_record(ledger, 6L, "BACKUP_PG")
  center <- v2_role_record(ledger, 10L, "BACKUP_C")
  testthat::expect_identical(pg$eligibility, "ELIGIBLE")
  testthat::expect_identical(center$eligibility, "NOT_ELIGIBLE")
  testthat::expect_identical(pg$source, "MANUAL_VERIFIED")
  testthat::expect_identical(pg$verification_status, "VERIFIED")
})


testthat::test_that("known eligibility rejects model and missing verification", {
  testthat::expect_error(new_v2_role_eligibility(
    6L, "BACKUP_PG", "ELIGIBLE", "VERIFIED", "model-v1", "score",
    evidence_class = "MODEL_EVIDENCE"
  ), "Model or unknown")
  testthat::expect_error(new_v2_role_eligibility(
    6L, "BACKUP_PG", "ELIGIBLE", "VERIFIED", "derived-v1", "rule",
    evidence_class = "DETERMINISTIC_DERIVATION", verification_status = "UNVERIFIED"
  ), "verified evidence")
  derived <- new_v2_role_eligibility(
    6L, "BACKUP_PG", "ELIGIBLE", "VERIFIED", "derived-v1", "rule",
    evidence_class = "DETERMINISTIC_DERIVATION", verification_status = "DERIVED_VERIFIED"
  )
  testthat::expect_identical(derived$verification_status, "DERIVED_VERIFIED")
  testthat::expect_error(new_v2_role_eligibility(
    6L, "BACKUP_PG", verification_status = "VERIFIED"
  ), "Unknown role eligibility")
})


testthat::test_that("malformed, duplicate, conflicting, and out-of-scope evidence is rejected", {
  roster <- v2_role_test_roster()
  good <- v2_role_manual(6L, "BACKUP_PG")
  malformed <- good
  malformed$reason <- ""
  testthat::expect_error(
    build_v2_role_eligibility_ledger(roster, "TEST", "2026-27", malformed),
    "reason"
  )
  testthat::expect_error(
    build_v2_role_eligibility_ledger(roster, "TEST", "2026-27", rbind(good, good)),
    "duplicate"
  )
  conflict <- rbind(good, transform(good, eligibility = "NOT_ELIGIBLE"))
  testthat::expect_error(
    build_v2_role_eligibility_ledger(roster, "TEST", "2026-27", conflict),
    "conflicting"
  )
  testthat::expect_error(
    build_v2_role_eligibility_ledger(roster, "OTHER", "2026-27"),
    "team"
  )
  testthat::expect_error(
    build_v2_role_eligibility_ledger(roster, "TEST", "2027-28"),
    "season"
  )
  testthat::expect_error(
    build_v2_role_eligibility_ledger(
      roster, "TEST", "2026-27", v2_role_manual(99L, "BACKUP_PG")
    ),
    "outside roster"
  )
})


testthat::test_that("completeness diagnostics report only verified knowledge", {
  roster <- v2_role_test_roster()
  manual <- rbind(
    v2_role_manual(6L, "BACKUP_PG"),
    v2_role_manual(10L, "BACKUP_C")
  )
  ledger <- build_v2_role_eligibility_ledger(roster, "TEST", "2026-27", manual)
  diagnostics <- summarize_v2_role_completeness(ledger, roster)

  testthat::expect_identical(diagnostics$verified_backup_pg_candidates, 1L)
  testthat::expect_identical(diagnostics$verified_backup_c_candidates, 1L)
  testthat::expect_false(diagnostics$missing_backup_pg_evidence)
  testthat::expect_false(diagnostics$missing_backup_c_evidence)
  testthat::expect_identical(diagnostics$team_role_coverage_status, "PASS")
  testthat::expect_gt(diagnostics$unknown_position_eligibility, 0L)
  testthat::expect_lte(diagnostics$unknown_position_eligibility, nrow(roster))
})


testthat::test_that("shadow consumes the ledger without changing selection rules", {
  roster <- v2_role_test_roster()
  lineup <- stats::setNames(1:5, c("PG", "SG", "SF", "PF", "C"))
  manual <- rbind(
    v2_role_manual(6L, "BACKUP_PG"),
    v2_role_manual(10L, "BACKUP_C")
  )
  result <- run_v2_rotation_shadow(
    "v2_shadow", "TEST", "2026-27", roster, lineup,
    manual_role_evidence = manual,
    manual_availability_evidence = v2_role_manual_availability(roster$player_id),
    availability_as_of_date = "2026-10-20"
  )

  testthat::expect_identical(result$execution_status, "COMPLETED")
  testthat::expect_identical(result$starter_state$status, "PASS")
  testthat::expect_identical(result$rotation_10$status, "PASS")
  testthat::expect_identical(result$rotation_11$status, "PASS")
  testthat::expect_identical(result$role_diagnostics$team_role_coverage_status, "PASS")
  testthat::expect_true(all(c(6L, 10L) %in% result$rotation_10$members$player_id))
})


testthat::test_that("starter-only position facts do not satisfy bench roles", {
  roster <- v2_role_test_roster()
  lineup <- stats::setNames(1:5, c("PG", "SG", "SF", "PF", "C"))
  result <- run_v2_rotation_shadow(
    "v2_shadow", "TEST", "2026-27", roster, lineup,
    manual_availability_evidence = v2_role_manual_availability(roster$player_id),
    availability_as_of_date = "2026-10-20"
  )

  testthat::expect_identical(result$starter_state$status, "PASS")
  testthat::expect_identical(result$rotation_10$status, "REVIEW")
  testthat::expect_identical(result$role_diagnostics$verified_backup_pg_candidates, 0L)
  testthat::expect_identical(result$role_diagnostics$verified_backup_c_candidates, 0L)
})


testthat::test_that("completely missing role evidence remains unknown", {
  roster <- v2_role_test_roster()
  roster$position <- "G"
  roster$primary_position <- "G"
  ledger <- build_v2_role_eligibility_ledger(roster, "TEST", "2026-27")
  testthat::expect_true(all(vapply(ledger$records, `[[`, character(1), "eligibility") == "UNKNOWN"))
  diagnostics <- summarize_v2_role_completeness(ledger, roster)
  testthat::expect_identical(diagnostics$verified_position_eligibility, 0L)
  testthat::expect_identical(diagnostics$unknown_position_eligibility, nrow(roster))
})


testthat::test_that("rookie starter and context switches remain deterministic", {
  roster <- v2_role_test_roster()
  roster$is_preseason_rookie[[1]] <- TRUE
  lineup <- stats::setNames(1:5, c("PG", "SG", "SF", "PF", "C"))
  manual <- rbind(v2_role_manual(6L, "BACKUP_PG"), v2_role_manual(10L, "BACKUP_C"))
  first <- run_v2_rotation_shadow(
    "v2_shadow", "TEST", "2026-27", roster, lineup,
    manual_role_evidence = manual
  )
  second <- run_v2_rotation_shadow(
    "v2_shadow", "TEST", "2026-27", roster, lineup,
    manual_role_evidence = manual
  )
  testthat::expect_true(1L %in% first$rotation_10$members$player_id)
  testthat::expect_identical(first$role_ledger, second$role_ledger)
  testthat::expect_identical(first$rotation_10, second$rotation_10)

  switched <- roster
  switched$team_id <- "OTHER"
  testthat::expect_error(
    build_v2_role_eligibility_ledger(switched, "TEST", "2026-27"),
    "team"
  )
  shadow_switched <- run_v2_rotation_shadow(
    "v2_shadow", "OTHER", "2026-27", roster, lineup,
    manual_role_evidence = manual
  )
  testthat::expect_identical(shadow_switched$execution_status, "ERROR")
  testthat::expect_match(shadow_switched$error$message, "team")
})


testthat::test_that("known raw role contracts cannot bypass ledger precedence", {
  roster <- v2_role_test_roster()
  lineup <- stats::setNames(1:5, c("PG", "SG", "SF", "PF", "C"))
  raw <- list(new_v2_role_eligibility(
    6L, "BACKUP_PG", "ELIGIBLE", "VERIFIED", "foreign", "role",
    team_id = "OTHER", season = "2025-26"
  ))
  result <- run_v2_rotation_shadow(
    "v2_shadow", "TEST", "2026-27", roster, lineup, role_eligibility = raw
  )
  testthat::expect_identical(result$execution_status, "ERROR")
  testthat::expect_match(result$error$message, "manual_role_evidence")
})


testthat::test_that("approved starter-lock conflicts remain blocking", {
  roster <- v2_role_test_roster()
  roster$approved_lock_conflict <- FALSE
  roster$approved_lock_conflict[1:2] <- TRUE
  lineup <- stats::setNames(1:5, c("PG", "SG", "SF", "PF", "C"))
  ledger <- build_v2_role_eligibility_ledger(roster, "TEST", "2026-27")
  starter <- v2_shadow_starter_state(
    "TEST", "2026-27", roster, lineup, role_ledger = ledger
  )
  testthat::expect_identical(starter$status, "FAIL")
  testthat::expect_true(starter$is_blocked)
  testthat::expect_true(any(vapply(
    starter$validation$findings,
    function(x) identical(x$code, "APPROVED_STARTER_LOCK_CONFLICT"), logical(1)
  )))
})
