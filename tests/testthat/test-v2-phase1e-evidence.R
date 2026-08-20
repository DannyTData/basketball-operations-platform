v2_phase1e_roster <- function() {
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
    creation_role = rep(c("PRIMARY CREATOR", "SECONDARY CREATOR", "SUPPORT PLAYMAKER"), 4L),
    creation_score = seq(90, 57, length.out = 12L),
    secondary_creation_score = seq(85, 52, length.out = 12L),
    playmaking_confidence = "SUPPORTED",
    defensive_role = rep(c("HIGH-END DEFENSIVE IMPACT", "FUNCTIONAL DEFENSIVE IMPACT"), 6L),
    interior_impact_score = seq(90, 57, length.out = 12L),
    defense_confidence = "SUPPORTED",
    bie_rating = seq(90, 60, length.out = 12L),
    projected_bie_rating = seq(91, 61, length.out = 12L),
    impact_score = seq(89, 59, length.out = 12L),
    is_preseason_rookie = FALSE,
    tbi_performance_available = TRUE,
    stringsAsFactors = FALSE
  )
}


v2_phase1e_manual_availability <- function(player_id,
                                            status = "AVAILABLE",
                                            team_id = "TEST",
                                            season = "2026-27",
                                            minute_restriction = NA_real_) {
  data.frame(
    player_id = player_id,
    team_id = team_id,
    season = season,
    availability_status = status,
    evidence_class = "AUTHORITATIVE_FACT",
    source = "MANUAL_VERIFIED",
    source_version = "test-source-1",
    verification_status = "VERIFIED",
    verified_by = "Test Basketball Operations",
    reason = "Explicit test availability evidence",
    effective_date = "2026-10-20",
    expiration_date = "2026-10-21",
    minute_restriction = minute_restriction,
    evidence_version = "1.0.0",
    stringsAsFactors = FALSE
  )
}


v2_phase1e_manual_role <- function(player_id,
                                    role,
                                    eligibility = "ELIGIBLE",
                                    team_id = "TEST",
                                    season = "2026-27") {
  data.frame(
    player_id = player_id,
    team_id = team_id,
    season = season,
    role = role,
    eligibility = eligibility,
    source = "MANUAL_VERIFIED",
    source_version = "test-source-1",
    verification_status = "VERIFIED",
    verified_by = "Test Basketball Operations",
    author_source_note = "Approved test evidence",
    reason = "Explicit Phase 1E fixture evidence",
    effective_date = "2026-10-20",
    expiration_date = "2026-10-21",
    evidence_version = "1.0.0",
    stringsAsFactors = FALSE
  )
}


testthat::test_that("availability contract preserves unknowns and validates provenance", {
  unknown <- new_v2_availability_evidence(1L, "TEST", "2026-27")
  testthat::expect_identical(unknown$contract_type, "tbi-v2-availability-evidence")
  testthat::expect_identical(unknown$contract_version, "1.0.0")
  testthat::expect_identical(unknown$availability_status, "UNKNOWN")
  testthat::expect_identical(unknown$status, "REVIEW")
  testthat::expect_match(unknown$input_signature, "^v2sig1-")
  sources <- v2_availability_evidence_sources()
  testthat::expect_false(any(sources$can_establish_status[
    sources$source %in% c("ROSTER_STATUS", "MINUTES_OR_RECENT_GAMES", "INJURY_HISTORY")
  ]))

  available <- new_v2_availability_evidence(
    1L, "TEST", "2026-27", "AVAILABLE", "VERIFIED", "AUTHORITATIVE_FACT",
    "MANUAL_VERIFIED", "source-1", "2026-10-20", "2026-10-21",
    "Basketball Ops", "Explicit availability confirmation"
  )
  testthat::expect_identical(available$availability_status, "AVAILABLE")
  testthat::expect_identical(available$status, "PASS")
  testthat::expect_error(
    new_v2_availability_evidence(1L, "TEST", "2026-27", "OUT"),
    "verified provenance"
  )
  testthat::expect_error(new_v2_availability_evidence(
    1L, "TEST", "2026-27", "AVAILABLE", "VERIFIED", "MODEL_EVIDENCE",
    "model", "1", "2026-10-20", NULL, "analyst", "model output"
  ), "cannot establish")
})


testthat::test_that("minute restrictions are explicit and limited to LIMITED", {
  limited <- new_v2_availability_evidence(
    1L, "TEST", "2026-27", "LIMITED", "VERIFIED", "AUTHORITATIVE_FACT",
    "MANUAL_VERIFIED", "source-1", "2026-10-20", "2026-10-21",
    "Basketball Ops", "Explicit minutes restriction", minute_restriction = 24
  )
  testthat::expect_identical(limited$minute_restriction, 24)
  testthat::expect_error(new_v2_availability_evidence(
    1L, "TEST", "2026-27", "OUT", "VERIFIED", "AUTHORITATIVE_FACT",
    "MANUAL_VERIFIED", "source-1", "2026-10-20", "2026-10-21",
    "Basketball Ops", "Out", minute_restriction = 24
  ), "LIMITED")
  testthat::expect_error(new_v2_availability_evidence(
    1L, "TEST", "2026-27", "LIMITED", "VERIFIED", "AUTHORITATIVE_FACT",
    "MANUAL_VERIFIED", "source-1", "2026-10-21", "2026-10-20",
    "Basketball Ops", "Bad window", minute_restriction = 24
  ), "window")
})


testthat::test_that("availability ledger never infers from roster fields", {
  roster <- v2_phase1e_roster()
  roster$availability_status <- c("OUT", rep("AVAILABLE", 11L))
  roster$roster_status <- "Active"
  roster$minutes_per_game <- 36
  ledger <- build_v2_availability_evidence_ledger(roster, "TEST", "2026-27")
  testthat::expect_true(all(vapply(
    ledger$records, `[[`, character(1), "availability_status"
  ) == "UNKNOWN"))
  testthat::expect_identical(ledger$status, "REVIEW")
  testthat::expect_identical(
    ledger$input_signature,
    build_v2_availability_evidence_ledger(
      roster[sample(nrow(roster)), ], "TEST", "2026-27"
    )$input_signature
  )
})


testthat::test_that("manual availability validates context duplicates and conflicts", {
  roster <- v2_phase1e_roster()
  good <- v2_phase1e_manual_availability(1L)
  ledger <- build_v2_availability_evidence_ledger(
    roster, "TEST", "2026-27", good, "2026-10-20"
  )
  testthat::expect_identical(v2_availability_record(ledger, 1L)$availability_status, "AVAILABLE")
  testthat::expect_error(build_v2_availability_evidence_ledger(
    roster, "TEST", "2026-27", transform(good, team_id = "OTHER"), "2026-10-20"
  ), "wrong team")
  testthat::expect_error(build_v2_availability_evidence_ledger(
    roster, "TEST", "2026-27", transform(good, season = "2027-28"), "2026-10-20"
  ), "wrong season")
  testthat::expect_error(build_v2_availability_evidence_ledger(
    roster, "TEST", "2026-27", transform(good, player_id = 99L), "2026-10-20"
  ), "outside roster")
  testthat::expect_error(build_v2_availability_evidence_ledger(
    roster, "TEST", "2026-27", rbind(good, good), "2026-10-20"
  ), "duplicate")
  testthat::expect_error(build_v2_availability_evidence_ledger(
    roster, "TEST", "2026-27", rbind(good, transform(good, availability_status = "OUT")),
    "2026-10-20"
  ), "conflicting")
  testthat::expect_error(build_v2_availability_evidence_ledger(
    roster, "TEST", "2026-27", transform(good, verified_by = ""), "2026-10-20"
  ), "verified_by")
  testthat::expect_error(build_v2_availability_evidence_ledger(
    roster, "TEST", "2026-27", good
  ), "as_of_date")
  expired <- build_v2_availability_evidence_ledger(
    roster, "TEST", "2026-27", good, "2026-10-22"
  )
  testthat::expect_identical(
    v2_availability_record(expired, 1L)$availability_status, "UNKNOWN"
  )
  testthat::expect_true("AVAILABILITY_EVIDENCE_OUTSIDE_EFFECTIVE_WINDOW" %in%
    v2_availability_record(expired, 1L)$reason_codes)
})


testthat::test_that("Phase 1E role policy extends vocabulary without promotion", {
  policy <- v2_role_policy()
  testthat::expect_true(all(c(
    "PRIMARY_CREATOR", "SECONDARY_CREATOR", "BALL_HANDLER", "RIM_PROTECTOR",
    "BACKUP_PG", "BACKUP_C", paste0("POSITION_", c("PG", "SG", "SF", "PF", "C"))
  ) %in% policy$supported_roles))
  roster <- v2_phase1e_roster()
  ledger <- build_v2_role_eligibility_ledger(roster, "TEST", "2026-27")
  for (role in c("PRIMARY_CREATOR", "SECONDARY_CREATOR", "BALL_HANDLER", "RIM_PROTECTOR")) {
    record <- v2_role_record(ledger, 1L, role)
    testthat::expect_identical(record$eligibility, "UNKNOWN")
    testthat::expect_identical(record$evidence_class, "MODEL_EVIDENCE")
    testthat::expect_identical(record$verification_status, "UNVERIFIED")
  }
})


testthat::test_that("manual verified Phase 1E roles support positive and negative facts", {
  roster <- v2_phase1e_roster()
  manual <- rbind(
    v2_phase1e_manual_role(1L, "PRIMARY_CREATOR"),
    v2_phase1e_manual_role(2L, "SECONDARY_CREATOR"),
    v2_phase1e_manual_role(3L, "BALL_HANDLER", "NOT_ELIGIBLE"),
    v2_phase1e_manual_role(5L, "RIM_PROTECTOR")
  )
  ledger <- build_v2_role_eligibility_ledger(roster, "TEST", "2026-27", manual)
  testthat::expect_identical(v2_role_record(ledger, 1L, "PRIMARY_CREATOR")$eligibility, "ELIGIBLE")
  testthat::expect_identical(v2_role_record(ledger, 3L, "BALL_HANDLER")$eligibility, "NOT_ELIGIBLE")
  malformed <- manual[1, , drop = FALSE]
  malformed$source_version <- ""
  testthat::expect_error(
    build_v2_role_eligibility_ledger(roster, "TEST", "2026-27", malformed),
    "source_version"
  )
})


testthat::test_that("combined completeness diagnostics remain truthful", {
  roster <- v2_phase1e_roster()
  roles <- rbind(
    v2_phase1e_manual_role(6L, "BACKUP_PG"),
    v2_phase1e_manual_role(10L, "BACKUP_C"),
    v2_phase1e_manual_role(1L, "PRIMARY_CREATOR"),
    v2_phase1e_manual_role(2L, "SECONDARY_CREATOR"),
    v2_phase1e_manual_role(1L, "BALL_HANDLER"),
    v2_phase1e_manual_role(5L, "RIM_PROTECTOR")
  )
  availability <- do.call(rbind, lapply(1:12, v2_phase1e_manual_availability))
  role_ledger <- build_v2_role_eligibility_ledger(roster, "TEST", "2026-27", roles)
  availability_ledger <- build_v2_availability_evidence_ledger(
    roster, "TEST", "2026-27", availability, "2026-10-20"
  )
  diagnostics <- summarize_v2_evidence_completeness(role_ledger, availability_ledger)
  testthat::expect_identical(diagnostics$evidence_type, c(
    "AVAILABILITY", "POSITION_ELIGIBILITY", "BACKUP_PG", "BACKUP_C",
    "PRIMARY_CREATOR", "SECONDARY_CREATOR", "BALL_HANDLER", "RIM_PROTECTOR"
  ))
  testthat::expect_true(all(diagnostics$conflicting_count == 0L))
  testthat::expect_true(all(diagnostics$malformed_count == 0L))
  testthat::expect_identical(
    diagnostics$coverage_status[diagnostics$evidence_type == "AVAILABILITY"], "PASS"
  )
})


testthat::test_that("shadow consumes verified availability and roles only", {
  roster <- v2_phase1e_roster()
  lineup <- stats::setNames(1:5, c("PG", "SG", "SF", "PF", "C"))
  roles <- rbind(
    v2_phase1e_manual_role(6L, "BACKUP_PG"),
    v2_phase1e_manual_role(10L, "BACKUP_C"),
    v2_phase1e_manual_role(1L, "PRIMARY_CREATOR"),
    v2_phase1e_manual_role(2L, "SECONDARY_CREATOR"),
    v2_phase1e_manual_role(5L, "RIM_PROTECTOR")
  )
  availability <- do.call(rbind, lapply(1:12, v2_phase1e_manual_availability))
  result <- run_v2_rotation_shadow(
    "v2_shadow", "TEST", "2026-27", roster, lineup,
    manual_role_evidence = roles,
    manual_availability_evidence = availability,
    availability_as_of_date = "2026-10-20"
  )
  testthat::expect_identical(result$execution_status, "COMPLETED")
  testthat::expect_identical(result$starter_state$status, "PASS")
  testthat::expect_identical(result$rotation_10$status, "PASS")
  testthat::expect_identical(result$rotation_11$status, "PASS")
  testthat::expect_identical(result$availability_diagnostics$coverage_status, "PASS")
  testthat::expect_true(all(result$rotation_10$members$availability_status == "AVAILABLE"))
})


testthat::test_that("availability adverse cases preserve lock authority", {
  roster <- v2_phase1e_roster()
  lineup <- stats::setNames(1:5, c("PG", "SG", "SF", "PF", "C"))
  roles <- rbind(
    v2_phase1e_manual_role(6L, "BACKUP_PG"),
    v2_phase1e_manual_role(10L, "BACKUP_C")
  )
  out_locked <- v2_phase1e_manual_availability(1L, "OUT")
  result <- run_v2_rotation_shadow(
    "v2_shadow", "TEST", "2026-27", roster, lineup,
    manual_role_evidence = roles,
    manual_availability_evidence = out_locked,
    availability_as_of_date = "2026-10-20"
  )
  testthat::expect_identical(result$execution_status, "COMPLETED")
  testthat::expect_identical(result$starter_state$status, "FAIL")
  testthat::expect_true(result$starter_state$is_blocked)
  testthat::expect_true(1L %in% result$starter_state$slots$player_id)

  out_bench <- v2_phase1e_manual_availability(12L, "OUT")
  bench_result <- run_v2_rotation_shadow(
    "v2_shadow", "TEST", "2026-27", roster, lineup,
    manual_role_evidence = roles,
    manual_availability_evidence = out_bench,
    availability_as_of_date = "2026-10-20"
  )
  testthat::expect_false(12L %in% bench_result$rotation_10$members$player_id)

  limited <- v2_phase1e_manual_availability(6L, "LIMITED", minute_restriction = 20)
  limited_result <- run_v2_rotation_shadow(
    "v2_shadow", "TEST", "2026-27", roster, lineup,
    manual_role_evidence = roles,
    manual_availability_evidence = limited,
    availability_as_of_date = "2026-10-20"
  )
  testthat::expect_identical(
    limited_result$availability_ledger$records[[6]]$minute_restriction, 20
  )
})


testthat::test_that("verified negative backup coverage blocks only when complete", {
  roster <- v2_phase1e_roster()
  lineup <- stats::setNames(1:5, c("PG", "SG", "SF", "PF", "C"))
  no_pg <- do.call(rbind, lapply(6:12, function(id) {
    v2_phase1e_manual_role(id, "BACKUP_PG", "NOT_ELIGIBLE")
  }))
  no_c <- do.call(rbind, lapply(6:12, function(id) {
    v2_phase1e_manual_role(id, "BACKUP_C", "NOT_ELIGIBLE")
  }))
  result <- run_v2_rotation_shadow(
    "v2_shadow", "TEST", "2026-27", roster, lineup,
    manual_role_evidence = rbind(no_pg, no_c)
  )
  testthat::expect_identical(result$rotation_10$status, "FAIL")
  testthat::expect_true(result$rotation_10$is_blocked)
  codes <- vapply(result$rotation_10$validation$findings, `[[`, character(1), "code")
  testthat::expect_true("BACKUP_PG_COVERAGE_UNMET" %in% codes)
  testthat::expect_true("BACKUP_C_COVERAGE_UNMET" %in% codes)
})


testthat::test_that("Phase 1E signatures and shadow reruns are deterministic", {
  roster <- v2_phase1e_roster()
  lineup <- stats::setNames(1:5, c("PG", "SG", "SF", "PF", "C"))
  availability <- do.call(rbind, lapply(1:12, v2_phase1e_manual_availability))
  first <- run_v2_rotation_shadow(
    "v2_shadow", "TEST", "2026-27", roster, lineup,
    manual_availability_evidence = availability,
    availability_as_of_date = "2026-10-20"
  )
  second <- run_v2_rotation_shadow(
    "v2_shadow", "TEST", "2026-27", roster, lineup,
    manual_availability_evidence = availability[sample(nrow(availability)), ],
    availability_as_of_date = "2026-10-20"
  )
  testthat::expect_identical(first$availability_ledger, second$availability_ledger)
  testthat::expect_identical(first$role_ledger, second$role_ledger)
  testthat::expect_identical(first$input_signature, second$input_signature)
})
