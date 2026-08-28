stagger_minute_fixture <- function(size = 10L, unknown = FALSE) {
  ids <- seq_len(size)
  assigned <- if (size == 10L) {
    c(rep(32L, 5), rep(16L, 5))
  } else {
    c(rep(32L, 5), 14L, 14L, 14L, 11L, 16L, 11L)
  }
  ledger <- data.frame(
    player_id = ids,
    player_name = paste("Player", ids),
    is_starter = c(rep(TRUE, 5), rep(FALSE, size - 5L)),
    rotation_role = c(rep("STARTER", 5), "SIXTH_MAN", rep("ROTATION", size - 6L)),
    availability_status = if (unknown) c("UNKNOWN", rep("AVAILABLE", size - 1L)) else "AVAILABLE",
    minimum_minutes = 0L,
    maximum_minutes = 48L,
    target_minutes = assigned,
    assigned_minutes = assigned,
    allocation_source = "TEST",
    override_source = NA_character_,
    explanation = "fixture",
    stringsAsFactors = FALSE
  )
  ledger$reason_codes <- I(lapply(ids, function(id) if (unknown && id == 1L) "AVAILABILITY_UNKNOWN" else "AVAILABILITY_AVAILABLE"))
  list(
    contract_type = "tbi-v2-minute-ledger",
    contract_version = "1.0.0",
    model_version = "2.0.0-phase2a",
    ledger = ledger,
    total_assigned_minutes = 240L,
    status = if (unknown) "REVIEW" else "PASS",
    is_blocked = FALSE,
    input_signature = v2_input_signature(ledger)
  )
}

stagger_role_fixture <- function(size = 10L, verified = FALSE) {
  roles <- c("PRIMARY_CREATOR", "BALL_HANDLER", "POSITION_C", "BACKUP_C")
  records <- unlist(lapply(seq_len(size), function(id) lapply(roles, function(role) {
    eligible <- verified && ((role %in% c("PRIMARY_CREATOR", "BALL_HANDLER") && id %in% c(1L, 6L)) ||
      (role %in% c("POSITION_C", "BACKUP_C") && id %in% c(5L, 10L)))
    list(
      player_id = id,
      role = role,
      eligibility = if (eligible) "ELIGIBLE" else if (verified) "NOT_ELIGIBLE" else "UNKNOWN",
      verification_status = if (verified) "VERIFIED" else "MISSING"
    )
  })), recursive = FALSE)
  list(
    contract_type = "tbi-v2-role-eligibility-ledger",
    contract_version = "1.0.0",
    records = records,
    input_signature = v2_input_signature(records)
  )
}

stagger_position_fixture <- function(size = 10L, eligibility = "ELIGIBLE") {
  base <- stagger_role_fixture(size, verified = TRUE)
  base$records <- Filter(function(record) {
    !identical(record$role, "POSITION_C")
  }, base$records)
  position_records <- unlist(lapply(seq_len(size), function(id) {
    lapply(c("PG", "SG", "SF", "PF", "C"), function(position) {
      position_eligibility <- if (
        identical(eligibility, "ELIGIBLE") &&
          identical(position, "C") &&
          !id %in% c(5L, 10L)
      ) {
        "NOT_ELIGIBLE"
      } else {
        eligibility
      }
      new_v2_role_eligibility(
        player_id = id,
        role = paste0("POSITION_", position),
        eligibility = position_eligibility,
        evidence_status = "VERIFIED",
        evidence_source = "TEST_VERIFIED_POSITION_LEDGER",
        evidence_fields = "fixture.position_eligibility",
        evidence_class = "AUTHORITATIVE_FACT",
        verification_status = "VERIFIED",
        reason_codes = paste0("TEST_POSITION_", position_eligibility),
        explanation = "Focused staggering position-legality fixture."
      )
    })
  }), recursive = FALSE)
  base$records <- c(base$records, position_records)
  base$input_signature <- v2_input_signature(base$records)
  base
}

stagger_exposure <- function(plan) {
  ids <- sort(unique(unlist(plan$segments$player_ids, use.names = FALSE)))
  setNames(vapply(ids, function(id) {
    sum(plan$segments$duration[vapply(plan$segments$player_ids, function(x) id %in% x, logical(1))])
  }, integer(1)), ids)
}

testthat::test_that("Phase 2B reconciles legal deterministic 10- and 11-player plans", {
  for (size in c(10L, 11L)) {
    minutes <- stagger_minute_fixture(size)
    roles <- stagger_role_fixture(size)
    first <- build_v2_stagger_plan(minutes, roles)
    second <- build_v2_stagger_plan(minutes, roles)

    testthat::expect_identical(first$contract_type, "tbi-v2-stagger-plan")
    testthat::expect_identical(first$contract_version, "1.0.0")
    testthat::expect_identical(first$model_version, "2.0.0-phase2b")
    testthat::expect_equal(sum(first$segments$duration) * 5L, 240L)
    testthat::expect_true(all(lengths(first$segments$player_ids) == 5L))
    testthat::expect_true(all(vapply(first$segments$player_ids, function(x) !anyDuplicated(x), logical(1))))
    testthat::expect_equal(unname(stagger_exposure(first)), minutes$ledger$assigned_minutes)
    testthat::expect_identical(first$segments, second$segments)
    testthat::expect_identical(first$substitution_events, second$substitution_events)
    testthat::expect_identical(first$input_signature, second$input_signature)
  }
})

testthat::test_that("Phase 2B reports unknown role coverage without inference", {
  plan <- build_v2_stagger_plan(stagger_minute_fixture(), stagger_role_fixture())
  testthat::expect_identical(plan$status, "REVIEW")
  testthat::expect_true(all(plan$segments$creator_coverage_status == "REVIEW"))
  testthat::expect_true(all(plan$segments$ball_handler_coverage_status == "REVIEW"))
  testthat::expect_true(all(plan$segments$big_center_coverage_status == "REVIEW"))
  testthat::expect_true(all(c("CREATOR_COVERAGE_UNKNOWN", "BALL_HANDLER_COVERAGE_UNKNOWN", "BIG_CENTER_COVERAGE_UNKNOWN") %in%
    unique(unlist(plan$segments$reason_codes, use.names = FALSE))))
})

testthat::test_that("Phase 2B uses verified role evidence and creates coherent bridges", {
  plan <- build_v2_stagger_plan(stagger_minute_fixture(), stagger_role_fixture(verified = TRUE))
  testthat::expect_true(all(plan$segments$creator_coverage_status == "PASS"))
  testthat::expect_true(all(plan$segments$ball_handler_coverage_status == "PASS"))
  testthat::expect_true(all(plan$segments$big_center_coverage_status == "PASS"))
  testthat::expect_true(any(plan$segments$starter_count > 0L & plan$segments$bench_count > 0L))
  testthat::expect_false(any(plan$segments$starter_count == 0L))
  testthat::expect_true(any(plan$segments$bench_count >= 2L))
  testthat::expect_true(all(vapply(plan$substitution_events$resulting_five, function(x) length(x) == 5L && !anyDuplicated(x), logical(1))))
})

testthat::test_that("Phase 2B proves every segment has a legal five-position assignment", {
  for (size in c(10L, 11L)) {
    minutes <- stagger_minute_fixture(size)
    roles <- stagger_position_fixture(size)
    plan <- build_v2_stagger_plan(minutes, roles)

    if (size == 10L) {
      testthat::expect_identical(plan$status, "PASS")
      testthat::expect_false(plan$is_blocked)
    }
    testthat::expect_true(all(plan$segments$position_legality_status == "PASS"))
    testthat::expect_true(all(vapply(seq_len(nrow(plan$segments)), function(i) {
      assignment <- plan$segments$assigned_positions[[i]]
      identical(sort(unname(assignment)), sort(c("PG", "SG", "SF", "PF", "C"))) &&
        identical(names(assignment), as.character(plan$segments$player_ids[[i]]))
    }, logical(1))))
    testthat::expect_equal(sum(plan$segments$duration) * 5L, 240L)
    testthat::expect_equal(unname(stagger_exposure(plan)), minutes$ledger$assigned_minutes)
  }
})

testthat::test_that("Phase 2B reviews unproven segment position legality without changing staggering math", {
  for (size in c(10L, 11L)) {
    minutes <- stagger_minute_fixture(size)
    unknown <- build_v2_stagger_plan(minutes, stagger_role_fixture(size, verified = TRUE))
    proven <- build_v2_stagger_plan(minutes, stagger_position_fixture(size))

    if (size == 10L) {
      testthat::expect_identical(unknown$status, "REVIEW")
      testthat::expect_false(unknown$is_blocked)
    }
    testthat::expect_true(all(unknown$segments$position_legality_status == "REVIEW"))
    testthat::expect_true(all(vapply(unknown$segments$assigned_positions, function(x) all(is.na(x)), logical(1))))
    testthat::expect_true("SEGMENT_POSITION_ASSIGNMENT_UNKNOWN" %in%
      vapply(unknown$validation$findings, `[[`, character(1), "code"))
    testthat::expect_identical(unknown$segments$player_ids, proven$segments$player_ids)
    testthat::expect_identical(unknown$segments$duration, proven$segments$duration)
    testthat::expect_equal(unname(stagger_exposure(unknown)), minutes$ledger$assigned_minutes)
  }
})

testthat::test_that("Phase 2B blocks a known-impossible segment position assignment", {
  for (size in c(10L, 11L)) {
    plan <- build_v2_stagger_plan(
      stagger_minute_fixture(size),
      stagger_position_fixture(size, eligibility = "NOT_ELIGIBLE")
    )

    testthat::expect_identical(plan$status, "FAIL")
    testthat::expect_true(plan$is_blocked)
    testthat::expect_true(all(plan$segments$position_legality_status == "FAIL"))
    testthat::expect_true(all(vapply(plan$segments$assigned_positions, function(x) all(is.na(x)), logical(1))))
    findings <- plan$validation$findings
    index <- match(
      "SEGMENT_POSITION_ASSIGNMENT_IMPOSSIBLE",
      vapply(findings, `[[`, character(1), "code")
    )
    testthat::expect_false(is.na(index))
    testthat::expect_true(findings[[index]]$is_blocking)
    testthat::expect_equal(sum(plan$segments$duration) * 5L, 240L)
  }
})

testthat::test_that("Phase 2B preserves availability limits already encoded by Phase 2A", {
  minutes <- stagger_minute_fixture()
  minutes$ledger$assigned_minutes[[10]] <- 0L
  minutes$ledger$assigned_minutes[[9]] <- 32L
  minutes$ledger$reason_codes[[10]] <- "AVAILABILITY_OUT"
  minutes$ledger$availability_status[[10]] <- "OUT"
  minutes$total_assigned_minutes <- 240L
  out <- build_v2_stagger_plan(minutes, stagger_role_fixture())
  testthat::expect_false(any(vapply(out$segments$player_ids, function(x) 10L %in% x, logical(1))))

  limited <- stagger_minute_fixture()
  limited$ledger$assigned_minutes[[1]] <- 20L
  limited$ledger$assigned_minutes[[6]] <- 28L
  limited$ledger$maximum_minutes[[1]] <- 20L
  limited$ledger$reason_codes[[1]] <- "AVAILABILITY_LIMITED"
  limited$ledger$availability_status[[1]] <- "LIMITED"
  limited_plan <- build_v2_stagger_plan(limited, stagger_role_fixture())
  testthat::expect_equal(stagger_exposure(limited_plan)[["1"]], 20L)
})

testthat::test_that("Phase 2B rejects malformed or infeasible minute ledgers", {
  malformed <- stagger_minute_fixture()
  malformed$ledger$assigned_minutes[[1]] <- 31L
  testthat::expect_error(build_v2_stagger_plan(malformed, stagger_role_fixture()), "240")

  duplicate <- stagger_minute_fixture()
  duplicate$ledger$player_id[[10]] <- duplicate$ledger$player_id[[9]]
  testthat::expect_error(build_v2_stagger_plan(duplicate, stagger_role_fixture()), "unique")

  unavailable <- stagger_minute_fixture()
  unavailable$ledger$reason_codes[[10]] <- "AVAILABILITY_OUT"
  unavailable$ledger$availability_status[[10]] <- "OUT"
  testthat::expect_error(build_v2_stagger_plan(unavailable, stagger_role_fixture()), "OUT")

  too_many <- stagger_minute_fixture()
  too_many$ledger$assigned_minutes <- c(49L, 47L, 32L, 32L, 32L, 16L, 8L, 8L, 8L, 8L)
  testthat::expect_error(build_v2_stagger_plan(too_many, stagger_role_fixture()), "regulation")
})

testthat::test_that("Phase 2B signatures respond to relevant input changes", {
  base <- stagger_minute_fixture()
  roles <- stagger_role_fixture()
  first <- build_v2_stagger_plan(base, roles)
  changed <- base
  changed$ledger$assigned_minutes[c(1L, 6L)] <- changed$ledger$assigned_minutes[c(1L, 6L)] + c(-1L, 1L)
  changed$input_signature <- v2_input_signature(changed$ledger)
  second <- build_v2_stagger_plan(changed, roles)
  testthat::expect_false(identical(first$input_signature, second$input_signature))
})
