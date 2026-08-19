# ============================================================
# TBI V2 Phase 1B rotation engine
# ============================================================

v2_engine_test_roster <- function(n = 12L) {
  data.frame(
    player_id = seq_len(n),
    player_name = sprintf("Player %02d", seq_len(n)),
    position = rep(c("PG", "SG", "SF", "PF", "C", "G"), length.out = n),
    depth_order = c(rep(1L, 5L), rep(2L, min(5L, n - 5L)), rep(3L, max(0L, n - 10L))),
    availability_status = "AVAILABLE",
    bie_rating = seq(90, 60, length.out = n),
    projected_bie_rating = seq(91, 61, length.out = n),
    impact_score = seq(89, 59, length.out = n),
    primary_role = "",
    archetype = "",
    impact_tier = "",
    is_preseason_rookie = FALSE,
    tbi_performance_available = TRUE,
    stringsAsFactors = FALSE
  )
}


v2_engine_test_starters <- function(roster,
                                    availability = rep("AVAILABLE", 5L)) {
  slots <- data.frame(
    position = c("PG", "SG", "SF", "PF", "C"),
    player_id = roster$player_id[1:5],
    player_name = roster$player_name[1:5],
    lock_status = "LOCKED",
    lock_source = "APPROVED",
    availability_status = availability,
    position_eligibility = "PASS",
    stringsAsFactors = FALSE
  )
  new_v2_starter_state("TEST", "2026-27", slots, v2_input_signature(roster))
}


v2_engine_test_roles <- function(pg_id = 6L,
                                 c_id = 7L,
                                 pg = "ELIGIBLE",
                                 center = "ELIGIBLE") {
  make <- function(player_id, role, eligibility) {
    if (eligibility == "UNKNOWN") {
      return(new_v2_role_eligibility(player_id, role))
    }
    new_v2_role_eligibility(
      player_id,
      role,
      eligibility,
      "VERIFIED",
      "test-role-ledger",
      paste0(tolower(role), "_eligible")
    )
  }
  list(make(pg_id, "BACKUP_PG", pg), make(c_id, "BACKUP_C", center))
}


v2_engine_build <- function(roster = v2_engine_test_roster(),
                            size = 10L,
                            starters = NULL,
                            roles = v2_engine_test_roles(),
                            availability = NULL) {
  if (is.null(starters)) starters <- v2_engine_test_starters(roster)
  build_v2_rotation(
    roster = roster,
    starter_state = starters,
    requested_rotation_size = size,
    role_eligibility = roles,
    availability = availability
  )
}


testthat::test_that("builds exact 10 and 11 player rotations", {
  ten <- v2_engine_build(size = 10L)
  eleven <- v2_engine_build(size = 11L)

  testthat::expect_identical(ten$status, "PASS")
  testthat::expect_identical(ten$actual_rotation_size, 10L)
  testthat::expect_identical(eleven$status, "PASS")
  testthat::expect_identical(eleven$actual_rotation_size, 11L)
  testthat::expect_identical(sum(ten$members$is_starter), 5L)
  testthat::expect_identical(ten$members$player_id[ten$members$is_starter], 1:5)
  testthat::expect_true(all(c(
    "selection_source", "rank_components", "evidence_fields", "missing_fields",
    "reason_codes", "explanation"
  ) %in% names(ten$members)))
})


testthat::test_that("identical inputs return deterministic results and IDs", {
  first <- v2_engine_build()
  second <- v2_engine_build()
  testthat::expect_identical(first, second)

  changed <- v2_engine_test_roster()
  changed$bie_rating[[8]] <- changed$bie_rating[[8]] + 1
  changed_result <- v2_engine_build(changed)
  testthat::expect_false(identical(first$input_signature, changed_result$input_signature))
})


testthat::test_that("ranking ties use deterministic player name order and NAs rank last", {
  roster <- v2_engine_test_roster(10L)
  roster$depth_order[6:8] <- 2L
  roster$bie_rating[6:7] <- 75
  roster$projected_bie_rating[6:7] <- 75
  roster$impact_score[6:7] <- 75
  roster$player_name[6:7] <- c("Beta", "Alpha")
  roster$bie_rating[[8]] <- NA_real_
  roster$projected_bie_rating[[8]] <- NA_real_
  roster$impact_score[[8]] <- NA_real_

  result <- v2_engine_build(roster, roles = v2_engine_test_roles(7L, 9L))
  bench <- result$members[!result$members$is_starter, , drop = FALSE]
  testthat::expect_lt(match(7L, bench$player_id), match(6L, bench$player_id))
  testthat::expect_gt(match(8L, bench$player_id), match(9L, bench$player_id))
  testthat::expect_gt(match(8L, bench$player_id), match(10L, bench$player_id))
  testthat::expect_identical(result$status, "REVIEW")
  testthat::expect_false(result$is_blocked)
})


testthat::test_that("unavailable non-starters are excluded with explanations", {
  roster <- v2_engine_test_roster()
  availability <- data.frame(
    player_id = roster$player_id,
    availability_status = c(rep("AVAILABLE", 5), "OUT", rep("AVAILABLE", 6))
  )
  result <- v2_engine_build(
    roster,
    roles = v2_engine_test_roles(7L, 8L),
    availability = availability
  )

  testthat::expect_false(6L %in% result$members$player_id)
  excluded <- result$excluded_players[result$excluded_players$player_id == 6L, , drop = FALSE]
  testthat::expect_identical(excluded$exclusion_type, "UNAVAILABLE")
  testthat::expect_match(excluded$explanation, "unavailable")
  testthat::expect_true(nzchar(excluded$reason_codes))
})


testthat::test_that("insufficient available players fails without shrinking silently", {
  roster <- v2_engine_test_roster(10L)
  roster$availability_status[8:10] <- "OUT"
  result <- v2_engine_build(roster, size = 10L)

  testthat::expect_identical(result$status, "FAIL")
  testthat::expect_true(result$is_blocked)
  testthat::expect_lt(result$actual_rotation_size, 10L)
  testthat::expect_true(any(vapply(
    result$validation$findings, `[[`, character(1), "code"
  ) == "ROTATION_SIZE_UNSATISFIABLE"))
})


testthat::test_that("blocked starter state cannot produce a sixth man or PASS", {
  roster <- v2_engine_test_roster()
  starters <- v2_engine_test_starters(roster, c("OUT", rep("AVAILABLE", 4)))
  result <- v2_engine_build(roster, starters = starters)

  testthat::expect_identical(result$status, "FAIL")
  testthat::expect_true(result$is_blocked)
  testthat::expect_false(any(result$members$rotation_role == "SIXTH_MAN"))
  testthat::expect_true(1L %in% result$members$player_id)
})


testthat::test_that("designated rookie starter remains selected without performance", {
  roster <- v2_engine_test_roster()
  roster$is_preseason_rookie[[1]] <- TRUE
  roster$tbi_performance_available[[1]] <- FALSE
  result <- v2_engine_build(roster)

  starter <- result$members[result$members$player_id == 1L, , drop = FALSE]
  testthat::expect_true(starter$is_starter)
  testthat::expect_true(is.na(starter$rank_components[[1]]$bie_rating))
  testthat::expect_true("PERFORMANCE_EVIDENCE_MISSING" %in% starter$reason_codes[[1]])
})


testthat::test_that("preseason rookie gate excludes non-starters without guessing grades", {
  roster <- v2_engine_test_roster()
  roster$is_preseason_rookie[[6]] <- TRUE
  roster$tbi_performance_available[[6]] <- FALSE
  result <- v2_engine_build(roster, roles = v2_engine_test_roles(7L, 8L))

  testthat::expect_false(6L %in% result$members$player_id)
  rookie <- result$excluded_players[result$excluded_players$player_id == 6L, , drop = FALSE]
  testthat::expect_identical(rookie$exclusion_type, "ROOKIE_GATE")
  testthat::expect_identical(rookie$reason_codes, "PRESEASON_ROOKIE_GATE")
})


testthat::test_that("valid rotations have one deterministic sixth man", {
  first <- v2_engine_build()
  second <- v2_engine_build()
  sixth <- first$members[first$members$rotation_role == "SIXTH_MAN", , drop = FALSE]

  testthat::expect_identical(nrow(sixth), 1L)
  testthat::expect_false(sixth$is_starter)
  testthat::expect_identical(
    sixth$player_id,
    second$members$player_id[second$members$rotation_role == "SIXTH_MAN"]
  )
  testthat::expect_match(sixth$explanation, "highest-ranked selected bench")
})


testthat::test_that("explicit backup PG and C evidence produces coverage PASS", {
  result <- v2_engine_build()
  testthat::expect_identical(result$status, "PASS")
  testthat::expect_true(all(c(6L, 7L) %in% result$members$player_id))
})


testthat::test_that("a starter cannot satisfy their own backup role", {
  roles <- v2_engine_test_roles(pg_id = 1L, c_id = 7L)
  result <- v2_engine_build(roles = roles)

  testthat::expect_identical(result$status, "REVIEW")
  testthat::expect_false(result$is_blocked)
  testthat::expect_true(any(vapply(
    result$validation$findings,
    function(x) identical(x$code, "BACKUP_PG_COVERAGE_UNKNOWN"), logical(1)
  )))
})


testthat::test_that("unknown backup PG or C evidence produces unblocked REVIEW", {
  pg_unknown <- v2_engine_build(roles = v2_engine_test_roles(pg = "UNKNOWN"))
  c_unknown <- v2_engine_build(roles = v2_engine_test_roles(center = "UNKNOWN"))

  testthat::expect_identical(pg_unknown$status, "REVIEW")
  testthat::expect_false(pg_unknown$is_blocked)
  testthat::expect_identical(c_unknown$status, "REVIEW")
  testthat::expect_false(c_unknown$is_blocked)
})


testthat::test_that("verified absent backup PG or C produces blocking FAIL", {
  no_pg <- v2_engine_build(roles = v2_engine_test_roles(pg = "NOT_ELIGIBLE"))
  no_c <- v2_engine_build(roles = v2_engine_test_roles(center = "NOT_ELIGIBLE"))

  testthat::expect_identical(no_pg$status, "FAIL")
  testthat::expect_true(no_pg$is_blocked)
  testthat::expect_identical(no_c$status, "FAIL")
  testthat::expect_true(no_c$is_blocked)
})


testthat::test_that("role evidence outside selected rotation cannot satisfy coverage", {
  roles <- v2_engine_test_roles(pg_id = 99L, c_id = 7L)
  roster <- v2_engine_test_roster()
  result <- v2_engine_build(roster, roles = roles)

  testthat::expect_false(99L %in% result$members$player_id)
  testthat::expect_identical(result$status, "FAIL")
  testthat::expect_true(result$is_blocked)
})


testthat::test_that("unknown evidence for an unselected player is non-blocking", {
  roles <- c(v2_engine_test_roles(), list(new_v2_role_eligibility(12L, "BACKUP_PG")))
  result <- v2_engine_build(roles = roles)

  testthat::expect_false(result$is_blocked)
  testthat::expect_false(any(vapply(
    result$validation$findings,
    function(x) identical(x$code, "ROLE_ELIGIBILITY_PLAYER_OUTSIDE_ROTATION"), logical(1)
  )))
})


testthat::test_that("duplicate roster IDs fail without duplicate selection", {
  roster <- v2_engine_test_roster()
  roster$player_id[[12]] <- 11L
  result <- v2_engine_build(roster)

  testthat::expect_identical(result$status, "FAIL")
  testthat::expect_true(result$is_blocked)
  testthat::expect_false(anyDuplicated(result$members$player_id) > 0L)
  testthat::expect_true(any(vapply(
    result$excluded_players$reason_codes,
    function(x) "ROSTER_PLAYER_ID_INVALID" %in% x, logical(1)
  )))
})


testthat::test_that("unknown selected availability remains explicit and reviews", {
  roster <- v2_engine_test_roster(10L)
  roster$availability_status[[6]] <- "UNKNOWN"
  result <- v2_engine_build(roster)
  player <- result$members[result$members$player_id == 6L, , drop = FALSE]

  testthat::expect_identical(player$availability_status, "UNKNOWN")
  testthat::expect_identical(result$status, "REVIEW")
  testthat::expect_false(result$is_blocked)
  testthat::expect_true("availability_status" %in% player$missing_fields[[1]])
})


testthat::test_that("excluded lower-ranked players carry complete ledger fields", {
  result <- v2_engine_build(size = 10L)
  testthat::expect_gt(nrow(result$excluded_players), 0L)
  testthat::expect_true(all(c(
    "player_id", "exclusion_type", "reason_codes", "explanation"
  ) %in% names(result$excluded_players)))
  testthat::expect_true(all(nzchar(result$excluded_players$explanation)))
})
