v2_shadow_test_input <- function() {
  roster <- data.frame(
    player_id = 1:12,
    player_name = sprintf("Player %02d", 1:12),
    position = rep(c("PG", "SG", "SF", "PF", "C", "G"), 2L),
    depth_order = c(rep(1L, 5L), rep(2L, 5L), rep(3L, 2L)),
    availability_status = "AVAILABLE",
    bie_rating = seq(90, 60, length.out = 12L),
    projected_bie_rating = seq(91, 61, length.out = 12L),
    impact_score = seq(89, 59, length.out = 12L),
    is_preseason_rookie = FALSE,
    tbi_performance_available = TRUE,
    stringsAsFactors = FALSE
  )
  lineup <- stats::setNames(roster$player_id[1:5], c("PG", "SG", "SF", "PF", "C"))
  list(roster = roster, lineup = lineup)
}


testthat::test_that("rotation routing defaults safely to V1", {
  testthat::expect_identical(tbi_rotation_model(NULL), "v1")
  testthat::expect_identical(tbi_rotation_model(""), "v1")
  invalid <- tbi_rotation_route("v2")
  testthat::expect_identical(invalid$model, "v1")
  testthat::expect_identical(invalid$status, "REVIEW")
  testthat::expect_match(invalid$diagnostic, "Unsupported")
  testthat::expect_identical(tbi_rotation_model("v2_shadow"), "v2_shadow")
})


testthat::test_that("V1 routing never executes the V2 shadow engine", {
  called <- 0L
  engine <- function(...) {
    called <<- called + 1L
    stop("must not execute")
  }
  input <- v2_shadow_test_input()
  v1 <- list(id = "v1-reference", rendered = input$lineup)
  result <- run_v2_rotation_shadow(
    "v1", "TEST", "2026-27", input$roster, input$lineup,
    v1_reference = v1, rotation_builder = engine
  )

  testthat::expect_identical(called, 0L)
  testthat::expect_identical(result$execution_status, "DISABLED")
  testthat::expect_identical(v1$rendered, input$lineup)
})


testthat::test_that("shadow routing builds deterministic comparison-only state", {
  input <- v2_shadow_test_input()
  roster_before <- input$roster
  lineup_before <- input$lineup
  first <- run_v2_rotation_shadow(
    "v2_shadow", "TEST", "2026-27", input$roster, input$lineup,
    v1_reference = list(id = "v1-reference")
  )
  second <- run_v2_rotation_shadow(
    "v2_shadow", "TEST", "2026-27", input$roster, input$lineup,
    v1_reference = list(id = "v1-reference")
  )

  testthat::expect_identical(first$execution_status, "COMPLETED")
  testthat::expect_identical(first$starter_state, second$starter_state)
  testthat::expect_identical(first$rotation_10, second$rotation_10)
  testthat::expect_identical(first$rotation_11, second$rotation_11)
  testthat::expect_identical(first$input_signature, second$input_signature)
  testthat::expect_identical(input$roster, roster_before)
  testthat::expect_identical(input$lineup, lineup_before)
  testthat::expect_identical(first$rotation_10$status, "REVIEW")
  testthat::expect_false(first$rotation_10$is_blocked)

  changed_lineup <- input$lineup
  changed_lineup[["PG"]] <- 6L
  changed <- run_v2_rotation_shadow(
    "v2_shadow", "TEST", "2026-27", input$roster, changed_lineup
  )
  testthat::expect_false(identical(first$input_signature, changed$input_signature))
})


testthat::test_that("shadow exceptions are isolated as diagnostics", {
  input <- v2_shadow_test_input()
  v1 <- list(id = "v1-reference", rendered = input$lineup)
  result <- run_v2_rotation_shadow(
    "v2_shadow", "TEST", "2026-27", input$roster, input$lineup,
    v1_reference = v1,
    rotation_builder = function(...) stop("shadow exploded")
  )

  testthat::expect_identical(result$execution_status, "ERROR")
  testthat::expect_match(result$error$message, "shadow exploded")
  testthat::expect_identical(result$v1_reference, v1)
  testthat::expect_null(result$rotation_10)
  testthat::expect_null(result$rotation_11)
})


testthat::test_that("malformed role evidence remains a shadow validation failure", {
  input <- v2_shadow_test_input()
  malformed <- list(list(
    contract_type = "tbi-v2-role-eligibility",
    player_id = 6L,
    role = "BACKUP_PG",
    eligibility = "ELIGIBLE"
  ))
  result <- run_v2_rotation_shadow(
    "v2_shadow", "TEST", "2026-27", input$roster, input$lineup,
    role_eligibility = malformed
  )

  testthat::expect_identical(result$execution_status, "COMPLETED")
  testthat::expect_identical(result$rotation_10$status, "FAIL")
  testthat::expect_true(result$rotation_10$is_blocked)
  testthat::expect_true(any(vapply(
    result$rotation_10$validation$findings,
    function(x) identical(x$code, "ROLE_ELIGIBILITY_CONTRACT_INVALID"), logical(1)
  )))
})


testthat::test_that("shadow starter adapter preserves approved lineup and unknown roles", {
  input <- v2_shadow_test_input()
  starter <- v2_shadow_starter_state(
    "TEST", "2026-27", input$roster, input$lineup
  )
  roles <- v2_shadow_unknown_role_eligibility(input$roster)

  testthat::expect_identical(starter$slots$player_id, unname(input$lineup))
  testthat::expect_true(all(starter$slots$lock_status == "LOCKED"))
  testthat::expect_true(all(starter$slots$lock_source == "APPROVED"))
  testthat::expect_true(all(starter$slots$position_eligibility == "REVIEW"))
  testthat::expect_true(all(vapply(roles, `[[`, character(1), "eligibility") == "UNKNOWN"))
  testthat::expect_length(roles, nrow(input$roster) * 2L)
})


testthat::test_that("missing rookie evidence stays unknown and decision-relevant", {
  input <- v2_shadow_test_input()
  input$roster$is_preseason_rookie <- NULL
  result <- run_v2_rotation_shadow(
    "v2_shadow", "TEST", "2026-27", input$roster, input$lineup
  )

  testthat::expect_true(all(is.na(v2_shadow_roster(input$roster)$is_preseason_rookie)))
  testthat::expect_identical(result$rotation_10$status, "REVIEW")
  testthat::expect_true(any(vapply(
    result$rotation_10$validation$findings,
    function(x) identical(x$code, "SELECTED_ROOKIE_ELIGIBILITY_UNKNOWN"), logical(1)
  )))
})
