testthat::test_that("Phase 1D validates all teams without database mutation", {
  path <- resolve_tbi_db_path(file.path("inst", "database", "tbi.sqlite"))
  hash_before <- unname(tools::md5sum(path))
  con <- connect_db(path, read_only = TRUE)
  tables_before <- DBI::dbListTables(con)
  schemas_before <- lapply(tables_before, function(table) DBI::dbListFields(con, table))
  names(schemas_before) <- tables_before
  portland_locks <- DBI::dbGetQuery(
    con,
    "
      SELECT dco.position, dco.player_id
      FROM depth_chart_overrides dco
      WHERE dco.team_id = 25
        AND dco.season = '2026-27'
        AND dco.is_starter = 1
      ORDER BY CASE dco.position
        WHEN 'PG' THEN 1
        WHEN 'SG' THEN 2
        WHEN 'SF' THEN 3
        WHEN 'PF' THEN 4
        WHEN 'C' THEN 5
        ELSE 6
      END
    "
  )
  disconnect_db(con)

  results <- v2_shadow_validate_30_teams("2026-27")
  portland_roster <- v2_role_authoritative_roster_snapshot("POR", "2026-27", path)
  portland_lineup <- v2_shadow_lineup_from_roster(portland_roster)
  portland <- run_v2_rotation_shadow(
    "v2_shadow", "POR", "2026-27", portland_roster, portland_lineup,
    v1_reference = list(approved_lineup = portland_lineup)
  )

  con <- connect_db(path, read_only = TRUE)
  tables_after <- DBI::dbListTables(con)
  schemas_after <- lapply(tables_after, function(table) DBI::dbListFields(con, table))
  names(schemas_after) <- tables_after
  disconnect_db(con)
  hash_after <- unname(tools::md5sum(path))

  testthat::expect_equal(nrow(results), 30L)
  testthat::expect_true(all(results$execution_status == "COMPLETED"))
  testthat::expect_true(all(results$deterministic))
  testthat::expect_true(all(results$starter_status %in% c("PASS", "REVIEW", "FAIL")))
  testthat::expect_equal(sum(results$starter_status == "PASS"), 0L)
  testthat::expect_equal(sum(results$starter_status == "REVIEW"), 30L)
  testthat::expect_equal(sum(results$starter_status == "FAIL"), 0L)
  testthat::expect_equal(sum(results$rotation_10_status == "PASS"), 0L)
  testthat::expect_equal(sum(results$rotation_10_status == "REVIEW"), 30L)
  testthat::expect_equal(sum(results$rotation_10_status == "FAIL"), 0L)
  testthat::expect_equal(sum(results$rotation_11_status == "PASS"), 0L)
  testthat::expect_equal(sum(results$rotation_11_status == "REVIEW"), 30L)
  testthat::expect_equal(sum(results$rotation_11_status == "FAIL"), 0L)
  testthat::expect_true(all(results$verified_backup_pg_count == 0L))
  testthat::expect_true(all(results$verified_backup_c_count == 0L))
  testthat::expect_true(all(results$verified_primary_creator_count == 0L))
  testthat::expect_true(all(results$verified_secondary_creator_count == 0L))
  testthat::expect_true(all(results$verified_ball_handler_count == 0L))
  testthat::expect_true(all(results$verified_rim_protector_count == 0L))
  testthat::expect_true(all(results$verified_availability_count == 0L))
  testthat::expect_true(all(results$unknown_availability_count > 0L))
  testthat::expect_true(all(results$role_coverage_status == "REVIEW"))
  testthat::expect_true(all(results$availability_coverage_status == "REVIEW"))
  testthat::expect_true(all(is.finite(results$evidence_ledger_seconds)))
  testthat::expect_true(all(is.finite(results$shadow_seconds)))
  testthat::expect_true(all(nzchar(results$review_reasons)))
  testthat::expect_true(all(grepl("BACKUP_PG_COVERAGE_UNKNOWN", results$review_reasons)))
  testthat::expect_true(all(grepl("BACKUP_C_COVERAGE_UNKNOWN", results$review_reasons)))
  testthat::expect_true(all(!nzchar(results$fail_reasons)))
  testthat::expect_identical(portland_locks$position, c("PG", "SG", "SF", "PF", "C"))
  testthat::expect_identical(portland_locks$player_id, c(217L, 121L, 139L, 534L, 154L))
  testthat::expect_identical(unname(portland_lineup), c(217L, 121L, 139L, 534L, 154L))
  testthat::expect_identical(portland$starter_state$status, "REVIEW")
  testthat::expect_false(portland$starter_state$is_blocked)
  testthat::expect_identical(portland$rotation_10$status, "REVIEW")
  testthat::expect_false(portland$rotation_10$is_blocked)
  testthat::expect_identical(portland$rotation_11$status, "REVIEW")
  testthat::expect_false(portland$rotation_11$is_blocked)
  portland_review_codes <- unique(vapply(
    Filter(function(x) identical(x$status, "REVIEW"), portland$validation_findings),
    `[[`, character(1), "code"
  ))
  testthat::expect_true(all(c(
    "STARTER_AVAILABILITY_UNKNOWN",
    "SELECTED_AVAILABILITY_UNKNOWN",
    "SELECTED_RANKING_EVIDENCE_INCOMPLETE",
    "SELECTED_ROOKIE_ELIGIBILITY_UNKNOWN",
    "BACKUP_PG_COVERAGE_UNKNOWN",
    "BACKUP_C_COVERAGE_UNKNOWN"
  ) %in% portland_review_codes))
  testthat::expect_false(any(vapply(
    portland$validation_findings,
    function(x) identical(x$status, "FAIL"),
    logical(1)
  )))
  testthat::expect_false(any(vapply(
    portland$validation_findings,
    function(x) identical(x$code, "APPROVED_STARTER_LOCK_CONFLICT"),
    logical(1)
  )))
  testthat::expect_identical(hash_before, hash_after)
  testthat::expect_identical(tables_before, tables_after)
  testthat::expect_identical(schemas_before, schemas_after)

  league <- summarize_v2_role_league(results)
  testthat::expect_identical(league$value[league$metric == "teams"], 30L)
})
