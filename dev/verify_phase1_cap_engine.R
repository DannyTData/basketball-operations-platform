# Verify Phase 1 salary-cap engine

required_packages <- c("devtools", "testthat", "DBI", "RSQLite")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages)) {
  stop(
    "Install required packages first: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

devtools::load_all(quiet = TRUE)

cat("\nRunning Phase 1 cap-engine tests...\n")
testthat::test_file("tests/testthat/test-cap_engine.R", reporter = "summary")

cat("\nChecking database-backed Boston summary...\n")
summary <- get_team_cap_summary("Boston Celtics", "2026-27")

stopifnot(
  summary$contract_count > 0,
  summary$team_salary > 0,
  nzchar(summary$status)
)

cat("Team salary: ", format(round(summary$team_salary), big.mark = ","), "\n", sep = "")
cat("Operating band: ", summary$status, "\n", sep = "")
cat("Second-apron distance: ", format(round(summary$second_apron_distance), big.mark = ","), "\n", sep = "")
cat("\nPHASE 1 STATUS: PASS\n")
