# ============================================================
# Thompson's Basketball Intelligence
# Phase 5A: Draft Assets Engine Tests
# ============================================================

create_draft_test_database <- function() {
  path <- tempfile(
    pattern = "tbi_draft_assets_",
    fileext = ".sqlite"
  )
  
  con <- DBI::dbConnect(
    RSQLite::SQLite(),
    path
  )
  
  DBI::dbExecute(
    con,
    "PRAGMA foreign_keys = ON;"
  )
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE teams (
      team_id INTEGER PRIMARY KEY,
      team_name TEXT NOT NULL UNIQUE,
      abbreviation TEXT NOT NULL UNIQUE
    );
    "
  )
  
  DBI::dbExecute(
    con,
    "
    INSERT INTO teams (
      team_id,
      team_name,
      abbreviation
    )
    VALUES
      (1, 'Boston Celtics', 'BOS'),
      (2, 'Brooklyn Nets', 'BKN'),
      (3, 'New York Knicks', 'NYK'),
      (4, 'Philadelphia 76ers', 'PHI');
    "
  )
  
  DBI::dbDisconnect(con)
  
  path
}


testthat::test_that("draft helpers normalize values safely", {
  testthat::expect_equal(
    draft_text("  Boston Celtics  "),
    "Boston Celtics"
  )
  
  testthat::expect_equal(
    draft_integer("2028"),
    2028L
  )
  
  testthat::expect_equal(
    draft_number("15.5"),
    15.5
  )
  
  testthat::expect_true(
    draft_flag("yes")
  )
  
  testthat::expect_false(
    draft_flag("no")
  )
})


testthat::test_that("controlled vocabularies normalize correctly", {
  testthat::expect_equal(
    normalize_draft_control_type("owned"),
    "Own"
  )
  
  testthat::expect_equal(
    normalize_draft_control_type("swap rights"),
    "Swap Right"
  )
  
  testthat::expect_equal(
    normalize_draft_round("1st"),
    "First"
  )
  
  testthat::expect_equal(
    normalize_draft_round("second round"),
    "Second"
  )
  
  testthat::expect_equal(
    normalize_draft_value_tier("high"),
    "High"
  )
  
  testthat::expect_equal(
    normalize_draft_verification_status("needs review"),
    "Needs Review"
  )
  
  testthat::expect_error(
    normalize_draft_control_type("Unknown"),
    "Unsupported control_type"
  )
  
  testthat::expect_error(
    normalize_draft_round("Third"),
    "Unsupported round"
  )
})


testthat::test_that("protection language is classified conservatively", {
  unprotected <- classify_draft_protection(
    "Unprotected"
  )
  
  testthat::expect_equal(
    unprotected$protection_type,
    "Unprotected"
  )
  
  testthat::expect_false(
    unprotected$requires_manual_review
  )
  
  lottery <- classify_draft_protection(
    "Lottery protected"
  )
  
  testthat::expect_equal(
    lottery$protection_type,
    "Lottery Protected"
  )
  
  testthat::expect_equal(
    lottery$protected_through_pick,
    14L
  )
  
  top_ten <- classify_draft_protection(
    "Top-10 protected"
  )
  
  testthat::expect_equal(
    top_ten$protection_type,
    "Top-N Protected"
  )
  
  testthat::expect_equal(
    top_ten$protected_through_pick,
    10L
  )
  
  best_of <- classify_draft_protection(
    "Best of Boston or Brooklyn"
  )
  
  testthat::expect_equal(
    best_of$protection_type,
    "Best Of"
  )
  
  testthat::expect_true(
    best_of$requires_manual_review
  )
  
  unknown <- classify_draft_protection(
    "TBD"
  )
  
  testthat::expect_equal(
    unknown$protection_type,
    "Unspecified"
  )
  
  testthat::expect_true(
    unknown$requires_manual_review
  )
})


testthat::test_that("draft asset validation requires core fields", {
  incomplete <- list(
    draft_year = 2028,
    round = "First"
  )
  
  testthat::expect_error(
    validate_draft_asset(incomplete),
    "missing required field"
  )
})


testthat::test_that("draft asset validation normalizes a valid asset", {
  asset <- list(
    draft_year = 2028,
    round = "1st",
    original_team = "Brooklyn Nets",
    current_team = "Boston Celtics",
    control_type = "incoming",
    counterparty = "Brooklyn Nets",
    protection_text = "Top-10 protected",
    strategic_value = "high",
    internal_value = 92,
    source_name = "Official transaction memo",
    verification_status = "verified",
    is_active = TRUE
  )
  
  result <- validate_draft_asset(asset)
  
  testthat::expect_equal(
    result$draft_year,
    2028L
  )
  
  testthat::expect_equal(
    result$round,
    "First"
  )
  
  testthat::expect_equal(
    result$control_type,
    "Incoming"
  )
  
  testthat::expect_equal(
    result$protection_type,
    "Top-N Protected"
  )
  
  testthat::expect_equal(
    result$protected_through_pick,
    10L
  )
  
  testthat::expect_equal(
    result$strategic_value,
    "High"
  )
  
  testthat::expect_equal(
    result$verification_status,
    "Verified"
  )
  
  testthat::expect_false(
    result$requires_manual_review
  )
})


testthat::test_that("draft asset validation catches invalid values", {
  bad_year <- list(
    draft_year = 1900,
    round = "First",
    original_team = "Boston Celtics",
    current_team = "Boston Celtics",
    control_type = "Own"
  )
  
  testthat::expect_error(
    validate_draft_asset(bad_year),
    "between 2000 and 2100"
  )
  
  bad_value <- list(
    draft_year = 2028,
    round = "First",
    original_team = "Boston Celtics",
    current_team = "Boston Celtics",
    control_type = "Own",
    internal_value = -1
  )
  
  testthat::expect_error(
    validate_draft_asset(bad_value),
    "cannot be negative"
  )
  
  bad_window <- list(
    draft_year = 2028,
    round = "First",
    original_team = "Boston Celtics",
    current_team = "Boston Celtics",
    control_type = "Own",
    conveyance_start_year = 2030,
    conveyance_end_year = 2029
  )
  
  testthat::expect_error(
    validate_draft_asset(bad_window),
    "cannot precede"
  )
})


testthat::test_that("draft tables and indexes are created", {
  path <- create_draft_test_database()
  
  con <- DBI::dbConnect(
    RSQLite::SQLite(),
    path
  )
  
  on.exit({
    DBI::dbDisconnect(con)
    unlink(path)
  }, add = TRUE)
  
  ensure_draft_asset_tables(con)
  
  tables <- DBI::dbListTables(con)
  
  testthat::expect_true(
    all(
      c(
        "draft_assets",
        "draft_asset_conditions",
        "draft_asset_audit"
      ) %in% tables
    )
  )
  
  indexes <- DBI::dbGetQuery(
    con,
    "
    SELECT name
    FROM sqlite_master
    WHERE type = 'index';
    "
  )$name
  
  testthat::expect_true(
    "idx_draft_assets_current_team_year" %in% indexes
  )
  
  testthat::expect_true(
    "idx_draft_conditions_asset" %in% indexes
  )
})


testthat::test_that("team resolution accepts name abbreviation and id", {
  path <- create_draft_test_database()
  
  con <- DBI::dbConnect(
    RSQLite::SQLite(),
    path
  )
  
  on.exit({
    DBI::dbDisconnect(con)
    unlink(path)
  }, add = TRUE)
  
  by_name <- resolve_draft_team(
    con,
    "Boston Celtics"
  )
  
  by_abbreviation <- resolve_draft_team(
    con,
    "BOS"
  )
  
  by_id <- resolve_draft_team(
    con,
    "1"
  )
  
  testthat::expect_equal(
    by_name$team_id[[1]],
    1
  )
  
  testthat::expect_equal(
    by_abbreviation$team_id[[1]],
    1
  )
  
  testthat::expect_equal(
    by_id$team_id[[1]],
    1
  )
  
  testthat::expect_error(
    resolve_draft_team(
      con,
      "Seattle SuperSonics"
    ),
    "Team not found"
  )
})


testthat::test_that("draft asset CRUD persists records and audit history", {
  path <- create_draft_test_database()
  
  asset <- list(
    draft_year = 2028,
    round = "First",
    original_team = "Brooklyn Nets",
    current_team = "Boston Celtics",
    control_type = "Incoming",
    counterparty = "Brooklyn Nets",
    protection_text = "Top-10 protected",
    strategic_value = "High",
    internal_value = 90,
    source_name = "Official memo",
    verification_status = "Verified",
    notes = "Initial test asset",
    is_active = TRUE
  )
  
  draft_asset_id <- save_draft_asset(
    asset = asset,
    db_path = path,
    changed_by = "Test User"
  )
  
  testthat::expect_true(
    is.integer(draft_asset_id)
  )
  
  loaded <- get_draft_assets(
    team_value = "Boston Celtics",
    db_path = path
  )
  
  testthat::expect_equal(
    nrow(loaded),
    1
  )
  
  testthat::expect_equal(
    loaded$draft_asset_id[[1]],
    draft_asset_id
  )
  
  testthat::expect_equal(
    loaded$control_type[[1]],
    "Incoming"
  )
  
  testthat::expect_equal(
    loaded$protection_type[[1]],
    "Top-N Protected"
  )
  
  updated_asset <- asset
  updated_asset$strategic_value <- "Medium"
  updated_asset$notes <- "Updated test asset"
  
  update_draft_asset(
    draft_asset_id = draft_asset_id,
    asset = updated_asset,
    db_path = path,
    changed_by = "Test User"
  )
  
  detail <- get_draft_asset_detail(
    draft_asset_id = draft_asset_id,
    db_path = path
  )
  
  testthat::expect_equal(
    detail$asset$strategic_value[[1]],
    "Medium"
  )
  
  testthat::expect_true(
    all(
      c("CREATE", "UPDATE") %in%
        detail$audit$action
    )
  )
  
  archive_draft_asset(
    draft_asset_id = draft_asset_id,
    db_path = path,
    changed_by = "Test User"
  )
  
  active <- get_draft_assets(
    team_value = "Boston Celtics",
    include_inactive = FALSE,
    db_path = path
  )
  
  all_records <- get_draft_assets(
    team_value = "Boston Celtics",
    include_inactive = TRUE,
    db_path = path
  )
  
  testthat::expect_equal(
    nrow(active),
    0
  )
  
  testthat::expect_equal(
    nrow(all_records),
    1
  )
  
  testthat::expect_equal(
    all_records$is_active[[1]],
    0
  )
  
  unlink(path)
})


testthat::test_that("draft conditions save and update by order", {
  path <- create_draft_test_database()
  
  asset_id <- save_draft_asset(
    asset = list(
      draft_year = 2029,
      round = "First",
      original_team = "Brooklyn Nets",
      current_team = "Boston Celtics",
      control_type = "Incoming",
      protection_text = "Top-8 protected",
      verification_status = "Verified"
    ),
    db_path = path
  )
  
  save_draft_asset_condition(
    draft_asset_id = asset_id,
    condition_order = 1,
    condition_year = 2029,
    condition_text = "Top-8 protected in 2029",
    outcome_if_not_conveyed =
      "Rolls to 2030 with lighter protection",
    converts_to_year = 2030,
    converts_to_round = "First",
    db_path = path
  )
  
  detail <- get_draft_asset_detail(
    draft_asset_id = asset_id,
    db_path = path
  )
  
  testthat::expect_equal(
    nrow(detail$conditions),
    1
  )
  
  testthat::expect_equal(
    detail$conditions$converts_to_year[[1]],
    2030
  )
  
  save_draft_asset_condition(
    draft_asset_id = asset_id,
    condition_order = 1,
    condition_year = 2029,
    condition_text = "Updated top-8 condition",
    outcome_if_not_conveyed =
      "Converts to two seconds",
    converts_to_year = 2030,
    converts_to_round = "Second",
    is_final_condition = TRUE,
    db_path = path
  )
  
  updated <- get_draft_asset_detail(
    draft_asset_id = asset_id,
    db_path = path
  )
  
  testthat::expect_equal(
    nrow(updated$conditions),
    1
  )
  
  testthat::expect_equal(
    updated$conditions$converts_to_round[[1]],
    "Second"
  )
  
  testthat::expect_equal(
    updated$conditions$is_final_condition[[1]],
    1
  )
  
  unlink(path)
})


testthat::test_that("team retrieval includes outgoing obligations", {
  path <- create_draft_test_database()
  
  save_draft_asset(
    asset = list(
      draft_year = 2028,
      round = "First",
      original_team = "Boston Celtics",
      current_team = "Brooklyn Nets",
      control_type = "Outgoing",
      counterparty = "Brooklyn Nets",
      protection_text = "Unprotected",
      verification_status = "Verified"
    ),
    db_path = path
  )
  
  boston <- get_draft_assets(
    team_value = "Boston Celtics",
    db_path = path
  )
  
  brooklyn <- get_draft_assets(
    team_value = "Brooklyn Nets",
    db_path = path
  )
  
  testthat::expect_equal(
    nrow(boston),
    1
  )
  
  testthat::expect_equal(
    boston$control_type[[1]],
    "Outgoing"
  )
  
  testthat::expect_equal(
    nrow(brooklyn),
    1
  )
  
  unlink(path)
})


testthat::test_that("portfolio summary handles an empty portfolio", {
  summary <- summarize_draft_assets(
    data.frame()
  )
  
  testthat::expect_equal(
    summary$total_assets,
    0L
  )
  
  testthat::expect_equal(
    summary$portfolio_status,
    "No Assets Loaded"
  )
})


testthat::test_that("portfolio summary calculates control and obligations", {
  assets <- data.frame(
    draft_year = c(
      2027,
      2028,
      2029,
      2030,
      2031
    ),
    round = c(
      "First",
      "First",
      "Second",
      "First",
      "First"
    ),
    control_type = c(
      "Own",
      "Incoming",
      "Own",
      "Outgoing",
      "Swap Right"
    ),
    strategic_value = c(
      "High",
      "High",
      "Medium",
      "Low",
      "Medium"
    ),
    verification_status = c(
      "Verified",
      "Verified",
      "Needs Review",
      "Verified",
      "Verified"
    ),
    requires_manual_review = c(
      FALSE,
      FALSE,
      TRUE,
      FALSE,
      FALSE
    ),
    stringsAsFactors = FALSE
  )
  
  summary <- summarize_draft_assets(
    assets
  )
  
  testthat::expect_equal(
    summary$total_assets,
    5
  )
  
  testthat::expect_equal(
    summary$controlled_first_round,
    3
  )
  
  testthat::expect_equal(
    summary$controlled_second_round,
    1
  )
  
  testthat::expect_equal(
    summary$outgoing_obligations,
    1
  )
  
  testthat::expect_equal(
    summary$swap_rights,
    1
  )
  
  testthat::expect_equal(
    summary$high_value_assets,
    2
  )
  
  testthat::expect_equal(
    summary$review_required,
    1
  )
  
  testthat::expect_equal(
    summary$portfolio_status,
    "Needs Verification"
  )
  
  testthat::expect_match(
    summary$executive_summary,
    "3 first-round assets"
  )
})


testthat::test_that("database-backed portfolio evaluation returns assets and summary", {
  path <- create_draft_test_database()
  
  save_draft_asset(
    asset = list(
      draft_year = 2028,
      round = "First",
      original_team = "Boston Celtics",
      current_team = "Boston Celtics",
      control_type = "Own",
      protection_text = "Unprotected",
      strategic_value = "High",
      verification_status = "Verified"
    ),
    db_path = path
  )
  
  result <- evaluate_draft_asset_portfolio(
    team_value = "Boston Celtics",
    db_path = path
  )
  
  testthat::expect_equal(
    nrow(result$assets),
    1
  )
  
  testthat::expect_equal(
    result$summary$controlled_first_round,
    1
  )
  
  testthat::expect_match(
    result$scope_note,
    "Official transaction documents"
  )
  
  unlink(path)
})