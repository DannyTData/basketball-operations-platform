draft_stabilization_fixture <- function() {
  assets <- data.frame(
    draft_asset_id = 1:6,
    draft_year = c(2027L, 2027L, 2028L, 2028L, 2029L, 2030L),
    round = c("First", "Second", "First", "Second", "First", "Second"),
    control_type = c("Own", "Incoming", "Outgoing", "Swap Right", "Own", "Incoming"),
    original_team = c("Boston Celtics", "Charlotte Hornets", "Boston Celtics", "Minnesota Timberwolves", "Boston Celtics", "Atlanta Hawks"),
    protection_text = c("Unprotected", "Top 55 protected", "", "Swap terms", "Unprotected", "Top 45 protected"),
    protection_type = c("Unprotected", "Range Protected", NA, "Unprotected", "Unprotected", "Range Protected"),
    verification_status = c("Verified", "Unverified", "Unverified", "Unverified", "Verified", "Unverified"),
    requires_manual_review = c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE),
    source_name = c("Fixture", "Fixture", "Fixture", "Fixture", "Fixture", NA),
    source_url = c("fixture://1", "fixture://2", "fixture://3", "fixture://4", "fixture://5", NA),
    source_date = c("2026-08-21", "2026-08-21", "2026-08-21", "2026-08-21", "2026-08-21", NA),
    condition_count = c(0L, 1L, 0L, 1L, 0L, 1L),
    transaction_reference = c("Own", "Trade A", "Trade B", NA, "Own", "Trade C"),
    stringsAsFactors = FALSE
  )

  valued <- data.frame(
    draft_asset_id = 1:6,
    expected_slot = c(15, 45, 18, 40, 12, 42),
    blended_value_score = c(30, 8, -25, 12, 50, 7),
    value_tier = c("Strong", "Depth", "Obligation", "Optionality", "Premium", "Depth"),
    requires_manual_review = assets$requires_manual_review,
    stringsAsFactors = FALSE
  )

  list(assets = assets, valued = valued)
}

testthat::test_that("Draft ledger filters and grouped years use governed server state", {
  fixture <- draft_stabilization_fixture()
  ledger <- prepare_draft_asset_ledger(fixture$assets, fixture$valued)

  testthat::expect_equal(nrow(filter_draft_asset_ledger(ledger, list(year = "2027"))), 2L)
  testthat::expect_equal(
    filter_draft_asset_ledger(ledger, list(round = "Second", control = "Incoming"))$draft_asset_id,
    c(2L, 6L)
  )
  testthat::expect_equal(
    filter_draft_asset_ledger(ledger, list(search = "minnesota", verification = "REQUIRES VERIFICATION"))$draft_asset_id,
    4L
  )
  testthat::expect_equal(filter_draft_asset_ledger(ledger, list(original_team = "Atlanta Hawks"))$draft_asset_id, 6L)
  testthat::expect_equal(filter_draft_asset_ledger(ledger, list(protection = "Range Protected"))$draft_asset_id, c(2L, 6L))
  testthat::expect_equal(filter_draft_asset_ledger(ledger, list(verification = "VERIFIED"))$draft_asset_id, c(1L, 5L))
  testthat::expect_equal(filter_draft_asset_ledger(ledger, list(search = "obligation"))$draft_asset_id, 3L)
  testthat::expect_equal(nrow(filter_draft_asset_ledger(ledger, list())), nrow(ledger))

  groups <- group_draft_asset_ledger(filter_draft_asset_ledger(ledger, list(round = "Second")))
  testthat::expect_equal(names(groups), c("2027", "2028", "2030"))
  testthat::expect_equal(unname(sort(unlist(lapply(groups, `[[`, "draft_asset_id")))), c(2L, 4L, 6L))
  testthat::expect_false(anyDuplicated(unlist(lapply(groups, `[[`, "draft_asset_id"))) > 0L)
})

testthat::test_that("Draft verification reasons identify supported missing evidence", {
  fixture <- draft_stabilization_fixture()
  ledger <- prepare_draft_asset_ledger(fixture$assets, fixture$valued)

  testthat::expect_equal(ledger$verification_reason[[1]], "Verified")
  testthat::expect_match(ledger$verification_reason[[3]], "Protection language incomplete", fixed = TRUE)
  testthat::expect_match(ledger$verification_reason[[4]], "Swap terms incomplete", fixed = TRUE)
  testthat::expect_match(ledger$verification_reason[[6]], "Source provenance incomplete", fixed = TRUE)
  testthat::expect_false(any(grepl("Manual Review", ledger$verification_reason, fixed = TRUE)))
})

testthat::test_that("Draft UI exposes compact governed filters and year groups", {
  html <- htmltools::renderTags(mod_draft_assets_ui("draft"))$html

  for (id in c(
    "draft_search", "draft_year_filter", "draft_round_filter",
    "draft_control_filter", "draft_original_team_filter",
    "draft_protection_filter", "draft_verification_filter", "clear_draft_filters"
  )) {
    testthat::expect_match(html, paste0('id="draft-', id, '"'), fixed = TRUE)
  }
  testthat::expect_match(html, "Records Requiring Verification", fixed = TRUE)
  testthat::expect_match(html, "draft-v2-filter-bar", fixed = TRUE)
  testthat::expect_match(html, "draft_asset_table", fixed = TRUE)
})

testthat::test_that("Draft recommendation is tied to actual portfolio facts", {
  fixture <- draft_stabilization_fixture()
  ledger <- prepare_draft_asset_ledger(fixture$assets, fixture$valued)
  summary <- list(net_portfolio_value = 67, portfolio_grade = "Average", review_required = 4L)
  recommendation <- draft_recommendation_facts(ledger, summary)

  testthat::expect_match(recommendation$posture, "PROTECT", fixed = TRUE)
  testthat::expect_match(recommendation$strongest_year, "2029", fixed = TRUE)
  testthat::expect_match(recommendation$constrained_year, "2028", fixed = TRUE)
  testthat::expect_match(recommendation$biggest_obligation, "2028", fixed = TRUE)
  testthat::expect_match(recommendation$verification, "4", fixed = TRUE)
  testthat::expect_match(recommendation$next_action, "2029", fixed = TRUE)

  unknown_values <- ledger
  unknown_values$blended_value_score <- NA_real_
  unknown_recommendation <- draft_recommendation_facts(unknown_values, summary)
  testthat::expect_match(unknown_recommendation$strongest_year, "No supported year", fixed = TRUE)
  testthat::expect_match(unknown_recommendation$biggest_obligation, "UNKNOWN", fixed = TRUE)
})

testthat::test_that("Draft error recommendations remain review-only and unknown", {
  recommendation <- draft_error_recommendation("fixture query failed")

  testthat::expect_identical(recommendation$posture, "REVIEW / UNKNOWN")
  testthat::expect_true(all(grepl(
    "UNKNOWN",
    unlist(recommendation[c(
      "strongest_year", "constrained_year", "verification", "biggest_obligation"
    )]),
    fixed = TRUE
  )))
  testthat::expect_match(recommendation$next_action, "Review only", fixed = TRUE)
  testthat::expect_false(grepl("ACQUIRE|PRESERVE", paste(unlist(recommendation), collapse = " ")))
})

testthat::test_that("Draft engine errors never reach the normal posture model", {
  posture_calls <- 0L
  testthat::local_mocked_bindings(
    evaluate_team_draft_value = function(...) stop("fixture query failed", call. = FALSE),
    draft_recommendation_facts = function(...) {
      posture_calls <<- posture_calls + 1L
      stop("normal posture model must not run", call. = FALSE)
    },
    .package = "basketballops"
  )

  team <- shiny::reactiveVal("Boston Celtics")
  season <- shiny::reactiveVal("2026-27")
  rendered_html <- function(value) {
    if (is.list(value) && !is.null(value$html)) value$html else as.character(value)
  }

  shiny::testServer(
    mod_draft_assets_server,
    args = list(selected_team = team, selected_season = season),
    {
      session$flushReact()
      decision_html <- rendered_html(output$draft_decision)
      recommendation_html <- rendered_html(output$draft_recommendation)
      opportunity_html <- rendered_html(output$draft_opportunities)

      testthat::expect_identical(posture_calls, 0L)
      testthat::expect_match(decision_html, "REVIEW / UNKNOWN", fixed = TRUE)
      testthat::expect_match(recommendation_html, "REVIEW / UNKNOWN", fixed = TRUE)
      testthat::expect_match(recommendation_html, "UNKNOWN", fixed = TRUE)
      testthat::expect_match(opportunity_html, "REVIEW / UNKNOWN", fixed = TRUE)
      testthat::expect_false(grepl(
        "ACQUIRE|PRESERVE",
        paste(recommendation_html, opportunity_html),
        ignore.case = TRUE
      ))
    }
  )
})

testthat::test_that("Draft module filters real team ledgers without stale team state", {
  team <- shiny::reactiveVal("Boston Celtics")
  season <- shiny::reactiveVal("2026-27")
  rendered_html <- function(value) {
    if (is.list(value) && !is.null(value$html)) value$html else as.character(value)
  }

  shiny::testServer(
    mod_draft_assets_server,
    args = list(selected_team = team, selected_season = season),
    {
      session$flushReact()
      boston <- filtered_draft_assets()
      testthat::expect_gt(nrow(boston), 0L)
      testthat::expect_equal(as.integer(output$snapshot_total_assets), nrow(boston))
      timeline_html <- rendered_html(output$asset_timeline)
      testthat::expect_match(timeline_html, "2027", fixed = TRUE)
      ledger_html <- rendered_html(output$draft_asset_table)
      testthat::expect_gte(lengths(regmatches(ledger_html, gregexpr('class="draft-v2-year-group" open', ledger_html, fixed = TRUE))), 2L)
      testthat::expect_match(ledger_html, "Expected slot:", fixed = TRUE)
      risk_html <- rendered_html(output$draft_risks)
      testthat::expect_match(risk_html, "Decision consequence", fixed = TRUE)

      session$setInputs(draft_round_filter = "First", draft_control_filter = "Own")
      session$flushReact()
      filtered <- filtered_draft_assets()
      testthat::expect_true(all(filtered$round == "First"))
      testthat::expect_true(all(filtered$control_type == "Own"))
      testthat::expect_lt(nrow(filtered), nrow(boston))

      session$setInputs(draft_search = "Boston")
      session$flushReact()
      testthat::expect_gt(nrow(filtered_draft_assets()), 0L)

      session$setInputs(draft_search = "", draft_round_filter = "", draft_control_filter = "")
      session$flushReact()
      testthat::expect_equal(nrow(filtered_draft_assets()), nrow(boston))

      session$setInputs(draft_original_team_filter = "Boston Celtics")
      session$setInputs(draft_search = "Boston")
      session$flushReact()
      testthat::expect_lt(nrow(filtered_draft_assets()), nrow(boston))

      prior_ids <- boston$draft_asset_id
      prior_portfolio <- rendered_html(output$portfolio_value_hero)
      prior_timeline <- rendered_html(output$asset_timeline)
      for (next_team in c("Minnesota Timberwolves", "Atlanta Hawks", "Charlotte Hornets")) {
        team(next_team)
        session$flushReact()
        current <- filtered_draft_assets()
        testthat::expect_gt(nrow(current), 0L)
        testthat::expect_equal(nrow(current), nrow(draft_ledger()))
        testthat::expect_false(identical(sort(current$draft_asset_id), sort(prior_ids)))
        current_portfolio <- rendered_html(output$portfolio_value_hero)
        current_timeline <- rendered_html(output$asset_timeline)
        testthat::expect_false(identical(current_portfolio, prior_portfolio))
        testthat::expect_false(identical(current_timeline, prior_timeline))
        testthat::expect_match(rendered_html(output$draft_recommendation), next_team, fixed = TRUE)
        prior_ids <- current$draft_asset_id
        prior_portfolio <- current_portfolio
        prior_timeline <- current_timeline
      }

      recommendation_html <- htmltools::renderTags(output$draft_recommendation)$html
      testthat::expect_match(recommendation_html, "Charlotte", fixed = TRUE)
      testthat::expect_match(recommendation_html, "Records requiring verification", fixed = TRUE)
      testthat::expect_match(
        rendered_html(output$portfolio_control_summary),
        "Records Requiring Source Verification",
        fixed = TRUE
      )
      testthat::expect_match(rendered_html(output$draft_asset_table), "Loaded source provenance remains unverified", fixed = TRUE)
    }
  )
})
