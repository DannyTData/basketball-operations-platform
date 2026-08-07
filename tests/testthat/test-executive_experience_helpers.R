# ============================================================
# Thompson's Basketball Intelligence
# Phase 7: Executive Experience Helper Tests
# ============================================================

testthat::test_that("executive helpers normalize values safely", {
  testthat::expect_equal(
    executive_text("  Advance  "),
    "Advance"
  )
  
  testthat::expect_equal(
    executive_number("72.5"),
    72.5
  )
  
  testthat::expect_true(
    executive_flag("yes")
  )
  
  testthat::expect_false(
    executive_flag("no")
  )
  
  testthat::expect_equal(
    executive_score(120),
    100
  )
  
  testthat::expect_equal(
    executive_score(-5),
    0
  )
})


testthat::test_that("nested executive values are read safely", {
  x <- list(
    a = list(
      b = 12
    )
  )
  
  testthat::expect_equal(
    executive_get(x, c("a", "b"), 0),
    12
  )
  
  testthat::expect_equal(
    executive_get(x, c("a", "missing"), 7),
    7
  )
})


testthat::test_that("status severity mapping is consistent", {
  testthat::expect_equal(
    executive_status_severity("PASS"),
    "positive"
  )
  
  testthat::expect_equal(
    executive_status_severity("PASS_WITH_REVIEW"),
    "caution"
  )
  
  testthat::expect_equal(
    executive_status_severity("Above First Apron"),
    "warning"
  )
  
  testthat::expect_equal(
    executive_status_severity("FAIL"),
    "negative"
  )
  
  testthat::expect_equal(
    executive_status_severity("Unknown"),
    "neutral"
  )
})


testthat::test_that("display labels normalize underscores", {
  testthat::expect_equal(
    executive_display_label("pass_with_review"),
    "Pass With Review"
  )
  
  testthat::expect_equal(
    executive_display_label("ABOVE_SECOND_APRON"),
    "Above Second Apron"
  )
})


testthat::test_that("score severity follows configured bands", {
  testthat::expect_equal(
    executive_score_severity(85),
    "positive"
  )
  
  testthat::expect_equal(
    executive_score_severity(70),
    "caution"
  )
  
  testthat::expect_equal(
    executive_score_severity(55),
    "neutral"
  )
  
  testthat::expect_equal(
    executive_score_severity(40),
    "warning"
  )
  
  testthat::expect_equal(
    executive_score_severity(20),
    "negative"
  )
})


testthat::test_that("status badge returns a Shiny tag", {
  badge <- executive_status_badge(
    label = "PASS",
    icon = "circle-check"
  )
  
  testthat::expect_s3_class(
    badge,
    "shiny.tag"
  )
  
  html <- as.character(badge)
  
  testthat::expect_match(
    html,
    "Pass"
  )
  
  testthat::expect_match(
    html,
    "tbi-exec-badge"
  )
})


testthat::test_that("data badge supports all states", {
  statuses <- c(
    "Verified",
    "Assumption-Based",
    "Needs Review",
    "Unavailable"
  )
  
  badges <- lapply(
    statuses,
    executive_data_badge
  )
  
  testthat::expect_true(
    all(
      vapply(
        badges,
        inherits,
        logical(1),
        what = "shiny.tag"
      )
    )
  )
})


testthat::test_that("metric card renders label and value", {
  card <- executive_metric_card(
    label = "Score",
    value = "82.4",
    subtitle = "Composite rating",
    status = "Positive",
    severity = "positive"
  )
  
  html <- as.character(card)
  
  testthat::expect_match(
    html,
    "Score"
  )
  
  testthat::expect_match(
    html,
    "82.4"
  )
  
  testthat::expect_match(
    html,
    "Composite rating"
  )
})


testthat::test_that("empty and loading states render", {
  empty <- executive_empty_state()
  loading <- executive_loading_state()
  
  testthat::expect_s3_class(
    empty,
    "shiny.tag"
  )
  
  testthat::expect_s3_class(
    loading,
    "shiny.tag"
  )
  
  testthat::expect_match(
    as.character(empty),
    "No data available"
  )
  
  testthat::expect_match(
    as.character(loading),
    "Loading executive intelligence"
  )
})


testthat::test_that("recommendation banner renders intelligence result", {
  result <- list(
    recommendation = "Advance with Conditions",
    classification = "Positive",
    score = 72.4,
    executive_summary = "The move is viable with manageable risk.",
    requires_manual_review = TRUE
  )
  
  banner <- executive_recommendation_banner(
    result
  )
  
  html <- as.character(banner)
  
  testthat::expect_match(
    html,
    "Advance with Conditions"
  )
  
  testthat::expect_match(
    html,
    "72.4"
  )
  
  testthat::expect_match(
    html,
    "Needs Review"
  )
})


testthat::test_that("recommendation banner handles missing input", {
  banner <- executive_recommendation_banner(
    NULL
  )
  
  testthat::expect_match(
    as.character(banner),
    "Recommendation unavailable"
  )
})


testthat::test_that("factor card renders score and explanation", {
  component <- list(
    score = 68.2,
    explanation = "Financial flexibility is moderate."
  )
  
  card <- executive_factor_card(
    label = "Financial Flexibility",
    component = component,
    icon = "scale-balanced"
  )
  
  html <- as.character(card)
  
  testthat::expect_match(
    html,
    "Financial Flexibility"
  )
  
  testthat::expect_match(
    html,
    "68.2"
  )
  
  testthat::expect_match(
    html,
    "Financial flexibility is moderate"
  )
})


testthat::test_that("scorecard renders all five factors", {
  result <- list(
    components = list(
      competitive_position = list(
        score = 80,
        explanation = "Strong competitive position."
      ),
      financial_flexibility = list(
        score = 60,
        explanation = "Moderate flexibility."
      ),
      roster_control = list(
        score = 70,
        explanation = "Good roster control."
      ),
      draft_capital = list(
        score = 75,
        explanation = "Strong draft portfolio."
      ),
      transaction_risk = list(
        score = 65,
        explanation = "Manageable transaction profile."
      )
    )
  )
  
  scorecard <- executive_intelligence_scorecard(
    result
  )
  
  html <- as.character(scorecard)
  
  expected_labels <- c(
    "Competitive Position",
    "Financial Flexibility",
    "Roster Control",
    "Draft Capital",
    "Transaction Profile"
  )
  
  testthat::expect_true(
    all(
      vapply(
        expected_labels,
        grepl,
        logical(1),
        x = html,
        fixed = TRUE
      )
    )
  )
})


testthat::test_that("scorecard handles missing components", {
  scorecard <- executive_intelligence_scorecard(
    list()
  )
  
  testthat::expect_match(
    as.character(scorecard),
    "Scorecard unavailable"
  )
})


testthat::test_that("risk panel renders supplied risks", {
  result <- list(
    key_risks = c(
      "First apron exposure.",
      "Limited future draft capital."
    )
  )
  
  panel <- executive_risk_panel(
    result
  )
  
  html <- as.character(panel)
  
  testthat::expect_match(
    html,
    "First apron exposure"
  )
  
  testthat::expect_match(
    html,
    "Limited future draft capital"
  )
})


testthat::test_that("risk panel handles empty risks", {
  panel <- executive_risk_panel(
    list(key_risks = character())
  )
  
  testthat::expect_match(
    as.character(panel),
    "No major structural risk identified"
  )
})


testthat::test_that("opportunity panel renders opportunities", {
  panel <- executive_opportunity_panel(
    c(
      "Preserve a future first-round pick.",
      "Maintain access to the non-taxpayer MLE."
    )
  )
  
  html <- as.character(panel)
  
  testthat::expect_match(
    html,
    "Preserve a future first-round pick"
  )
  
  testthat::expect_match(
    html,
    "Maintain access to the non-taxpayer MLE"
  )
})


testthat::test_that("scenario cards identify preferred scenario", {
  decision <- list(
    score = 78,
    classification = "Aggressive",
    executive_summary = "Best path."
  )
  
  card <- executive_scenario_card(
    label = "Trade Now",
    decision = decision,
    is_preferred = TRUE
  )
  
  html <- as.character(card)
  
  testthat::expect_match(
    html,
    "Trade Now"
  )
  
  testthat::expect_match(
    html,
    "Preferred"
  )
})


testthat::test_that("scenario comparison renders both options", {
  comparison <- list(
    label_a = "Trade Now",
    label_b = "Hold",
    preferred = "Trade Now",
    executive_summary = "Trade Now is preferred by 8 points."
  )
  
  decision_a <- list(
    score = 76,
    classification = "Positive",
    executive_summary = "Improves short-term outlook."
  )
  
  decision_b <- list(
    score = 68,
    classification = "Positive",
    executive_summary = "Preserves flexibility."
  )
  
  panel <- executive_scenario_comparison(
    comparison_result = comparison,
    decision_a = decision_a,
    decision_b = decision_b
  )
  
  html <- as.character(panel)
  
  testthat::expect_match(
    html,
    "Trade Now"
  )
  
  testthat::expect_match(
    html,
    "Hold"
  )
  
  testthat::expect_match(
    html,
    "preferred by 8 points"
  )
})


testthat::test_that("data quality panel renders all confidence categories", {
  panel <- executive_data_quality_panel(
    verified_items = 8,
    assumption_items = 3,
    review_items = 2,
    unavailable_items = 1,
    updated_at = "2026-08-06"
  )
  
  html <- as.character(panel)
  
  expected <- c(
    "Verified",
    "Assumption-Based",
    "Needs Review",
    "Unavailable",
    "2026-08-06"
  )
  
  testthat::expect_true(
    all(
      vapply(
        expected,
        grepl,
        logical(1),
        x = html,
        fixed = TRUE
      )
    )
  )
})


testthat::test_that("complete decision view renders all major sections", {
  intelligence <- list(
    recommendation = "Advance with Conditions",
    classification = "Positive",
    score = 71,
    executive_summary = "The proposed move is viable.",
    scope_note = "Decision-support only.",
    key_risks = "First apron exposure.",
    components = list(
      competitive_position = list(
        score = 82,
        explanation = "Contender."
      ),
      financial_flexibility = list(
        score = 58,
        explanation = "Moderate flexibility."
      ),
      roster_control = list(
        score = 65,
        explanation = "Adequate control."
      ),
      draft_capital = list(
        score = 72,
        explanation = "Strong assets."
      ),
      transaction_risk = list(
        score = 63,
        explanation = "Manageable risk."
      )
    )
  )
  
  view <- executive_decision_view(
    intelligence_result = intelligence,
    opportunities = "Retains one future first-round pick.",
    data_quality = list(
      verified_items = 6,
      assumption_items = 2,
      review_items = 1,
      unavailable_items = 0
    )
  )
  
  html <- as.character(view)
  
  testthat::expect_match(
    html,
    "Executive Recommendation"
  )
  
  testthat::expect_match(
    html,
    "Basketball Intelligence Scorecard"
  )
  
  testthat::expect_match(
    html,
    "Key Risks"
  )
  
  testthat::expect_match(
    html,
    "Key Opportunities"
  )
  
  testthat::expect_match(
    html,
    "Data Confidence"
  )
  
  testthat::expect_match(
    html,
    "Decision-support only"
  )
})