testthat::test_that("Draft Overview and Portfolio use scoped three-column workspaces", {
  html <- htmltools::renderTags(mod_draft_assets_ui("draft"))$html

  testthat::expect_match(html, "draft-v2-overview-balance-grid", fixed = TRUE)
  testthat::expect_match(html, "draft-v2-portfolio-balance-grid", fixed = TRUE)
  testthat::expect_match(html, 'id="draft-overview_control_summary"', fixed = TRUE)
  testthat::expect_match(html, 'id="draft-portfolio_control_summary"', fixed = TRUE)
  normalized_html <- gsub("\\s+", " ", html)
  testthat::expect_match(
    normalized_html,
    ".tbi-v2-draft-page :is(.draft-v2-overview-balance-grid,.draft-v2-portfolio-balance-grid)",
    fixed = TRUE
  )
  testthat::expect_match(
    normalized_html,
    "grid-template-columns:repeat(3,minmax(0,1fr)) !important",
    fixed = TRUE
  )
  testthat::expect_match(html, "CONTROL / VERIFICATION", fixed = TRUE)
  testthat::expect_match(html, "CONTROL &amp; OBLIGATIONS", fixed = TRUE)
  testthat::expect_false(grepl("overflow-y:auto", html, fixed = TRUE))
})

testthat::test_that("balanced Draft panels update through required team switches", {
  team <- shiny::reactiveVal("Boston Celtics")
  season <- shiny::reactiveVal("2026-27")
  rendered_html <- function(value) {
    if (is.list(value) && !is.null(value$html)) value$html else as.character(value)
  }

  shiny::testServer(
    mod_draft_assets_server,
    args = list(selected_team = team, selected_season = season),
    {
      prior_signature <- NULL

      for (next_team in c(
        "Boston Celtics",
        "LA Clippers",
        "Minnesota Timberwolves",
        "Charlotte Hornets"
      )) {
        team(next_team)
        session$flushReact()

        overview <- rendered_html(output$overview_control_summary)
        strength <- rendered_html(output$portfolio_value_hero)
        control <- rendered_html(output$portfolio_control_summary)
        timeline <- rendered_html(output$asset_timeline)
        decision <- rendered_html(output$draft_decision)
        value <- rendered_html(output$portfolio_scorecard)

        testthat::expect_match(overview, "Records Requiring Source Verification", fixed = TRUE)
        testthat::expect_match(control, "Records Requiring Source Verification", fixed = TRUE)
        testthat::expect_match(strength, "Gross asset value", fixed = TRUE)
        testthat::expect_match(strength, "Obligation value", fixed = TRUE)
        testthat::expect_match(timeline, "draft-v2-year-obligations", fixed = TRUE)
        testthat::expect_equal(
          lengths(regmatches(timeline, gregexpr('class="draft-v2-year-card"', timeline, fixed = TRUE))),
          5L
        )
        testthat::expect_true(nzchar(decision))
        testthat::expect_true(nzchar(value))

        signature <- paste(overview, strength, control, timeline, decision, value)
        if (!is.null(prior_signature)) {
          testthat::expect_false(identical(signature, prior_signature))
        }
        prior_signature <- signature
      }
    }
  )
})
