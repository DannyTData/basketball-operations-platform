trade_output_test_project_root <- function() {
  candidates <- c(".", "..", "../..", "../../..")

  for (candidate in candidates) {
    if (file.exists(file.path(candidate, "DESCRIPTION"))) {
      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }
  }

  stop("Could not locate project root from Trade output tests.", call. = FALSE)
}

trade_rendered_ids <- function(ui) {
  html <- htmltools::renderTags(ui)$html
  matches <- regmatches(
    html,
    gregexpr('id="[^"]+"', html, perl = TRUE)
  )[[1]]

  gsub('^id="|"$', "", matches)
}

testthat::test_that("Trade UI has unique canonical and mirror output IDs", {
  module_id <- "trade_output_test"
  ids <- trade_rendered_ids(mod_trade_analyzer_ui(module_id))
  duplicate_ids <- unique(ids[duplicated(ids)])
  relevant_ids <- c(
    "snapshot_team_a_status",
    "center_team_a_status",
    "snapshot_cba_result",
    "center_cba_result",
    "team_b_short_name",
    "team_b_panel_short_name"
  )
  relevant_counts <- vapply(
    paste0(module_id, "-", relevant_ids),
    function(output_id) sum(ids == output_id),
    integer(1)
  )

  testthat::expect_length(duplicate_ids, 0L)
  testthat::expect_equal(
    unname(relevant_counts),
    rep(1L, length(relevant_ids))
  )
})

testthat::test_that("the application mounts one Trade module instance", {
  root <- trade_output_test_project_root()
  app_ui_source <- paste(
    readLines(file.path(root, "R", "app_ui.R"), warn = FALSE),
    collapse = "\n"
  )
  rendered <- htmltools::renderTags(app_ui(NULL))$html

  testthat::expect_equal(
    lengths(
      regmatches(
        app_ui_source,
        gregexpr("mod_trade_analyzer_ui\\s*\\(", app_ui_source, perl = TRUE)
      )
    ),
    1L
  )
  testthat::expect_equal(
    lengths(
      regmatches(
        rendered,
        gregexpr(
          'class="tbi-module-page tbi-v2-trade-page"',
          rendered,
          fixed = TRUE
        )
      )
    ),
    1L
  )
})

testthat::test_that("Trade canonical and mirror outputs render identical text", {
  shiny::testServer(
    app = function(input, output, session) {
      partner_name <- shiny::reactive(input$partner_name)
      team_a_status <- shiny::reactive(input$team_a_status)
      cba_status <- shiny::reactive(input$cba_status)

      bind_trade_text_output_mirror(
        output,
        "team_b_short_name",
        "team_b_panel_short_name",
        partner_name
      )
      bind_trade_text_output_mirror(
        output,
        "snapshot_team_a_status",
        "center_team_a_status",
        team_a_status
      )
      bind_trade_text_output_mirror(
        output,
        "snapshot_cba_result",
        "center_cba_result",
        cba_status
      )
    },
    {
      session$setInputs(
        partner_name = "Atlanta Hawks",
        team_a_status = "Select players",
        cba_status = "REVIEW"
      )

      testthat::expect_identical(
        output$team_b_short_name,
        output$team_b_panel_short_name
      )
      testthat::expect_identical(
        output$snapshot_team_a_status,
        output$center_team_a_status
      )
      testthat::expect_identical(
        output$snapshot_cba_result,
        output$center_cba_result
      )
    }
  )
})
