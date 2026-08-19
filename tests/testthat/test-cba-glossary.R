cba_glossary_rendered_ids <- function(ui) {
  html <- htmltools::renderTags(ui)$html
  matches <- regmatches(
    html,
    gregexpr('id="[^"]+"', html, perl = TRUE)
  )[[1]]

  gsub('^id="|"$', "", matches)
}


testthat::test_that("CBA glossary keeps canonical terms unique and source metadata explicit", {
  glossary <- tbi_cba_glossary_data()

  testthat::expect_true(
    all(
      c(
        "term",
        "category",
        "short_definition",
        "front_office_impact",
        "module",
        "example",
        "affects",
        "related_terms",
        "aliases",
        "source",
        "source_reference",
        "verification_status"
      ) %in% names(glossary)
    )
  )
  testthat::expect_false(anyDuplicated(glossary$term) > 0L)
  testthat::expect_false(any(!nzchar(trimws(glossary$term))))
  testthat::expect_true(nrow(glossary) >= 60L)
  testthat::expect_true(all(nzchar(trimws(glossary$source))))
  testthat::expect_true(all(nzchar(trimws(glossary$source_reference))))
  testthat::expect_setequal(
    unique(glossary$verification_status),
    c("Supported summary", "Requires source verification")
  )
})


testthat::test_that("CBA aliases resolve to one canonical term", {
  expected <- c(
    "Bird Rights" = "Bird Exception",
    "Early Bird Rights" = "Early Bird Exception",
    "Non-Bird Rights" = "Non-Bird Exception",
    "Minimum Salary Exception" = "Minimum Player Salary Exception",
    "Room Exception" = "Room Mid-Level Exception",
    "TPE" = "Traded Player Exception (TPE)",
    "Aggregation" = "Salary Aggregation",
    "RFA" = "Restricted Free Agent (RFA)",
    "UFA" = "Unrestricted Free Agent (UFA)",
    "Rookie Extension" = "Rookie-Scale Extension",
    "Supermax" = "Supermax / Designated Veteran Terminology"
  )

  resolved <- vapply(
    names(expected),
    tbi_cba_resolve_term,
    character(1)
  )

  testthat::expect_identical(unname(resolved), unname(expected))
  testthat::expect_identical(
    tbi_cba_resolve_term("Salary Cap"),
    "Salary Cap"
  )
  testthat::expect_true(is.na(tbi_cba_resolve_term("Not a CBA term")))

  alias_index <- tbi_cba_glossary_alias_index()
  testthat::expect_false(anyDuplicated(names(alias_index)) > 0L)
  testthat::expect_true(all(unname(alias_index) %in% tbi_cba_glossary_data()$term))
})


testthat::test_that("CBA expansion covers requested front-office concepts", {
  requested_terms <- c(
    "Early Termination Option",
    "Mid-Level Exception",
    "Disabled Player Exception",
    "Poison Pill Provision",
    "Minimum Team Salary / Salary Floor",
    "Guaranteed Salary",
    "Non-Guaranteed Salary",
    "Partial Guarantee",
    "Roster Charge",
    "Maximum Salary",
    "Maximum Extension",
    "Extension Eligibility",
    "Over-38 Rule",
    "Cash Considerations",
    "Draft Rights",
    "First-Round Pick",
    "Second-Round Pick",
    "Pick Protection",
    "Pick Swap",
    "Stepien Rule",
    "Seven-Year Rule",
    "Conveyance",
    "Pick Obligation",
    "Reacquisition Restrictions",
    "Recently Traded Restriction",
    "Recently Signed Restriction"
  )

  resolved <- vapply(
    requested_terms,
    tbi_cba_resolve_term,
    character(1)
  )

  testthat::expect_false(anyNA(resolved))
})


testthat::test_that("CBA search includes aliases and metadata", {
  glossary <- tbi_cba_glossary_data()
  bird_results <- tbi_cba_filter_glossary(
    glossary,
    search_value = "bird rights"
  )
  source_results <- tbi_cba_filter_glossary(
    glossary,
    search_value = "source verification"
  )
  draft_results <- tbi_cba_filter_glossary(
    glossary,
    category = "Draft Assets"
  )

  testthat::expect_true("Bird Exception" %in% bird_results$term)
  testthat::expect_true(nrow(source_results) > 0L)
  testthat::expect_true(nrow(draft_results) > 0L)
  testthat::expect_true(all(draft_results$category == "Draft Assets"))
})


testthat::test_that("CBA UI preserves IDs and exposes the responsive index workspace", {
  module_id <- "cba_glossary_contract"
  ui <- mod_cba_glossary_ui(module_id)
  rendered <- htmltools::renderTags(ui)$html
  ids <- cba_glossary_rendered_ids(ui)
  expected_ids <- c(
    "term_count",
    "search",
    "category",
    "glossary_list",
    "term_detail",
    "recent_terms",
    "favorite_terms"
  )

  for (shiny_id in expected_ids) {
    testthat::expect_equal(
      sum(ids == paste0(module_id, "-", shiny_id)),
      1L,
      info = paste("Missing or duplicated CBA ID:", shiny_id)
    )
  }

  testthat::expect_match(rendered, "cba-kb-index-panel", fixed = TRUE)
  testthat::expect_match(rendered, "cba-kb-workspace-panel", fixed = TRUE)
  testthat::expect_match(rendered, "data-cba-index-toggle", fixed = TRUE)
  testthat::expect_match(rendered, "data-cba-index-close", fixed = TRUE)
  testthat::expect_match(rendered, "aria-expanded", fixed = TRUE)
  testthat::expect_match(rendered, "@media(max-width:1000px)", fixed = TRUE)
  testthat::expect_match(rendered, "translateX", fixed = TRUE)
  testthat::expect_false(anyDuplicated(ids) > 0L)
})


testthat::test_that("shared UX wiring routes CBA term and module context links", {
  root_candidates <- c(".", "..", "../..", "../../..")
  root <- root_candidates[
    vapply(
      root_candidates,
      function(candidate) file.exists(file.path(candidate, "DESCRIPTION")),
      logical(1)
    )
  ][[1]]
  javascript <- paste(
    readLines(
      file.path(root, "inst", "app", "www", "tbi_ux_foundation.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  testthat::expect_match(javascript, "[data-cba-term-link]", fixed = TRUE)
  testthat::expect_match(javascript, "[data-cba-module-link]", fixed = TRUE)
  testthat::expect_match(javascript, "'cba_open_term'", fixed = TRUE)
  testthat::expect_match(javascript, "'cba_open_module'", fixed = TRUE)
  testthat::expect_match(javascript, "priority: 'event'", fixed = TRUE)
})


testthat::test_that("CBA deep links resolve aliases and preserve valid selection", {
  external_request <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_cba_glossary_server,
    args = list(external_term = external_request),
    {
      session$flushReact()
      testthat::expect_identical(selected_term_value(), "Salary Cap")

      external_request(list(term = "Bird Rights", nonce = 1))
      session$flushReact()
      testthat::expect_identical(selected_term_value(), "Bird Exception")

      session$setInputs(
        category = "All Categories",
        search = "salary matching"
      )
      session$flushReact()
      testthat::expect_identical(selected_term_value(), "Bird Exception")

      external_request(list(term = "Not a CBA term", nonce = 2))
      session$flushReact()
      testthat::expect_identical(selected_term_value(), "Bird Exception")
    }
  )
})
