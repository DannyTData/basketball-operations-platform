roster_stabilization_fixture <- function() {
  data.frame(
    player_id = 1:6,
    player_name = c("Alpha Guard", "Bravo Wing", "Charlie Big", "Delta Guard", "Echo Wing", "Foxtrot Big"),
    primary_position = c("PG", "SG, SF", "C", "PG, SG", "PF", "C"),
    player_age = c(24, 29, 31, 22, 27, 26),
    roster_status = c("Active", "Active", "Active", "Two-Way", "Active", "Active"),
    two_way_flag = c(0, 0, 0, 1, 0, 0),
    contract_id = c(101, 102, 103, 104, NA, 106),
    contract_type = c("Rookie Scale Contract", "Veteran Contract", "Veteran Extension", "Two-Way Contract", NA, "Exhibit 10"),
    contract_end_season = c("2028-29", "2026-27", "2029-30", "2026-27", NA, "2026-27"),
    total_value = c(20000000, 90000000, 120000000, 1200000, NA, 2200000),
    base_salary = c(5000000, 30000000, 40000000, 600000, NA, 1100000),
    cap_hit = c(5000000, 30000000, 40000000, 600000, NA, 1100000),
    guaranteed_amount = c(5000000, 30000000, 40000000, 600000, NA, 0),
    free_agent_year = c(2029, 2027, 2030, 2027, NA, 2027),
    bird_rights = c("Bird", "Bird", "Early Bird", NA, NA, "Non-Bird"),
    option_type = c("Team Option", NA, NA, NA, NA, NA),
    stringsAsFactors = FALSE
  )
}

testthat::test_that("Complete Roster preserves numeric sorting payload and governed quality states", {
  display <- roster_complete_table_data(roster_stabilization_fixture())

  testthat::expect_type(display$`Cap Hit`, "double")
  testthat::expect_type(display$Age, "double")
  testthat::expect_identical(
    display$Player[order(display$`Cap Hit`, decreasing = TRUE, na.last = TRUE)],
    c("Charlie Big", "Bravo Wing", "Alpha Guard", "Foxtrot Big", "Delta Guard", "Echo Wing")
  )
  testthat::expect_identical(
    display$Player[order(display$`Cap Hit`, decreasing = FALSE, na.last = TRUE)],
    c("Delta Guard", "Foxtrot Big", "Alpha Guard", "Bravo Wing", "Charlie Big", "Echo Wing")
  )
  testthat::expect_identical(display$`Data Quality`[[5]], "UNKNOWN")
  testthat::expect_identical(display$Category[[5]], "UNKNOWN")
  testthat::expect_true(all(display$`Data Quality`[-5] == "REQUIRES REVIEW"))

  partial_contract <- roster_stabilization_fixture()[1, , drop = FALSE]
  partial_contract$base_salary <- NA_real_
  testthat::expect_true(is.na(roster_complete_table_data(partial_contract)$`Remaining Money`[[1]]))

  widget <- roster_complete_reactable(display)
  attributes <- widget$x$tag$attribs
  payload <- jsonlite::fromJSON(as.character(attributes$data), simplifyVector = TRUE)
  testthat::expect_true(is.numeric(payload$`Cap Hit`))
  testthat::expect_false(is.character(payload$`Cap Hit`))
  testthat::expect_true(isTRUE(attributes$defaultSortDesc))
  testthat::expect_identical(attributes$defaultSorted[[1]]$id, "Cap Hit")
})

testthat::test_that("Roster filters work alone, together, and clear without changing source rows", {
  source <- roster_stabilization_fixture()

  guards <- roster_filter_records(source, position = "PG")
  testthat::expect_setequal(guards$player_name, c("Alpha Guard", "Delta Guard"))

  veteran_2027 <- roster_filter_records(
    source,
    contract_type = "Veteran Contract",
    free_agent_year = "2027"
  )
  testthat::expect_identical(veteran_2027$player_name, "Bravo Wing")

  search_sorted <- roster_filter_records(source, player_search = "big")
  search_sorted <- search_sorted[order(search_sorted$cap_hit, decreasing = TRUE), , drop = FALSE]
  testthat::expect_identical(search_sorted$player_name, c("Charlie Big", "Foxtrot Big"))

  rights <- roster_filter_records(source, bird_rights = "Bird")
  testthat::expect_setequal(rights$player_name, c("Alpha Guard", "Bravo Wing"))

  two_way <- roster_filter_records(source, roster_status = "Two-Way")
  testthat::expect_identical(two_way$player_name, "Delta Guard")

  cleared <- roster_filter_records(source)
  testthat::expect_identical(cleared, source)

  testthat::expect_equal(nrow(roster_filter_records(source, contract_type = "STALE TEAM VALUE")), 0)
  testthat::expect_identical(roster_filter_selection(source$contract_type, "STALE TEAM VALUE"), "")
})

testthat::test_that("all advertised Complete Roster columns retain sortable values", {
  display <- roster_complete_table_data(roster_stabilization_fixture())

  testthat::expect_true(is.numeric(display$Age))
  testthat::expect_true(is.numeric(display$`Cap Hit`))
  testthat::expect_identical(display$Player[order(display$Player, method = "radix")][[1]], "Alpha Guard")
  testthat::expect_identical(display$Position[order(display$Position, method = "radix")][[1]], "C")
  testthat::expect_identical(display$`Contract Through`[order(display$`Contract Through`, method = "radix")][[1]], "2026-27")
  testthat::expect_identical(display$`FA Year`[order(display$`FA Year`, method = "radix")][[1]], "2027")
})

testthat::test_that("governed reconciliation states override fallback contract quality", {
  source <- roster_stabilization_fixture()[1:3, , drop = FALSE]
  source$contract_reconciliation_status <- c("CURRENT", "STALE", "not governed")
  display <- roster_complete_table_data(source)
  testthat::expect_identical(
    display$`Data Quality`,
    c("CURRENT", "STALE", "REQUIRES REVIEW")
  )

  source$contract_reconciliation_status <- NULL
  source$reconciliation_status <- rep("CURRENT", nrow(source))
  testthat::expect_true(all(roster_complete_table_data(source)$`Data Quality` == "REQUIRES REVIEW"))
})

testthat::test_that("Roster page exposes five purposeful tabs and mounted functional filters", {
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/", mustWork = TRUE)
  ui <- htmltools::renderTags(mod_roster_contracts_ui("roster_contracts"))$html
  js <- paste(readLines(file.path(root, "inst", "app", "www", "tbi_ux_foundation.js"), warn = FALSE), collapse = "\n")
  css <- paste(readLines(file.path(root, "inst", "app", "www", "tbi_ux_foundation.css"), warn = FALSE), collapse = "\n")
  between <- function(value, start, end) {
    after_start <- strsplit(value, start, fixed = TRUE)[[1]][[2]]
    strsplit(after_start, end, fixed = TRUE)[[1]][[1]]
  }
  roster_js <- between(js, "// >>> TBI_ROSTER_SUBTABS_START >>>", "// <<< TBI_ROSTER_SUBTABS_END <<<")
  roster_css <- between(css, "/* >>> TBI_ROSTER_STABILIZATION_START >>> */", "/* <<< TBI_ROSTER_STABILIZATION_END <<< */")

  for (definition in c(
    "['overview', 'Overview']",
    "['construction', 'Roster Construction']",
    "['assessment', 'Roster Assessment']",
    "['risk', 'Risks & Opportunities']",
    "['roster', 'Complete Roster']"
  )) {
    testthat::expect_match(roster_js, definition, fixed = TRUE)
  }

  for (id in c(
    "roster_player_search", "roster_position_filter", "roster_contract_filter",
    "roster_status_filter", "roster_fa_filter", "roster_rights_filter", "clear_roster_filters"
  )) {
    testthat::expect_match(ui, paste0('id="roster_contracts-', id, '"'), fixed = TRUE)
  }

  testthat::expect_match(ui, 'data-tbi-roster-tab="construction"', fixed = TRUE)
  testthat::expect_match(ui, 'data-tbi-roster-tab="assessment"', fixed = TRUE)
  testthat::expect_match(ui, 'data-tbi-roster-tab="risk"', fixed = TRUE)
  testthat::expect_match(roster_css, ".tbi-v2-roster-page .roster-filter-bar", fixed = TRUE)
  testthat::expect_match(roster_css, ".tbi-v2-roster-page .roster-data-token", fixed = TRUE)
  testthat::expect_match(roster_css, ".tbi-v2-roster-page .tbi-v2-score-row", fixed = TRUE)
  testthat::expect_match(roster_css, "grid-template-columns:repeat(2,minmax(0,1fr))", fixed = TRUE)
  testthat::expect_match(roster_css, "@media (max-width:900px)", fixed = TRUE)
  testthat::expect_match(roster_css, "@media (max-width:560px)", fixed = TRUE)
  testthat::expect_match(roster_css, ".roster-table-wrap", fixed = TRUE)
  testthat::expect_match(roster_css, "overflow-x:auto", fixed = TRUE)

  id_hits <- regmatches(ui, gregexpr('id="[^"]+"', ui))[[1]]
  ids <- sub('id="([^"]+)"', "\\1", id_hits)
  testthat::expect_false(anyDuplicated(ids) > 0)
})

testthat::test_that("Risks and opportunities expose fact impact and decision consequence", {
  source <- paste(
    readLines(file.path(testthat::test_path(), "..", "..", "R", "mod_roster_contracts.R"), warn = FALSE),
    collapse = "\n"
  )
  testthat::expect_match(source, 'finding$evidence_label %||% "Fact"', fixed = TRUE)
  testthat::expect_match(source, 'roster_finding_field("Impact"', fixed = TRUE)
  testthat::expect_match(source, 'roster_finding_field("Decision consequence"', fixed = TRUE)
  testthat::expect_match(source, 'evidence_label = "MODEL OUTPUT"', fixed = TRUE)
  testthat::expect_false(grepl("Verified position-value pressure", source, fixed = TRUE))
})

testthat::test_that("Roster Assessment presents existing conclusions in one balanced decision workspace", {
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/", mustWork = TRUE)
  source <- paste(
    readLines(file.path(testthat::test_path(), "..", "..", "R", "mod_roster_contracts.R"), warn = FALSE),
    collapse = "\n"
  )
  ui <- htmltools::renderTags(mod_roster_contracts_ui("roster_contracts"))$html
  css <- paste(
    readLines(file.path(root, "inst", "app", "www", "tbi_ux_foundation.css"), warn = FALSE),
    collapse = "\n"
  )
  css_compact <- gsub("\\s+", " ", css)

  testthat::expect_match(source, 'class = "roster-assessment-readout"', fixed = TRUE)
  for (label in c("Current state", "Strength", "Primary concern", "Control / flexibility", "Decision watch")) {
    testthat::expect_match(source, paste0('"', label, '"'), fixed = TRUE)
  }
  testthat::expect_false(grepl('class = "roster-assessment-grid"', source, fixed = TRUE))
  testthat::expect_false(grepl('class = "roster-assessment-region"', source, fixed = TRUE))
  testthat::expect_false(grepl('class = "roster-assessment-control"', source, fixed = TRUE))
  testthat::expect_equal(lengths(regmatches(ui, gregexpr('id="roster_contracts-roster_composition"', ui, fixed = TRUE))), 1L)
  testthat::expect_equal(lengths(regmatches(ui, gregexpr('id="roster_contracts-roster_assessment"', ui, fixed = TRUE))), 1L)

  testthat::expect_match(
    css_compact,
    '[data-tbi-roster-active-tab="assessment"] .tbi-v2-cap-detail-grid { grid-template-columns:minmax(0,1.08fr) minmax(360px,.92fr) !important;',
    fixed = TRUE
  )
  testthat::expect_match(
    css_compact,
    '[data-tbi-roster-active-tab="assessment"] [id$="-roster_composition"] { grid-template-columns:repeat(2,minmax(0,1fr));',
    fixed = TRUE
  )
  testthat::expect_match(css_compact, ".roster-assessment-item { display:grid; grid-template-columns:minmax(120px,.34fr) minmax(0,1fr);", fixed = TRUE)
  testthat::expect_match(css_compact, ".roster-assessment-item dd { color:#b8c4d2; font-size:.79rem;", fixed = TRUE)
  testthat::expect_match(css_compact, ".roster-assessment-item { grid-template-columns:minmax(0,1fr);", fixed = TRUE)
})

testthat::test_that("Roster Assessment preserves governed UNKNOWN contract evidence", {
  rendered <- htmltools::renderTags(
    roster_assessment_readout(
      c(
        "18 players are represented in the selected roster view.",
        "PG has the strongest Position Value 2.0 score at 89 / 100.",
        "PF has the lowest Position Value 2.0 score at 48 / 100 and merits roster review."
      ),
      roster_assessment_control(NA_real_)
    )
  )$html

  testthat::expect_match(rendered, "UNKNOWN", fixed = TRUE)
  testthat::expect_match(rendered, "Verify contract guarantees", fixed = TRUE)
  testthat::expect_match(rendered, 'class="roster-assessment-item roster-assessment-item-watch"', fixed = TRUE)
  testthat::expect_match(roster_assessment_control(.25)[[1]], "25.0%", fixed = TRUE)
})

testthat::test_that("Overview pairs Decision and Headlines in one responsive desktop row", {
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/", mustWork = TRUE)
  ui <- htmltools::renderTags(mod_roster_contracts_ui("roster_contracts"))$html
  css <- paste(
    readLines(file.path(root, "inst", "app", "www", "tbi_ux_foundation.css"), warn = FALSE),
    collapse = "\n"
  )
  testthat::expect_match(ui, 'class="roster-overview-decision-grid tbi-roster-tab-target"', fixed = TRUE)
  overview_start <- regexpr('class="roster-overview-decision-grid tbi-roster-tab-target"', ui, fixed = TRUE)[[1]]
  decision_start <- regexpr("ROSTER DECISION", ui, fixed = TRUE)[[1]]
  headlines_start <- regexpr("ROSTER HEADLINES", ui, fixed = TRUE)[[1]]
  testthat::expect_gt(decision_start, overview_start)
  testthat::expect_gt(headlines_start, decision_start)
  testthat::expect_equal(lengths(regmatches(ui, gregexpr("ROSTER DECISION", ui, fixed = TRUE))), 1L)
  testthat::expect_equal(lengths(regmatches(ui, gregexpr("ROSTER HEADLINES", ui, fixed = TRUE))), 1L)

  testthat::expect_match(css, ".roster-overview-decision-grid {", fixed = TRUE)
  testthat::expect_match(css, "grid-template-columns:minmax(0,1.04fr) minmax(0,.96fr)", fixed = TRUE)
  testthat::expect_match(css, "align-items:stretch !important", fixed = TRUE)
  testthat::expect_match(css, ".roster-overview-decision-grid .tbi-v2-decision-main", fixed = TRUE)
  testthat::expect_match(css, "min-height:108px !important", fixed = TRUE)
  testthat::expect_match(css, "@media (max-width:900px)", fixed = TRUE)
  testthat::expect_match(css, ".roster-overview-decision-grid {\n    grid-template-columns:minmax(0,1fr) !important", fixed = TRUE)
})

testthat::test_that("Roster Construction pairs existing panels in one responsive workspace", {
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/", mustWork = TRUE)
  ui <- htmltools::renderTags(mod_roster_contracts_ui("roster_contracts"))$html
  css <- paste(
    readLines(file.path(root, "inst", "app", "www", "tbi_ux_foundation.css"), warn = FALSE),
    collapse = "\n"
  )
  css_compact <- gsub("\\s+", " ", css)

  testthat::expect_equal(lengths(regmatches(ui, gregexpr("POSITION VALUE 2.0", ui, fixed = TRUE))), 1L)
  testthat::expect_equal(lengths(regmatches(ui, gregexpr("ROSTER COMPOSITION", ui, fixed = TRUE))), 1L)
  testthat::expect_equal(lengths(regmatches(ui, gregexpr('id="roster_contracts-position_value_scorecard"', ui, fixed = TRUE))), 1L)
  testthat::expect_equal(lengths(regmatches(ui, gregexpr('id="roster_contracts-roster_composition"', ui, fixed = TRUE))), 1L)

  testthat::expect_match(css, '.tbi-v2-roster-page[data-tbi-roster-active-tab="construction"] {', fixed = TRUE)
  testthat::expect_match(css, "grid-template-columns:minmax(0,1.08fr) minmax(360px,.92fr)", fixed = TRUE)
  testthat::expect_match(css, "gap:12px !important", fixed = TRUE)
  testthat::expect_match(css_compact, '> .tbi-v2-exec-main-grid { grid-column:1;', fixed = TRUE)
  testthat::expect_match(css_compact, '> .tbi-v2-cap-detail-grid { grid-column:2;', fixed = TRUE)
  testthat::expect_match(css, '[data-tbi-roster-active-tab="construction"] [id$="-roster_composition"]', fixed = TRUE)
  testthat::expect_match(css, "grid-template-columns:repeat(2,minmax(0,1fr))", fixed = TRUE)
  testthat::expect_match(css, '[data-tbi-roster-active-tab="construction"] .tbi-v2-scorecard-header', fixed = TRUE)
  testthat::expect_match(css, "min-height:54px", fixed = TRUE)
  testthat::expect_match(css, '@media (max-width:1100px)', fixed = TRUE)
  testthat::expect_match(css_compact, '[data-tbi-roster-active-tab="construction"] { grid-template-columns:minmax(0,1fr) !important;', fixed = TRUE)
})

testthat::test_that("live module filters loaded rosters and switches teams without stale exact filters", {
  team <- shiny::reactiveVal("Boston Celtics")
  season <- shiny::reactiveVal("2026-27")
  rendered_html <- function(value) {
    if (is.list(value) && !is.null(value$html)) value$html else as.character(value)
  }

  shiny::testServer(
    mod_roster_contracts_server,
    args = list(selected_team = team, selected_season = season),
    {
      session$setInputs(active_subtab = "roster")
      session$flushReact()

      boston <- selected_roster()
      testthat::expect_gt(nrow(boston), 0)
      testthat::expect_true(length(output$roster_table) > 0)

      session$setInputs(active_subtab = "overview")
      session$flushReact()
      decision_html <- rendered_html(output$roster_decision)
      headlines_html <- rendered_html(output$roster_headlines)
      testthat::expect_match(decision_html, "ROSTER SCORE", fixed = TRUE)
      testthat::expect_match(headlines_html, "players are currently represented", fixed = TRUE)

      session$setInputs(active_subtab = "construction")
      session$flushReact()
      boston_position_html <- rendered_html(output$position_value_scorecard)
      boston_composition_html <- rendered_html(output$roster_composition)
      testthat::expect_match(boston_position_html, ">PG<", fixed = TRUE)
      testthat::expect_match(boston_composition_html, "Standard roster", fixed = TRUE)

      session$setInputs(active_subtab = "assessment")
      session$flushReact()
      boston_assessment_html <- rendered_html(output$roster_assessment)
      testthat::expect_match(boston_assessment_html, paste0(nrow(selected_roster()), " players"), fixed = TRUE)
      testthat::expect_match(boston_assessment_html, "Primary concern", fixed = TRUE)

      session$setInputs(active_subtab = "roster")
      session$flushReact()

      first_position <- trimws(strsplit(as.character(boston$primary_position[[1]]), "[,/]")[[1]][[1]])
      session$setInputs(roster_position_filter = first_position)
      session$flushReact()
      position_rows <- filtered_roster()
      testthat::expect_gt(nrow(position_rows), 0)
      testthat::expect_true(all(roster_position_matches(position_rows$primary_position, first_position)))

      search_term <- strsplit(tolower(boston$player_name[[1]]), " ")[[1]][[1]]
      session$setInputs(roster_position_filter = "", roster_player_search = search_term)
      session$flushReact()
      testthat::expect_true(all(grepl(search_term, tolower(filtered_roster()$player_name), fixed = TRUE)))

      session$setInputs(roster_player_search = "", roster_contract_filter = "STALE TEAM VALUE")
      for (next_team in c("Oklahoma City Thunder", "Charlotte Hornets", "Cleveland Cavaliers")) {
        team(next_team)
        session$flushReact()
        current <- selected_roster()
        testthat::expect_gt(nrow(current), 0)
        testthat::expect_equal(nrow(filtered_roster()), nrow(current))
        testthat::expect_true(all(current$team_name == next_team))

        session$setInputs(active_subtab = "construction")
        session$flushReact()
        switched_position_html <- rendered_html(output$position_value_scorecard)
        switched_composition_html <- rendered_html(output$roster_composition)
        testthat::expect_match(switched_position_html, ">PG<", fixed = TRUE)
        testthat::expect_match(switched_composition_html, "Standard roster", fixed = TRUE)
        testthat::expect_match(switched_composition_html, paste0(">", nrow(standard_roster()), "</strong>"), fixed = TRUE)

        session$setInputs(active_subtab = "assessment")
        session$flushReact()
        switched_assessment_html <- rendered_html(output$roster_assessment)
        current_scores <- position_scores()
        strongest <- current_scores[which.max(current_scores$score), , drop = FALSE]
        weakest <- current_scores[which.min(current_scores$score), , drop = FALSE]
        testthat::expect_match(switched_assessment_html, paste0(nrow(current), " players"), fixed = TRUE)
        testthat::expect_match(switched_assessment_html, paste0(strongest$position[[1]], " has the strongest Position Value 2.0 score at ", round(strongest$score[[1]])), fixed = TRUE)
        testthat::expect_match(switched_assessment_html, paste0(weakest$position[[1]], " has the lowest Position Value 2.0 score at ", round(weakest$score[[1]])), fixed = TRUE)
      }

      session$setInputs(active_subtab = "overview")
      session$flushReact()
      switched_headlines_html <- rendered_html(output$roster_headlines)
      testthat::expect_match(switched_headlines_html, paste0(nrow(selected_roster()), " players"), fixed = TRUE)

      testthat::expect_false(identical(boston_position_html, switched_position_html))
      testthat::expect_false(identical(boston_assessment_html, switched_assessment_html))

      session$setInputs(active_subtab = "risk")
      session$flushReact()
      risk_html <- rendered_html(output$roster_risks)
      testthat::expect_match(risk_html, "Decision consequence", fixed = TRUE)

      session$setInputs(active_subtab = "assessment")
      session$flushReact()
      assessment_html <- rendered_html(output$roster_assessment)
      testthat::expect_match(assessment_html, "Control / flexibility", fixed = TRUE)
      testthat::expect_match(assessment_html, 'class="roster-assessment-readout"', fixed = TRUE)
    }
  )
})
