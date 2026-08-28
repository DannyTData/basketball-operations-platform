testthat::test_that("Extension presentation translates canonical statuses without changing them", {
  testthat::expect_match(extension_ui_status_label("ELIGIBLE_WITH_REVIEW"), "Eligible.*Review Required")
  testthat::expect_identical(extension_ui_status_label("NOT_ELIGIBLE"), "Not Eligible")
  testthat::expect_match(extension_ui_status_label("PASS_WITH_REVIEW"), "Pass.*Review Required")
  testthat::expect_identical(extension_ui_status_label("FAIL"), "Fail")
})

testthat::test_that("Extension manual facts use a strict tri-state contract", {
  testthat::expect_identical(extension_manual_fact_state(NULL), "UNKNOWN")
  testthat::expect_identical(extension_manual_fact_state(NA_character_), "UNKNOWN")
  testthat::expect_identical(extension_manual_fact_state("unexpected"), "UNKNOWN")
  testthat::expect_identical(extension_manual_fact_state(TRUE), "MET")
  testthat::expect_identical(extension_manual_fact_state(FALSE), "NOT_MET")
  testthat::expect_identical(extension_manual_fact_state("met"), "MET")
  testthat::expect_identical(extension_manual_fact_state("not_met"), "NOT_MET")

  veteran <- extension_manual_fact_contract(
    list(
      timing_window_open = "UNKNOWN",
      designated_rookie_qualified = "MET"
    ),
    "veteran"
  )
  testthat::expect_false(veteran$resolved)
  testthat::expect_identical(veteran$unresolved, "timing_window_open")
  testthat::expect_false(veteran$flags[["designated_rookie_qualified"]])

  rookie <- extension_manual_fact_contract(
    list(
      is_first_round_pick = "NOT_MET",
      rookie_options_exercised = "MET",
      timing_window_open = "MET",
      designated_veteran_qualified = "MET"
    ),
    "rookie_scale"
  )
  testthat::expect_true(rookie$resolved)
  testthat::expect_false(rookie$flags[["is_first_round_pick"]])
  testthat::expect_true(rookie$flags[["rookie_options_exercised"]])
  testthat::expect_false(rookie$flags[["designated_veteran_qualified"]])
})

testthat::test_that("Extension service years use explicit facts and never age or a silent default", {
  early <- data.frame(nba_service_years = 1L, player_age = 21L)
  veteran <- data.frame(nba_service_years = 11L, player_age = 34L)
  missing <- data.frame(player_age = 29L)

  testthat::expect_identical(extension_service_years_from_row(early), 1L)
  testthat::expect_identical(extension_service_years_from_row(veteran), 11L)
  testthat::expect_true(is.na(extension_service_years_from_row(missing)))
  testthat::expect_true(is.na(extension_service_years_from_row(data.frame(player_age = 40L))))
  testthat::expect_true(is.na(extension_manual_input_defaults()$service_years))
  testthat::expect_identical(extension_service_year_state("UNKNOWN"), NA_integer_)
  testthat::expect_identical(extension_service_year_state("1"), 1L)
  testthat::expect_identical(extension_service_year_state("11"), 11L)
})

testthat::test_that("Extension pending analysis lists only current unresolved source facts", {
  pending <- structure(
    list(
      status = "REVIEW",
      passes_screen = NA,
      missing_fields = c(
        "service_years",
        "timing_window_open",
        "designated_veteran_qualified"
      )
    ),
    class = c("tbi_extension_review", "tbi_extension_error")
  )

  facts <- extension_analysis_pending_facts(pending)
  testthat::expect_identical(
    facts$field,
    c(
      "service_years",
      "timing_window_open",
      "designated_veteran_qualified"
    )
  )
  testthat::expect_identical(
    facts$label,
    c(
      "NBA service years",
      "Applicable extension signing window",
      "Designated-veteran award qualification"
    )
  )
  testthat::expect_identical(
    facts$state,
    c("UNKNOWN", "REQUIRES SOURCE VERIFICATION", "UNKNOWN")
  )
  testthat::expect_false(any(grepl("original-team", facts$label, fixed = TRUE)))
})

testthat::test_that("Extension UI is compact, responsive, and keeps the four-workspace contract", {
  html <- htmltools::renderTags(mod_extension_simulator_ui("extension_v2"))$html
  testthat::expect_match(html, "ext-verify-options", fixed = TRUE)
  fact_ids <- c(
    "is_first_round_pick",
    "rookie_options_exercised",
    "timing_window_open",
    "designated_rookie_qualified",
    "designated_veteran_qualified",
    "original_team_requirement_met"
  )
  for (fact_id in fact_ids) {
    testthat::expect_match(
      html,
      sprintf('id="extension_v2-%s"', fact_id),
      fixed = TRUE
    )
  }
  testthat::expect_match(html, 'value="UNKNOWN"', fixed = TRUE)
  testthat::expect_match(html, 'id="extension_v2-service_years"', fixed = TRUE)
  testthat::expect_match(html, "Service years are not loaded from the current authoritative database", fixed = TRUE)
  testthat::expect_match(html, 'value="MET"', fixed = TRUE)
  testthat::expect_match(html, 'value="NOT_MET"', fixed = TRUE)
  testthat::expect_match(html, "ext-financial-grid", fixed = TRUE)
  testthat::expect_match(html, "ext-recommendation-brief", fixed = TRUE)
  testthat::expect_match(html, "View rule explanations and verification items", fixed = TRUE)
  testthat::expect_match(html, "View BIE value and timeline details", fixed = TRUE)
  testthat::expect_match(html, 'id="extension_v2-financial_analysis_pending"', fixed = TRUE)
  testthat::expect_match(html, 'id="extension_v2-recommendation_analysis_pending"', fixed = TRUE)
  testthat::expect_match(
    html,
    'data-display-if="output.extension_analysis_pending_flag === &#39;pending&#39;"',
    fixed = TRUE
  )
  ready_condition <-
    'data-display-if="output.extension_analysis_pending_flag === &#39;ready&#39;"'
  testthat::expect_equal(
    lengths(regmatches(
      html,
      gregexpr(ready_condition, html, fixed = TRUE)
    )),
    7L
  )
  testthat::expect_match(
    html,
    paste0(
      ready_condition,
      '[^>]*data-tbi-extension-section="proposed-extension-schedule"'
    ),
    perl = TRUE
  )
  testthat::expect_match(
    html,
    paste0(
      ready_condition,
      '[^>]*data-tbi-extension-section="front-office-readout"'
    ),
    perl = TRUE
  )

  recommendation <- regexpr("recommended-contract-action", html, fixed = TRUE)[[1L]]
  risks <- regexpr("contract-risks", html, fixed = TRUE)[[1L]]
  testthat::expect_lt(recommendation, risks)

  javascript <- paste(readLines(testthat::test_path("..", "..", "inst", "app", "www", "tbi_ux_foundation.js"), warn = FALSE), collapse = "\n")
  extension_javascript <- sub(
    "^[\\s\\S]*TBI_EXTENSION_SIMULATOR_TABS_START >>>",
    "",
    javascript,
    perl = TRUE
  )
  extension_javascript <- sub(
    "<<< TBI_EXTENSION_SIMULATOR_TABS_END <<<[\\s\\S]*$",
    "",
    extension_javascript,
    perl = TRUE
  )
  testthat::expect_match(extension_javascript, "tbi-extension-tab-", fixed = TRUE)
  testthat::expect_match(extension_javascript, "aria-controls", fixed = TRUE)
  testthat::expect_match(extension_javascript, "ArrowRight", fixed = TRUE)
  testthat::expect_match(extension_javascript, "data-tbi-extension-active-tab", fixed = TRUE)
})

testthat::test_that("Extension live state supports review, fail dominance, schedule reconciliation, and player switching", {
  selected_team <- shiny::reactiveVal("Boston Celtics")
  selected_season <- shiny::reactiveVal("2026-27")

  shiny::testServer(
    mod_extension_simulator_server,
    args = list(selected_team = selected_team, selected_season = selected_season),
    {
      rendered_html <- function(value) {
        if (is.null(value)) return("")
        if (is.list(value) && "html" %in% names(value)) {
          return(paste(value$html, collapse = ""))
        }
        paste(as.character(value), collapse = "")
      }

      pool <- player_pool()
      testthat::expect_gte(nrow(pool), 3L)
      player_ids <- as.character(utils::head(pool$player_id, 3L))
      seen <- character()

      for (player_id in player_ids) {
        session$setInputs(
          player_id = player_id,
          extension_type = "veteran",
          guarantee_structure = "Fully guaranteed",
          starting_salary_m = 1,
          years = 2,
          raise_pct = 5,
          first_season = "2027-28"
        )
        session$setInputs(
          service_years = 6,
          remaining_years = 1,
          timing_window_open = "MET"
        )

        result <- extension_result()
        testthat::expect_false(inherits(result, "tbi_extension_error"))
        testthat::expect_true(result$status %in% c("PASS", "PASS_WITH_REVIEW"))
        testthat::expect_equal(sum(result$schedule$salary), result$schedule_summary$total_value)

        player_name <- selected_player_row()$player_name[[1L]]
        player_html <- rendered_html(output$player_card)
        recommendation_html <- rendered_html(output$executive_recommendation)
        testthat::expect_match(player_html, player_name, fixed = TRUE)
        testthat::expect_match(recommendation_html, player_name, fixed = TRUE)
        testthat::expect_match(recommendation_html, "REVIEW REQUIRED|ADVANCE")
        testthat::expect_match(recommendation_html, "REVIEW REQUIRED BECAUSE", fixed = TRUE)
        testthat::expect_match(recommendation_html, "Confirm official signing dates and extension window.", fixed = TRUE)
        testthat::expect_match(recommendation_html, "Confirm option, guarantee, bonus, and trade-kicker language before approval.", fixed = TRUE)
        testthat::expect_false(grepl("award-based qualification", recommendation_html, fixed = TRUE))
        seen <- c(seen, player_name)
      }

      testthat::expect_length(unique(seen), 3L)

      session$setInputs(starting_salary_m = 100, years = 5, raise_pct = 20)
      failed <- extension_result()
      testthat::expect_false(failed$passes_screen)
      testthat::expect_true(length(failed$failures) > 0L)
      testthat::expect_true(any(grepl("starting salary exceeds", failed$failures, ignore.case = TRUE)))

      decision_html <- rendered_html(output$extension_decision)
      recommendation_html <- rendered_html(output$executive_recommendation)
      testthat::expect_match(decision_html, "DO NOT ADVANCE", fixed = TRUE)
      testthat::expect_match(recommendation_html, "DO NOT ADVANCE", fixed = TRUE)
      testthat::expect_match(recommendation_html, "Reduce starting salary", fixed = TRUE)
    }
  )
})

testthat::test_that("Extension manual facts review when unknown and reset at every context boundary", {
  selected_team <- shiny::reactiveVal("Boston Celtics")
  selected_season <- shiny::reactiveVal("2026-27")

  shiny::testServer(
    mod_extension_simulator_server,
    args = list(selected_team = selected_team, selected_season = selected_season),
    {
      rendered_html <- function(value) {
        if (is.null(value)) return("")
        if (is.list(value) && "html" %in% names(value)) {
          return(paste(value$html, collapse = ""))
        }
        paste(as.character(value), collapse = "")
      }
      pool <- player_pool()
      testthat::expect_gte(nrow(pool), 2L)
      player_ids <- as.character(utils::head(pool$player_id, 2L))

      session$setInputs(player_id = player_ids[[1L]])
      session$flushReact()
      session$setInputs(
        service_years = 9,
        remaining_years = 2,
        timing_window_open = "UNKNOWN",
        extension_type = "veteran",
        guarantee_structure = "Fully guaranteed",
        starting_salary_m = 1,
        years = 2,
        raise_pct = 5,
        first_season = "2027-28"
      )

      pending <- extension_result()
      testthat::expect_true(inherits(pending, "tbi_extension_review"))
      testthat::expect_identical(pending$status, "REVIEW")
      testthat::expect_identical(pending$missing_fields, "timing_window_open")
      testthat::expect_true(extension_analysis_is_pending(pending))
      testthat::expect_identical(output$extension_analysis_pending_flag, "pending")

      financial_pending_html <- rendered_html(output$financial_analysis_pending)
      recommendation_pending_html <- rendered_html(output$recommendation_analysis_pending)

      for (pending_html in c(financial_pending_html, recommendation_pending_html)) {
        testthat::expect_equal(
          lengths(regmatches(
            pending_html,
            gregexpr("EXTENSION ANALYSIS PENDING", pending_html, fixed = TRUE)
          )),
          1L
        )
        testthat::expect_match(
          pending_html,
          "The modeled extension cannot be calculated until required eligibility and timing facts are verified.",
          fixed = TRUE
        )
        testthat::expect_match(
          pending_html,
          "Applicable extension signing window",
          fixed = TRUE
        )
        testthat::expect_match(
          pending_html,
          "REQUIRES SOURCE VERIFICATION",
          fixed = TRUE
        )
        testthat::expect_false(grepl("NBA service years", pending_html, fixed = TRUE))
        testthat::expect_false(grepl("Designated-veteran", pending_html, fixed = TRUE))
        testthat::expect_match(pending_html, "NEXT ACTION", fixed = TRUE)
        testthat::expect_match(
          pending_html,
          "Complete the required source verification on the Proposal tab.",
          fixed = TRUE
        )
        testthat::expect_false(grepl("REVIEW REQUIRED", pending_html, fixed = TRUE))
      }

      testthat::expect_identical(output$snapshot_total_value, "")
      testthat::expect_match(
        rendered_html(output$extension_schedule),
        '"x":null',
        fixed = TRUE
      )
      testthat::expect_identical(rendered_html(output$contract_readout), "")
      testthat::expect_identical(rendered_html(output$executive_recommendation), "")
      testthat::expect_identical(rendered_html(output$extension_risks), "")
      testthat::expect_identical(rendered_html(output$extension_opportunities), "")
      testthat::expect_null(bie_extension_value_result())

      session$setInputs(
        timing_window_open = "MET",
        designated_rookie_qualified = "MET"
      )
      resolved <- extension_result()
      testthat::expect_false(inherits(resolved, "tbi_extension_error"))
      testthat::expect_false(extension_analysis_is_pending(resolved))
      testthat::expect_identical(output$extension_analysis_pending_flag, "ready")
      testthat::expect_false(extension_player()$designated_rookie_qualified)
      testthat::expect_equal(
        sum(resolved$schedule$salary),
        resolved$schedule_summary$total_value
      )
      testthat::expect_identical(
        output$snapshot_total_value,
        money(resolved$schedule_summary$total_value)
      )
      testthat::expect_false(is.null(output$extension_schedule))
      testthat::expect_match(
        rendered_html(output$contract_readout),
        "Total contract value",
        fixed = TRUE
      )
      testthat::expect_match(
        rendered_html(output$executive_recommendation),
        "ext-recommendation-brief",
        fixed = TRUE
      )

      session$setInputs(
        extension_type = "rookie_scale",
        service_years = 3,
        is_first_round_pick = "NOT_MET",
        rookie_options_exercised = "MET",
        timing_window_open = "MET"
      )
      not_met <- extension_result()
      testthat::expect_false(not_met$passes_screen)
      testthat::expect_true(any(grepl("first-round", not_met$failures, fixed = TRUE)))

      session$setInputs(timing_window_open = "UNKNOWN")
      known_blocker_with_unknown_fact <- extension_result()
      testthat::expect_identical(known_blocker_with_unknown_fact$status, "FAIL")
      testthat::expect_false(known_blocker_with_unknown_fact$passes_screen)
      testthat::expect_identical(
        known_blocker_with_unknown_fact$reason_code,
        "MODELED_ELIGIBILITY_FAILURE"
      )
      testthat::expect_true(
        "timing_window_open" %in% known_blocker_with_unknown_fact$missing_fields
      )
      testthat::expect_true(any(grepl(
        "first-round",
        known_blocker_with_unknown_fact$failures,
        fixed = TRUE
      )))
      testthat::expect_false(extension_analysis_is_pending(known_blocker_with_unknown_fact))
      testthat::expect_identical(output$extension_analysis_pending_flag, "ready")
      testthat::expect_match(
        rendered_html(output$extension_decision),
        "DO NOT ADVANCE",
        fixed = TRUE
      )
      testthat::expect_match(
        rendered_html(output$executive_recommendation),
        "DO NOT ADVANCE",
        fixed = TRUE
      )

      session$setInputs(
        service_years = "UNKNOWN",
        timing_window_open = "MET"
      )
      known_blocker_with_unknown_service <- extension_result()
      testthat::expect_identical(known_blocker_with_unknown_service$status, "FAIL")
      testthat::expect_false(known_blocker_with_unknown_service$passes_screen)
      testthat::expect_identical(
        known_blocker_with_unknown_service$reason_code,
        "MODELED_ELIGIBILITY_FAILURE"
      )
      testthat::expect_true(
        "service_years" %in% known_blocker_with_unknown_service$missing_fields
      )
      testthat::expect_true(any(grepl(
        "first-round",
        known_blocker_with_unknown_service$failures,
        fixed = TRUE
      )))
      testthat::expect_identical(output$snapshot_result, "FAIL")
      testthat::expect_identical(output$scorecard_result, "FAIL")
      testthat::expect_false(extension_analysis_is_pending(known_blocker_with_unknown_service))
      testthat::expect_identical(output$extension_analysis_pending_flag, "ready")
      testthat::expect_match(
        rendered_html(output$extension_decision),
        "DO NOT ADVANCE",
        fixed = TRUE
      )
      testthat::expect_match(
        rendered_html(output$executive_recommendation),
        "DO NOT ADVANCE",
        fixed = TRUE
      )
      testthat::expect_false(grepl(
        "REVIEW REQUIRED",
        rendered_html(output$extension_decision),
        fixed = TRUE
      ))

      session$setInputs(
        extension_type = "veteran",
        service_years = 9,
        remaining_years = 2,
        timing_window_open = "MET"
      )
      before_switch <- manual_extension_inputs()
      testthat::expect_identical(before_switch$service_years, 9L)
      testthat::expect_identical(before_switch$timing_window_open, "MET")

      session$setInputs(
        player_id = player_ids[[2L]],
        timing_window_open = "NOT_MET"
      )
      after_player <- manual_extension_inputs()
      testthat::expect_true(is.na(after_player$service_years))
      testthat::expect_identical(after_player$remaining_years, 1L)
      testthat::expect_true(all(
        unlist(after_player[extension_manual_fact_ids()], use.names = FALSE) ==
          "UNKNOWN"
      ))
      testthat::expect_true(inherits(extension_result(), "tbi_extension_review"))
      switched_pending_html <- rendered_html(output$financial_analysis_pending)
      testthat::expect_match(switched_pending_html, "NBA service years", fixed = TRUE)
      testthat::expect_match(
        switched_pending_html,
        "Applicable extension signing window",
        fixed = TRUE
      )

      session$setInputs(service_years = 8)
      after_single_edit <- manual_extension_inputs()
      testthat::expect_identical(after_single_edit$service_years, 8L)
      testthat::expect_identical(after_single_edit$timing_window_open, "UNKNOWN")
      switched_fact_html <- rendered_html(output$financial_analysis_pending)
      testthat::expect_false(grepl("NBA service years", switched_fact_html, fixed = TRUE))
      testthat::expect_match(
        switched_fact_html,
        "Applicable extension signing window",
        fixed = TRUE
      )

      session$setInputs(timing_window_open = "MET")
      selected_team("Charlotte Hornets")
      session$flushReact()
      after_team <- manual_extension_inputs()
      testthat::expect_true(is.na(after_team$service_years))
      testthat::expect_identical(after_team$timing_window_open, "UNKNOWN")

      session$setInputs(service_years = 7, timing_window_open = "MET")
      selected_season("2099-00")
      session$flushReact()
      after_season <- manual_extension_inputs()
      testthat::expect_true(is.na(after_season$service_years))
      testthat::expect_identical(after_season$timing_window_open, "UNKNOWN")
    }
  )
})

testthat::test_that("Extension eligibility blockers dominate unknown facts without financial inputs", {
  facts <- extension_manual_fact_contract(
    list(
      is_first_round_pick = "NOT_MET",
      rookie_options_exercised = "MET",
      timing_window_open = "UNKNOWN"
    ),
    "rookie_scale"
  )
  permissive_flags <- facts$flags
  permissive_flags[facts$unresolved] <- TRUE

  failure <- extension_known_eligibility_failure(
    list(
      service_years = 3L,
      remaining_contract_years = 1L,
      contract_type = "rookie scale",
      is_first_round_pick = permissive_flags[["is_first_round_pick"]],
      rookie_option_years_exercised = permissive_flags[["rookie_options_exercised"]],
      timing_window_open = permissive_flags[["timing_window_open"]],
      contract_allows_extension = TRUE
    ),
    "rookie_scale"
  )

  testthat::expect_identical(facts$states[["timing_window_open"]], "UNKNOWN")
  testthat::expect_identical(failure$status, "FAIL")
  testthat::expect_false(failure$passes_screen)
  testthat::expect_identical(
    failure$reason_code,
    "MODELED_ELIGIBILITY_FAILURE"
  )
  testthat::expect_true(any(grepl("first-round", failure$failures, fixed = TRUE)))
  testthat::expect_null(failure$schedule)
  testthat::expect_null(failure$starting_salary_screen)
})
