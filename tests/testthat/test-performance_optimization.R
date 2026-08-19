# ============================================================
# Phase 13 Tests: Performance + Loading Optimization
# ============================================================

testthat::test_that(
  "roster signature is stable to row order",
  {
    d <- data.frame(
      player_id = c(2, 1),
      player_name = c("B", "A"),
      position = c("SG", "PG"),
      stringsAsFactors = FALSE
    )
    
    testthat::expect_identical(
      phase13_roster_signature(d),
      phase13_roster_signature(
        d[c(2, 1), , drop = FALSE]
      )
    )
  }
)


testthat::test_that(
  "roster signature changes with basketball inputs",
  {
    d <- data.frame(
      player_id = 1:2,
      player_name = c("A", "B"),
      bie_rating = c(80, 70)
    )
    
    d2 <- d
    d2$bie_rating[[2]] <- 75
    
    testthat::expect_false(
      identical(
        phase13_roster_signature(d),
        phase13_roster_signature(d2)
      )
    )
  }
)


testthat::test_that(
  "trade signature tracks incoming and outgoing players",
  {
    s1 <- list(
      team = "A",
      partner_team = "B",
      outgoing_players =
        data.frame(player_id = 1),
      incoming_players =
        data.frame(player_id = 2),
      outgoing_salary = 10,
      incoming_salary = 12
    )
    
    s2 <- s1
    s2$incoming_players <-
      data.frame(player_id = 3)
    
    testthat::expect_false(
      identical(
        phase13_trade_signature(s1),
        phase13_trade_signature(s2)
      )
    )
  }
)


testthat::test_that(
  "cache stores and retrieves exact key",
  {
    cache <- phase13_cache_new()
    
    cache <- phase13_cache_store(
      cache,
      "abc",
      list(value = 1)
    )
    
    testthat::expect_equal(
      phase13_cache_get(
        cache,
        "abc"
      )$value,
      1
    )
    
    testthat::expect_null(
      phase13_cache_get(
        cache,
        "xyz"
      )
    )
  }
)


testthat::test_that(
  "performance status calculates hit rate",
  {
    a <- phase13_cache_new()
    b <- phase13_cache_new()
    
    a$hits <- 3L
    a$misses <- 1L
    b$hits <- 1L
    b$misses <- 1L
    
    status <-
      phase13_performance_status(
        list(a, b)
      )
    
    testthat::expect_equal(
      status$hits,
      4L
    )
    
    testthat::expect_equal(
      status$misses,
      2L
    )
    
    testthat::expect_equal(
      status$hit_rate,
      4 / 6
    )
  }
)


performance_project_root <- function() {
  candidates <- c(".", "..", "../..", "../../..")

  for (candidate in candidates) {
    if (file.exists(file.path(candidate, "DESCRIPTION"))) {
      return(
        normalizePath(
          candidate,
          winslash = "/",
          mustWork = TRUE
        )
      )
    }
  }

  stop(
    "Could not locate project root from performance tests.",
    call. = FALSE
  )
}


performance_source_block <- function(path,
                                     start_pattern,
                                     end_pattern) {
  lines <- readLines(path, warn = FALSE)
  start <- grep(start_pattern, lines, fixed = TRUE)[[1]]
  later_ends <- grep(end_pattern, lines, fixed = TRUE)
  end <- later_ends[later_ends > start][[1]]

  paste(
    lines[start:(end - 1L)],
    collapse = "\n"
  )
}


testthat::test_that(
  "league payroll rankings select the current team in memory",
  {
    rankings <- data.frame(
      team_name = c("Alpha", "Beta", "Gamma"),
      rank = c(1L, 2L, 3L),
      stringsAsFactors = FALSE
    )

    testthat::expect_identical(
      phase13_payroll_rank_for_team(rankings, "Beta"),
      2L
    )
    testthat::expect_identical(
      phase13_payroll_rank_for_team(rankings, "Missing"),
      NA_integer_
    )
    testthat::expect_identical(
      phase13_payroll_rank_for_team(data.frame(), "Alpha"),
      NA_integer_
    )
  }
)


testthat::test_that(
  "loaded team metadata preserves abbreviation fallback behavior",
  {
    teams <- data.frame(
      team_name = c("Alpha Club", "Beta Club"),
      abbreviation = c("ALP", NA_character_),
      stringsAsFactors = FALSE
    )

    testthat::expect_identical(
      phase13_team_abbreviation(teams, "Alpha Club"),
      "ALP"
    )
    testthat::expect_identical(
      phase13_team_abbreviation(teams, "Beta Club"),
      NA_character_
    )
    testthat::expect_identical(
      phase13_team_abbreviation(teams, "Gamma Club"),
      "Gam"
    )
  }
)


testthat::test_that(
  "modules reuse league rankings and loaded team metadata",
  {
    root <- performance_project_root()
    executive <- paste(
      readLines(
        file.path(root, "R", "mod_executive_dashboard.R"),
        warn = FALSE
      ),
      collapse = "\n"
    )
    trade <- paste(
      readLines(
        file.path(root, "R", "mod_trade_analyzer.R"),
        warn = FALSE
      ),
      collapse = "\n"
    )

    testthat::expect_false(
      grepl("get_team_payroll_rank(", executive, fixed = TRUE)
    )
    testthat::expect_equal(
      lengths(regmatches(executive, gregexpr("get_payroll_rankings()", executive, fixed = TRUE))),
      1L
    )
    testthat::expect_equal(
      lengths(regmatches(trade, gregexpr("get_teams()", trade, fixed = TRUE))),
      1L
    )
    testthat::expect_equal(
      lengths(regmatches(trade, gregexpr("phase13_team_abbreviation(", trade, fixed = TRUE))),
      2L
    )
  }
)


testthat::test_that(
  "full-league standings reads do not depend on current team",
  {
    root <- performance_project_root()
    executive <- performance_source_block(
      file.path(root, "R", "mod_executive_dashboard.R"),
      "standings_table <- shiny::reactive({",
      "team_data <- shiny::reactive({"
    )
    overview <- performance_source_block(
      file.path(root, "R", "mod_team_overview.R"),
      "standings_table <- shiny::reactive({",
      "team_data <- shiny::reactive({"
    )

    testthat::expect_false(
      grepl("selected_team()", executive, fixed = TRUE)
    )
    testthat::expect_false(
      grepl("selected_team()", overview, fixed = TRUE)
    )
  }
)


testthat::test_that(
  "stable base reads use session cache keys and scenario overlays do not",
  {
    root <- performance_project_root()
    cache_contracts <- list(
      mod_roster_contracts.R = c(
        "base_selected_roster <- shiny::bindCache(",
        "selected_team()",
        "selected_season()",
        'cache = "session"'
      ),
      mod_team_overview.R = c(
        "base_roster_contracts <- shiny::bindCache(",
        "selected_team()",
        "current_season()",
        'cache = "session"'
      ),
      mod_player_manager.R = c(
        "base_player_pool <- shiny::bindCache(",
        "selected_team()",
        "selected_season()",
        'cache = "session"'
      ),
      mod_salary_cap.R = c(
        "base_salary_data <- shiny::bindCache(",
        "selected_team()",
        "selected_season()",
        'cache = "session"'
      )
    )

    for (file_name in names(cache_contracts)) {
      source <- paste(
        readLines(
          file.path(root, "R", file_name),
          warn = FALSE
        ),
        collapse = "\n"
      )

      for (contract in cache_contracts[[file_name]]) {
        testthat::expect_true(
          grepl(contract, source, fixed = TRUE),
          info = paste(file_name, "is missing", contract)
        )
      }
    }

    source <- paste(
      readLines(
        file.path(root, "R", "mod_player_manager.R"),
        warn = FALSE
      ),
      collapse = "\n"
    )
    testthat::expect_false(
      grepl(
        "(?m)^\\s*player_pool\\s*<-\\s*shiny::bindCache\\(",
        source,
        perl = TRUE
      )
    )
  }
)


testthat::test_that(
  "sub-tab state is namespaced and gates only selected heavy outputs",
  {
    root <- performance_project_root()
    module_files <- c(
      "mod_executive_dashboard.R",
      "mod_team_overview.R",
      "mod_player_manager.R",
      "mod_roster_contracts.R"
    )

    for (file_name in module_files) {
      source <- paste(
        readLines(
          file.path(root, "R", file_name),
          warn = FALSE
        ),
        collapse = "\n"
      )

      testthat::expect_true(
        grepl(
          '`data-tbi-subtab-input` = ns("active_subtab")',
          source,
          fixed = TRUE
        ),
        info = paste(file_name, "is missing its namespaced sub-tab input")
      )
      testthat::expect_true(
        grepl("subtab_ready <- function", source, fixed = TRUE),
        info = paste(file_name, "is missing its server-side gate")
      )
    }

    javascript <- paste(
      readLines(
        file.path(root, "inst", "app", "www", "tbi_ux_foundation.js"),
        warn = FALSE
      ),
      collapse = "\n"
    )

    testthat::expect_true(
      grepl("notifySubtab: notifySubtab", javascript, fixed = TRUE)
    )
    testthat::expect_true(
      grepl("window.TBIUX.notifySubtab(page, tabName);", javascript, fixed = TRUE)
    )
    testthat::expect_true(
      grepl("window.TBIUX.notifySubtab(page, tab);", javascript, fixed = TRUE)
    )
  }
)
