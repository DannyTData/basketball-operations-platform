# ============================================================
# Thompson's Basketball Intelligence
# Phase 15: Final 30-Team QA + NBA v1.0 Freeze Readiness
# ============================================================

phase15_text <- function(x, default = "") {
  if (is.null(x) || length(x) == 0L || is.na(x[[1]])) {
    return(default)
  }
  
  value <- trimws(as.character(x[[1]]))
  
  if (!nzchar(value)) {
    return(default)
  }
  
  value
}


phase15_number <- function(x, default = NA_real_) {
  if (is.null(x) || length(x) == 0L) {
    return(default)
  }
  
  value <- suppressWarnings(as.numeric(x[[1]]))
  
  if (!is.finite(value)) {
    return(default)
  }
  
  value
}


phase15_connect_readonly <- function() {
  if (!exists(
    "connect_db",
    mode = "function",
    inherits = TRUE
  )) {
    stop(
      "connect_db() is not available.",
      call. = FALSE
    )
  }
  
  tryCatch(
    connect_db(
      read_only = TRUE
    ),
    error = function(e) {
      connect_db()
    }
  )
}


phase15_latest_depth_season <- function() {
  con <- phase15_connect_readonly()
  
  on.exit(
    disconnect_db(con),
    add = TRUE
  )
  
  tables <- DBI::dbListTables(con)
  
  if (!"depth_chart" %in% tables) {
    stop(
      "The depth_chart table is missing.",
      call. = FALSE
    )
  }
  
  seasons <- DBI::dbGetQuery(
    con,
    "
      SELECT
        season,
        COUNT(*) AS player_rows,
        COUNT(DISTINCT team_id) AS team_count
      FROM depth_chart
      GROUP BY season
      ORDER BY season DESC
    "
  )
  
  if (!nrow(seasons)) {
    stop(
      "The depth_chart table contains no seasons.",
      call. = FALSE
    )
  }
  
  complete <- seasons[
    suppressWarnings(
      as.integer(
        seasons$team_count
      )
    ) >= 30L,
    ,
    drop = FALSE
  ]
  
  chosen <- if (nrow(complete)) {
    complete[1, , drop = FALSE]
  } else {
    seasons[1, , drop = FALSE]
  }
  
  as.character(
    chosen$season[[1]]
  )
}


phase15_active_teams <- function() {
  if (!exists(
    "get_teams",
    mode = "function",
    inherits = TRUE
  )) {
    stop(
      "get_teams() is not available.",
      call. = FALSE
    )
  }
  
  teams <- get_teams()
  
  if (
    "is_active" %in% names(teams)
  ) {
    active <- suppressWarnings(
      as.integer(
        teams$is_active
      )
    )
    
    keep <- is.na(active) | active == 1L
    teams <- teams[keep, , drop = FALSE]
  }
  
  required <- c(
    "team_id",
    "team_name",
    "abbreviation"
  )
  
  missing <- setdiff(
    required,
    names(teams)
  )
  
  if (length(missing)) {
    stop(
      paste0(
        "Teams data is missing required fields: ",
        paste(
          missing,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  teams <- teams[
    order(
      teams$team_name
    ),
    ,
    drop = FALSE
  ]
  
  rownames(teams) <- NULL
  teams
}


phase15_required_functions <- function() {
  c(
    # database / roster
    "connect_db",
    "disconnect_db",
    "get_teams",
    "get_depth_chart_records",
    
    # basketball intelligence
    "evaluate_bie_players",
    
    # Phase 8
    "build_minute_allocation",
    
    # Phase 9
    "build_lineup_optimization",
    "lineup_optimization_table",
    
    # Phase 10
    "build_scenario_comparison",
    
    # front-office engines
    "evaluate_two_team_trade",
    "evaluate_extension_proposal",
    "get_draft_assets",
    "simulate_team_draft_portfolio",
    
    # app shell
    "app_ui",
    "app_server"
  )
}


phase15_function_audit <- function() {
  functions <- phase15_required_functions()
  
  data.frame(
    function_name = functions,
    loaded = vapply(
      functions,
      exists,
      mode = "function",
      inherits = TRUE,
      FUN.VALUE = logical(1)
    ),
    stringsAsFactors = FALSE
  )
}


phase15_required_tables <- function() {
  c(
    "teams",
    "players",
    "roster_history",
    "depth_chart",
    "contract_years",
    "contracts",
    "player_season_stats",
    "player_season_advanced",
    "player_season_shooting",
    "player_season_playmaking",
    "player_season_defense_rebounding",
    "player_season_roles",
    "player_season_impact",
    "player_projection_intelligence"
  )
}


phase15_database_audit <- function() {
  con <- phase15_connect_readonly()
  
  on.exit(
    disconnect_db(con),
    add = TRUE
  )
  
  tables <- DBI::dbListTables(con)
  required <- phase15_required_tables()
  
  data.frame(
    table_name = required,
    present = required %in% tables,
    stringsAsFactors = FALSE
  )
}


phase15_required_source_files <- function() {
  c(
    "DESCRIPTION",
    file.path("R", "app_ui.R"),
    file.path("R", "app_server.R"),
    file.path("R", "database.R"),
    file.path("R", "cap_engine.R"),
    file.path("R", "trade_engine.R"),
    file.path("R", "extension_engine.R"),
    file.path("R", "draft_assets_engine.R"),
    file.path("R", "draft_value_engine.R"),
    file.path("R", "draft_simulation_engine.R"),
    file.path("R", "basketball_intelligence_engine.R"),
    file.path("R", "minute_allocation_engine.R"),
    file.path("R", "lineup_optimization_engine.R"),
    file.path("R", "scenario_comparison_engine.R"),
    file.path("R", "roster_intelligence_integration.R"),
    file.path("R", "performance_optimization.R"),
    file.path("R", "ui_polish_phase14.R"),
    file.path("R", "mod_depth_chart.R"),
    file.path("inst", "app", "www", "tbi_phase14.css"),
    file.path("inst", "database", "tbi.sqlite")
  )
}


phase15_source_file_audit <- function() {
  files <- phase15_required_source_files()
  
  data.frame(
    file = files,
    present = file.exists(files),
    stringsAsFactors = FALSE
  )
}


phase15_lineup_types <- function(lineup_result) {
  table <- lineup_optimization_table(
    lineup_result
  )
  
  if (
    is.null(table) ||
    !is.data.frame(table) ||
    nrow(table) == 0L ||
    !"lineup_type" %in% names(table)
  ) {
    return(character())
  }
  
  unique(
    toupper(
      trimws(
        as.character(
          table$lineup_type
        )
      )
    )
  )
}


phase15_expected_lineup_types <- function() {
  c(
    "BALANCED",
    "OFFENSE",
    "DEFENSE",
    "CLOSING"
  )
}


phase15_run_team_qa <- function(team_name,
                                abbreviation,
                                season,
                                rotation_size = 10L) {
  start_time <- proc.time()[["elapsed"]]
  
  result <- list(
    team_name = team_name,
    abbreviation = abbreviation,
    season = season,
    roster_rows = 0L,
    bie_rows = 0L,
    minute_total = NA_real_,
    rotation_size = NA_integer_,
    lineup_types = "",
    lineup_type_count = 0L,
    roster_pass = FALSE,
    bie_pass = FALSE,
    minutes_pass = FALSE,
    lineups_pass = FALSE,
    overall_pass = FALSE,
    elapsed_seconds = NA_real_,
    error = ""
  )
  
  tryCatch(
    {
      roster <- get_depth_chart_records(
        team_value = team_name,
        season = season
      )
      
      result$roster_rows <- if (
        is.data.frame(roster)
      ) {
        nrow(roster)
      } else {
        0L
      }
      
      result$roster_pass <-
        is.data.frame(roster) &&
        nrow(roster) >= 8L &&
        all(
          c(
            "player_id",
            "player_name",
            "position",
            "depth_order",
            "is_starter"
          ) %in% names(roster)
        )
      
      if (!result$roster_pass) {
        stop(
          "Roster/depth-chart validation failed.",
          call. = FALSE
        )
      }
      
      bie <- evaluate_bie_players(
        roster
      )
      
      result$bie_rows <- if (
        is.data.frame(bie)
      ) {
        nrow(bie)
      } else {
        0L
      }
      
      result$bie_pass <-
        is.data.frame(bie) &&
        nrow(bie) >= 8L &&
        "bie_player_score" %in% names(bie)
      
      if (!result$bie_pass) {
        stop(
          "BIE roster evaluation failed.",
          call. = FALSE
        )
      }
      
      # Phase 8's public input contract uses bie_rating, while the
      # production Basketball Intelligence Engine exposes
      # bie_player_score. Preserve the production field and provide
      # the Phase 8 compatibility alias only inside this QA pipeline.
      if (!"bie_rating" %in% names(bie)) {
        bie$bie_rating <- suppressWarnings(
          as.numeric(
            bie$bie_player_score
          )
        )
      }
      
      use_rotation_size <- min(
        as.integer(rotation_size),
        nrow(bie)
      )
      
      use_rotation_size <- max(
        8L,
        use_rotation_size
      )
      
      minutes <- build_minute_allocation(
        roster = bie,
        rotation_size = use_rotation_size,
        total_minutes = 240L
      )
      
      result$minute_total <- sum(
        suppressWarnings(
          as.numeric(
            minutes$allocation$
              recommended_minutes
          )
        ),
        na.rm = TRUE
      )
      
      result$rotation_size <-
        suppressWarnings(
          as.integer(
            minutes$summary$
              rotation_size
          )
        )
      
      result$minutes_pass <-
        identical(
          as.numeric(
            result$minute_total
          ),
          240
        ) &&
        isTRUE(
          result$rotation_size >= 8L
        )
      
      if (!result$minutes_pass) {
        stop(
          "Minute allocation did not satisfy the 240-minute invariant.",
          call. = FALSE
        )
      }
      
      lineups <- build_lineup_optimization(
        roster_or_allocation = minutes,
        pool_size = use_rotation_size
      )
      
      lineup_types <-
        phase15_lineup_types(
          lineups
        )
      
      result$lineup_types <- paste(
        lineup_types,
        collapse = " | "
      )
      
      result$lineup_type_count <-
        length(
          lineup_types
        )
      
      expected <-
        phase15_expected_lineup_types()
      
      result$lineups_pass <-
        all(
          expected %in%
            lineup_types
        )
      
      if (!result$lineups_pass) {
        stop(
          paste0(
            "Missing lineup type(s): ",
            paste(
              setdiff(
                expected,
                lineup_types
              ),
              collapse = ", "
            )
          ),
          call. = FALSE
        )
      }
      
      result$overall_pass <- TRUE
    },
    error = function(e) {
      result$error <<-
        conditionMessage(e)
      
      result$overall_pass <<- FALSE
    }
  )
  
  result$elapsed_seconds <- round(
    proc.time()[["elapsed"]] -
      start_time,
    3
  )
  
  as.data.frame(
    result,
    stringsAsFactors = FALSE
  )
}


phase15_run_30_team_qa <- function(season = NULL,
                                   rotation_size = 10L,
                                   write_report = TRUE,
                                   report_dir = "qa") {
  if (is.null(season)) {
    season <-
      phase15_latest_depth_season()
  }
  
  teams <- phase15_active_teams()
  
  if (nrow(teams) != 30L) {
    stop(
      paste0(
        "Expected 30 active NBA teams but found ",
        nrow(teams),
        "."
      ),
      call. = FALSE
    )
  }
  
  rows <- lapply(
    seq_len(nrow(teams)),
    function(i) {
      phase15_run_team_qa(
        team_name =
          teams$team_name[[i]],
        abbreviation =
          teams$abbreviation[[i]],
        season = season,
        rotation_size =
          rotation_size
      )
    }
  )
  
  team_results <- do.call(
    rbind,
    rows
  )
  
  function_audit <-
    phase15_function_audit()
  
  database_audit <-
    phase15_database_audit()
  
  source_audit <-
    phase15_source_file_audit()
  
  summary <- list(
    season = season,
    teams_tested = nrow(team_results),
    teams_passed = sum(
      team_results$overall_pass
    ),
    teams_failed = sum(
      !team_results$overall_pass
    ),
    all_teams_pass =
      all(
        team_results$overall_pass
      ),
    functions_pass =
      all(
        function_audit$loaded
      ),
    database_pass =
      all(
        database_audit$present
      ),
    source_files_pass =
      all(
        source_audit$present
      )
  )
  
  summary$overall_pass <-
    isTRUE(
      summary$teams_tested == 30L
    ) &&
    isTRUE(
      summary$all_teams_pass
    ) &&
    isTRUE(
      summary$functions_pass
    ) &&
    isTRUE(
      summary$database_pass
    ) &&
    isTRUE(
      summary$source_files_pass
    )
  
  output <- list(
    summary = summary,
    teams = team_results,
    functions = function_audit,
    database = database_audit,
    source_files = source_audit
  )
  
  if (isTRUE(write_report)) {
    dir.create(
      report_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )
    
    utils::write.csv(
      team_results,
      file.path(
        report_dir,
        "phase15_30_team_qa.csv"
      ),
      row.names = FALSE
    )
    
    utils::write.csv(
      function_audit,
      file.path(
        report_dir,
        "phase15_function_audit.csv"
      ),
      row.names = FALSE
    )
    
    utils::write.csv(
      database_audit,
      file.path(
        report_dir,
        "phase15_database_audit.csv"
      ),
      row.names = FALSE
    )
    
    utils::write.csv(
      source_audit,
      file.path(
        report_dir,
        "phase15_source_file_audit.csv"
      ),
      row.names = FALSE
    )
    
    summary_lines <- c(
      "THOMPSON'S BASKETBALL INTELLIGENCE",
      "PHASE 15 — FINAL 30-TEAM QA",
      "",
      paste0(
        "Season: ",
        summary$season
      ),
      paste0(
        "Teams tested: ",
        summary$teams_tested
      ),
      paste0(
        "Teams passed: ",
        summary$teams_passed
      ),
      paste0(
        "Teams failed: ",
        summary$teams_failed
      ),
      paste0(
        "Function audit: ",
        if (
          summary$functions_pass
        ) "PASS" else "FAIL"
      ),
      paste0(
        "Database audit: ",
        if (
          summary$database_pass
        ) "PASS" else "FAIL"
      ),
      paste0(
        "Source-file audit: ",
        if (
          summary$source_files_pass
        ) "PASS" else "FAIL"
      ),
      paste0(
        "OVERALL: ",
        if (
          summary$overall_pass
        ) "PASS" else "FAIL"
      )
    )
    
    writeLines(
      summary_lines,
      con = file.path(
        report_dir,
        "phase15_summary.txt"
      )
    )
  }
  
  output
}


phase15_freeze_manifest <- function(qa_result,
                                    version = "NBA-v1.0") {
  if (
    is.null(qa_result) ||
    is.null(
      qa_result$summary
    ) ||
    !isTRUE(
      qa_result$summary$
      overall_pass
    )
  ) {
    stop(
      "NBA v1.0 cannot be frozen because Phase 15 QA is not a clean PASS.",
      call. = FALSE
    )
  }
  
  list(
    product =
      "Thompson's Basketball Intelligence — NBA Basketball Operations Platform",
    version = version,
    season =
      qa_result$summary$season,
    teams_verified =
      qa_result$summary$teams_tested,
    teams_passed =
      qa_result$summary$teams_passed,
    status = "FROZEN",
    freeze_standard =
      "Phase 15 clean 30-team QA",
    minute_invariant =
      "240 regulation minutes",
    lineup_profiles =
      paste(
        phase15_expected_lineup_types(),
        collapse = ", "
      )
  )
}