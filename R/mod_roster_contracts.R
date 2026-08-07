#' Roster and contracts UI Function
#' @noRd
mod_roster_contracts_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    class = "roster-intelligence-page",
    
    shiny::div(
      class = "roster-page-heading",
      shiny::div(class = "roster-page-eyebrow", "ROSTER CONSTRUCTION"),
      shiny::h2(class = "roster-page-title", "Roster Intelligence"),
      shiny::p(
        class = "roster-page-description",
        paste(
          "Evaluate roster balance, player timelines,",
          "contract structure, and positional depth."
        )
      )
    ),
    
    shiny::div(
      class = "roster-summary-grid",
      roster_summary_card(
        icon = "award",
        label = "ROSTER GRADE",
        value = shiny::textOutput(ns("roster_grade"), inline = TRUE),
        detail = shiny::textOutput(ns("roster_grade_detail"), inline = TRUE)
      ),
      roster_summary_card(
        icon = "calendar",
        label = "AVERAGE AGE",
        value = shiny::textOutput(ns("average_age"), inline = TRUE),
        detail = shiny::textOutput(ns("average_age_detail"), inline = TRUE)
      ),
      roster_summary_card(
        icon = "currency-dollar",
        label = "TOTAL PAYROLL",
        value = shiny::textOutput(ns("total_payroll"), inline = TRUE),
        detail = shiny::textOutput(ns("payroll_detail"), inline = TRUE)
      ),
      roster_summary_card(
        icon = "people-fill",
        label = "ACTIVE PLAYERS",
        value = shiny::textOutput(ns("active_players"), inline = TRUE),
        detail = shiny::textOutput(ns("active_players_detail"), inline = TRUE)
      )
    ),
    
    shiny::div(
      class = "roster-main-grid",
      shiny::div(
        class = "roster-panel roster-table-panel",
        shiny::div(
          class = "roster-panel-header",
          shiny::div(
            shiny::div(class = "roster-panel-eyebrow", "PERSONNEL"),
            shiny::h3(class = "roster-panel-title", "Active Roster")
          ),
          shiny::uiOutput(ns("roster_count_badge"))
        ),
        shiny::div(
          class = "roster-table-wrapper",
          shiny::tableOutput(ns("roster_table"))
        )
      ),
      
      shiny::div(
        class = "roster-panel roster-depth-panel",
        shiny::div(
          class = "roster-panel-header",
          shiny::div(
            shiny::div(class = "roster-panel-eyebrow", "ROTATION"),
            shiny::h3(class = "roster-panel-title", "Depth Chart")
          )
        ),
        shiny::uiOutput(ns("depth_chart_ui"))
      )
    ),
    
    shiny::div(
      class = "roster-bottom-grid",
      shiny::div(
        class = "roster-panel",
        shiny::div(
          class = "roster-panel-header",
          shiny::div(
            shiny::div(class = "roster-panel-eyebrow", "POSITIONAL ANALYSIS"),
            shiny::h3(class = "roster-panel-title", "Position Strength")
          )
        ),
        shiny::uiOutput(ns("position_strength_ui"))
      ),
      
      shiny::div(
        class = "roster-panel",
        shiny::div(
          class = "roster-panel-header",
          shiny::div(
            shiny::div(class = "roster-panel-eyebrow", "EXECUTIVE BRIEF"),
            shiny::h3(class = "roster-panel-title", "Roster Assessment")
          )
        ),
        shiny::uiOutput(ns("roster_assessment_ui"))
      )
    )
  )
}

#' Roster and contracts Server Functions
#' @noRd
mod_roster_contracts_server <- function(id, selected_team, selected_season) {
  shiny::moduleServer(id, function(input, output, session) {

    database_path <- function() resolve_tbi_db_path()

    money_label <- function(x) {
      x <- suppressWarnings(as.numeric(x))
      if (!length(x) || is.na(x)) return("--")
      if (abs(x) >= 1e6) return(sprintf("$%.1fM", x / 1e6))
      paste0("$", format(round(x), big.mark = ",", scientific = FALSE))
    }

    status_group <- function(status, two_way_flag = 0L, contract = "") {
      status_text <- tolower(trimws(ifelse(is.na(status), "", status)))
      contract_text <- tolower(trimws(ifelse(is.na(contract), "", contract)))
      if (isTRUE(as.integer(two_way_flag) == 1L) || grepl("two-way|two way", paste(status_text, contract_text))) return("Two-Way")
      if (grepl("exhibit 10|exhibit-10", paste(status_text, contract_text))) return("Exhibit 10")
      if (grepl("qualifying offer", paste(status_text, contract_text))) return("Qualifying Offer")
      if (grepl("restricted free agent|rfa", paste(status_text, contract_text))) return("Restricted Free Agent")
      "Standard"
    }

    selected_roster <- shiny::reactive({
      shiny::req(selected_team(), selected_season())
      con <- DBI::dbConnect(RSQLite::SQLite(), dbname = database_path())
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      season_start <- suppressWarnings(as.integer(substr(selected_season(), 1, 4)))
      roster <- DBI::dbGetQuery(
        con,
        "
        WITH current_contract AS (
          SELECT
            cy.player_id, cy.team_id, cy.season,
            MAX(COALESCE(cy.cap_hit, cy.base_salary, 0)) AS salary,
            MAX(c.contract_end_season) AS contract_end_season,
            MAX(c.free_agent_year) AS free_agent_year,
            GROUP_CONCAT(DISTINCT COALESCE(NULLIF(TRIM(c.contract_type), ''), NULLIF(TRIM(cy.option_type), ''), 'Standard')) AS contract
          FROM contract_years cy
          LEFT JOIN contracts c ON cy.contract_id = c.contract_id
          GROUP BY cy.player_id, cy.team_id, cy.season
        ),
        loaded_remaining AS (
          SELECT
            cy.player_id, cy.team_id,
            SUM(COALESCE(cy.guaranteed_amount, cy.cap_hit, cy.base_salary, 0)) AS loaded_money_remaining,
            COUNT(DISTINCT cy.season) AS loaded_years_remaining
          FROM contract_years cy
          WHERE CAST(SUBSTR(cy.season, 1, 4) AS INTEGER) >= ?
          GROUP BY cy.player_id, cy.team_id
        )
        SELECT
          t.team_name, t.abbreviation, rh.season,
          p.player_id, p.player_name AS player, p.primary_position AS position,
          p.height_inches, p.weight_lbs, p.player_age AS age,
          cc.salary, cc.contract, cc.contract_end_season, cc.free_agent_year,
          COALESCE(lr.loaded_money_remaining, cc.salary) AS loaded_money_remaining,
          COALESCE(lr.loaded_years_remaining, CASE WHEN cc.salary IS NULL THEN 0 ELSE 1 END) AS loaded_years_remaining,
          rh.roster_status AS status, rh.two_way_flag
        FROM roster_history rh
        INNER JOIN players p ON rh.player_id = p.player_id
        INNER JOIN teams t ON rh.team_id = t.team_id
        LEFT JOIN current_contract cc
          ON rh.player_id = cc.player_id AND rh.team_id = cc.team_id AND rh.season = cc.season
        LEFT JOIN loaded_remaining lr
          ON rh.player_id = lr.player_id AND rh.team_id = lr.team_id
        WHERE t.team_name = ? AND rh.season = ?
          AND LOWER(COALESCE(rh.roster_status, '')) IN (
            'active', 'two-way', 'two way', 'exhibit 10', 'qualifying offer',
            'restricted free agent', 'rfa'
          )
        ",
        params = list(season_start, selected_team(), selected_season())
      )

      if (nrow(roster) == 0) return(roster)
      roster$group <- mapply(status_group, roster$status, roster$two_way_flag, roster$contract, USE.NAMES = FALSE)
      roster$years_remaining <- ifelse(
        !is.na(roster$free_agent_year),
        pmax(0L, as.integer(roster$free_agent_year) - season_start),
        roster$loaded_years_remaining
      )
      roster$estimated_money_remaining <- ifelse(
        roster$loaded_years_remaining >= roster$years_remaining & roster$loaded_years_remaining > 0,
        roster$loaded_money_remaining,
        ifelse(roster$years_remaining > 0 & !is.na(roster$salary), roster$salary * roster$years_remaining, roster$loaded_money_remaining)
      )
      roster$remaining_quality <- ifelse(
        roster$loaded_years_remaining >= roster$years_remaining & roster$years_remaining > 0,
        "Loaded",
        "Estimated"
      )
      group_order <- match(roster$group, c("Standard", "Two-Way", "Exhibit 10", "Qualifying Offer", "Restricted Free Agent"))
      roster <- roster[order(group_order, -ifelse(is.na(roster$salary), -1, roster$salary), roster$player), , drop = FALSE]
      rownames(roster) <- NULL
      roster
    })

    normalized_position <- function(position) normalize_depth_position(position)

    depth_chart_data <- shiny::reactive({
      shiny::req(selected_team(), selected_season())
      positions <- c("PG", "SG", "SF", "PF", "C")
      records <- get_depth_chart_records(selected_team(), selected_season(), database_path())
      rows <- lapply(positions, function(pos) {
        pp <- records[records$position == pos, , drop = FALSE]
        data.frame(
          position = pos,
          starter = if (nrow(pp) >= 1) pp$player_name[[1]] else "No player available",
          reserve = if (nrow(pp) >= 2) pp$player_name[[2]] else "No reserve available",
          stringsAsFactors = FALSE
        )
      })
      do.call(rbind, rows)
    })

    output$active_players <- shiny::renderText(nrow(selected_roster()))
    output$active_players_detail <- shiny::renderText({
      n <- nrow(selected_roster())
      if (n == 0) "No roster data available" else paste(max(0, 18 - n), "roster openings")
    })
    output$average_age <- shiny::renderText({
      r <- selected_roster(); if (nrow(r)==0 || all(is.na(r$age))) "--" else sprintf("%.1f", mean(r$age, na.rm=TRUE))
    })
    output$average_age_detail <- shiny::renderText({
      r <- selected_roster(); if (nrow(r)==0 || all(is.na(r$age))) return("Age data not loaded")
      a <- mean(r$age, na.rm=TRUE); if (a < 25) "Young development timeline" else if (a < 28) "Prime competitive window" else "Veteran roster timeline"
    })
    output$total_payroll <- shiny::renderText({
      r <- selected_roster(); if (nrow(r)==0 || all(is.na(r$salary))) "--" else money_label(sum(r$salary, na.rm=TRUE))
    })
    output$payroll_detail <- shiny::renderText({
      r <- selected_roster(); if (nrow(r)==0 || all(is.na(r$salary))) return("Contract data not loaded")
      p <- sum(r$salary, na.rm=TRUE); if (p >= 180e6) "High payroll commitment" else if (p >= 140e6) "Moderate payroll flexibility" else "Strong payroll flexibility"
    })
    output$roster_grade <- shiny::renderText({
      r <- selected_roster(); if (nrow(r)==0) return("--"); if (all(is.na(r$age)) || all(is.na(r$salary))) return("N/A")
      a <- mean(r$age,na.rm=TRUE); p <- sum(r$salary,na.rm=TRUE); if (a>=24 && a<=28 && p>=140e6) "A-" else if (a<=29 && p>=110e6) "B+" else "B"
    })
    output$roster_grade_detail <- shiny::renderText({
      r <- selected_roster(); if (nrow(r)==0) return("Roster evaluation unavailable"); if (all(is.na(r$age)) || all(is.na(r$salary))) return("Awaiting age and contract data")
      if (mean(r$age,na.rm=TRUE)<=27) "Balanced competitive timeline" else "Veteran-focused construction"
    })
    output$roster_count_badge <- shiny::renderUI(shiny::span(class="roster-panel-badge", paste(nrow(selected_roster()), "Players")))

    output$roster_table <- shiny::renderTable({
      r <- selected_roster()
      shiny::validate(shiny::need(nrow(r)>0, paste("No roster data is currently available for", selected_team(), "during", selected_season())))
      data.frame(
        Player = r$player,
        Position = ifelse(is.na(r$position), "--", r$position),
        Age = ifelse(is.na(r$age), "--", as.character(r$age)),
        Salary = vapply(r$salary, money_label, character(1)),
        `Money Remaining` = paste0(vapply(r$estimated_money_remaining, money_label, character(1)), ifelse(r$remaining_quality=="Estimated", " est.", "")),
        `Years Left` = ifelse(r$years_remaining > 0, r$years_remaining, "--"),
        Contract = ifelse(is.na(r$contract) | trimws(as.character(r$contract))=="", "Not loaded", as.character(r$contract)),
        Status = r$group,
        check.names=FALSE, stringsAsFactors=FALSE
      )
    }, striped=FALSE, bordered=FALSE, hover=TRUE, spacing="s", width="100%")

    output$depth_chart_ui <- shiny::renderUI({
      d <- depth_chart_data()
      shiny::div(class="depth-chart", lapply(seq_len(nrow(d)), function(i) depth_chart_row(d$position[[i]], d$starter[[i]], d$reserve[[i]])))
    })

    output$position_strength_ui <- shiny::renderUI({
      r <- selected_roster(); if (nrow(r)==0) return(shiny::p("No positional data available."))
      pos_names <- c(PG="Point Guard", SG="Shooting Guard", SF="Small Forward", PF="Power Forward", C="Center")
      records <- get_depth_chart_records(selected_team(), selected_season(), database_path())
      evaluations <- lapply(names(pos_names), function(pos) {
        position_value_v2(
          position = pos,
          roster = r,
          depth_records = records,
          database_path = database_path()
        )
      })
      shiny::tagList(lapply(seq_along(pos_names), function(i) {
        e <- evaluations[[i]]
        shiny::div(
          class = "position-value-v2",
          position_strength_row(unname(pos_names[[i]]), e$score),
          shiny::div(
            class = "position-value-detail",
            shiny::span(e$tier),
            shiny::tags$small(e$explanation)
          )
        )
      }))
    })

    output$roster_assessment_ui <- shiny::renderUI({
      r <- selected_roster(); if (nrow(r)==0) return(shiny::tags$ul(class="roster-assessment-list",shiny::tags$li("No roster data is available for this selection.")))
      records <- get_depth_chart_records(selected_team(), selected_season(), database_path())
      evaluations <- lapply(c("PG", "SG", "SF", "PF", "C"), function(pos) position_value_v2(pos, r, records, database_path()))
      scores <- vapply(evaluations, function(x) x$score, numeric(1))
      names(scores) <- c("PG", "SG", "SF", "PF", "C")
      strongest <- names(scores)[which.max(scores)]; weakest <- names(scores)[which.min(scores)]
      estimated <- sum(r$remaining_quality == "Estimated", na.rm=TRUE)
      shiny::tags$ul(class="roster-assessment-list",
        shiny::tags$li(paste(nrow(r), "players are included across standard, two-way, Exhibit 10, and qualifying-offer categories.")),
        shiny::tags$li(paste(strongest, "has the highest Position Value 2.0 score at", round(scores[[strongest]]), "/ 100.")),
        shiny::tags$li(paste(weakest, "has the lowest Position Value 2.0 score at", round(scores[[weakest]]), "/ 100 and merits roster review.")),
        shiny::tags$li(if (estimated>0) paste(estimated, "remaining-money values are estimates until full future salary schedules are loaded.") else "Remaining contract commitments are fully loaded.")
      )
    })
  })
}


#' Position Value 2.0
#'
#' A transparent roster-construction score using the data currently available
#' in the platform. It intentionally avoids claiming on-court player quality
#' without a performance-statistics feed.
#' @noRd
position_value_v2 <- function(position, roster, depth_records, database_path = NULL) {
  position <- normalize_depth_position(position)
  valid <- c("PG", "SG", "SF", "PF", "C")
  if (!position %in% valid) return(list(score = 0, tier = "Unavailable", explanation = "Position data unavailable."))

  assigned <- depth_records[depth_records$position == position, , drop = FALSE]
  player_ids <- unique(assigned$player_id)
  group <- roster[roster$player_id %in% player_ids, , drop = FALSE]

  coverage <- if (nrow(assigned) >= 1) 30 else 0
  depth <- min(25, nrow(assigned) / 3 * 25)

  ages <- suppressWarnings(as.numeric(group$age))
  mean_age <- if (length(ages) && any(!is.na(ages))) mean(ages, na.rm = TRUE) else NA_real_
  age_score <- if (is.na(mean_age)) 7.5 else if (mean_age >= 23 && mean_age <= 28) 15 else if (mean_age <= 31) 11 else 7

  salaries <- suppressWarnings(as.numeric(group$salary))
  roster_salaries <- suppressWarnings(as.numeric(roster$salary))
  median_salary <- if (any(!is.na(roster_salaries))) stats::median(roster_salaries, na.rm = TRUE) else NA_real_
  contract_score <- if (!length(salaries) || all(is.na(salaries)) || is.na(median_salary) || median_salary <= 0) {
    7.5
  } else {
    group_salary <- sum(salaries, na.rm = TRUE)
    expected <- median_salary * max(1, nrow(group))
    ratio <- group_salary / expected
    if (ratio <= 1.35) 15 else if (ratio <= 2) 11 else 7
  }

  flexibility_score <- 0
  if (length(player_ids)) {
    path <- resolve_tbi_db_path(database_path %||% file.path("inst", "database", "tbi.sqlite"))
    con <- connect_db(db_path = path, read_only = TRUE)
    on.exit(disconnect_db(con), add = TRUE)
    marks <- paste(rep("?", length(player_ids)), collapse = ",")
    q <- DBI::dbGetQuery(con, paste0("SELECT player_id, COUNT(*) AS position_count FROM player_positions WHERE player_id IN (", marks, ") GROUP BY player_id"), params = as.list(as.integer(player_ids)))
    avg_positions <- if (nrow(q)) mean(q$position_count, na.rm = TRUE) else 1
    flexibility_score <- min(15, 7.5 + max(0, avg_positions - 1) * 5)
  }

  score <- round(min(100, coverage + depth + age_score + contract_score + flexibility_score))
  tier <- if (score >= 85) "Strong" else if (score >= 70) "Stable" else if (score >= 55) "Watch" else "Priority"
  explanation <- paste0(
    nrow(assigned), " assigned player", if (nrow(assigned) == 1) "" else "s",
    if (is.na(mean_age)) "; age data incomplete" else paste0("; average age ", sprintf("%.1f", mean_age)),
    "; score reflects coverage, depth, timeline, contract balance, and positional flexibility."
  )
  list(score = score, tier = tier, explanation = explanation)
}
