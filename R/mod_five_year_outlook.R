#' Five-year outlook UI Function
#' @noRd
mod_five_year_outlook_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "tbi-module-page",
    shiny::div(
      class = "tbi-page-header",
      shiny::div(
        shiny::div(class = "tbi-page-eyebrow", "LONG-RANGE PLANNING"),
        shiny::h2("Five-Year Outlook"),
        shiny::p("Track contract control, salary commitments, and the projected competitive window across five seasons.")
      ),
      shiny::div(class = "tbi-page-icon", bsicons::bs_icon("graph-up-arrow"))
    ),

    bslib::layout_columns(
      col_widths = c(3, 3, 3, 3),
      shiny::div(class = "tbi-framework-kpi", shiny::span("CURRENT PAYROLL"), shiny::strong(shiny::textOutput(ns("current_payroll"), inline = TRUE))),
      shiny::div(class = "tbi-framework-kpi", shiny::span("PLAYERS CONTROLLED 3+ YRS"), shiny::strong(shiny::textOutput(ns("long_control"), inline = TRUE))),
      shiny::div(class = "tbi-framework-kpi", shiny::span("NEXT FA WAVE"), shiny::strong(shiny::textOutput(ns("next_fa_wave"), inline = TRUE))),
      shiny::div(class = "tbi-framework-kpi", shiny::span("WINDOW READ"), shiny::strong(shiny::textOutput(ns("window_read"), inline = TRUE)))
    ),

    bslib::layout_columns(
      col_widths = c(8, 4),
      shiny::div(
        class = "tbi-panel tbi-framework-panel",
        shiny::div(class = "tbi-panel-kicker", "FIVE-SEASON CONTROL MAP"),
        shiny::h3("Tracked commitments and contracts under control"),
        shiny::p("Future salary is estimated from currently tracked contracts. Only the selected season contains verified contract-year salary values."),
        shiny::uiOutput(ns("outlook_bars")),
        reactable::reactableOutput(ns("outlook_table"))
      ),
      shiny::div(
        class = "tbi-panel tbi-framework-panel",
        shiny::div(class = "tbi-panel-kicker", "COMPETITIVE WINDOW"),
        shiny::h3("Front-office readout"),
        shiny::uiOutput(ns("window_signals")),
        shiny::hr(),
        shiny::tags$small("Framework status: contract-control logic is live. Future cap thresholds, draft capital, and player-performance forecasts will be layered in after verified datasets are loaded.")
      )
    )
  )
}

#' Five-year outlook Server Functions
#' @noRd
mod_five_year_outlook_server <- function(id, selected_team, selected_season) {
  shiny::moduleServer(id, function(input, output, session) {

    outlook_source <- shiny::reactive({
      shiny::req(selected_team(), selected_season())
      con <- connect_db()
      on.exit(disconnect_db(con), add = TRUE)
      DBI::dbGetQuery(
        con,
        "
        SELECT p.player_name, p.player_age,
               COALESCE(cy.cap_hit, cy.base_salary, 0) AS current_salary,
               c.contract_end_season, c.free_agent_year
        FROM roster_history rh
        JOIN players p ON p.player_id = rh.player_id
        JOIN teams t ON t.team_id = rh.team_id
        LEFT JOIN contract_years cy
          ON cy.player_id = rh.player_id AND cy.team_id = rh.team_id AND cy.season = rh.season
        LEFT JOIN contracts c ON c.contract_id = cy.contract_id
        WHERE t.team_name = ? AND rh.season = ?
        ORDER BY current_salary DESC, p.player_name
        ",
        params = list(selected_team(), selected_season())
      )
    })

    season_start <- shiny::reactive({
      suppressWarnings(as.integer(substr(selected_season(), 1, 4)))
    })

    outlook <- shiny::reactive({
      d <- outlook_source()
      start <- season_start()
      shiny::req(!is.na(start))
      seasons <- paste0(start:(start + 4), "-", substr((start + 1):(start + 5), 3, 4))
      end_year <- suppressWarnings(as.integer(substr(d$contract_end_season, 1, 4)))
      current_total <- sum(d$current_salary, na.rm = TRUE)
      current_count <- nrow(d)

      rows <- lapply(seq_along(seasons), function(i) {
        yr <- start + i - 1
        controlled <- sum(!is.na(end_year) & end_year >= yr)
        ratio <- if (current_count > 0) controlled / current_count else 0
        estimated <- if (i == 1) current_total else current_total * ratio
        data.frame(Season = seasons[[i]], EstimatedCommitment = estimated, PlayersControlled = controlled, ControlRate = ratio)
      })
      do.call(rbind, rows)
    })

    money <- function(x) {
      x <- suppressWarnings(as.numeric(x))
      if (!length(x) || is.na(x)) return("$0")
      if (abs(x) >= 1e6) return(sprintf("$%.1fM", x / 1e6))
      paste0("$", format(round(x), big.mark = ",", scientific = FALSE))
    }

    output$current_payroll <- shiny::renderText(money(sum(outlook_source()$current_salary, na.rm = TRUE)))

    output$long_control <- shiny::renderText({
      d <- outlook_source()
      start <- season_start()
      end_year <- suppressWarnings(as.integer(substr(d$contract_end_season, 1, 4)))
      sum(!is.na(end_year) & end_year >= start + 2)
    })

    output$next_fa_wave <- shiny::renderText({
      fa <- outlook_source()$free_agent_year
      fa <- fa[!is.na(fa)]
      if (!length(fa)) return("—")
      counts <- table(fa)
      names(counts)[which.max(counts)]
    })

    output$window_read <- shiny::renderText({
      d <- outlook()
      rate3 <- d$ControlRate[pmin(3, nrow(d))]
      if (rate3 >= .65) "Stable core" else if (rate3 >= .40) "Transition point" else "High flexibility"
    })

    output$outlook_bars <- shiny::renderUI({
      d <- outlook()
      max_value <- max(d$EstimatedCommitment, na.rm = TRUE)
      bars <- lapply(seq_len(nrow(d)), function(i) {
        width <- if (max_value > 0) 100 * d$EstimatedCommitment[[i]] / max_value else 0
        shiny::div(
          class = "tbi-outlook-row",
          shiny::div(class = "tbi-outlook-label", shiny::strong(d$Season[[i]]), shiny::span(paste(d$PlayersControlled[[i]], "players"))),
          shiny::div(class = "tbi-outlook-track", shiny::div(class = "tbi-outlook-fill", style = sprintf("width: %.1f%%;", width))),
          shiny::div(class = "tbi-outlook-value", money(d$EstimatedCommitment[[i]]))
        )
      })
      do.call(shiny::tagList, bars)
    })

    output$outlook_table <- reactable::renderReactable({
      d <- outlook()
      display <- data.frame(
        Season = d$Season,
        `Estimated Tracked Commitment` = vapply(d$EstimatedCommitment, money, character(1)),
        `Players Under Control` = d$PlayersControlled,
        `Roster Control` = sprintf("%.0f%%", 100 * d$ControlRate),
        check.names = FALSE
      )
      reactable::reactable(
        display, pagination = FALSE, highlight = TRUE,
        theme = reactable::reactableTheme(
          backgroundColor = "transparent", color = "#f5f8ff", borderColor = "#20334f",
          headerStyle = list(backgroundColor = "#0b1728", color = "#83a7d8")
        )
      )
    })

    output$window_signals <- shiny::renderUI({
      d <- outlook_source()
      start <- season_start()
      end_year <- suppressWarnings(as.integer(substr(d$contract_end_season, 1, 4)))
      expiring_one <- sum(!is.na(end_year) & end_year <= start)
      controlled_three <- sum(!is.na(end_year) & end_year >= start + 2)
      controlled_five <- sum(!is.na(end_year) & end_year >= start + 4)
      ages <- suppressWarnings(as.numeric(d$player_age))
      avg_age <- if (any(!is.na(ages))) mean(ages, na.rm = TRUE) else NA_real_

      shiny::tagList(
        shiny::div(class = "tbi-signal-row", shiny::span("Current-season expirations"), shiny::strong(expiring_one)),
        shiny::div(class = "tbi-signal-row", shiny::span("Controlled through Year 3"), shiny::strong(controlled_three)),
        shiny::div(class = "tbi-signal-row", shiny::span("Controlled through Year 5"), shiny::strong(controlled_five)),
        shiny::div(class = "tbi-signal-row", shiny::span("Current average age"), shiny::strong(if (is.na(avg_age)) "—" else sprintf("%.1f", avg_age)))
      )
    })
  })
}
