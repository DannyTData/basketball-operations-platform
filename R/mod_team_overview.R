#' Team overview UI Function
#' @noRd
mod_team_overview_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "tbi-module-page",
    shiny::div(
      class = "tbi-page-header",
      shiny::div(
        shiny::div(class = "tbi-page-eyebrow", "ORGANIZATIONAL SNAPSHOT"),
        shiny::h2("Team Overview"),
        shiny::p("A concise front-office view of team identity, roster construction, and organizational direction.")
      ),
      shiny::div(class = "tbi-page-icon", bsicons::bs_icon("building"))
    ),

    bslib::layout_columns(
      col_widths = c(3, 3, 3, 3),
      shiny::div(class = "tbi-framework-kpi", shiny::span("ACTIVE ROSTER"), shiny::strong(shiny::textOutput(ns("roster_count"), inline = TRUE))),
      shiny::div(class = "tbi-framework-kpi", shiny::span("TEAM PAYROLL"), shiny::strong(shiny::textOutput(ns("team_payroll"), inline = TRUE))),
      shiny::div(class = "tbi-framework-kpi", shiny::span("AVERAGE AGE"), shiny::strong(shiny::textOutput(ns("average_age"), inline = TRUE))),
      shiny::div(class = "tbi-framework-kpi", shiny::span("HIGHEST PAID"), shiny::strong(shiny::textOutput(ns("highest_paid"), inline = TRUE)))
    ),

    bslib::layout_columns(
      col_widths = c(5, 7),
      shiny::div(
        class = "tbi-panel tbi-framework-panel",
        shiny::div(class = "tbi-panel-kicker", "TEAM IDENTITY"),
        shiny::h3(shiny::textOutput(ns("team_heading"), inline = TRUE)),
        shiny::uiOutput(ns("team_identity")),
        shiny::hr(),
        shiny::div(class = "tbi-panel-kicker", "ORGANIZATIONAL DIRECTION"),
        shiny::uiOutput(ns("direction_readout"))
      ),
      shiny::div(
        class = "tbi-panel tbi-framework-panel",
        shiny::div(class = "tbi-panel-kicker", "EXECUTIVE SUMMARY"),
        shiny::h3("Current roster construction read"),
        shiny::uiOutput(ns("executive_summary")),
        shiny::hr(),
        shiny::div(class = "tbi-panel-kicker", "TOP SALARY COMMITMENTS"),
        reactable::reactableOutput(ns("top_contracts"))
      )
    )
  )
}

#' Team overview Server Functions
#' @noRd
mod_team_overview_server <- function(id, selected_team, selected_season) {
  shiny::moduleServer(id, function(input, output, session) {

    overview_data <- shiny::reactive({
      shiny::req(selected_team(), selected_season())
      con <- connect_db()
      on.exit(disconnect_db(con), add = TRUE)

      team <- DBI::dbGetQuery(
        con,
        "SELECT team_name, abbreviation, conference, division FROM teams WHERE team_name = ? LIMIT 1",
        params = list(selected_team())
      )

      roster <- DBI::dbGetQuery(
        con,
        "
        SELECT p.player_name, p.primary_position, p.player_age,
               COALESCE(cy.cap_hit, cy.base_salary, 0) AS cap_hit,
               c.contract_end_season, c.free_agent_year
        FROM roster_history rh
        JOIN players p ON p.player_id = rh.player_id
        JOIN teams t ON t.team_id = rh.team_id
        LEFT JOIN contract_years cy
          ON cy.player_id = rh.player_id AND cy.team_id = rh.team_id AND cy.season = rh.season
        LEFT JOIN contracts c ON c.contract_id = cy.contract_id
        WHERE t.team_name = ? AND rh.season = ?
        ORDER BY cap_hit DESC, p.player_name
        ",
        params = list(selected_team(), selected_season())
      )

      list(team = team, roster = roster)
    })

    money <- function(x) {
      x <- suppressWarnings(as.numeric(x))
      if (!length(x) || is.na(x)) return("$0")
      if (abs(x) >= 1e6) return(sprintf("$%.1fM", x / 1e6))
      if (abs(x) >= 1e3) return(sprintf("$%.0fK", x / 1e3))
      paste0("$", format(round(x), big.mark = ",", scientific = FALSE))
    }

    output$roster_count <- shiny::renderText(nrow(overview_data()$roster))

    output$team_payroll <- shiny::renderText({
      money(sum(overview_data()$roster$cap_hit, na.rm = TRUE))
    })

    output$average_age <- shiny::renderText({
      ages <- suppressWarnings(as.numeric(overview_data()$roster$player_age))
      ages <- ages[!is.na(ages)]
      if (!length(ages)) return("—")
      sprintf("%.1f", mean(ages))
    })

    output$highest_paid <- shiny::renderText({
      d <- overview_data()$roster
      if (!nrow(d)) return("—")
      d$player_name[which.max(d$cap_hit)]
    })

    output$team_heading <- shiny::renderText({
      team <- overview_data()$team
      if (!nrow(team)) selected_team() else paste0(team$team_name, " · ", selected_season())
    })

    output$team_identity <- shiny::renderUI({
      team <- overview_data()$team
      shiny::validate(shiny::need(nrow(team) > 0, "Team identity is unavailable."))
      shiny::tagList(
        shiny::div(class = "tbi-signal-row", shiny::span("Abbreviation"), shiny::strong(team$abbreviation[[1]])),
        shiny::div(class = "tbi-signal-row", shiny::span("Conference"), shiny::strong(team$conference[[1]])),
        shiny::div(class = "tbi-signal-row", shiny::span("Division"), shiny::strong(team$division[[1]])),
        shiny::div(class = "tbi-signal-row", shiny::span("Season"), shiny::strong(selected_season()))
      )
    })

    output$direction_readout <- shiny::renderUI({
      d <- overview_data()$roster
      total <- sum(d$cap_hit, na.rm = TRUE)
      top3 <- if (total > 0) sum(utils::head(d$cap_hit, 3), na.rm = TRUE) / total else 0
      ages <- suppressWarnings(as.numeric(d$player_age))
      avg_age <- if (any(!is.na(ages))) mean(ages, na.rm = TRUE) else NA_real_
      direction <- if (!is.na(avg_age) && avg_age <= 25.5) "Development window" else if (top3 >= .55) "Star-concentrated build" else "Balanced competitive build"
      shiny::div(class = "tbi-callout", shiny::strong(direction), shiny::p("A lightweight classification based on roster age and salary concentration."))
    })

    output$executive_summary <- shiny::renderUI({
      d <- overview_data()$roster
      total <- sum(d$cap_hit, na.rm = TRUE)
      top3_share <- if (total > 0) 100 * sum(utils::head(d$cap_hit, 3), na.rm = TRUE) / total else 0
      expirations <- sum(!is.na(d$free_agent_year) & d$free_agent_year <= 2028)
      ages <- suppressWarnings(as.numeric(d$player_age))
      avg_age <- if (any(!is.na(ages))) mean(ages, na.rm = TRUE) else NA_real_

      shiny::tags$ul(
        class = "tbi-roster-assessment-list",
        shiny::tags$li(sprintf("The current roster carries %s in tracked salary commitments.", money(total))),
        shiny::tags$li(sprintf("The three largest contracts account for %.1f%% of tracked payroll.", top3_share)),
        shiny::tags$li(sprintf("%s players are scheduled to reach free agency by 2028.", expirations)),
        shiny::tags$li(if (is.na(avg_age)) "Roster age data is incomplete." else sprintf("The roster's average age is %.1f years.", avg_age))
      )
    })

    output$top_contracts <- reactable::renderReactable({
      d <- utils::head(overview_data()$roster, 5)
      shiny::validate(shiny::need(nrow(d) > 0, "No roster records are available."))
      display <- data.frame(
        Player = d$player_name,
        Position = d$primary_position,
        `Cap Hit` = vapply(d$cap_hit, money, character(1)),
        `Contract Through` = ifelse(is.na(d$contract_end_season), "—", d$contract_end_season),
        check.names = FALSE
      )
      reactable::reactable(
        display,
        pagination = FALSE,
        highlight = TRUE,
        theme = reactable::reactableTheme(
          backgroundColor = "transparent", color = "#f5f8ff", borderColor = "#20334f",
          headerStyle = list(backgroundColor = "#0b1728", color = "#83a7d8")
        )
      )
    })
  })
}
