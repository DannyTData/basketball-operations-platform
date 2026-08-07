#' Extension simulator UI Function
#' @noRd
mod_extension_simulator_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "tbi-module-page",
    shiny::div(
      class = "tbi-page-header",
      shiny::div(
        shiny::div(class = "tbi-page-eyebrow", "CONTRACT DECISIONS"),
        shiny::h2("Extension Simulator"),
        shiny::p("Build a simple extension structure and review its annual salary path and total commitment.")
      ),
      shiny::div(class = "tbi-page-icon", bsicons::bs_icon("calculator"))
    ),

    bslib::layout_columns(
      col_widths = c(4, 8),
      shiny::div(
        class = "tbi-panel tbi-framework-panel",
        shiny::div(class = "tbi-panel-kicker", "SCENARIO INPUTS"),
        shiny::selectInput(ns("player"), "Player", choices = character(0)),
        shiny::sliderInput(ns("years"), "Extension years", min = 1, max = 5, value = 4, step = 1),
        shiny::numericInput(ns("starting_salary"), "Year 1 salary ($)", value = 25000000, min = 0, step = 500000),
        shiny::sliderInput(ns("raise_pct"), "Annual raise", min = 0, max = 8, value = 8, step = 0.5, post = "%"),
        shiny::selectInput(ns("guarantee"), "Guarantee structure", choices = c("Fully guaranteed", "Final year team option", "Final year player option", "Partial guarantee")),
        shiny::tags$small("Framework model only. Eligibility, maximum-salary rules, designated-player rules, and official cap percentages require verified CBA inputs.")
      ),
      shiny::div(
        bslib::layout_columns(
          col_widths = c(4, 4, 4),
          shiny::div(class = "tbi-framework-kpi", shiny::span("TOTAL VALUE"), shiny::strong(shiny::textOutput(ns("total_value"), inline = TRUE))),
          shiny::div(class = "tbi-framework-kpi", shiny::span("AVERAGE SALARY"), shiny::strong(shiny::textOutput(ns("average_salary"), inline = TRUE))),
          shiny::div(class = "tbi-framework-kpi", shiny::span("FINAL-YEAR SALARY"), shiny::strong(shiny::textOutput(ns("final_salary"), inline = TRUE)))
        ),
        shiny::div(
          class = "tbi-panel tbi-framework-panel",
          shiny::div(class = "tbi-panel-kicker", "YEAR-BY-YEAR STRUCTURE"),
          shiny::h3(shiny::textOutput(ns("scenario_title"), inline = TRUE)),
          reactable::reactableOutput(ns("extension_table")),
          shiny::uiOutput(ns("extension_readout"))
        )
      )
    )
  )
}

#' Extension simulator Server Functions
#' @noRd
mod_extension_simulator_server <- function(id, selected_team, selected_season) {
  shiny::moduleServer(id, function(input, output, session) {

    roster <- shiny::reactive({
      shiny::req(selected_team(), selected_season())
      con <- connect_db()
      on.exit(disconnect_db(con), add = TRUE)
      DBI::dbGetQuery(
        con,
        "
        SELECT p.player_name, COALESCE(cy.cap_hit, cy.base_salary, 0) AS current_salary
        FROM roster_history rh
        JOIN players p ON p.player_id = rh.player_id
        JOIN teams t ON t.team_id = rh.team_id
        LEFT JOIN contract_years cy
          ON cy.player_id = rh.player_id AND cy.team_id = rh.team_id AND cy.season = rh.season
        WHERE t.team_name = ? AND rh.season = ?
        ORDER BY current_salary DESC, p.player_name
        ",
        params = list(selected_team(), selected_season())
      )
    })

    shiny::observeEvent(roster(), {
      d <- roster()
      shiny::updateSelectInput(session, "player", choices = d$player_name, selected = if (nrow(d)) d$player_name[[1]] else character(0))
      if (nrow(d)) shiny::updateNumericInput(session, "starting_salary", value = round(max(d$current_salary[[1]], 1000000), -5))
    }, ignoreInit = FALSE)

    shiny::observeEvent(input$player, {
      d <- roster()
      shiny::req(input$player, nrow(d) > 0)
      selected <- d[d$player_name == input$player, , drop = FALSE]
      if (nrow(selected) > 0 && !is.na(selected$current_salary[[1]]) && selected$current_salary[[1]] > 0) {
        shiny::updateNumericInput(
          session,
          "starting_salary",
          value = round(selected$current_salary[[1]], -5)
        )
      }
    }, ignoreInit = TRUE)

    salary_schedule <- shiny::reactive({
      shiny::req(input$years, input$starting_salary, input$raise_pct)
      years <- seq_len(input$years)
      salary <- input$starting_salary * (1 + input$raise_pct / 100)^(years - 1)
      data.frame(Year = years, Salary = salary, stringsAsFactors = FALSE)
    })

    money <- function(x) {
      x <- suppressWarnings(as.numeric(x))
      if (!length(x) || is.na(x)) return("$0")
      if (abs(x) >= 1e6) return(sprintf("$%.1fM", x / 1e6))
      paste0("$", format(round(x), big.mark = ",", scientific = FALSE))
    }

    output$total_value <- shiny::renderText(money(sum(salary_schedule()$Salary)))
    output$average_salary <- shiny::renderText(money(mean(salary_schedule()$Salary)))
    output$final_salary <- shiny::renderText(money(utils::tail(salary_schedule()$Salary, 1)))

    output$scenario_title <- shiny::renderText({
      player <- input$player %||% "Selected player"
      paste0(player, " · ", input$years, "-year scenario")
    })

    output$extension_table <- reactable::renderReactable({
      d <- salary_schedule()
      display <- data.frame(
        `Extension Year` = paste("Year", d$Year),
        Salary = vapply(d$Salary, money, character(1)),
        `Raise vs. Prior Year` = c("—", sprintf("%.1f%%", rep(input$raise_pct, max(0, nrow(d) - 1)))),
        Structure = c(rep("Guaranteed", max(0, nrow(d) - 1)), input$guarantee),
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

    output$extension_readout <- shiny::renderUI({
      d <- salary_schedule()
      shiny::div(
        class = "tbi-callout",
        shiny::strong("Scenario read"),
        shiny::p(sprintf("This structure commits %s over %s years, with a Year 1 salary of %s and a final-year salary of %s.", money(sum(d$Salary)), nrow(d), money(d$Salary[[1]]), money(utils::tail(d$Salary, 1))))
      )
    })
  })
}
