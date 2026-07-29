#' Trade analyzer UI Function
#' @noRd
mod_trade_analyzer_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "tbi-module-page",
    shiny::div(
      class = "tbi-page-header",
      shiny::div(
        shiny::div(class = "tbi-page-eyebrow", "TRANSACTION STRATEGY"),
        shiny::h2("Trade Intelligence"),
        shiny::p("Build a two-team salary framework and evaluate the financial shape of a potential transaction.")
      ),
      shiny::div(class = "tbi-page-icon", bsicons::bs_icon("arrow-left-right"))
    ),

    bslib::layout_columns(
      col_widths = c(5, 2, 5),
      shiny::div(
        class = "tbi-panel tbi-framework-panel",
        shiny::div(class = "tbi-panel-kicker", "YOUR ORGANIZATION"),
        shiny::h3(shiny::textOutput(ns("team_a_name"), inline = TRUE)),
        shiny::p("Select outgoing players"),
        shiny::uiOutput(ns("team_a_players")),
        shiny::div(class = "tbi-trade-total", shiny::span("Outgoing salary"), shiny::strong(shiny::textOutput(ns("outgoing_salary"), inline = TRUE)))
      ),
      shiny::div(class = "tbi-trade-center", bsicons::bs_icon("arrow-left-right"), shiny::span("SCENARIO")),
      shiny::div(
        class = "tbi-panel tbi-framework-panel",
        shiny::div(class = "tbi-panel-kicker", "TRADE PARTNER"),
        shiny::selectInput(ns("partner_team"), "Organization", choices = NULL),
        shiny::p("Select incoming players"),
        shiny::uiOutput(ns("team_b_players")),
        shiny::div(class = "tbi-trade-total", shiny::span("Incoming salary"), shiny::strong(shiny::textOutput(ns("incoming_salary"), inline = TRUE)))
      )
    ),

    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      shiny::div(class = "tbi-framework-kpi", shiny::span("SALARY DIFFERENCE"), shiny::strong(shiny::textOutput(ns("salary_difference"), inline = TRUE))),
      shiny::div(class = "tbi-framework-kpi", shiny::span("SALARY RATIO"), shiny::strong(shiny::textOutput(ns("salary_ratio"), inline = TRUE))),
      shiny::div(class = "tbi-framework-kpi", shiny::span("FRAMEWORK RESULT"), shiny::strong(shiny::textOutput(ns("framework_result"), inline = TRUE)))
    ),

    shiny::div(
      class = "tbi-panel tbi-framework-panel",
      shiny::div(class = "tbi-panel-kicker", "DECISION NOTES"),
      shiny::h3("Transaction review framework"),
      shiny::uiOutput(ns("trade_notes")),
      shiny::tags$small("This is a salary-comparison framework, not a final CBA trade validation. Apron status, exceptions, aggregation restrictions, bonuses, and other transaction rules must be added before a trade can be labeled legal.")
    )
  )
}

#' Trade analyzer Server Functions
#' @noRd
mod_trade_analyzer_server <- function(id, selected_team, selected_season) {
  shiny::moduleServer(id, function(input, output, session) {

    teams <- get_teams()
    shiny::observe({
      shiny::req(selected_team())
      choices <- setNames(teams$team_name[teams$team_name != selected_team()], teams$team_name[teams$team_name != selected_team()])
      shiny::updateSelectInput(session, "partner_team", choices = choices, selected = choices[[1]])
    })

    team_salaries <- function(team_name) {
      con <- connect_db()
      on.exit(disconnect_db(con), add = TRUE)
      DBI::dbGetQuery(
        con,
        "SELECT p.player_name, cy.cap_hit
         FROM contract_years cy
         JOIN players p ON p.player_id = cy.player_id
         JOIN teams t ON t.team_id = cy.team_id
         WHERE t.team_name = ? AND cy.season = ?
         ORDER BY cy.cap_hit DESC, p.player_name",
        params = list(team_name, selected_season())
      )
    }

    team_a <- shiny::reactive({ shiny::req(selected_team(), selected_season()); team_salaries(selected_team()) })
    team_b <- shiny::reactive({ shiny::req(input$partner_team, selected_season()); team_salaries(input$partner_team) })

    money <- function(x) {
      x <- suppressWarnings(as.numeric(x)); if (!length(x) || is.na(x)) return("$0")
      if (abs(x) >= 1e6) sprintf("$%.1fM", x / 1e6) else sprintf("$%.0fK", x / 1e3)
    }

    output$team_a_name <- shiny::renderText(selected_team())

    output$team_a_players <- shiny::renderUI({
      d <- team_a()
      shiny::checkboxGroupInput(session$ns("outgoing_players"), NULL, choices = setNames(d$player_name, paste0(d$player_name, " — ", vapply(d$cap_hit, money, character(1)))))
    })

    output$team_b_players <- shiny::renderUI({
      d <- team_b()
      shiny::checkboxGroupInput(session$ns("incoming_players"), NULL, choices = setNames(d$player_name, paste0(d$player_name, " — ", vapply(d$cap_hit, money, character(1)))))
    })

    outgoing <- shiny::reactive({
      d <- team_a(); if (is.null(input$outgoing_players)) return(0)
      sum(d$cap_hit[d$player_name %in% input$outgoing_players], na.rm = TRUE)
    })

    incoming <- shiny::reactive({
      d <- team_b(); if (is.null(input$incoming_players)) return(0)
      sum(d$cap_hit[d$player_name %in% input$incoming_players], na.rm = TRUE)
    })

    output$outgoing_salary <- shiny::renderText(money(outgoing()))
    output$incoming_salary <- shiny::renderText(money(incoming()))
    output$salary_difference <- shiny::renderText(money(incoming() - outgoing()))
    output$salary_ratio <- shiny::renderText({ if (outgoing() <= 0) "—" else sprintf("%.2fx", incoming() / outgoing()) })
    output$framework_result <- shiny::renderText({
      if (outgoing() <= 0 || incoming() <= 0) "Select players"
      else if (incoming() <= outgoing() * 1.25 + 250000) "Reviewable"
      else "Needs adjustment"
    })

    output$trade_notes <- shiny::renderUI({
      shiny::tagList(
        shiny::div(class = "tbi-signal-row", shiny::span("Outgoing players"), shiny::strong(length(input$outgoing_players %||% character()))),
        shiny::div(class = "tbi-signal-row", shiny::span("Incoming players"), shiny::strong(length(input$incoming_players %||% character()))),
        shiny::div(class = "tbi-signal-row", shiny::span("Net salary change"), shiny::strong(money(incoming() - outgoing()))),
        shiny::div(class = "tbi-signal-row", shiny::span("Next validation layer"), shiny::strong("CBA rule engine"))
      )
    })
  })
}
