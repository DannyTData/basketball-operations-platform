#' Salary cap UI Function
#' @noRd
mod_salary_cap_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "tbi-module-page",
    shiny::div(
      class = "tbi-page-header",
      shiny::div(
        shiny::div(class = "tbi-page-eyebrow", "FINANCIAL STRATEGY"),
        shiny::h2("Salary Cap Intelligence"),
        shiny::p("Review current payroll commitments, contract concentration, and flexibility signals.")
      ),
      shiny::div(class = "tbi-page-icon", bsicons::bs_icon("currency-dollar"))
    ),

    bslib::layout_columns(
      col_widths = c(3, 3, 3, 3),
      shiny::div(class = "tbi-framework-kpi", shiny::span("TEAM PAYROLL"), shiny::strong(shiny::textOutput(ns("team_payroll"), inline = TRUE))),
      shiny::div(class = "tbi-framework-kpi", shiny::span("ACTIVE CONTRACTS"), shiny::strong(shiny::textOutput(ns("contract_count"), inline = TRUE))),
      shiny::div(class = "tbi-framework-kpi", shiny::span("TOP-3 CONCENTRATION"), shiny::strong(shiny::textOutput(ns("top3_share"), inline = TRUE))),
      shiny::div(class = "tbi-framework-kpi", shiny::span("EXPIRING BY 2028"), shiny::strong(shiny::textOutput(ns("expiring_count"), inline = TRUE)))
    ),

    shiny::uiOutput(ns("threshold_strip")),

    bslib::layout_columns(
      col_widths = c(8, 4),
      shiny::div(
        class = "tbi-panel tbi-framework-panel",
        shiny::div(class = "tbi-panel-kicker", "CONTRACT LEDGER"),
        shiny::h3("Current-season commitments"),
        shiny::p("Live contract-year data for the selected organization and season."),
        reactable::reactableOutput(ns("salary_table"))
      ),
      shiny::div(
        class = "tbi-panel tbi-framework-panel",
        shiny::div(class = "tbi-panel-kicker", "FLEXIBILITY SIGNALS"),
        shiny::h3("Front-office readout"),
        shiny::uiOutput(ns("cap_signals")),
        shiny::hr(),
        shiny::tags$small("Framework status: payroll and contract concentration are live. Official cap, tax, and apron thresholds are shown for seasons with verified league values.")
      )
    )
  )
}

#' Salary cap Server Functions
#' @noRd
mod_salary_cap_server <- function(id, selected_team, selected_season) {
  shiny::moduleServer(id, function(input, output, session) {

    salary_data <- shiny::reactive({
      shiny::req(selected_team(), selected_season())
      con <- connect_db()
      on.exit(disconnect_db(con), add = TRUE)

      DBI::dbGetQuery(
        con,
        "
        SELECT p.player_name,
               p.primary_position,
               cy.base_salary,
               cy.cap_hit,
               cy.guaranteed_amount,
               c.contract_type,
               c.contract_end_season,
               c.free_agent_year,
               c.bird_rights
        FROM contract_years cy
        JOIN players p ON p.player_id = cy.player_id
        JOIN teams t ON t.team_id = cy.team_id
        LEFT JOIN contracts c ON c.contract_id = cy.contract_id
        WHERE t.team_name = ? AND cy.season = ?
        ORDER BY cy.cap_hit DESC, p.player_name
        ",
        params = list(selected_team(), selected_season())
      )
    })

    threshold_data <- shiny::reactive({
      shiny::req(selected_season())
      get_cap_thresholds(selected_season())
    })

    pct_of <- function(value, threshold) {
      value <- suppressWarnings(as.numeric(value))
      threshold <- suppressWarnings(as.numeric(threshold))
      if (!length(value) || !length(threshold) || is.na(value) || is.na(threshold) || threshold <= 0) return("—")
      sprintf("%.1f%%", 100 * value / threshold)
    }

    output$threshold_strip <- shiny::renderUI({
      th <- threshold_data()
      if (!nrow(th)) {
        return(shiny::div(class = "tbi-panel tbi-framework-panel", "Official cap thresholds are not loaded for this season."))
      }
      payroll <- sum(salary_data()$cap_hit, na.rm = TRUE)
      cards <- list(
        c("SALARY CAP", th$salary_cap[[1]]),
        c("LUXURY TAX", th$luxury_tax[[1]]),
        c("FIRST APRON", th$first_apron[[1]]),
        c("SECOND APRON", th$second_apron[[1]])
      )
      bslib::layout_columns(
        col_widths = c(3, 3, 3, 3),
        lapply(cards, function(card) {
          threshold <- as.numeric(card[[2]])
          shiny::div(
            class = "tbi-framework-kpi tbi-threshold-card",
            shiny::span(card[[1]]),
            shiny::strong(money(threshold)),
            shiny::tags$small(paste("Payroll:", pct_of(payroll, threshold)))
          )
        })
      )
    })

    money <- function(x) {
      x <- suppressWarnings(as.numeric(x))
      if (!length(x) || is.na(x)) return("$0")
      if (abs(x) >= 1e6) return(sprintf("$%.1fM", x / 1e6))
      if (abs(x) >= 1e3) return(sprintf("$%.0fK", x / 1e3))
      paste0("$", format(round(x), big.mark = ",", scientific = FALSE))
    }

    output$team_payroll <- shiny::renderText({
      d <- salary_data()
      money(sum(d$cap_hit, na.rm = TRUE))
    })

    output$contract_count <- shiny::renderText(nrow(salary_data()))

    output$top3_share <- shiny::renderText({
      d <- salary_data()
      total <- sum(d$cap_hit, na.rm = TRUE)
      if (!nrow(d) || total <= 0) return("0%")
      sprintf("%.1f%%", 100 * sum(utils::head(d$cap_hit, 3), na.rm = TRUE) / total)
    })

    output$expiring_count <- shiny::renderText({
      d <- salary_data()
      sum(!is.na(d$free_agent_year) & d$free_agent_year <= 2028)
    })

    output$salary_table <- reactable::renderReactable({
      d <- salary_data()
      shiny::validate(shiny::need(nrow(d) > 0, "No contract-year records are available for this team and season."))

      display <- data.frame(
        Player = d$player_name,
        Position = d$primary_position,
        `Cap Hit` = vapply(d$cap_hit, money, character(1)),
        `% of Cap` = if (nrow(threshold_data())) vapply(d$cap_hit, pct_of, character(1), threshold = threshold_data()$salary_cap[[1]]) else "—",
        `% of 1st Apron` = if (nrow(threshold_data())) vapply(d$cap_hit, pct_of, character(1), threshold = threshold_data()$first_apron[[1]]) else "—",
        `% of 2nd Apron` = if (nrow(threshold_data())) vapply(d$cap_hit, pct_of, character(1), threshold = threshold_data()$second_apron[[1]]) else "—",
        Contract = ifelse(is.na(d$contract_type), "Not classified", d$contract_type),
        `Contract Through` = ifelse(is.na(d$contract_end_season), "—", d$contract_end_season),
        `FA Year` = ifelse(is.na(d$free_agent_year), "—", d$free_agent_year),
        check.names = FALSE
      )

      reactable::reactable(
        display,
        searchable = TRUE,
        striped = FALSE,
        highlight = TRUE,
        pagination = TRUE,
        defaultPageSize = 10,
        theme = reactable::reactableTheme(
          backgroundColor = "transparent",
          color = "#f5f8ff",
          borderColor = "#20334f",
          headerStyle = list(backgroundColor = "#0b1728", color = "#83a7d8")
        )
      )
    })

    output$cap_signals <- shiny::renderUI({
      d <- salary_data()
      total <- sum(d$cap_hit, na.rm = TRUE)
      max_salary <- if (nrow(d)) max(d$cap_hit, na.rm = TRUE) else 0
      top_share <- if (total > 0) max_salary / total else 0
      expiring <- sum(!is.na(d$free_agent_year) & d$free_agent_year <= 2028)

      shiny::tagList(
        shiny::div(class = "tbi-signal-row", shiny::span("Largest contract"), shiny::strong(money(max_salary))),
        shiny::div(class = "tbi-signal-row", shiny::span("Largest-contract share"), shiny::strong(sprintf("%.1f%%", 100 * top_share))),
        shiny::div(class = "tbi-signal-row", shiny::span("Near-term expirations"), shiny::strong(expiring)),
        shiny::div(class = "tbi-signal-row", shiny::span("Current read"), shiny::strong(if (top_share >= .35) "Concentrated" else "Balanced"))
      )
    })
  })
}
