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
        shiny::p("Review current payroll commitments, threshold position, and operating flexibility.")
      ),
      shiny::div(class = "tbi-page-icon", bsicons::bs_icon("currency-dollar"))
    ),

    shiny::div(
      class = "tbi-salary-kpi-grid",
      shiny::div(class = "tbi-framework-kpi", shiny::span("TEAM SALARY"), shiny::strong(shiny::textOutput(ns("team_payroll"), inline = TRUE))),
      shiny::div(class = "tbi-framework-kpi", shiny::span("CAP STATUS"), shiny::strong(shiny::textOutput(ns("cap_status"), inline = TRUE))),
      shiny::div(class = "tbi-framework-kpi", shiny::span("CAP ROOM / OVERAGE"), shiny::strong(shiny::textOutput(ns("cap_position"), inline = TRUE))),
      shiny::div(class = "tbi-framework-kpi", shiny::span("SECOND-APRON ROOM"), shiny::strong(shiny::textOutput(ns("second_apron_room"), inline = TRUE)))
    ),

    shiny::uiOutput(ns("threshold_strip")),

    shiny::div(
      class = "tbi-salary-main-grid",
      shiny::div(
        class = "tbi-panel tbi-framework-panel tbi-salary-ledger-panel",
        shiny::div(class = "tbi-panel-kicker", "CONTRACT LEDGER"),
        shiny::h3("Current-season commitments"),
        shiny::p("Loaded contract-year cap hits for the selected organization and season."),
        reactable::reactableOutput(ns("salary_table"))
      ),
      shiny::div(
        class = "tbi-panel tbi-framework-panel tbi-salary-readout-panel",
        shiny::div(class = "tbi-panel-kicker", "CAP ENGINE"),
        shiny::h3("Front-office readout"),
        shiny::uiOutput(ns("cap_signals")),
        shiny::hr(),
        shiny::uiOutput(ns("calculation_scope"))
      )
    )
  )
}

#' Salary cap Server Functions
#' @noRd
mod_salary_cap_server <- function(id, selected_team, selected_season) {
  shiny::moduleServer(id, function(input, output, session) {

    money <- function(x, signed = FALSE) {
      x <- suppressWarnings(as.numeric(x))
      if (!length(x) || is.na(x)) return("—")
      prefix <- if (signed && x > 0) "+" else ""
      if (abs(x) >= 1e6) return(sprintf("%s$%.1fM", prefix, x / 1e6))
      if (abs(x) >= 1e3) return(sprintf("%s$%.0fK", prefix, x / 1e3))
      paste0(prefix, "$", format(round(x), big.mark = ",", scientific = FALSE))
    }

    pct_of <- function(value, threshold) {
      value <- suppressWarnings(as.numeric(value))
      threshold <- suppressWarnings(as.numeric(threshold))
      if (!length(value) || !length(threshold) || is.na(value) || is.na(threshold) || threshold <= 0) return("—")
      sprintf("%.1f%%", 100 * value / threshold)
    }

    cap_summary <- shiny::reactive({
      shiny::req(selected_team(), selected_season())
      get_team_cap_summary(selected_team(), selected_season())
    })

    salary_data <- shiny::reactive(cap_summary()$contracts)
    threshold_data <- shiny::reactive(cap_summary()$thresholds)

    output$team_payroll <- shiny::renderText(money(cap_summary()$team_salary))
    output$cap_status <- shiny::renderText(cap_summary()$status)

    output$cap_position <- shiny::renderText({
      result <- cap_summary()
      if (result$is_over_cap) paste0(money(result$over_cap_by), " over") else paste0(money(result$cap_room), " room")
    })

    output$second_apron_room <- shiny::renderText({
      result <- cap_summary()
      if (result$is_above_second_apron) paste0(money(result$over_second_apron_by), " over") else money(result$second_apron_room)
    })

    output$threshold_strip <- shiny::renderUI({
      result <- cap_summary()
      cards <- list(
        list(label = "SALARY CAP", threshold = result$salary_cap, distance = result$cap_distance),
        list(label = "LUXURY TAX", threshold = result$luxury_tax, distance = result$tax_distance),
        list(label = "FIRST APRON", threshold = result$first_apron, distance = result$first_apron_distance),
        list(label = "SECOND APRON", threshold = result$second_apron, distance = result$second_apron_distance)
      )

      shiny::div(
        class = "tbi-salary-threshold-grid",
        lapply(cards, function(card) {
          distance_text <- if (card$distance >= 0) {
            paste(money(card$distance), "below")
          } else {
            paste(money(abs(card$distance)), "above")
          }
          shiny::div(
            class = "tbi-framework-kpi tbi-threshold-card",
            shiny::span(card$label),
            shiny::strong(money(card$threshold)),
            shiny::tags$small(paste(pct_of(result$team_salary, card$threshold), "used ·", distance_text))
          )
        })
      )
    })

    output$salary_table <- reactable::renderReactable({
      d <- salary_data()
      th <- threshold_data()
      shiny::validate(shiny::need(nrow(d) > 0, "No contract-year records are available for this team and season."))

      display <- data.frame(
        Player = d$player_name,
        Position = d$primary_position,
        `Cap Hit` = vapply(d$cap_hit, money, character(1)),
        Guaranteed = vapply(d$guaranteed_amount, money, character(1)),
        `% of Cap` = vapply(d$cap_hit, pct_of, character(1), threshold = th$salary_cap[[1]]),
        `% of 1st Apron` = vapply(d$cap_hit, pct_of, character(1), threshold = th$first_apron[[1]]),
        Contract = ifelse(is.na(d$contract_type), "Not classified", d$contract_type),
        Option = ifelse(is.na(d$option_type) | !nzchar(d$option_type), "—", d$option_type),
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
      result <- cap_summary()
      flexibility <- if (result$is_above_second_apron) {
        "Severely restricted"
      } else if (result$is_above_first_apron) {
        "Restricted"
      } else if (result$is_tax_team) {
        "Tax-sensitive"
      } else if (result$is_over_cap) {
        "Exception-dependent"
      } else {
        "Cap room available"
      }

      minimum_read <- if (is.na(result$minimum_team_salary) || result$minimum_team_salary <= 0) {
        "Not loaded"
      } else if (result$minimum_salary_shortfall > 0) {
        paste(money(result$minimum_salary_shortfall), "short")
      } else {
        "Requirement met"
      }

      shiny::tagList(
        shiny::div(class = "tbi-signal-row", shiny::span("Operating band"), shiny::strong(result$status)),
        shiny::div(class = "tbi-signal-row", shiny::span("Flexibility"), shiny::strong(flexibility)),
        shiny::div(class = "tbi-signal-row", shiny::span("Guaranteed salary"), shiny::strong(money(result$guaranteed_salary))),
        shiny::div(class = "tbi-signal-row", shiny::span("Non-guaranteed exposure"), shiny::strong(money(result$non_guaranteed_exposure))),
        shiny::div(class = "tbi-signal-row", shiny::span("Top-three concentration"), shiny::strong(sprintf("%.1f%%", 100 * result$top3_share))),
        shiny::div(class = "tbi-signal-row", shiny::span("Minimum salary"), shiny::strong(minimum_read))
      )
    })

    output$calculation_scope <- shiny::renderUI({
      result <- cap_summary()
      shiny::tagList(
        shiny::tags$small(shiny::strong("Calculation scope")),
        shiny::tags$ul(
          class = "tbi-assumption-list",
          lapply(result$assumptions, shiny::tags$li)
        )
      )
    })
  })
}
