#' Draft assets UI Function
#' @noRd
mod_draft_assets_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "tbi-module-page",
    shiny::div(
      class = "tbi-page-header",
      shiny::div(
        shiny::div(class = "tbi-page-eyebrow", "DRAFT CAPITAL"),
        shiny::h2("Draft Intelligence"),
        shiny::p("Create a working draft-asset register for picks, swaps, protections, and obligations.")
      ),
      shiny::div(class = "tbi-page-icon", bsicons::bs_icon("calendar-event"))
    ),

    bslib::layout_columns(
      col_widths = c(4, 8),
      shiny::div(
        class = "tbi-panel tbi-framework-panel",
        shiny::div(class = "tbi-panel-kicker", "ADD ASSET"),
        shiny::selectInput(ns("draft_year"), "Draft year", choices = 2027:2033),
        shiny::selectInput(ns("round"), "Round", choices = c("First", "Second")),
        shiny::selectInput(ns("control_type"), "Control type", choices = c("Own", "Incoming", "Outgoing", "Swap right", "Swap obligation")),
        shiny::textInput(ns("counterparty"), "Counterparty", placeholder = "Team or original owner"),
        shiny::textInput(ns("protection"), "Protection / condition", placeholder = "Unprotected, top-10 protected, best of..."),
        shiny::selectInput(ns("strategic_value"), "Strategic value", choices = c("High", "Medium", "Low", "Unrated")),
        shiny::actionButton(ns("add_asset"), "Add to register", class = "btn-primary"),
        shiny::actionButton(ns("clear_assets"), "Clear session", class = "btn-outline-secondary")
      ),
      shiny::div(
        class = "tbi-panel tbi-framework-panel",
        shiny::div(class = "tbi-panel-kicker", "ASSET REGISTER"),
        shiny::h3(shiny::textOutput(ns("register_title"), inline = TRUE)),
        shiny::p("Session-based planning workspace. Entries are not written to the permanent database."),
        reactable::reactableOutput(ns("asset_table"))
      )
    ),

    bslib::layout_columns(
      col_widths = c(3, 3, 3, 3),
      shiny::div(class = "tbi-framework-kpi", shiny::span("TOTAL ASSETS"), shiny::strong(shiny::textOutput(ns("asset_count"), inline = TRUE))),
      shiny::div(class = "tbi-framework-kpi", shiny::span("FIRST-ROUND CONTROL"), shiny::strong(shiny::textOutput(ns("first_round_count"), inline = TRUE))),
      shiny::div(class = "tbi-framework-kpi", shiny::span("OUTGOING OBLIGATIONS"), shiny::strong(shiny::textOutput(ns("outgoing_count"), inline = TRUE))),
      shiny::div(class = "tbi-framework-kpi", shiny::span("HIGH-VALUE ASSETS"), shiny::strong(shiny::textOutput(ns("high_value_count"), inline = TRUE)))
    )
  )
}

#' Draft assets Server Functions
#' @noRd
mod_draft_assets_server <- function(id, selected_team, selected_season) {
  shiny::moduleServer(id, function(input, output, session) {
    assets <- shiny::reactiveVal(data.frame(
      Year = integer(), Round = character(), Control = character(), Counterparty = character(), Protection = character(), Value = character(),
      stringsAsFactors = FALSE
    ))

    shiny::observeEvent(input$add_asset, {
      row <- data.frame(
        Year = as.integer(input$draft_year),
        Round = input$round,
        Control = input$control_type,
        Counterparty = if (nzchar(trimws(input$counterparty))) trimws(input$counterparty) else "—",
        Protection = if (nzchar(trimws(input$protection))) trimws(input$protection) else "Unspecified",
        Value = input$strategic_value,
        stringsAsFactors = FALSE
      )
      assets(rbind(assets(), row))
    })

    shiny::observeEvent(input$clear_assets, assets(assets()[0, , drop = FALSE]))

    output$register_title <- shiny::renderText(paste(selected_team(), "Draft Asset Register"))
    output$asset_count <- shiny::renderText(nrow(assets()))
    output$first_round_count <- shiny::renderText(sum(assets()$Round == "First" & assets()$Control %in% c("Own", "Incoming", "Swap right")))
    output$outgoing_count <- shiny::renderText(sum(assets()$Control %in% c("Outgoing", "Swap obligation")))
    output$high_value_count <- shiny::renderText(sum(assets()$Value == "High"))

    output$asset_table <- reactable::renderReactable({
      d <- assets()
      if (!nrow(d)) {
        d <- data.frame(
          Year = "—", Round = "—", Control = "No assets entered", Counterparty = "—", Protection = "Use the form to build the register", Value = "—",
          check.names = FALSE
        )
      }
      reactable::reactable(
        d,
        pagination = nrow(d) > 8,
        defaultPageSize = 8,
        highlight = TRUE,
        theme = reactable::reactableTheme(
          backgroundColor = "transparent",
          color = "#f5f8ff",
          borderColor = "#20334f",
          headerStyle = list(backgroundColor = "#0b1728", color = "#83a7d8")
        )
      )
    })
  })
}
