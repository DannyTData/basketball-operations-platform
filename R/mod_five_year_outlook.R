#' Five-year outlook UI Function
#' @noRd
mod_five_year_outlook_ui <- function(id) {
  ns <- shiny::NS(id)
  tbi_module_placeholder(
    icon = "graph-up-arrow",
    eyebrow = "LONG-RANGE PLANNING",
    title = "Five-Year Outlook",
    description = "Visualize the relationship between payroll, draft capital, player age, and the competitive window.",
    features = c("Multi-year payroll forecast", "Core-player timeline", "Draft capital by season", "Competitive-window assessment")
  )
}

#' Five-year outlook Server Functions
#' @noRd
mod_five_year_outlook_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {})
}
