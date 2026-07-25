#' Salary cap UI Function
#' @noRd
mod_salary_cap_ui <- function(id) {
  ns <- shiny::NS(id)
  tbi_module_placeholder(
    icon = "cash-stack",
    eyebrow = "FINANCIAL STRATEGY",
    title = "Salary Cap Intelligence",
    description = "Track payroll commitments, tax exposure, apron pressure, and future flexibility.",
    features = c("Current and future payroll", "Luxury-tax and apron tracking", "Exceptions and cap holds", "Scenario-based flexibility analysis")
  )
}

#' Salary cap Server Functions
#' @noRd
mod_salary_cap_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {})
}
