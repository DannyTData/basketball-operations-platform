#' Extension simulator UI Function
#' @noRd
mod_extension_simulator_ui <- function(id) {
  ns <- shiny::NS(id)
  tbi_module_placeholder(
    icon = "calculator",
    eyebrow = "CONTRACT DECISIONS",
    title = "Extension Simulator",
    description = "Model extension structures and understand their effect on value, payroll, and roster flexibility.",
    features = c("Annual salary and raise modeling", "Guaranteed-money scenarios", "Cap percentage analysis", "Long-term payroll impact")
  )
}

#' Extension simulator Server Functions
#' @noRd
mod_extension_simulator_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {})
}
