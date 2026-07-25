#' Roster and contracts UI Function
#' @noRd
mod_roster_contracts_ui <- function(id) {
  ns <- shiny::NS(id)
  tbi_module_placeholder(
    icon = "people",
    eyebrow = "ROSTER CONSTRUCTION",
    title = "Roster Intelligence",
    description = "Evaluate roster balance, contract structure, player timelines, and decision pressure points.",
    features = c("Depth chart and positional balance", "Contract years and guarantees", "Age curve and core timeline", "Extension and option alerts")
  )
}

#' Roster and contracts Server Functions
#' @noRd
mod_roster_contracts_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {})
}
