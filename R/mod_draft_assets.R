#' Draft assets UI Function
#' @noRd
mod_draft_assets_ui <- function(id) {
  ns <- shiny::NS(id)
  tbi_module_placeholder(
    icon = "calendar3",
    eyebrow = "DRAFT CAPITAL",
    title = "Draft Intelligence",
    description = "Organize future picks, protections, obligations, and strategic draft flexibility.",
    features = c("Owned and outgoing picks", "Protections and swap rights", "Pick-value framework", "Prospect and team-need layer")
  )
}

#' Draft assets Server Functions
#' @noRd
mod_draft_assets_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {})
}
