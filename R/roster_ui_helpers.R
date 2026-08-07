#' Roster summary card
#' @noRd
roster_summary_card <- function(icon, label, value, detail) {
  shiny::div(
    class = "roster-summary-card",
    shiny::div(
      class = "roster-summary-card-icon",
      shiny::icon(icon)
    ),
    shiny::div(
      class = "roster-summary-card-content",
      shiny::div(class = "roster-summary-card-label", label),
      shiny::div(class = "roster-summary-card-value", value),
      shiny::div(class = "roster-summary-card-detail", detail)
    )
  )
}

#' Depth-chart row
#' @noRd
depth_chart_row <- function(position, starter, reserve = NULL) {
  shiny::div(
    class = "depth-chart-row",
    shiny::div(class = "depth-chart-position", position),
    shiny::div(
      class = "depth-chart-player depth-chart-starter",
      shiny::span(class = "depth-chart-role", "Starter"),
      shiny::span(class = "depth-chart-name", starter)
    ),
    shiny::div(
      class = "depth-chart-player depth-chart-reserve",
      shiny::span(class = "depth-chart-role", "Reserve"),
      shiny::span(
        class = "depth-chart-name",
        if (is.null(reserve) || length(reserve) == 0) "—" else reserve
      )
    )
  )
}

#' Position-strength row
#' @noRd
position_strength_row <- function(position, score) {
  score <- max(0, min(100, as.numeric(score)))
  
  shiny::div(
    class = "position-strength-row",
    shiny::div(class = "position-strength-position", position),
    shiny::div(class = "position-strength-score", paste0(round(score), "/100")),
    shiny::div(
      class = "position-strength-bar",
      shiny::div(
        class = "position-strength-fill",
        style = paste0("width: ", score, "%;")
      )
    )
  )
}