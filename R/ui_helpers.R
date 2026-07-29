# Internal UI helper used by modules that are still being developed.
tbi_module_placeholder <- function(icon, eyebrow, title, description, features) {
  shiny::div(
    class = "tbi-module-page",
    shiny::div(
      class = "tbi-page-header",
      shiny::div(
        shiny::div(class = "tbi-page-eyebrow", eyebrow),
        shiny::h2(title),
        shiny::p(description)
      ),
      shiny::div(class = "tbi-page-icon", bsicons::bs_icon(icon))
    ),
    bslib::layout_columns(
      col_widths = c(8, 4),
      shiny::div(
        class = "tbi-panel tbi-roadmap-panel",
        shiny::div(class = "tbi-panel-kicker", "WORKSPACE PREVIEW"),
        shiny::h3("Built for front-office decisions"),
        shiny::p("This workspace is connected to the platform shell and ready for the next analytics layer."),
        shiny::div(
          class = "tbi-feature-grid",
          lapply(features, function(feature) {
            shiny::div(
              class = "tbi-feature-item",
              shiny::span(class = "tbi-feature-check", bsicons::bs_icon("check2")),
              shiny::span(feature)
            )
          })
        )
      ),
      shiny::div(
        class = "tbi-panel tbi-signal-panel",
        shiny::div(class = "tbi-panel-kicker", "MODULE STATUS"),
        shiny::div(class = "tbi-big-status", "READY"),
        shiny::p("Interface foundation complete"),
        shiny::div(class = "tbi-progress-track", shiny::div(class = "tbi-progress-fill")),
        shiny::tags$small("Data workflows and decision rules can now be added without redesigning the product shell.")
      )
    )
  )
}
