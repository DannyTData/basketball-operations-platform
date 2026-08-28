#' Run the feedback-safe Shiny application
#'
#' `run_tbi_feedback()` is the single feedback-candidate launcher. It requires
#' an explicit future expiration and creates a separate disposable database for
#' every Shiny session. The protected production `run_app()` launcher is not
#' changed by this feedback boundary.
#'
#' This launcher does not authenticate viewers, and expiration is not an
#' authentication mechanism. Any network deployment must use platform or
#' reverse-proxy authentication, or an intentionally private access policy.
#'
#' @param expires_at Required feedback expiration, either a `POSIXt` value or
#'   text in `YYYY-mm-dd HH:MM:SS` form.
#' @param source_db Optional explicit source database. An explicit path must
#'   exist; it never falls back to another database.
#' @param timezone Time zone used to parse a character `expires_at` value.
#' @inheritParams shiny::shinyApp
#' @param ... arguments passed to `golem::with_golem_options()`.
#'
#' @export
run_tbi_feedback <- function(expires_at,
                             source_db = NULL,
                             timezone = "America/New_York",
                             onStart = NULL,
                             options = list(),
                             enableBookmarking = NULL,
                             uiPattern = "/",
                             ...) {
  feedback_config <- tbi_feedback_config(
    expires_at = expires_at,
    source_db = source_db,
    timezone = timezone
  )

  feedback_ui <- function(request) {
    tbi_web_demo_ui(request, feedback_config = feedback_config)
  }
  feedback_server <- function(input, output, session) {
    tbi_web_demo_server(
      input,
      output,
      session,
      feedback_config = feedback_config
    )
  }

  golem::with_golem_options(
    app = shiny::shinyApp(
      ui = feedback_ui,
      server = feedback_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
