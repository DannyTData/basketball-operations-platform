# ------------------------------------------------------------
# Shared CBA Context Links
# Thompson Basketball Intelligence
# ------------------------------------------------------------

#' Render a clickable CBA term that opens the CBA Info Hub.
#'
#' @param term Exact term in tbi_cba_glossary_data().
#' @param label Optional display label.
#' @param class Optional additional CSS class.
#' @noRd
tbi_cba_link <- function(
    term,
    label = NULL,
    class = NULL) {
  
  term <- as.character(term)
  
  if (
    is.null(label) ||
    !length(label) ||
    !nzchar(trimws(as.character(label[[1]])))
  ) {
    label <- term
  }
  
  shiny::tags$button(
    type = "button",
    class = paste(
      "tbi-cba-context-link",
      class %||% ""
    ),
    `data-cba-term-link` = term,
    title = paste0(
      "Open ",
      term,
      " in CBA Info Hub"
    ),
    label
  )
}


#' Render a CBA link only when the term exists in the hub.
#'
#' @param term Exact CBA term.
#' @param fallback Optional visible text.
#' @noRd
tbi_cba_link_or_text <- function(
    term,
    fallback = NULL) {
  
  available <- tryCatch(
    term %in% tbi_cba_glossary_data()$term,
    error = function(e) FALSE
  )
  
  if (isTRUE(available)) {
    return(
      tbi_cba_link(
        term = term,
        label = fallback %||% term
      )
    )
  }
  
  shiny::span(
    fallback %||% term
  )
}