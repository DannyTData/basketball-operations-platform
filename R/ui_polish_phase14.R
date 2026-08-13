# ============================================================
# Thompson's Basketball Intelligence
# Phase 14: Final UI / UX Polish Contract
# ============================================================

phase14_ui_contract <- function() {
  list(
    stylesheet = "tbi_phase14.css",
    shell_class = "tbi-phase14-shell",
    priorities = c(
      "compact executive workspace",
      "consistent card hierarchy",
      "responsive laptop layout",
      "internal table scrolling",
      "loading feedback",
      "error-state visibility",
      "keyboard focus visibility",
      "reduced-motion accessibility"
    ),
    version = "TBI_UI_PHASE14_v1"
  )
}


phase14_required_css_markers <- function() {
  c(
    ".tbi-phase14-shell",
    ".shiny-bound-output.recalculating",
    ".shiny-output-error",
    ".dataTables_scrollBody",
    "@media (max-width: 1440px)",
    "@media (max-width: 680px)",
    "@media (prefers-reduced-motion: reduce)"
  )
}


phase14_validate_stylesheet <- function(path) {
  if (!file.exists(path)) {
    return(
      list(
        ok = FALSE,
        missing = "FILE_NOT_FOUND"
      )
    )
  }
  
  css <- paste(
    readLines(
      path,
      warn = FALSE
    ),
    collapse = "\n"
  )
  
  markers <- phase14_required_css_markers()
  
  missing <- markers[
    !vapply(
      markers,
      function(marker) {
        grepl(
          marker,
          css,
          fixed = TRUE
        )
      },
      logical(1)
    )
  ]
  
  list(
    ok = length(missing) == 0L,
    missing = missing
  )
}