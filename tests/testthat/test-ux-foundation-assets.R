ux_foundation_project_root <- function() {
  candidates <- c(".", "..", "../..", "../../..")

  for (candidate in candidates) {
    if (file.exists(file.path(candidate, "DESCRIPTION"))) {
      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }
  }

  stop("Could not locate project root from UX foundation tests.", call. = FALSE)
}

testthat::test_that("UX foundation uses one shared DOM observer", {
  root <- ux_foundation_project_root()
  javascript <- readLines(
    file.path(root, "inst", "app", "www", "tbi_ux_foundation.js"),
    warn = FALSE
  )

  testthat::expect_equal(
    sum(grepl("new MutationObserver", javascript, fixed = TRUE)),
    1L
  )
  testthat::expect_true(
    any(grepl("window.TBIUX.register", javascript, fixed = TRUE))
  )
})

testthat::test_that("UX foundation script is deferred and content-versioned", {
  root <- ux_foundation_project_root()
  ui <- app_ui(NULL)
  rendered <- htmltools::renderTags(ui)
  html <- paste(rendered$head, rendered$html, sep = "\n")
  script_path <- file.path(
    root,
    "inst",
    "app",
    "www",
    "tbi_ux_foundation.js"
  )
  script_version <- unname(tools::md5sum(script_path))
  app_ui_source <- paste(
    readLines(file.path(root, "R", "app_ui.R"), warn = FALSE),
    collapse = "\n"
  )

  testthat::expect_true(
    grepl(
      paste0(
        'src="tbi-assets/tbi_ux_foundation.js?v=',
        script_version,
        '"'
      ),
      html,
      fixed = TRUE
    )
  )
  testthat::expect_true(
    grepl("defer", html, fixed = TRUE)
  )
  testthat::expect_match(
    app_ui_source,
    'includeCSS\\(\\s+app_sys\\("app/www/tbi_ux_foundation[.]css"\\)'
  )
})

testthat::test_that("global navigation keeps all eleven destinations visible without a scroll rail", {
  root <- ux_foundation_project_root()
  rendered <- htmltools::renderTags(app_ui(NULL))$html
  nav_match <- regexpr(
    '<nav class="tbi-primary-navigation tbi-v2-navigation">[\\s\\S]*?</nav>',
    rendered,
    perl = TRUE
  )

  testthat::expect_gt(unname(nav_match), 0L)
  if (unname(nav_match) < 1L) return(invisible(NULL))

  navigation <- regmatches(rendered, nav_match)
  expected <- c(
    nav_executive = "Command Center",
    nav_team = "Team Overview",
    nav_roster = "Roster Intelligence",
    nav_depth = "Depth Chart",
    nav_player_manager = "Player Management",
    nav_salary = "Cap Intelligence",
    nav_extension = "Extension Simulator",
    nav_trade = "Trade Intelligence",
    nav_draft = "Draft Intelligence",
    nav_outlook = "Five-Year Outlook",
    nav_cba_glossary = "CBA Info Hub"
  )
  id_positions <- vapply(names(expected), function(id) {
    unname(regexpr(paste0('id="', id, '"'), navigation, fixed = TRUE))
  }, integer(1))

  testthat::expect_true(all(id_positions > 0L))
  testthat::expect_true(all(diff(id_positions) > 0L))
  for (label in unname(expected)) {
    testthat::expect_match(navigation, label, fixed = TRUE)
  }
  testthat::expect_equal(
    lengths(regmatches(navigation, gregexpr('id="nav_[^"]+"', navigation, perl = TRUE))),
    11L
  )

  stylesheet <- paste(
    readLines(file.path(root, "inst", "app", "www", "tbi_ux_foundation.css"), warn = FALSE),
    collapse = "\n"
  )
  start_marker <- "/* >>> TBI_TOP_NAV_VISIBILITY_HOTFIX_START >>> */"
  end_marker <- "/* <<< TBI_TOP_NAV_VISIBILITY_HOTFIX_END <<< */"
  block <- strsplit(stylesheet, start_marker, fixed = TRUE)[[1L]][[2L]]
  block <- strsplit(block, end_marker, fixed = TRUE)[[1L]][[1L]]

  testthat::expect_match(block, "grid-template-rows:\\s*auto minmax\\(0,1fr\\)\\s*!important")
  testthat::expect_match(block, "overflow:\\s*visible\\s*!important")
  testthat::expect_match(block, "scrollbar-width:\\s*none\\s*!important")
  testthat::expect_false(grepl("overflow-x:auto", block, fixed = TRUE))
  testthat::expect_match(block, "@media\\s*\\(min-width:\\s*1800px\\)")
  testthat::expect_match(block, "@media\\s*\\(max-width:\\s*1439px\\)")
  testthat::expect_match(block, "grid-template-columns:\\s*repeat\\(6,minmax\\(0,1fr\\)\\)\\s*!important")
  testthat::expect_match(block, "@media\\s*\\(max-width:\\s*700px\\)")
  testthat::expect_match(block, "grid-template-columns:\\s*repeat\\(2,minmax\\(0,1fr\\)\\)\\s*!important")
  testthat::expect_match(block, "min-height:\\s*44px\\s*!important")
  testthat::expect_match(block, "flex:\\s*0 0 auto\\s*!important")
  testthat::expect_match(block, "white-space:\\s*nowrap\\s*!important")
  testthat::expect_false(grepl("text-overflow", block, fixed = TRUE))
})

testthat::test_that("all internal sub-tab rails share responsive distribution", {
  root <- ux_foundation_project_root()
  stylesheet <- paste(
    readLines(
      file.path(root, "inst", "app", "www", "tbi_ux_foundation.css"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  rail_classes <- c(
    "tbi-pm-subnav",
    "tbi-roster-subnav",
    "tbi-depth-subnav",
    "tbi-cap-subnav",
    "tbi-trade-subnav",
    "tbi-draft-subnav",
    "tbi-command-subnav",
    "tbi-team-subnav",
    "tbi-outlook-subnav",
    "tbi-extension-subnav"
  )
  tab_classes <- sub("subnav$", "subtab", rail_classes)
  desktop_tab_selectors <- c(
    ".pi-page .tbi-pm-subtab",
    ".tbi-v2-roster-page .tbi-roster-subtab",
    ".depth-v21-shell .tbi-depth-subtab",
    ".tbi-v2-cap-page .tbi-cap-subtab",
    ".tbi-v2-trade-page .tbi-trade-subtab",
    ".tbi-v2-draft-page .tbi-draft-subtab",
    ".tbi-exec-dashboard-v2 .tbi-command-subtab",
    ".tbi-v2-team-page .tbi-team-subtab",
    ".tbi-v2-outlook-page .tbi-outlook-subtab",
    ".tbi-v2-extension-page .tbi-extension-subtab"
  )

  start_marker <- "/* >>> TBI_GLOBAL_SUBTAB_DISTRIBUTION_START >>> */"
  end_marker <- "/* <<< TBI_GLOBAL_SUBTAB_DISTRIBUTION_END <<< */"
  has_start_marker <- grepl(start_marker, stylesheet, fixed = TRUE)
  has_end_marker <- grepl(end_marker, stylesheet, fixed = TRUE)

  testthat::expect_true(has_start_marker)
  testthat::expect_true(has_end_marker)

  if (!has_start_marker || !has_end_marker) {
    return(invisible(NULL))
  }

  distribution_start <- as.integer(
    regexpr(start_marker, stylesheet, fixed = TRUE)[[1L]]
  )
  distribution_end <- as.integer(
    regexpr(end_marker, stylesheet, fixed = TRUE)[[1L]]
  )
  distribution_block <- substr(
    stylesheet,
    distribution_start + nchar(start_marker),
    distribution_end - 1L
  )

  for (name in c(rail_classes, tab_classes, desktop_tab_selectors)) {
    testthat::expect_match(distribution_block, name, fixed = TRUE)
  }
  testthat::expect_match(distribution_block, "min-height:44px !important", fixed = TRUE)
  testthat::expect_match(distribution_block, "padding:8px 12px !important", fixed = TRUE)
  testthat::expect_match(distribution_block, "font-size:.72rem !important", fixed = TRUE)
  testthat::expect_match(distribution_block, "line-height:1 !important", fixed = TRUE)
  testthat::expect_match(distribution_block, "background:rgba(37,99,235,.17) !important", fixed = TRUE)
  testthat::expect_match(distribution_block, ":focus-visible", fixed = TRUE)
  desktop_media <- regexpr(
    "@media\\s*\\(min-width:\\s*701px\\)",
    distribution_block,
    perl = TRUE
  )
  mobile_media <- regexpr(
    "@media\\s*\\(max-width:\\s*700px\\)",
    distribution_block,
    perl = TRUE
  )
  desktop_media_start <- as.integer(desktop_media[[1L]])
  mobile_media_start <- as.integer(mobile_media[[1L]])

  testthat::expect_true(desktop_media_start > 0L)
  testthat::expect_true(mobile_media_start > desktop_media_start)

  if (
    desktop_media_start < 1L ||
      mobile_media_start <= desktop_media_start
  ) {
    return(invisible(NULL))
  }

  base_block <- substr(
    distribution_block,
    1L,
    desktop_media_start - 1L
  )
  desktop_block <- substr(
    distribution_block,
    desktop_media_start,
    mobile_media_start - 1L
  )
  mobile_block <- substr(
    distribution_block,
    mobile_media_start,
    nchar(distribution_block)
  )

  extract_rule <- function(css, declaration_pattern) {
    rule_pattern <- paste0(
      "(?s):is\\([^}]+\\)\\s*\\{[^}]*",
      declaration_pattern,
      "[^}]*\\}"
    )
    rule_match <- regexpr(rule_pattern, css, perl = TRUE)

    testthat::expect_true(
      unname(rule_match) > 0L,
      info = paste("Missing responsive rule for:", declaration_pattern)
    )

    if (unname(rule_match) < 1L) {
      return("")
    }

    regmatches(css, rule_match)
  }

  base_rail_rule <- extract_rule(
    base_block,
    "box-sizing:\\s*border-box\\s*!important"
  )
  desktop_rail_rule <- extract_rule(
    desktop_block,
    "align-items:\\s*stretch\\s*!important"
  )
  desktop_tab_rule <- extract_rule(
    desktop_block,
    "flex:\\s*1 1 0\\s*!important"
  )
  mobile_rail_rule <- extract_rule(
    mobile_block,
    "overflow-x:\\s*auto\\s*!important"
  )
  mobile_tab_rule <- extract_rule(
    mobile_block,
    "flex:\\s*0 0 auto\\s*!important"
  )

  for (class_name in rail_classes) {
    testthat::expect_true(
      grepl(paste0(".", class_name), base_rail_rule, fixed = TRUE),
      info = paste("Missing base rail selector:", class_name)
    )
    testthat::expect_true(
      grepl(paste0(".", class_name), desktop_rail_rule, fixed = TRUE),
      info = paste("Missing desktop rail selector:", class_name)
    )
    testthat::expect_true(
      grepl(paste0(".", class_name), mobile_rail_rule, fixed = TRUE),
      info = paste("Missing mobile rail selector:", class_name)
    )
  }

  for (class_name in tab_classes) {
    testthat::expect_true(
      grepl(paste0(".", class_name), desktop_tab_rule, fixed = TRUE),
      info = paste("Missing desktop tab selector:", class_name)
    )
    testthat::expect_true(
      grepl(paste0(".", class_name), mobile_tab_rule, fixed = TRUE),
      info = paste("Missing mobile tab selector:", class_name)
    )
  }

  for (selector in desktop_tab_selectors) {
    testthat::expect_true(
      grepl(selector, desktop_tab_rule, fixed = TRUE),
      info = paste("Desktop selector lacks module specificity:", selector)
    )
  }

  testthat::expect_match(base_rail_rule, "width:\\s*100%\\s*!important")
  testthat::expect_match(desktop_tab_rule, "height:\\s*auto\\s*!important")
  testthat::expect_match(
    desktop_tab_rule,
    "white-space:\\s*normal\\s*!important"
  )
  testthat::expect_match(
    mobile_rail_rule,
    "justify-content:\\s*flex-start\\s*!important"
  )
  testthat::expect_match(
    mobile_tab_rule,
    "min-width:\\s*max-content\\s*!important"
  )
  testthat::expect_match(
    mobile_tab_rule,
    "white-space:\\s*nowrap\\s*!important"
  )
})

testthat::test_that("Player Management Development uses the full responsive canvas", {
  root <- ux_foundation_project_root()
  stylesheet <- paste(
    readLines(
      file.path(root, "inst", "app", "www", "tbi_ux_foundation.css"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  start_marker <- "/* >>> TBI_PM_DEVELOPMENT_GRID_START >>> */"
  end_marker <- "/* <<< TBI_PM_DEVELOPMENT_GRID_END <<< */"
  has_start_marker <- grepl(start_marker, stylesheet, fixed = TRUE)
  has_end_marker <- grepl(end_marker, stylesheet, fixed = TRUE)

  testthat::expect_true(has_start_marker)
  testthat::expect_true(has_end_marker)

  if (!has_start_marker || !has_end_marker) {
    return(invisible(NULL))
  }

  block_start <- as.integer(
    regexpr(start_marker, stylesheet, fixed = TRUE)[[1L]]
  )
  block_end <- as.integer(
    regexpr(end_marker, stylesheet, fixed = TRUE)[[1L]]
  )
  development_block <- substr(
    stylesheet,
    block_start + nchar(start_marker),
    block_end - 1L
  )

  testthat::expect_match(
    development_block,
    ':has\\([.]tbi-pm-subtab\\[data-tab="development"\\][.]active\\)'
  )
  testthat::expect_match(
    development_block,
    "grid-template-columns:\\s*repeat\\(3,\\s*minmax\\(0,\\s*1fr\\)\\)"
  )
  testthat::expect_match(
    development_block,
    "display:\\s*contents\\s*!important"
  )
  testthat::expect_match(
    development_block,
    "[.]pi-right-rail[^}]+display:\\s*none\\s*!important",
    perl = TRUE
  )
  testthat::expect_match(
    development_block,
    "@media\\s*\\(min-width:\\s*701px\\)\\s*and\\s*\\(max-width:\\s*1250px\\)"
  )
  testthat::expect_match(
    development_block,
    "grid-template-columns:\\s*repeat\\(2,\\s*minmax\\(0,\\s*1fr\\)\\)"
  )
  testthat::expect_match(
    development_block,
    "grid-column:\\s*1\\s*/\\s*-1\\s*!important"
  )
  testthat::expect_match(
    development_block,
    "@media\\s*\\(max-width:\\s*700px\\)"
  )
  testthat::expect_match(
    development_block,
    "grid-template-columns:\\s*minmax\\(0,\\s*1fr\\)"
  )
  testthat::expect_match(
    development_block,
    "min-width:\\s*0\\s*!important"
  )
})
