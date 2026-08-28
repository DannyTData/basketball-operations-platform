phase3_presentation_fixture <- function() {
  players <- data.frame(
    player_id = 1:10,
    player_name = paste("Player", 1:10),
    is_starter = c(rep(TRUE, 5), rep(FALSE, 5)),
    rotation_role = c(rep("STARTER", 5), "SIXTH_MAN", rep("BENCH", 4)),
    stringsAsFactors = FALSE
  )
  minutes <- transform(
    players,
    availability_status = "UNKNOWN",
    assigned_minutes = c(32L, 31L, 30L, 29L, 28L, 22L, 20L, 18L, 16L, 14L)
  )
  segments <- data.frame(
    period = 1L,
    start_clock = "12:00",
    end_clock = "06:00",
    duration = 6L,
    creator_coverage_status = "UNKNOWN",
    big_center_coverage_status = "UNKNOWN",
    stringsAsFactors = FALSE
  )
  segments$player_ids <- I(list(1:5))
  lineups <- data.frame(
    lineup_type = c("BASE", "CLOSING"),
    legality_status = "PASS",
    explanation = "Legal governed unit.",
    reliability = "INDIVIDUAL_EVIDENCE_INCOMPLETE",
    stringsAsFactors = FALSE
  )
  lineups$player_ids <- I(list(1:5, 1:5))
  list(
    execution_status = "COMPLETED",
    phase2_diagnostics = list(status = "REVIEW", is_blocked = FALSE),
    rotation_10 = list(members = players),
    minute_ledger = list(ledger = minutes, total_assigned_minutes = 240L, status = "REVIEW"),
    stagger_plan = list(segments = segments),
    lineup_portfolio = list(lineups = lineups),
    validation_findings = list(list(status = "REVIEW", code = "ROLE_COVERAGE_UNKNOWN"))
  )
}

testthat::test_that("Phase 3 TPE money labels use the shared V2 formatter", {
  values <- c(12400000, 850000, 1250, 0, -2500000, NA_real_)
  labels <- vapply(values, v2_trade_money, character(1))

  testthat::expect_identical(
    unname(labels),
    c("$12.4M", "$850.0K", "$1.2K", "$0", "$-2.5M", "UNKNOWN")
  )
})

testthat::test_that("Phase 3 V2 presentation is explicitly non-authoritative", {
  html <- htmltools::renderTags(v2_ui_global_context(
    phase3_presentation_fixture(), list(active = FALSE), "Boston Celtics"
  ))$html
  testthat::expect_match(html, "DEVELOPMENT / NON-AUTHORITATIVE", fixed = TRUE)
  testthat::expect_match(html, "V1 remains the authoritative save path", fixed = TRUE)
  testthat::expect_match(html, "MODEL OUTPUT", fixed = TRUE)
})

testthat::test_that("global context distinguishes shared scenarios from Trade-local work", {
  shared <- htmltools::renderTags(v2_ui_global_context(
    phase3_presentation_fixture(),
    list(active = TRUE, scenario_scope = "SHARED_SUPPORTED"),
    "Boston Celtics"
  ))$html
  local <- htmltools::renderTags(v2_ui_global_context(
    phase3_presentation_fixture(),
    list(active = TRUE, scenario_scope = "TRADE_LOCAL"),
    "Boston Celtics"
  ))$html

  testthat::expect_match(shared, "SCENARIO ACTIVE", fixed = TRUE)
  testthat::expect_match(local, "BASELINE", fixed = TRUE)
  testthat::expect_false(grepl("SCENARIO ACTIVE", local, fixed = TRUE))
})

testthat::test_that("app-visible shadow uses authoritative roster team identity", {
  roster <- data.frame(team_id = rep(2L, 10L))
  testthat::expect_identical(depth_chart_v2_shadow_team(roster, "Boston Celtics"), "2")
  testthat::expect_identical(depth_chart_v2_shadow_team(data.frame(), "Boston Celtics"), "Boston Celtics")
})

testthat::test_that("Phase 3 Depth Chart exposes the complete shadow plan", {
  html <- htmltools::renderTags(v2_ui_depth_intelligence(phase3_presentation_fixture()))$html
  for (label in c("Who Plays and How Much?", "Lineup Portfolio", "Staggering", "48-Minute Game Plan", "Starting Lineup", "Closing Lineup", "Evidence, provenance, and human review")) {
    testthat::expect_match(html, label, fixed = TRUE)
  }
  testthat::expect_match(html, "240 / 240", fixed = TRUE)
  testthat::expect_match(html, "Lineup synergy and clutch evidence are not asserted", fixed = TRUE)
})

testthat::test_that("Phase 3 Command Center keeps CBA authority explicit", {
  html <- htmltools::renderTags(v2_ui_command_intelligence(
    phase3_presentation_fixture(), list(active = TRUE, scenario_scope = "SHARED_SUPPORTED")
  ))$html
  testthat::expect_match(html, "Basketball Plan", fixed = TRUE)
  testthat::expect_match(html, "CBA FAIL remains controlling", fixed = TRUE)
  testthat::expect_match(html, "USER SCENARIO", fixed = TRUE)
  testthat::expect_match(html, "HUMAN REVIEW REQUIRED", fixed = TRUE)
})

testthat::test_that("Command Center displays the governed V2 scenario recommendation", {
  scenario <- list(
    active = TRUE,
    scenario_scope = "SHARED_SUPPORTED",
    v2_organizational_impact = list(
      contract_type = "tbi-v2-organizational-impact",
      executive_recommendation = "DO NOT PROCEED"
    )
  )
  html <- htmltools::renderTags(v2_ui_command_intelligence(
    phase3_presentation_fixture(), scenario
  ))$html
  testthat::expect_match(html, "DO NOT PROCEED", fixed = TRUE)
  testthat::expect_match(html, "CBA FAIL remains controlling", fixed = TRUE)
})

testthat::test_that("Command Center does not consume Trade-local organizational impact", {
  scenario <- list(
    active = TRUE,
    scenario_scope = "TRADE_LOCAL",
    v2_organizational_impact = list(
      contract_type = "tbi-v2-organizational-impact",
      executive_recommendation = "DO NOT PROCEED"
    )
  )
  html <- htmltools::renderTags(v2_ui_command_intelligence(
    phase3_presentation_fixture(), scenario
  ))$html

  testthat::expect_match(html, "BASELINE", fixed = TRUE)
  testthat::expect_false(grepl("USER SCENARIO", html, fixed = TRUE))
  testthat::expect_false(grepl("DO NOT PROCEED", html, fixed = TRUE))
})

testthat::test_that("Phase 3 fallback imagery is accessible and layout-stable", {
  player <- htmltools::renderTags(v2_ui_player_token(1L, "Ada Player", "24 min"))$html
  team <- htmltools::renderTags(v2_ui_team_mark("Boston Celtics"))$html
  testthat::expect_match(player, 'role="img"', fixed = TRUE)
  testthat::expect_match(player, 'aria-label="Fallback initials for Ada Player"', fixed = TRUE)
  testthat::expect_match(team, 'aria-label="Boston Celtics team abbreviation fallback"', fixed = TRUE)
  testthat::expect_false(grepl("<img", paste(player, team), fixed = TRUE))
})

testthat::test_that("verified media uses the same stable image containers", {
  players <- data.frame(
    player_id = "1", asset_url = "assets/verified-player.png",
    source = "approved fixture", source_version = "1",
    verification_status = "VERIFIED", stringsAsFactors = FALSE
  )
  teams <- v2_team_media_registry()
  teams$asset_url[teams$official_abbreviation == "BOS"] <- "assets/verified-team.png"
  teams$source[teams$official_abbreviation == "BOS"] <- "approved fixture"
  teams$source_version[teams$official_abbreviation == "BOS"] <- "1"
  teams$verification_status[teams$official_abbreviation == "BOS"] <- "VERIFIED"
  player <- htmltools::renderTags(v2_ui_player_token("1", "Ada Player", media_registry = players))$html
  team <- htmltools::renderTags(v2_ui_team_mark("BOS", teams))$html
  testthat::expect_match(player, 'class="tbi-p3-avatar has-image"', fixed = TRUE)
  testthat::expect_match(team, 'class="tbi-p3-team-mark has-image"', fixed = TRUE)
  testthat::expect_match(player, 'alt="Ada Player headshot"', fixed = TRUE)
  testthat::expect_match(team, 'alt="BOS logo"', fixed = TRUE)
})

testthat::test_that("Trade Intelligence labels the V2 foundation as development-only", {
  html <- htmltools::renderTags(v2_ui_transaction_foundation_banner())$html
  testthat::expect_match(html, "Governed scenario contracts available", fixed = TRUE)
  testthat::expect_match(html, "Unsupported CBA rules remain REVIEW", fixed = TRUE)
  testthat::expect_match(html, "protected V1 two-team UI remains authoritative", fixed = TRUE)
})

testthat::test_that("Phase 3 assets define desktop and responsive contracts", {
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/", mustWork = TRUE)
  css <- paste(readLines(file.path(root, "inst", "app", "www", "tbi_phase3.css"), warn = FALSE), collapse = "\n")
  ui <- paste(readLines(file.path(root, "R", "app_ui.R"), warn = FALSE), collapse = "\n")
  server <- paste(readLines(file.path(root, "R", "app_server.R"), warn = FALSE), collapse = "\n")
  testthat::expect_match(ui, "tbi_phase3.css", fixed = TRUE)
  testthat::expect_match(ui, "phase3_global_context", fixed = TRUE)
  testthat::expect_match(server, 'unset = "v2_shadow"', fixed = TRUE)
  testthat::expect_match(css, "@media (min-width:1800px)", fixed = TRUE)
  testthat::expect_match(css, "@media (max-width:900px)", fixed = TRUE)
  testthat::expect_match(css, "@media (max-width:700px)", fixed = TRUE)
  testthat::expect_match(css, "overflow-x:hidden", fixed = TRUE)
  testthat::expect_match(css, "prefers-reduced-motion", fixed = TRUE)
})

testthat::test_that("all eleven pages receive a compact decision lens", {
  rendered <- htmltools::renderTags(app_ui(NULL))
  html <- paste(rendered$head, rendered$html, collapse = "\n")
  lens_count <- lengths(regmatches(
    html,
    gregexpr('class="tbi-p3-page-lens"', html, fixed = TRUE)
  ))
  testthat::expect_equal(lens_count, 11L)
  testthat::expect_match(html, "CBA RULES CONTROL", fixed = TRUE)
  testthat::expect_match(html, "V2 SHADOW", fixed = TRUE)

  document <- xml2::read_html(html)
  ids <- xml2::xml_attr(xml2::xml_find_all(document, '//*[@id]'), "id")
  duplicate_ids <- unique(ids[duplicated(ids)])
  testthat::expect_equal(
    length(duplicate_ids),
    0L,
    info = paste("Duplicate IDs:", paste(duplicate_ids, collapse = ", "))
  )
})

testthat::test_that("Phase 3K defines one shared page-equality canvas", {
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/", mustWork = TRUE)
  css <- paste(readLines(file.path(root, "inst", "app", "www", "tbi_phase3.css"), warn = FALSE), collapse = "\n")

  markers <- c(
    "--p3-canvas-max:2048px",
    "--p3-canvas-gutter:clamp(12px,1.25vw,24px)",
    ".tbi-v2-page-content > .tbi-module-page",
    ".tbi-v2-page-content > .depth-v21-page",
    ".tbi-v2-page-content > .cba-kb-page",
    ".tbi-player-manager-page",
    "max-width:var(--p3-canvas-max) !important",
    "grid-template-columns:minmax(760px,1.75fr) minmax(500px,1fr) !important"
  )
  for (marker in markers) testthat::expect_match(css, marker, fixed = TRUE)

  # The equality contract must appear after legacy Phase 3 rules so it wins
  # without altering protected page IDs or server behavior.
  testthat::expect_gt(
    regexpr("Phase 3K", css, fixed = TRUE)[[1]],
    regexpr("@media (prefers-reduced-motion:reduce)", css, fixed = TRUE)[[1]]
  )
})

testthat::test_that("Phase 3K viewport contract uses balanced gutters without overflow", {
  canvas <- function(viewport, sidebar = 235, mobile = FALSE) {
    available <- if (mobile) viewport else viewport - sidebar
    gutter <- if (viewport <= 700) 10 else if (viewport <= 900) 12 else min(24, max(12, viewport * .0125))
    content <- min(2048, available - 2 * gutter)
    c(content = content, left = (available - content) / 2, right = (available - content) / 2,
      overflow = max(0, content + 2 * gutter - available))
  }

  measurements <- rbind(
    `2560` = canvas(2560),
    `1920` = canvas(1920),
    `1440` = canvas(1440),
    `900` = canvas(900),
    `390` = canvas(390, mobile = TRUE)
  )
  testthat::expect_true(all(measurements[, "content"] > 0))
  testthat::expect_equal(unname(measurements[, "left"]), unname(measurements[, "right"]))
  testthat::expect_equal(unname(measurements[, "overflow"]), rep(0, 5))
  testthat::expect_gte(measurements["2560", "content"], 2000)
})

testthat::test_that("Phase 3K responsive rules protect dense pages and localize table overflow", {
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/", mustWork = TRUE)
  css <- paste(readLines(file.path(root, "inst", "app", "www", "tbi_phase3.css"), warn = FALSE), collapse = "\n")
  for (marker in c(
    "@media(max-width:900px)",
    ".depth-v21-shell { grid-template-columns:minmax(0,1fr) !important; }",
    ".tbi-trade-workspace-grid",
    "@media (max-width:700px)",
    ".tbi-module-page .table-responsive",
    "overflow-x:auto"
  )) testthat::expect_match(css, marker, fixed = TRUE)
})
