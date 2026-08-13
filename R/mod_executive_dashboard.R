# ============================================================
# PHASE 2 STEP 13 — FINAL INTEGRATION / QA
# Executive Dashboard
# Stable checkpoint: no visual redesign in this pass.
# ============================================================

# ============================================================
# TBI NBA Basketball Operations Platform
# Executive Dashboard — UI Freeze Final
# Executive Transaction Brief:
#   - bidirectional team perspective
#   - both-team SENDS labeling
#   - player context
#   - financial / roster impact
#   - CBA screen context
#   - executive summary
# ============================================================

# ------------------------------------------------------------
# Module: Executive Dashboard
# Phase 2 Step 13 — FINAL QA / Executive Front Office Intelligence
# ------------------------------------------------------------

# ============================================================
# UI
# ============================================================

#' Executive Dashboard UI
#'
#' @param id Internal module ID.
#' @noRd
mod_executive_dashboard_ui <- function(id) {
  ns <- shiny::NS(id)
  
  kpi_tile <- function(
    label,
    output_id,
    icon,
    cba_term = NULL) {
    
    rendered_label <- if (
      !is.null(cba_term) &&
      nzchar(as.character(cba_term)) &&
      exists("tbi_cba_link", mode = "function")
    ) {
      tbi_cba_link(
        term = cba_term,
        label = label,
        class = "exec-cba-link"
      )
    } else {
      shiny::span(
        class = "terminal-kpi-label",
        label
      )
    }
    
    shiny::div(
      class = "terminal-kpi",
      shiny::div(
        class = "terminal-kpi-top",
        rendered_label,
        shiny::span(
          class = "terminal-kpi-icon",
          bsicons::bs_icon(icon)
        )
      ),
      shiny::div(
        class = "terminal-kpi-value",
        shiny::textOutput(
          ns(output_id),
          inline = TRUE
        )
      )
    )
  }
  
  metric_cell <- function(
    label,
    output_id,
    cba_term = NULL) {
    
    rendered_label <- if (
      !is.null(cba_term) &&
      nzchar(as.character(cba_term)) &&
      exists("tbi_cba_link", mode = "function")
    ) {
      tbi_cba_link(
        term = cba_term,
        label = label,
        class = "exec-cba-link"
      )
    } else {
      shiny::div(
        class = "metric-cell-label",
        label
      )
    }
    
    shiny::div(
      class = "metric-cell",
      rendered_label,
      shiny::div(
        class = "metric-cell-value",
        shiny::textOutput(
          ns(output_id),
          inline = TRUE
        )
      )
    )
  }
  
  shiny::div(
    class = "executive-dashboard terminal-dashboard tbi-exec-dashboard-v2",
    
    shiny::tags$style(shiny::HTML("\n      .tbi-exec-dashboard-v2 { display:grid; gap:20px; }
      .tbi-exec-dashboard-v2 .exec-cba-link {
        color:#72adff !important;
        font-weight:800 !important;
        text-decoration:none !important;
      }
      .tbi-exec-dashboard-v2 .exec-cba-link::after {
        content:'  ↗';
        color:#5f9fee;
        font-size:.66em;
        opacity:.78;
      }
      .tbi-exec-dashboard-v2 .exec-cba-link:hover {
        color:#a8ceff !important;
        text-decoration:underline !important;
        text-underline-offset:2px;
      }

      .tbi-exec-dashboard-v2 .exec-scenario-shell {
        border:1px solid rgba(96,165,250,.24);
        border-radius:14px;
        background:
          linear-gradient(
            135deg,
            rgba(37,99,235,.10),
            rgba(15,23,42,.88)
          );
        overflow:hidden;
      }

      .tbi-exec-dashboard-v2 .exec-scenario-header {
        min-height:48px;
        padding:11px 15px;
        display:flex;
        align-items:center;
        justify-content:space-between;
        gap:14px;
        border-bottom:1px solid rgba(148,163,184,.10);
      }

      .tbi-exec-dashboard-v2 .exec-scenario-title {
        display:flex;
        align-items:center;
        gap:9px;
        color:#67a8ff;
        font-size:.68rem;
        font-weight:900;
        letter-spacing:.08em;
        text-transform:uppercase;
      }

      .tbi-exec-dashboard-v2 .exec-scenario-live {
        display:inline-flex;
        align-items:center;
        gap:6px;
        color:#34d399;
        font-size:.56rem;
        font-weight:850;
        letter-spacing:.08em;
      }

      .tbi-exec-dashboard-v2 .exec-scenario-live-dot {
        width:7px;
        height:7px;
        border-radius:50%;
        background:#22c55e;
        box-shadow:0 0 10px rgba(34,197,94,.55);
      }

      .tbi-exec-dashboard-v2 .exec-scenario-grid {
        display:grid;
        grid-template-columns:repeat(6,minmax(0,1fr));
      }

      .tbi-exec-dashboard-v2 .exec-scenario-cell {
        min-width:0;
        padding:14px 15px;
        border-right:1px solid rgba(148,163,184,.10);
      }

      .tbi-exec-dashboard-v2 .exec-scenario-cell:last-child {
        border-right:0;
      }

      .tbi-exec-dashboard-v2 .exec-scenario-label {
        display:block;
        margin-bottom:5px;
        color:#7e91aa;
        font-size:.52rem;
        font-weight:850;
        letter-spacing:.08em;
        text-transform:uppercase;
      }

      .tbi-exec-dashboard-v2 .exec-scenario-value {
        display:block;
        overflow:hidden;
        color:#f4f8fc;
        font-size:.82rem;
        font-weight:850;
        text-overflow:ellipsis;
        white-space:nowrap;
      }

      .tbi-exec-dashboard-v2 .exec-scenario-value.positive {
        color:#34d399;
      }

      .tbi-exec-dashboard-v2 .exec-scenario-value.warning {
        color:#fbbf24;
      }

      .tbi-exec-dashboard-v2 .exec-scenario-value.negative {
        color:#fb7185;
      }

      .tbi-exec-dashboard-v2 .exec-scenario-note {
        padding:10px 15px 12px;
        border-top:1px solid rgba(148,163,184,.08);
        color:#8295ad;
        font-size:.58rem;
        line-height:1.5;
      }

      .tbi-exec-dashboard-v2 .exec-brief-grid {
        display:grid;
        grid-template-columns:1.08fr 1.08fr .94fr 1fr;
      }

      .tbi-exec-dashboard-v2 .exec-brief-block {
        min-width:0;
        padding:15px 16px;
        border-right:1px solid rgba(148,163,184,.10);
      }

      .tbi-exec-dashboard-v2 .exec-brief-block:last-child {
        border-right:0;
      }

      .tbi-exec-dashboard-v2 .exec-brief-eyebrow {
        display:block;
        margin-bottom:10px;
        color:#7187a3;
        font-size:.50rem;
        font-weight:900;
        letter-spacing:.10em;
        text-transform:uppercase;
      }

      .tbi-exec-dashboard-v2 .exec-brief-player-list {
        display:grid;
        gap:9px;
      }

      .tbi-exec-dashboard-v2 .exec-brief-player {
        display:grid;
        grid-template-columns:7px minmax(0,1fr);
        gap:8px;
        align-items:start;
        min-width:0;
      }

      .tbi-exec-dashboard-v2 .exec-brief-player-dot {
        width:7px;
        height:7px;
        margin-top:5px;
        border-radius:50%;
      }

      .tbi-exec-dashboard-v2 .exec-brief-player-dot.out {
        background:#fb7185;
        box-shadow:0 0 8px rgba(251,113,133,.20);
      }

      .tbi-exec-dashboard-v2 .exec-brief-player-dot.in {
        background:#34d399;
        box-shadow:0 0 8px rgba(52,211,153,.18);
      }

      .tbi-exec-dashboard-v2 .exec-brief-player-name {
        color:#eef4fb;
        font-size:.66rem;
        font-weight:850;
        line-height:1.25;
      }

      .tbi-exec-dashboard-v2 .exec-brief-player-meta {
        display:flex;
        flex-wrap:wrap;
        gap:4px 7px;
        margin-top:3px;
        color:#7890ae;
        font-size:.50rem;
        line-height:1.4;
      }

      .tbi-exec-dashboard-v2 .exec-brief-player-meta strong {
        color:#9bb5d7;
        font-weight:800;
      }

      .tbi-exec-dashboard-v2 .exec-brief-metric {
        display:grid;
        gap:4px;
        margin-bottom:10px;
      }

      .tbi-exec-dashboard-v2 .exec-brief-metric:last-child {
        margin-bottom:0;
      }

      .tbi-exec-dashboard-v2 .exec-brief-metric span {
        color:#72869f;
        font-size:.48rem;
        font-weight:850;
        letter-spacing:.07em;
        text-transform:uppercase;
      }

      .tbi-exec-dashboard-v2 .exec-brief-metric strong {
        color:#f3f7fb;
        font-size:.73rem;
        font-weight:850;
        line-height:1.25;
      }

      .tbi-exec-dashboard-v2 .exec-brief-metric strong.positive {
        color:#34d399;
      }

      .tbi-exec-dashboard-v2 .exec-brief-metric strong.warning {
        color:#fbbf24;
      }

      .tbi-exec-dashboard-v2 .exec-brief-metric strong.fail {
        color:#fb7185;
      }

      .tbi-exec-dashboard-v2 .exec-brief-recommendation {
        margin-top:4px;
        color:#f3f7fb;
        font-size:.78rem;
        font-weight:900;
        line-height:1.3;
      }

      .tbi-exec-dashboard-v2 .exec-brief-score {
        margin-top:7px;
        color:#73aaff;
        font-size:.63rem;
        font-weight:850;
      }

      .tbi-exec-dashboard-v2 .exec-brief-cba-note {
        margin-top:5px;
        color:#8ca0ba;
        font-size:.52rem;
        line-height:1.45;
      }

      .tbi-exec-dashboard-v2 .exec-brief-summary {
        display:grid;
        grid-template-columns:auto minmax(0,1fr);
        gap:9px;
        align-items:start;
        padding:11px 15px;
        border-top:1px solid rgba(148,163,184,.09);
        background:rgba(59,130,246,.025);
      }

      .tbi-exec-dashboard-v2 .exec-brief-summary-label {
        color:#60a5fa;
        font-size:.49rem;
        font-weight:900;
        letter-spacing:.09em;
        text-transform:uppercase;
        white-space:nowrap;
      }

      .tbi-exec-dashboard-v2 .exec-brief-summary-copy {
        color:#b8c6d8;
        font-size:.59rem;
        line-height:1.5;
      }

      @media(max-width:1100px) {
        .tbi-exec-dashboard-v2 .exec-brief-grid {
          grid-template-columns:1fr 1fr;
        }
      }

      @media(max-width:760px) {
        .tbi-exec-dashboard-v2 .exec-brief-grid {
          grid-template-columns:1fr;
        }

        .tbi-exec-dashboard-v2 .exec-brief-block {
          border-right:0;
          border-bottom:1px solid rgba(148,163,184,.08);
        }
      }

      @media(max-width:1200px) {
        .tbi-exec-dashboard-v2 .exec-scenario-grid {
          grid-template-columns:repeat(3,minmax(0,1fr));
        }
      }

      @media(max-width:760px) {
        .tbi-exec-dashboard-v2 .exec-scenario-grid {
          grid-template-columns:1fr;
        }

        .tbi-exec-dashboard-v2 .exec-scenario-cell {
          border-right:0;
          border-bottom:1px solid rgba(148,163,184,.08);
        }
      }\n      .tbi-exec-dashboard-v2 .executive-intelligence-shell { display:grid; gap:18px; }\n      .tbi-exec-dashboard-v2 .tbi-exec-recommendation { border:1px solid #26364a; border-left:5px solid #4f8cff; border-radius:14px; background:linear-gradient(135deg,#0d1828,#101f33); padding:22px; box-shadow:0 16px 34px rgba(0,0,0,.20); }\n      .tbi-exec-dashboard-v2 .tbi-exec-recommendation__header { display:flex; justify-content:space-between; align-items:flex-start; gap:18px; }\n      .tbi-exec-dashboard-v2 .tbi-exec-recommendation__eyebrow,\n      .tbi-exec-dashboard-v2 .tbi-exec-section-header__eyebrow { color:#8ca6c3; font-size:11px; font-weight:800; letter-spacing:.12em; text-transform:uppercase; }\n      .tbi-exec-dashboard-v2 .tbi-exec-recommendation__title { margin:4px 0 0; color:#f4f8fc; font-size:28px; }\n      .tbi-exec-dashboard-v2 .tbi-exec-recommendation__score { color:#f4f8fc; white-space:nowrap; }\n      .tbi-exec-dashboard-v2 .tbi-exec-recommendation__score-value { font-size:34px; font-weight:800; }\n      .tbi-exec-dashboard-v2 .tbi-exec-recommendation__score-scale { color:#8ca6c3; }\n      .tbi-exec-dashboard-v2 .tbi-exec-recommendation__meta { display:flex; gap:10px; flex-wrap:wrap; margin-top:16px; }\n      .tbi-exec-dashboard-v2 .tbi-exec-recommendation__summary { color:#c4d2e1; margin:16px 0 0; line-height:1.55; }\n      .tbi-exec-dashboard-v2 .tbi-exec-badge { display:inline-flex; align-items:center; gap:6px; border-radius:999px; padding:6px 10px; font-size:11px; font-weight:800; letter-spacing:.04em; background:#17263a; color:#dbe7f4; border:1px solid #2b4059; }\n      .tbi-exec-dashboard-v2 .tbi-exec--positive { --exec-accent:#3ecf8e; }\n      .tbi-exec-dashboard-v2 .tbi-exec--caution { --exec-accent:#e4b84e; }\n      .tbi-exec-dashboard-v2 .tbi-exec--warning { --exec-accent:#ef8a4c; }\n      .tbi-exec-dashboard-v2 .tbi-exec--negative { --exec-accent:#ef5f6c; }\n      .tbi-exec-dashboard-v2 .tbi-exec--neutral { --exec-accent:#7294b8; }\n      .tbi-exec-dashboard-v2 .tbi-exec-badge[class*='tbi-exec--'],\n      .tbi-exec-dashboard-v2 [class*='tbi-exec--'] .tbi-exec-factor-card__score { color:var(--exec-accent); }\n      .tbi-exec-dashboard-v2 .tbi-exec-scorecard,\n      .tbi-exec-dashboard-v2 .tbi-exec-risk-panel,\n      .tbi-exec-dashboard-v2 .tbi-exec-opportunity-panel,\n      .tbi-exec-dashboard-v2 .tbi-exec-data-quality { background:#0b1422; border:1px solid #1f3044; border-radius:14px; padding:18px; }\n      .tbi-exec-dashboard-v2 .tbi-exec-section-header { display:flex; align-items:center; justify-content:space-between; gap:12px; margin-bottom:14px; }\n      .tbi-exec-dashboard-v2 .tbi-exec-section-header__title { color:#eef5fc; margin:2px 0 0; font-size:17px; }\n      .tbi-exec-dashboard-v2 .tbi-exec-scorecard__grid { display:grid; grid-template-columns:repeat(5,minmax(0,1fr)); gap:12px; }\n      .tbi-exec-dashboard-v2 .tbi-exec-factor-card { background:#101d2d; border:1px solid #22364c; border-top:3px solid var(--exec-accent,#7294b8); border-radius:11px; padding:14px; min-width:0; }\n      .tbi-exec-dashboard-v2 .tbi-exec-factor-card__header { display:flex; justify-content:space-between; gap:10px; align-items:flex-start; }\n      .tbi-exec-dashboard-v2 .tbi-exec-factor-card__title-group { display:flex; gap:7px; color:#dbe7f4; }\n      .tbi-exec-dashboard-v2 .tbi-exec-factor-card__title { font-size:12px; margin:0; }\n      .tbi-exec-dashboard-v2 .tbi-exec-factor-card__score { font-size:20px; font-weight:800; }\n      .tbi-exec-dashboard-v2 .tbi-exec-factor-card__meter { height:5px; background:#203044; border-radius:999px; margin:12px 0; overflow:hidden; }\n      .tbi-exec-dashboard-v2 .tbi-exec-factor-card__meter-fill { height:100%; background:var(--exec-accent,#7294b8); }\n      .tbi-exec-dashboard-v2 .tbi-exec-factor-card__explanation { color:#91a7bd; font-size:11px; line-height:1.45; margin:0; }\n      .tbi-exec-dashboard-v2 .tbi-executive-decision-view__two-column { display:grid; grid-template-columns:1fr 1fr; gap:18px; }\n      .tbi-exec-dashboard-v2 .tbi-exec-risk-panel__list { list-style:none; padding:0; margin:0; display:grid; gap:9px; }\n      .tbi-exec-dashboard-v2 .tbi-exec-risk-panel__item { display:flex; gap:9px; color:#cbd8e6; font-size:13px; }\n      .tbi-exec-dashboard-v2 .tbi-exec-risk-panel__item svg { color:#ef8a4c; flex:0 0 auto; margin-top:2px; }\n      .tbi-exec-dashboard-v2 .tbi-exec-opportunity-panel__grid { display:grid; gap:10px; }\n      .tbi-exec-dashboard-v2 .tbi-exec-callout { display:flex; gap:12px; background:#101d2d; border:1px solid #22364c; border-left:3px solid var(--exec-accent,#3ecf8e); border-radius:10px; padding:12px; }\n      .tbi-exec-dashboard-v2 .tbi-exec-callout__title { color:#edf5fc; font-size:12px; margin:0 0 3px; }\n      .tbi-exec-dashboard-v2 .tbi-exec-callout__message { color:#9fb1c4; font-size:12px; margin:0; }\n      .tbi-exec-dashboard-v2 .tbi-exec-data-quality__grid { display:grid; grid-template-columns:repeat(4,minmax(0,1fr)); gap:12px; }\n      .tbi-exec-dashboard-v2 .tbi-exec-metric-card { background:#101d2d; border:1px solid #22364c; border-radius:10px; padding:13px; }\n      .tbi-exec-dashboard-v2 .tbi-exec-metric-card__header { display:flex; justify-content:space-between; color:#8fa5bc; font-size:11px; }\n      .tbi-exec-dashboard-v2 .tbi-exec-metric-card__value { color:#f1f6fb; font-size:24px; font-weight:800; margin:5px 0; }\n      .tbi-exec-dashboard-v2 .tbi-executive-decision-view__scope-note { color:#7189a2; font-size:11px; display:flex; gap:7px; align-items:flex-start; }\n      .tbi-exec-dashboard-v2 .tbi-exec-empty-state { padding:24px; text-align:center; border:1px dashed #30465f; border-radius:12px; color:#91a7bd; background:#0b1422; }\n
      .tbi-exec-dashboard-v2 .bie-exec-command {
        border:1px solid rgba(96,165,250,.18);
        border-radius:14px;
        overflow:hidden;
        background:linear-gradient(145deg,#0b1727,#08121e);
      }
      .tbi-exec-dashboard-v2 .bie-exec-command-head {
        display:flex;
        justify-content:space-between;
        gap:14px;
        align-items:center;
        padding:14px 16px;
        border-bottom:1px solid rgba(148,163,184,.08);
      }
      .tbi-exec-dashboard-v2 .bie-exec-command-kicker {
        color:#60a5fa;
        font-size:.52rem;
        font-weight:900;
        letter-spacing:.1em;
        text-transform:uppercase;
      }
      .tbi-exec-dashboard-v2 .bie-exec-command-title {
        margin:4px 0 0;
        color:#f2f6fb;
        font-size:1rem;
        font-weight:900;
      }
      .tbi-exec-dashboard-v2 .bie-exec-command-chip {
        padding:5px 8px;
        border:1px solid rgba(96,165,250,.22);
        border-radius:999px;
        color:#8fc0ff;
        background:rgba(59,130,246,.06);
        font-size:.48rem;
        font-weight:900;
      }
      .tbi-exec-dashboard-v2 .bie-exec-metrics {
        display:grid;
        grid-template-columns:repeat(6,minmax(0,1fr));
        gap:8px;
        padding:14px 16px 0;
      }
      .tbi-exec-dashboard-v2 .bie-exec-metric {
        min-width:0;
        padding:10px 11px;
        border:1px solid rgba(148,163,184,.09);
        border-radius:9px;
        background:rgba(255,255,255,.015);
      }
      .tbi-exec-dashboard-v2 .bie-exec-metric span {
        display:block;
        color:#70859e;
        font-size:.42rem;
        font-weight:900;
        letter-spacing:.07em;
        text-transform:uppercase;
      }
      .tbi-exec-dashboard-v2 .bie-exec-metric strong {
        display:block;
        margin-top:5px;
        color:#eef4fb;
        font-size:.72rem;
        font-weight:900;
        line-height:1.3;
      }
      .tbi-exec-dashboard-v2 .bie-exec-metric strong.accent {
        color:#60a5fa;
      }
      .tbi-exec-dashboard-v2 .bie-exec-two-col {
        display:grid;
        grid-template-columns:1fr 1fr;
        gap:12px;
        padding:12px 16px 14px;
      }
      .tbi-exec-dashboard-v2 .bie-exec-box {
        padding:12px;
        border:1px solid rgba(148,163,184,.08);
        border-radius:10px;
        background:rgba(255,255,255,.012);
      }
      .tbi-exec-dashboard-v2 .bie-exec-box-title {
        margin-bottom:7px;
        color:#70859e;
        font-size:.45rem;
        font-weight:900;
        letter-spacing:.08em;
        text-transform:uppercase;
      }
      .tbi-exec-dashboard-v2 .bie-exec-item {
        display:flex;
        gap:7px;
        margin-bottom:6px;
        color:#9fb1c5;
        font-size:.55rem;
        line-height:1.45;
      }
      .tbi-exec-dashboard-v2 .bie-exec-item:last-child {
        margin-bottom:0;
      }
      .tbi-exec-dashboard-v2 .bie-exec-dot {
        width:5px;
        height:5px;
        margin-top:6px;
        flex:0 0 auto;
        border-radius:50%;
        background:#fbbf24;
      }
      .tbi-exec-dashboard-v2 .bie-exec-dot.strength {
        background:#34d399;
      }
      .tbi-exec-dashboard-v2 .bie-exec-foot {
        padding:0 16px 14px;
        color:#7f94ad;
        font-size:.52rem;
        line-height:1.5;
      }
      @media (max-width:1100px) {
        .tbi-exec-dashboard-v2 .bie-exec-metrics {
          grid-template-columns:repeat(3,minmax(0,1fr));
        }
      }
      @media (max-width:760px) {
        .tbi-exec-dashboard-v2 .bie-exec-metrics {
          grid-template-columns:1fr 1fr;
        }
        .tbi-exec-dashboard-v2 .bie-exec-two-col {
          grid-template-columns:1fr;
        }
      }

      @media (max-width:1100px) { .tbi-exec-dashboard-v2 .tbi-exec-scorecard__grid { grid-template-columns:repeat(2,minmax(0,1fr)); } }\n      @media (max-width:760px) { .tbi-exec-dashboard-v2 .tbi-executive-decision-view__two-column, .tbi-exec-dashboard-v2 .tbi-exec-data-quality__grid { grid-template-columns:1fr; } .tbi-exec-dashboard-v2 .tbi-exec-scorecard__grid { grid-template-columns:1fr; } }\n    ")),
    
    shiny::div(
      class = "terminal-command-bar",
      shiny::div(
        class = "terminal-command-left",
        shiny::span(class = "terminal-code", "TBI / EXEC"),
        shiny::span(class = "terminal-divider"),
        shiny::span(class = "terminal-live-dot"),
        shiny::span(class = "terminal-command-copy", "LIVE DECISION ENVIRONMENT")
      ),
      shiny::div(
        class = "terminal-command-right",
        shiny::span("INTELLIGENCE"),
        shiny::span("FINANCE"),
        shiny::span("CONTEXT")
      )
    ),
    
    shiny::div(
      class = "executive-header-row",
      shiny::div(
        class = "executive-header-copy",
        shiny::div(class = "tbi-page-eyebrow", "FRONT OFFICE COMMAND CENTER"),
        shiny::h1(
          class = "executive-main-title",
          shiny::textOutput(ns("dashboard_title"), inline = TRUE)
        ),
        shiny::p(
          class = "executive-subtitle",
          "Integrated basketball, financial, roster, and asset decision support"
        )
      ),
      shiny::div(
        class = "executive-header-badge",
        shiny::span(class = "header-badge-label", "MODEL"),
        shiny::strong("TBI v1")
      )
    ),
    
    shiny::uiOutput(ns("executive_decision"), class = "executive-intelligence-shell"),
    
    shiny::uiOutput(
      ns("bie_front_office_intelligence")
    ),
    
    shiny::uiOutput(ns("executive_status")),
    
    shiny::uiOutput(
      ns("executive_scenario")
    ),
    
    shiny::uiOutput(
      ns("executive_cba_reference")
    ),
    
    shiny::div(
      class = "terminal-kpi-grid terminal-kpi-grid-six",
      kpi_tile(
        "Team Payroll",
        "team_payroll",
        "currency-dollar",
        cba_term = "Team Salary"
      ),
      kpi_tile("Payroll Rank", "payroll_rank", "list-ol"),
      kpi_tile("Contracts", "contract_count", "person-vcard"),
      kpi_tile("Conference Rank", "conference_rank", "list-ol"),
      kpi_tile("Highest Paid Player", "highest_paid_player", "person-badge"),
      kpi_tile("Scoring", "kpi_scoring", "bullseye")
    ),
    
    shiny::div(
      class = "terminal-main-grid",
      shiny::tags$section(
        class = "terminal-panel snapshot-panel",
        shiny::div(
          class = "terminal-panel-header",
          shiny::div(
            shiny::div(class = "terminal-panel-kicker", "TEAM PROFILE"),
            shiny::h3("Team Snapshot")
          ),
          shiny::span(class = "terminal-panel-tag", "CURRENT")
        ),
        shiny::div(
          class = "metric-cell-grid",
          metric_cell("Conference", "snapshot_conference"),
          metric_cell("Conference Position", "snapshot_conference_rank"),
          metric_cell("Division Position", "snapshot_division_rank"),
          metric_cell("Scoring Average", "snapshot_scoring")
        )
      ),
      shiny::tags$section(
        class = "terminal-panel outlook-panel",
        shiny::div(
          class = "terminal-panel-header",
          shiny::div(
            shiny::div(class = "terminal-panel-kicker", "DECISION SUPPORT"),
            shiny::h3(shiny::textOutput(ns("outlook_heading"), inline = TRUE))
          ),
          shiny::span(
            class = "terminal-panel-tag terminal-panel-tag-accent",
            "ASSESSMENT"
          )
        ),
        shiny::div(
          class = "outlook-summary-terminal",
          shiny::textOutput(ns("outlook_summary"), inline = TRUE)
        ),
        shiny::div(
          class = "outlook-signal-grid",
          metric_cell("Competitive Position", "competitive_position"),
          metric_cell("Division Position", "division_position"),
          metric_cell("Scoring Profile", "scoring_profile")
        )
      )
    ),
    
    shiny::tags$section(
      class = "terminal-panel standings-panel",
      shiny::div(
        class = "terminal-panel-header",
        shiny::div(
          shiny::div(class = "terminal-panel-kicker", "LEAGUE CONTEXT"),
          shiny::h3(shiny::textOutput(ns("standings_heading"), inline = TRUE))
        ),
        shiny::span(class = "terminal-panel-tag", "LIVE TABLE")
      ),
      shiny::div(
        class = "terminal-table-wrap",
        reactable::reactableOutput(ns("conference_standings"))
      )
    )
  )
}


# ============================================================
# SERVER
# ============================================================

#' Executive Dashboard Server
#'
#' @param id Internal module ID.
#' @param selected_team Reactive expression containing selected team name.
#' @noRd
mod_executive_dashboard_server <- function(
    id,
    selected_team,
    selected_season = NULL,
    transaction_state = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    
    safe_value <- function(expr, default = NULL) {
      tryCatch(expr, error = function(e) default)
    }
    
    safe_payroll <- shiny::reactive({
      shiny::req(selected_team())
      result <- safe_value(get_team_payroll(selected_team()), data.frame())
      if (is.null(result)) data.frame() else result
    })
    
    current_payroll_value <- shiny::reactive({
      
      payroll <- safe_payroll()
      
      if (
        !nrow(payroll) ||
        !"cap_hit" %in% names(payroll)
      ) {
        return(NA_real_)
      }
      
      value <- suppressWarnings(
        as.numeric(
          payroll$cap_hit[[1]]
        )
      )
      
      if (
        !length(value) ||
        is.na(value)
      ) {
        NA_real_
      } else {
        value
      }
    })
    
    
    active_trade_scenario <- shiny::reactive({
      
      if (
        is.null(transaction_state) ||
        is.null(transaction_state$snapshot)
      ) {
        return(NULL)
      }
      
      scenario <- transaction_state$snapshot()
      
      if (
        !isTRUE(scenario$active) ||
        !identical(
          as.character(
            scenario$scenario_type
          ),
          "trade"
        )
      ) {
        return(NULL)
      }
      
      if (
        !is.null(selected_season) &&
        is.function(selected_season) &&
        !identical(
          as.character(
            scenario$season
          ),
          as.character(
            selected_season()
          )
        )
      ) {
        return(NULL)
      }
      
      selected <- as.character(
        selected_team()
      )
      
      primary_team <- as.character(
        scenario$team
      )
      
      partner_team_name <- as.character(
        scenario$partner_team
      )
      
      if (
        !selected %in%
        c(
          primary_team,
          partner_team_name
        )
      ) {
        return(NULL)
      }
      
      scenario$view_side <- "team_a"
      
      if (identical(selected, partner_team_name)) {
        
        tmp_team <- scenario$team
        tmp_outgoing <- scenario$outgoing_players
        tmp_incoming <- scenario$incoming_players
        tmp_outgoing_salary <- scenario$outgoing_salary
        tmp_incoming_salary <- scenario$incoming_salary
        
        scenario$team <- scenario$partner_team
        scenario$partner_team <- tmp_team
        
        scenario$outgoing_players <- tmp_incoming
        scenario$incoming_players <- tmp_outgoing
        
        scenario$outgoing_salary <- tmp_incoming_salary
        scenario$incoming_salary <- tmp_outgoing_salary
        scenario$salary_delta <-
          scenario$incoming_salary -
          scenario$outgoing_salary
        
        scenario$view_side <- "team_b"
      }
      
      scenario
    })
    
    
    proposed_payroll_value <- shiny::reactive({
      
      current <- current_payroll_value()
      
      if (
        is.na(current) ||
        is.null(transaction_state) ||
        !exists(
          "tbi_apply_trade_scenario_to_payroll",
          mode = "function"
        )
      ) {
        return(current)
      }
      
      tbi_apply_trade_scenario_to_payroll(
        current_payroll = current,
        transaction_state = transaction_state,
        team_name = selected_team()
      )
    })
    
    
    scenario_post_trade_status <- shiny::reactive({
      
      scenario <- active_trade_scenario()
      
      if (
        is.null(scenario) ||
        is.null(scenario$evaluation)
      ) {
        return(NULL)
      }
      
      evaluation <- scenario$evaluation
      
      side <- if (
        identical(
          as.character(
            scenario$view_side
          ),
          "team_b"
        )
      ) {
        tryCatch(
          evaluation$team_b,
          error = function(e) NULL
        )
      } else {
        tryCatch(
          evaluation$team_a,
          error = function(e) NULL
        )
      }
      
      if (is.null(side)) {
        return(NULL)
      }
      
      status <- tryCatch(
        side$post_trade_status,
        error = function(e) NULL
      )
      
      if (
        is.null(status) ||
        !length(status) ||
        is.na(status[[1]]) ||
        !nzchar(
          trimws(
            as.character(
              status[[1]]
            )
          )
        )
      ) {
        return(NULL)
      }
      
      as.character(
        status[[1]]
      )
    })
    
    
    scenario_cba_result <- shiny::reactive({
      
      scenario <- active_trade_scenario()
      
      if (
        is.null(scenario) ||
        is.null(scenario$evaluation)
      ) {
        return("Pending")
      }
      
      evaluation <- scenario$evaluation
      
      if (
        isTRUE(
          tryCatch(
            evaluation$is_trade_screen_pass,
            error = function(e) FALSE
          )
        ) &&
        isTRUE(
          tryCatch(
            evaluation$requires_manual_review,
            error = function(e) FALSE
          )
        )
      ) {
        return("REVIEW")
      }
      
      value <- tryCatch(
        evaluation$status,
        error = function(e) NULL
      )
      
      if (
        is.null(value) ||
        !length(value)
      ) {
        "Pending"
      } else {
        as.character(
          value[[1]]
        )
      }
    })
    
    
    format_money_m <- function(x) {
      
      x <- suppressWarnings(
        as.numeric(x)
      )
      
      if (
        !length(x) ||
        is.na(x)
      ) {
        return("Unavailable")
      }
      
      paste0(
        "$",
        format(
          round(
            x / 1e6,
            1
          ),
          nsmall = 1,
          trim = TRUE
        ),
        "M"
      )
    }
    
    
    safe_payroll_rank <- shiny::reactive({
      shiny::req(selected_team())
      value <- safe_value(get_team_payroll_rank(selected_team()), NA_real_)
      if (is.null(value) || !length(value)) return(NA_real_)
      suppressWarnings(as.numeric(value[[1]]))
    })
    
    safe_highest_paid <- shiny::reactive({
      shiny::req(selected_team())
      result <- safe_value(get_highest_paid_player(selected_team()), data.frame())
      if (is.null(result)) data.frame() else result
    })
    
    # Shared database first; legacy DuckDB only as a guarded fallback.
    standings_table <- shiny::reactive({
      shiny::req(selected_team())
      
      standings <- NULL
      
      if (exists("connect_db", mode = "function")) {
        standings <- safe_value({
          con <- connect_db(read_only = TRUE)
          on.exit(disconnect_db(con), add = TRUE)
          if (!"standings" %in% DBI::dbListTables(con)) return(NULL)
          DBI::dbGetQuery(
            con,
            "SELECT team_name, wins, losses, win_pct, conference_rank, division_rank, points_per_game, point_diff, conference FROM standings"
          )
        }, NULL)
      }
      
      if (is.null(standings)) {
        legacy_path <- file.path("inst", "app", "data", "basketball_ops.duckdb")
        if (file.exists(legacy_path) && requireNamespace("duckdb", quietly = TRUE)) {
          standings <- safe_value({
            con <- DBI::dbConnect(duckdb::duckdb(), dbdir = legacy_path, read_only = TRUE)
            on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
            DBI::dbGetQuery(
              con,
              "SELECT team_name, wins, losses, win_pct, conference_rank, division_rank, points_per_game, point_diff, conference FROM standings"
            )
          }, NULL)
        }
      }
      
      if (is.null(standings) || !nrow(standings)) return(NULL)
      
      numeric_columns <- intersect(
        c("wins", "losses", "win_pct", "conference_rank", "division_rank", "points_per_game", "point_diff"),
        names(standings)
      )
      standings[numeric_columns] <- lapply(standings[numeric_columns], function(x) suppressWarnings(as.numeric(x)))
      
      for (conference_name in unique(standings$conference)) {
        idx <- which(standings$conference == conference_name)
        current_rank <- standings$conference_rank[idx]
        if (any(is.na(current_rank))) {
          win_pct <- standings$win_pct[idx]; win_pct[is.na(win_pct)] <- -Inf
          wins <- standings$wins[idx]; wins[is.na(wins)] <- -Inf
          point_diff <- standings$point_diff[idx]; point_diff[is.na(point_diff)] <- -Inf
          ord <- order(-win_pct, -wins, -point_diff, standings$team_name[idx])
          computed <- integer(length(idx)); computed[ord] <- seq_along(ord)
          current_rank[is.na(current_rank)] <- computed[is.na(current_rank)]
          standings$conference_rank[idx] <- current_rank
        }
      }
      
      standings[order(standings$conference, standings$conference_rank), , drop = FALSE]
    })
    
    team_data <- shiny::reactive({
      standings <- standings_table()
      if (is.null(standings) || !nrow(standings)) return(NULL)
      team <- standings[standings$team_name == selected_team(), , drop = FALSE]
      if (!nrow(team)) NULL else team[1, , drop = FALSE]
    })
    
    conference_data <- shiny::reactive({
      standings <- standings_table()
      team <- team_data()
      if (is.null(standings) || is.null(team)) return(NULL)
      result <- standings[
        standings$conference == team$conference[[1]],
        c("conference_rank", "team_name", "wins", "losses", "win_pct", "point_diff"),
        drop = FALSE
      ]
      result[order(result$conference_rank, -result$wins), , drop = FALSE]
    })
    
    competitive_tier <- shiny::reactive({
      team <- team_data()
      if (is.null(team)) return("Unknown")
      rank <- suppressWarnings(as.numeric(team$conference_rank[[1]]))
      if (is.na(rank)) return("Unknown")
      if (rank <= 3) "Contender" else if (rank <= 6) "Playoff" else if (rank <= 10) "Play-In" else "Rebuilding"
    })
    
    executive_cba_status_term <- function(status) {
      
      status <- as.character(
        status %||% ""
      )
      
      if (
        grepl(
          "Second Apron",
          status,
          fixed = TRUE
        )
      ) {
        return("Second Apron")
      }
      
      if (
        grepl(
          "First Apron",
          status,
          fixed = TRUE
        )
      ) {
        return("First Apron")
      }
      
      if (
        grepl(
          "Tax",
          status,
          ignore.case = TRUE
        )
      ) {
        return("Luxury Tax")
      }
      
      if (
        grepl(
          "Cap",
          status,
          ignore.case = TRUE
        )
      ) {
        return("Salary Cap")
      }
      
      NULL
    }
    
    
    payroll_operating_status <- shiny::reactive({
      
      scenario_status <- scenario_post_trade_status()
      
      if (
        !is.null(scenario_status)
      ) {
        return(
          scenario_status
        )
      }
      
      # Current-state fallback until the shared cap engine is passed
      # directly into the Executive Dashboard.
      rank <- safe_payroll_rank()
      
      if (is.na(rank)) {
        return("Unknown")
      }
      
      if (rank <= 5) {
        "Above Second Apron"
      } else if (rank <= 10) {
        "Above First Apron"
      } else if (rank <= 20) {
        "Tax Team"
      } else {
        "Over Cap"
      }
    })
    
    draft_value_result <- shiny::reactive({
      if (exists("evaluate_team_draft_value", mode = "function")) {
        result <- safe_value(evaluate_team_draft_value(selected_team()), NULL)
        if (!is.null(result)) return(result)
      }
      list(summary = list(portfolio_grade = "Unrated", review_required = 0L))
    })
    
    draft_simulation_result <- shiny::reactive({
      if (exists("simulate_team_draft_portfolio", mode = "function")) {
        result <- safe_value(
          simulate_team_draft_portfolio(selected_team(), iterations = 250L),
          NULL
        )
        if (!is.null(result)) return(result)
      }
      NULL
    })
    
    basketball_intelligence <- shiny::reactive({
      team <- team_data()
      payroll <- safe_payroll()
      rank <- safe_payroll_rank()
      
      contract_count <- if (nrow(payroll) && "contracts" %in% names(payroll)) {
        suppressWarnings(as.integer(payroll$contracts[[1]]))
      } else {
        NA_integer_
      }
      
      inputs <- list(
        competitive = list(
          competitive_tier = competitive_tier(),
          projected_wins = if (!is.null(team)) suppressWarnings(as.numeric(team$wins[[1]])) else NA_real_,
          playoff_probability = NA_real_,
          championship_probability = NA_real_
        ),
        cap_result = list(
          operating_status = payroll_operating_status()
        ),
        financial = list(
          top_three_concentration = NA_real_,
          future_committed_salary_ratio = NA_real_
        ),
        roster = list(
          guaranteed_roster_spots = contract_count,
          expiring_contracts = NA_integer_,
          team_options = NA_integer_,
          player_options = NA_integer_,
          two_way_contracts = NA_integer_,
          dead_money_ratio = NA_real_
        ),
        draft_value_result = draft_value_result(),
        draft_simulation_result = draft_simulation_result(),
        transaction = list(
          manual_review_items = {
            
            scenario <- active_trade_scenario()
            
            if (
              !is.null(scenario) &&
              !is.null(scenario$evaluation)
            ) {
              if (
                isTRUE(
                  tryCatch(
                    scenario$evaluation$requires_manual_review,
                    error = function(e) FALSE
                  )
                )
              ) {
                1L
              } else {
                0L
              }
            } else if (is.na(rank)) {
              1L
            } else {
              0L
            }
          }
        )
      )
      
      if (exists("evaluate_basketball_decision", mode = "function")) {
        return(safe_value(evaluate_basketball_decision(inputs), NULL))
      }
      
      NULL
    })
    
    # ------------------------------------------------------
    # BIE Executive Front Office Intelligence
    # ------------------------------------------------------
    
    bie_executive_roster <- shiny::reactive({
      
      if (
        is.null(selected_season) ||
        !is.function(selected_season) ||
        !exists(
          "connect_db",
          mode = "function"
        )
      ) {
        return(
          data.frame()
        )
      }
      
      safe_value(
        {
          con <- connect_db(
            read_only = TRUE
          )
          
          on.exit(
            disconnect_db(con),
            add = TRUE
          )
          
          DBI::dbGetQuery(
            con,
            "
            SELECT
              p.player_id,
              p.player_name,
              p.primary_position,
              p.player_age,

              rh.roster_status,
              COALESCE(rh.two_way_flag, 0) AS two_way_flag,

              c.contract_end_season,
              c.free_agent_year,

              cy.base_salary,
              cy.cap_hit

            FROM roster_history rh

            INNER JOIN players p
              ON p.player_id = rh.player_id

            INNER JOIN teams t
              ON t.team_id = rh.team_id

            LEFT JOIN contract_years cy
              ON cy.player_id = rh.player_id
              AND cy.team_id = rh.team_id
              AND cy.season = rh.season

            LEFT JOIN contracts c
              ON c.contract_id = cy.contract_id

            WHERE t.team_name = ?
              AND rh.season = ?

            ORDER BY
              COALESCE(cy.cap_hit, 0) DESC,
              p.player_name
            ",
            params = list(
              selected_team(),
              selected_season()
            )
          )
        },
        data.frame()
      )
    })
    
    
    bie_executive_cache <- shiny::reactiveVal(
      list(
        key = NULL,
        result = NULL
      )
    )
    
    
    bie_front_office_result <- shiny::reactive({
      
      roster <- bie_executive_roster()
      
      if (
        !nrow(roster) ||
        !exists(
          "evaluate_bie_roster_decisions",
          mode = "function"
        ) ||
        !exists(
          "evaluate_bie_roster_needs",
          mode = "function"
        ) ||
        !exists(
          "evaluate_bie_executive_front_office",
          mode = "function"
        )
      ) {
        return(NULL)
      }
      
      roster_key <- if (
        exists(
          "bie_roster_signature",
          mode = "function"
        )
      ) {
        bie_roster_signature(
          roster
        )
      } else {
        paste(
          nrow(roster),
          paste(
            roster$player_id,
            collapse = ","
          ),
          sep = "|"
        )
      }
      
      scenario <- active_trade_scenario()
      
      scenario_key <- if (
        is.null(scenario)
      ) {
        "NO_TRADE"
      } else {
        paste(
          scenario$team %||% "",
          scenario$partner_team %||% "",
          paste(
            scenario$outgoing_players %||%
              character(),
            collapse = ","
          ),
          paste(
            scenario$incoming_players %||%
              character(),
            collapse = ","
          ),
          sep = "|"
        )
      }
      
      key <- paste(
        roster_key,
        competitive_tier(),
        payroll_operating_status(),
        scenario_key,
        sep = "||"
      )
      
      cached <- shiny::isolate(bie_executive_cache())
      
      if (
        !is.null(cached$key) &&
        identical(
          cached$key,
          key
        ) &&
        !is.null(cached$result)
      ) {
        return(
          cached$result
        )
      }
      
      evaluated <- tryCatch(
        if (
          exists(
            "bie_ensure_evaluated_players",
            mode = "function"
          )
        ) {
          bie_ensure_evaluated_players(
            roster
          )
        } else {
          evaluate_bie_players(
            roster
          )
        },
        error = function(e) NULL
      )
      
      if (
        is.null(evaluated) ||
        !nrow(evaluated)
      ) {
        return(NULL)
      }
      
      roster_decision <- tryCatch(
        evaluate_bie_roster_decisions(
          evaluated
        ),
        error = function(e) NULL
      )
      
      if (is.null(roster_decision)) {
        return(NULL)
      }
      
      roster_needs <- tryCatch(
        evaluate_bie_roster_needs(
          roster_players =
            evaluated,
          roster_decision =
            roster_decision
        ),
        error = function(e) NULL
      )
      
      result <- tryCatch(
        evaluate_bie_executive_front_office(
          roster_decision =
            roster_decision,
          roster_needs =
            roster_needs,
          competitive_tier =
            competitive_tier(),
          financial_status =
            payroll_operating_status(),
          active_trade =
            scenario
        ),
        error = function(e) {
          list(
            status = "ERROR",
            confidence =
              "FOUNDATION",
            explanation =
              conditionMessage(e)
          )
        }
      )
      
      bie_executive_cache(
        list(
          key = key,
          result = result
        )
      )
      
      result
    })
    
    
    output$bie_front_office_intelligence <- shiny::renderUI({
      
      result <- bie_front_office_result()
      
      if (is.null(result)) {
        return(NULL)
      }
      
      if (
        !identical(
          result$status,
          "OK"
        )
      ) {
        return(
          shiny::div(
            class =
              "bie-exec-command",
            shiny::div(
              class =
                "bie-exec-command-head",
              shiny::div(
                shiny::div(
                  class =
                    "bie-exec-command-kicker",
                  "BASKETBALL INTELLIGENCE ENGINE"
                ),
                shiny::div(
                  class =
                    "bie-exec-command-title",
                  "Executive Front Office Intelligence"
                )
              )
            ),
            shiny::div(
              class = "bie-exec-foot",
              result$explanation %||%
                "Executive BIE intelligence is unavailable."
            )
          )
        )
      }
      
      metric <- function(
    label,
    value,
    accent = FALSE) {
        
        shiny::div(
          class = "bie-exec-metric",
          shiny::span(label),
          shiny::strong(
            class = if (
              accent
            ) {
              "accent"
            } else {
              ""
            },
            value
          )
        )
      }
      
      
      item_list <- function(
    items,
    strength = FALSE) {
        
        shiny::tagList(
          lapply(
            head(
              items,
              4
            ),
            function(item) {
              shiny::div(
                class =
                  "bie-exec-item",
                shiny::span(
                  class = paste(
                    "bie-exec-dot",
                    if (
                      strength
                    ) {
                      "strength"
                    } else {
                      ""
                    }
                  )
                ),
                shiny::span(item)
              )
            }
          )
        )
      }
      
      
      shiny::div(
        class = "bie-exec-command",
        
        shiny::div(
          class =
            "bie-exec-command-head",
          
          shiny::div(
            shiny::div(
              class =
                "bie-exec-command-kicker",
              "BASKETBALL INTELLIGENCE ENGINE"
            ),
            shiny::div(
              class =
                "bie-exec-command-title",
              "Executive Front Office Intelligence"
            )
          ),
          
          shiny::span(
            class =
              "bie-exec-command-chip",
            paste0(
              "CONFIDENCE: ",
              result$confidence
            )
          )
        ),
        
        shiny::div(
          class = "bie-exec-metrics",
          
          metric(
            "EXECUTIVE SCORE",
            sprintf(
              "%.1f / 100",
              result$score
            ),
            TRUE
          ),
          
          metric(
            "RECOMMENDATION",
            result$recommendation,
            TRUE
          ),
          
          metric(
            "STRATEGIC DIRECTION",
            result$strategic_direction
          ),
          
          metric(
            "PRIMARY NEED",
            result$primary_need
          ),
          
          metric(
            "COMPETITIVE TIER",
            result$competitive_tier
          ),
          
          metric(
            "FINANCIAL STATUS",
            result$financial_status
          )
        ),
        
        shiny::div(
          class =
            "bie-exec-two-col",
          
          shiny::div(
            class =
              "bie-exec-box",
            shiny::div(
              class =
                "bie-exec-box-title",
              "FRONT-OFFICE PRIORITIES"
            ),
            item_list(
              result$priorities,
              FALSE
            )
          ),
          
          shiny::div(
            class =
              "bie-exec-box",
            shiny::div(
              class =
                "bie-exec-box-title",
              "ROSTER STRENGTHS"
            ),
            item_list(
              result$strengths,
              TRUE
            )
          )
        ),
        
        shiny::div(
          class = "bie-exec-foot",
          shiny::strong(
            "Evidence scope: "
          ),
          result$evidence_scope,
          shiny::br(),
          result$explanation
        )
      )
    })
    
    
    executive_opportunities <- shiny::reactive({
      intelligence <- basketball_intelligence()
      if (is.null(intelligence)) return(character())
      opportunities <- character()
      components <- intelligence$components
      
      if (!is.null(components$competitive_position$score) && components$competitive_position$score >= 65) {
        opportunities <- c(opportunities, "Competitive positioning supports targeted win-now evaluation.")
      }
      if (!is.null(components$roster_control$score) && components$roster_control$score >= 60) {
        opportunities <- c(opportunities, "Current roster control provides optionality for sequencing future moves.")
      }
      if (!is.null(components$draft_capital$score) && components$draft_capital$score >= 60) {
        opportunities <- c(opportunities, "Draft capital can support a selective transaction without exhausting the asset base.")
      }
      if (!length(opportunities)) {
        opportunities <- "Preserve flexibility while comparing lower-cost paths to improve the roster."
      }
      opportunities
    })
    
    data_quality_summary <- shiny::reactive({
      team_available <- !is.null(team_data())
      payroll_available <- nrow(safe_payroll()) > 0
      draft_grade <- executive_get(draft_value_result(), c("summary", "portfolio_grade"), "Unrated")
      draft_available <- !identical(draft_grade, "Unrated")
      
      list(
        verified_items = sum(c(team_available, payroll_available, draft_available)),
        assumption_items = 2L,
        review_items = if (draft_available) 0L else 1L,
        unavailable_items = sum(!c(team_available, payroll_available, draft_available)),
        updated_at = format(Sys.Date(), "%Y-%m-%d")
      )
    })
    
    output$executive_decision <- shiny::renderUI({
      intelligence <- basketball_intelligence()
      if (is.null(intelligence)) {
        return(executive_empty_state(
          title = "Executive intelligence unavailable",
          message = "The dashboard could not assemble a complete decision-support result from the currently loaded data.",
          icon = "cpu"
        ))
      }
      
      executive_decision_view(
        intelligence_result = intelligence,
        opportunities = executive_opportunities(),
        data_quality = data_quality_summary()
      )
    })
    
    output$dashboard_title <- shiny::renderText({
      paste(selected_team(), "Executive Dashboard")
    })
    
    output$executive_status <- shiny::renderUI({
      intelligence <- basketball_intelligence()
      if (is.null(intelligence)) return(NULL)
      shiny::div(
        class = "executive-status-strip",
        shiny::div(
          class = "executive-status-item",
          shiny::span(class = "executive-status-label", "Recommendation"),
          shiny::strong(intelligence$recommendation)
        ),
        shiny::div(
          class = "executive-status-item",
          shiny::span(class = "executive-status-label", "Decision Score"),
          shiny::strong(sprintf("%.1f / 100", intelligence$score))
        ),
        shiny::div(
          class = "executive-status-item",
          shiny::span(class = "executive-status-label", "Competitive Tier"),
          shiny::strong(competitive_tier())
        ),
        shiny::div(
          class = "executive-status-item",
          shiny::span(
            class = "executive-status-label",
            "Financial Status"
          ),
          shiny::strong(
            {
              status <- payroll_operating_status()
              term <- executive_cba_status_term(status)
              
              if (
                !is.null(term) &&
                exists("tbi_cba_link", mode = "function")
              ) {
                tbi_cba_link(
                  term = term,
                  label = status,
                  class = "exec-cba-link"
                )
              } else {
                shiny::span(status)
              }
            }
          )
        )
      )
    })
    
    output$executive_scenario <- shiny::renderUI({
      
      scenario <- active_trade_scenario()
      
      if (is.null(scenario)) {
        return(NULL)
      }
      
      current_team <- as.character(
        selected_team()
      )
      
      partner_team <- as.character(
        scenario$partner_team
      )
      
      current_payroll <- current_payroll_value()
      proposed_payroll <- proposed_payroll_value()
      
      payroll_delta <- if (
        is.na(current_payroll) ||
        is.na(proposed_payroll)
      ) {
        NA_real_
      } else {
        proposed_payroll -
          current_payroll
      }
      
      payroll_delta_label <- if (
        is.na(payroll_delta)
      ) {
        "Unavailable"
      } else if (
        abs(payroll_delta) < 1
      ) {
        "$0.0M"
      } else if (
        payroll_delta > 0
      ) {
        paste0(
          "+",
          format_money_m(
            payroll_delta
          )
        )
      } else {
        paste0(
          "-",
          format_money_m(
            abs(
              payroll_delta
            )
          )
        )
      }
      
      payroll_delta_class <- if (
        is.na(payroll_delta) ||
        abs(payroll_delta) < 1
      ) {
        ""
      } else if (
        payroll_delta < 0
      ) {
        "positive"
      } else {
        "warning"
      }
      
      outgoing_salary <- suppressWarnings(
        as.numeric(
          scenario$outgoing_salary
        )
      )
      
      incoming_salary <- suppressWarnings(
        as.numeric(
          scenario$incoming_salary
        )
      )
      
      if (
        !length(outgoing_salary) ||
        is.na(outgoing_salary[[1]])
      ) {
        outgoing_salary <- NA_real_
      } else {
        outgoing_salary <- outgoing_salary[[1]]
      }
      
      if (
        !length(incoming_salary) ||
        is.na(incoming_salary[[1]])
      ) {
        incoming_salary <- NA_real_
      } else {
        incoming_salary <- incoming_salary[[1]]
      }
      
      cba_result <- scenario_cba_result()
      post_trade_status <- scenario_post_trade_status()
      
      outgoing_players <- if (
        is.data.frame(
          scenario$outgoing_players
        )
      ) {
        scenario$outgoing_players
      } else {
        data.frame()
      }
      
      incoming_players <- if (
        is.data.frame(
          scenario$incoming_players
        )
      ) {
        scenario$incoming_players
      } else {
        data.frame()
      }
      
      outgoing_count <- nrow(
        outgoing_players
      )
      
      incoming_count <- nrow(
        incoming_players
      )
      
      roster_delta <-
        incoming_count -
        outgoing_count
      
      roster_delta_label <- if (
        roster_delta > 0
      ) {
        paste0(
          "+",
          roster_delta,
          " player",
          if (
            roster_delta == 1
          ) "" else "s"
        )
      } else if (
        roster_delta < 0
      ) {
        paste0(
          roster_delta,
          " player",
          if (
            abs(roster_delta) == 1
          ) "" else "s"
        )
      } else {
        "No net change"
      }
      
      # ------------------------------------------------------
      # Safe player-field helpers
      # ------------------------------------------------------
      
      first_field <- function(
    row,
    candidates,
    fallback = NULL) {
        
        if (
          is.null(row) ||
          !is.data.frame(row) ||
          !nrow(row)
        ) {
          return(fallback)
        }
        
        available <- candidates[
          candidates %in%
            names(row)
        ]
        
        if (!length(available)) {
          return(fallback)
        }
        
        for (nm in available) {
          
          value <- row[[nm]][[1]]
          
          if (
            !is.null(value) &&
            length(value) &&
            !is.na(value) &&
            nzchar(
              trimws(
                as.character(
                  value
                )
              )
            )
          ) {
            return(value)
          }
        }
        
        fallback
      }
      
      
      player_name_value <- function(row) {
        
        value <- first_field(
          row,
          c(
            "player_name",
            "name"
          ),
          "Unknown player"
        )
        
        as.character(value)
      }
      
      
      player_meta <- function(row) {
        
        position <- first_field(
          row,
          c(
            "primary_position",
            "position",
            "pos"
          )
        )
        
        age <- first_field(
          row,
          c(
            "player_age",
            "age"
          )
        )
        
        salary <- first_field(
          row,
          c(
            "cap_hit",
            "salary",
            "base_salary"
          )
        )
        
        contract_end <- first_field(
          row,
          c(
            "contract_end_season",
            "free_agent_year"
          )
        )
        
        pieces <- list()
        
        if (!is.null(position)) {
          pieces <- c(
            pieces,
            list(
              shiny::span(
                shiny::strong(
                  as.character(
                    position
                  )
                )
              )
            )
          )
        }
        
        age_num <- suppressWarnings(
          as.numeric(age)
        )
        
        if (
          !is.null(age) &&
          length(age_num) &&
          !is.na(age_num[[1]])
        ) {
          pieces <- c(
            pieces,
            list(
              shiny::span(
                paste0(
                  "Age ",
                  round(
                    age_num[[1]]
                  )
                )
              )
            )
          )
        }
        
        salary_num <- suppressWarnings(
          as.numeric(salary)
        )
        
        if (
          !is.null(salary) &&
          length(salary_num) &&
          !is.na(salary_num[[1]])
        ) {
          pieces <- c(
            pieces,
            list(
              shiny::span(
                format_money_m(
                  salary_num[[1]]
                )
              )
            )
          )
        }
        
        if (!is.null(contract_end)) {
          pieces <- c(
            pieces,
            list(
              shiny::span(
                paste0(
                  "Control: ",
                  as.character(
                    contract_end
                  )
                )
              )
            )
          )
        }
        
        if (!length(pieces)) {
          return(NULL)
        }
        
        shiny::div(
          class = "exec-brief-player-meta",
          pieces
        )
      }
      
      
      player_list <- function(
    player_data,
    direction) {
        
        if (
          is.null(player_data) ||
          !is.data.frame(player_data) ||
          !nrow(player_data)
        ) {
          return(
            shiny::div(
              class = "exec-scenario-text",
              "No players"
            )
          )
        }
        
        shiny::div(
          class = "exec-brief-player-list",
          
          lapply(
            seq_len(
              nrow(player_data)
            ),
            function(i) {
              
              row <- player_data[
                i,
                ,
                drop = FALSE
              ]
              
              shiny::div(
                class = "exec-brief-player",
                
                shiny::span(
                  class = paste(
                    "exec-brief-player-dot",
                    direction
                  )
                ),
                
                shiny::div(
                  shiny::div(
                    class = "exec-brief-player-name",
                    player_name_value(
                      row
                    )
                  ),
                  player_meta(
                    row
                  )
                )
              )
            }
          )
        )
      }
      
      # ------------------------------------------------------
      # Decision support
      # ------------------------------------------------------
      
      intelligence <- basketball_intelligence()
      
      recommendation <- if (
        !is.null(intelligence) &&
        !is.null(
          intelligence$recommendation
        )
      ) {
        as.character(
          intelligence$recommendation
        )
      } else if (
        identical(
          toupper(
            cba_result
          ),
          "FAIL"
        )
      ) {
        "DO NOT ADVANCE"
      } else if (
        identical(
          toupper(
            cba_result
          ),
          "PASS"
        )
      ) {
        "ADVANCE TO BASKETBALL REVIEW"
      } else {
        "REVIEW REQUIRED"
      }
      
      decision_score <- if (
        !is.null(intelligence) &&
        !is.null(
          intelligence$score
        )
      ) {
        suppressWarnings(
          as.numeric(
            intelligence$score
          )
        )
      } else {
        NA_real_
      }
      
      # Use a reason only when the transaction engine actually
      # exposes one. Never infer a CBA failure reason here.
      cba_reason <- NULL
      
      if (
        !is.null(
          scenario$evaluation
        )
      ) {
        
        evaluation <- scenario$evaluation
        
        candidates <- c(
          "reason",
          "failure_reason",
          "status_reason",
          "message",
          "trade_screen_reason"
        )
        
        for (nm in candidates) {
          
          value <- tryCatch(
            evaluation[[nm]],
            error = function(e) NULL
          )
          
          if (
            !is.null(value) &&
            length(value) &&
            !is.na(value[[1]]) &&
            nzchar(
              trimws(
                as.character(
                  value[[1]]
                )
              )
            )
          ) {
            cba_reason <-
              as.character(
                value[[1]]
              )
            
            break
          }
        }
      }
      
      cba_note <- if (
        !is.null(
          cba_reason
        )
      ) {
        cba_reason
      } else if (
        identical(
          toupper(
            cba_result
          ),
          "FAIL"
        )
      ) {
        "Open Trade Intelligence for the controlling CBA failure detail."
      } else if (
        identical(
          toupper(
            cba_result
          ),
          "REVIEW"
        )
      ) {
        "Manual CBA review is required before advancing the transaction."
      } else {
        NULL
      }
      
      cba_class <- if (
        identical(
          toupper(
            cba_result
          ),
          "FAIL"
        )
      ) {
        "fail"
      } else if (
        identical(
          toupper(
            cba_result
          ),
          "REVIEW"
        )
      ) {
        "warning"
      } else {
        ""
      }
      
      # ------------------------------------------------------
      # Executive summary — uses only currently loaded facts
      # ------------------------------------------------------
      
      payroll_sentence <- if (
        is.na(payroll_delta)
      ) {
        "Payroll impact is unavailable from the currently loaded scenario."
      } else if (
        abs(payroll_delta) < 1
      ) {
        "The transaction is approximately payroll-neutral."
      } else if (
        payroll_delta < 0
      ) {
        paste(
          "The transaction reduces projected payroll by",
          format_money_m(
            abs(
              payroll_delta
            )
          ),
          "."
        )
      } else {
        paste(
          "The transaction increases projected payroll by",
          format_money_m(
            payroll_delta
          ),
          "."
        )
      }
      
      roster_sentence <- if (
        roster_delta > 0
      ) {
        paste0(
          "The proposed roster adds ",
          roster_delta,
          " net player",
          if (
            roster_delta == 1
          ) "" else "s",
          "."
        )
      } else if (
        roster_delta < 0
      ) {
        paste0(
          "The proposed roster removes ",
          abs(
            roster_delta
          ),
          " net player",
          if (
            abs(roster_delta) == 1
          ) "" else "s",
          "."
        )
      } else {
        "The transaction creates no net roster-count change."
      }
      
      cba_sentence <- if (
        identical(
          toupper(
            cba_result
          ),
          "FAIL"
        )
      ) {
        "The current construction does not pass the loaded CBA screen."
      } else if (
        identical(
          toupper(
            cba_result
          ),
          "REVIEW"
        )
      ) {
        "The transaction requires manual CBA review."
      } else if (
        identical(
          toupper(
            cba_result
          ),
          "PASS"
        )
      ) {
        "The transaction passes the loaded CBA screen."
      } else {
        "The CBA screen is pending."
      }
      
      executive_summary <- paste(
        payroll_sentence,
        roster_sentence,
        cba_sentence
      )
      
      # ------------------------------------------------------
      # Render
      # ------------------------------------------------------
      
      shiny::div(
        class = "exec-scenario-shell",
        
        shiny::div(
          class = "exec-scenario-header",
          
          shiny::div(
            class = "exec-scenario-title",
            bsicons::bs_icon(
              "clipboard-data"
            ),
            "Executive Transaction Brief"
          ),
          
          shiny::div(
            class = "exec-scenario-live",
            shiny::span(
              class = "exec-scenario-live-dot"
            ),
            "LIVE SCENARIO"
          )
        ),
        
        shiny::div(
          class = "exec-brief-grid",
          
          # The first two columns deliberately show what
          # EACH organization sends. This avoids the prior
          # "Boston Sends / Boston Receives" labeling bug.
          shiny::div(
            class = "exec-brief-block",
            
            shiny::span(
              class = "exec-brief-eyebrow",
              paste0(
                current_team,
                " SENDS"
              )
            ),
            
            player_list(
              outgoing_players,
              "out"
            )
          ),
          
          shiny::div(
            class = "exec-brief-block",
            
            shiny::span(
              class = "exec-brief-eyebrow",
              paste0(
                partner_team,
                " SENDS"
              )
            ),
            
            player_list(
              incoming_players,
              "in"
            )
          ),
          
          shiny::div(
            class = "exec-brief-block",
            
            shiny::span(
              class = "exec-brief-eyebrow",
              paste0(
                "FINANCIAL + ROSTER • ",
                current_team
              )
            ),
            
            shiny::div(
              class = "exec-brief-metric",
              shiny::span(
                "Current Payroll"
              ),
              shiny::strong(
                format_money_m(
                  current_payroll
                )
              )
            ),
            
            shiny::div(
              class = "exec-brief-metric",
              shiny::span(
                "Proposed Payroll"
              ),
              shiny::strong(
                format_money_m(
                  proposed_payroll
                )
              )
            ),
            
            shiny::div(
              class = "exec-brief-metric",
              shiny::span(
                "Net Payroll"
              ),
              shiny::strong(
                class = payroll_delta_class,
                payroll_delta_label
              )
            ),
            
            shiny::div(
              class = "exec-brief-metric",
              shiny::span(
                "Outgoing Salary"
              ),
              shiny::strong(
                if (
                  is.na(
                    outgoing_salary
                  )
                ) {
                  "Unavailable"
                } else {
                  format_money_m(
                    outgoing_salary
                  )
                }
              )
            ),
            
            shiny::div(
              class = "exec-brief-metric",
              shiny::span(
                "Incoming Salary"
              ),
              shiny::strong(
                if (
                  is.na(
                    incoming_salary
                  )
                ) {
                  "Unavailable"
                } else {
                  format_money_m(
                    incoming_salary
                  )
                }
              )
            ),
            
            shiny::div(
              class = "exec-brief-metric",
              shiny::span(
                "Roster Change"
              ),
              shiny::strong(
                roster_delta_label
              )
            )
          ),
          
          shiny::div(
            class = "exec-brief-block",
            
            shiny::span(
              class = "exec-brief-eyebrow",
              paste0(
                "DECISION SUPPORT • ",
                current_team
              )
            ),
            
            shiny::div(
              class = "exec-brief-metric",
              shiny::span(
                "Trade Partner"
              ),
              shiny::strong(
                partner_team
              )
            ),
            
            shiny::div(
              class = "exec-brief-metric",
              shiny::span(
                "CBA Screen"
              ),
              shiny::strong(
                class = cba_class,
                cba_result
              ),
              if (
                !is.null(
                  cba_note
                )
              ) {
                shiny::div(
                  class = "exec-brief-cba-note",
                  cba_note
                )
              }
            ),
            
            shiny::div(
              class = "exec-brief-metric",
              shiny::span(
                "Post-Trade Status"
              ),
              shiny::strong(
                if (
                  is.null(
                    post_trade_status
                  )
                ) {
                  "Pending"
                } else {
                  post_trade_status
                }
              )
            ),
            
            shiny::div(
              class = "exec-brief-recommendation",
              recommendation
            ),
            
            if (
              !is.na(
                decision_score
              )
            ) {
              shiny::div(
                class = "exec-brief-score",
                paste0(
                  "Decision Score ",
                  sprintf(
                    "%.1f",
                    decision_score
                  ),
                  " / 100"
                )
              )
            }
          )
        ),
        
        shiny::div(
          class = "exec-brief-summary",
          
          shiny::div(
            class = "exec-brief-summary-label",
            "EXECUTIVE SUMMARY"
          ),
          
          shiny::div(
            class = "exec-brief-summary-copy",
            executive_summary
          )
        ),
        
        shiny::div(
          class = "exec-scenario-note",
          paste0(
            "Pending transaction preview for ",
            current_team,
            " ↔ ",
            partner_team,
            ". Financial, roster, and executive decision-support outputs are scenario-aware. No database records have been changed."
          )
        )
      )
    })
    
    
    output$executive_cba_reference <- shiny::renderUI({
      
      status <- payroll_operating_status()
      term <- executive_cba_status_term(
        status
      )
      
      if (
        is.null(term) ||
        !exists("tbi_cba_link", mode = "function")
      ) {
        return(NULL)
      }
      
      shiny::div(
        class = "executive-status-item",
        shiny::span(
          class = "executive-status-label",
          "CBA Reference"
        ),
        shiny::strong(
          tbi_cba_link(
            term = term,
            label = term,
            class = "exec-cba-link"
          )
        )
      )
    })
    
    output$team_payroll <- shiny::renderText({
      
      value <- if (
        is.null(
          active_trade_scenario()
        )
      ) {
        current_payroll_value()
      } else {
        proposed_payroll_value()
      }
      
      format_money_m(
        value
      )
    })
    
    output$payroll_rank <- shiny::renderText({
      rank <- safe_payroll_rank()
      if (is.na(rank)) "Unavailable" else paste0("#", rank)
    })
    
    output$contract_count <- shiny::renderText({
      payroll <- safe_payroll()
      if (!nrow(payroll) || !"contracts" %in% names(payroll)) return("Unavailable")
      as.character(payroll$contracts[[1]])
    })
    
    output$conference_rank <- shiny::renderText({
      team <- team_data()
      if (is.null(team)) "Unavailable" else paste0("#", team$conference_rank[[1]])
    })
    
    output$highest_paid_player <- shiny::renderText({
      player <- safe_highest_paid()
      if (!nrow(player) || !"player_name" %in% names(player)) "Unavailable" else as.character(player$player_name[[1]])
    })
    
    output$kpi_scoring <- shiny::renderText({
      team <- team_data()
      if (is.null(team)) "Unavailable" else paste0(sprintf("%.1f", team$points_per_game[[1]]), " PPG")
    })
    
    output$snapshot_conference <- shiny::renderText({
      team <- team_data()
      if (is.null(team)) "Unavailable" else as.character(team$conference[[1]])
    })
    
    output$snapshot_conference_rank <- shiny::renderText({
      team <- team_data()
      if (is.null(team)) "Unavailable" else paste0("#", team$conference_rank[[1]])
    })
    
    output$snapshot_division_rank <- shiny::renderText({
      team <- team_data()
      if (is.null(team) || is.na(team$division_rank[[1]])) "Unavailable" else paste0("#", team$division_rank[[1]])
    })
    
    output$snapshot_scoring <- shiny::renderText({
      team <- team_data()
      if (is.null(team)) "Unavailable" else paste0(sprintf("%.1f", team$points_per_game[[1]]), " PPG")
    })
    
    output$division_position <- shiny::renderText({
      team <- team_data()
      if (is.null(team) || is.na(team$division_rank[[1]])) "Unavailable" else paste0("#", team$division_rank[[1]], " in division")
    })
    
    output$competitive_position <- shiny::renderText({
      team <- team_data()
      if (is.null(team)) return("Unavailable")
      paste0("#", team$conference_rank[[1]], " in conference — ", competitive_tier())
    })
    
    output$scoring_profile <- shiny::renderText({
      team <- team_data()
      if (is.null(team)) return("Unavailable")
      paste0(
        sprintf("%.1f", team$points_per_game[[1]]),
        " PPG | ",
        sprintf("%+.1f", team$point_diff[[1]]),
        " differential"
      )
    })
    
    output$outlook_heading <- shiny::renderText({
      paste(selected_team(), "Strategic Outlook")
    })
    
    output$outlook_summary <- shiny::renderText({
      team <- team_data()
      intelligence <- basketball_intelligence()
      if (is.null(team) || is.null(intelligence)) {
        return(paste("A complete strategic outlook is unavailable for", selected_team()))
      }
      paste0(
        selected_team(), " is currently #", team$conference_rank[[1]],
        " in the ", team$conference[[1]], " Conference with a ",
        sprintf("%+.1f", team$point_diff[[1]]), " point differential. ",
        intelligence$executive_summary
      )
    })
    
    output$standings_heading <- shiny::renderText({
      team <- team_data()
      if (is.null(team)) "Conference Standings" else paste(team$conference[[1]], "Conference Standings")
    })
    
    output$conference_standings <- reactable::renderReactable({
      standings <- conference_data()
      shiny::validate(shiny::need(!is.null(standings) && nrow(standings) > 0, "Conference standings are unavailable."))
      
      reactable::reactable(
        standings,
        searchable = FALSE,
        pagination = FALSE,
        compact = TRUE,
        bordered = FALSE,
        striped = FALSE,
        highlight = TRUE,
        defaultSorted = "conference_rank",
        rowStyle = function(index) {
          row <- standings[index, ]
          if (identical(as.character(row$team_name[[1]]), as.character(selected_team()))) {
            return(list(fontWeight = "700", background = "#122033", color = "white", borderLeft = "4px solid #4f8cff"))
          }
          list(background = "#0b1422", color = "#d8e2ef", borderBottom = "1px solid #1d2c40")
        },
        defaultColDef = reactable::colDef(
          headerStyle = list(background = "#08111f", color = "#9fb3c8", fontWeight = 600, borderBottom = "1px solid #243244"),
          style = list(fontSize = "13px")
        ),
        columns = list(
          conference_rank = reactable::colDef(name = "Rank", align = "center", width = 70),
          team_name = reactable::colDef(name = "Team", minWidth = 180),
          wins = reactable::colDef(name = "W", align = "center", width = 60),
          losses = reactable::colDef(name = "L", align = "center", width = 60),
          win_pct = reactable::colDef(name = "Win %", align = "center", width = 90, format = reactable::colFormat(digits = 3)),
          point_diff = reactable::colDef(name = "+/-", align = "center", width = 80, format = reactable::colFormat(digits = 1))
        )
      )
    })
  })
}
