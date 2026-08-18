# ------------------------------------------------------------
# Module: Player Intelligence / Player Management
# Version 2.1.1 Executive Player Workspace — BIE Player Fit Clean
# ------------------------------------------------------------

# ============================================================
# UI
# ============================================================

#' Player Intelligence UI
#'
#' @param id Internal module ID.
#' @noRd
mod_player_manager_ui <- function(id) {
  ns <- shiny::NS(id)
  
  metric_box <- function(label, output_id, suffix = NULL) {
    shiny::div(
      class = "pi-metric-box",
      shiny::span(class = "pi-metric-label", label),
      shiny::div(
        class = "pi-metric-value",
        shiny::textOutput(ns(output_id), inline = TRUE),
        if (!is.null(suffix)) shiny::span(class = "pi-metric-suffix", suffix)
      )
    )
  }
  
  stat_cell <- function(label, output_id) {
    shiny::div(
      class = "pi-stat-cell",
      shiny::span(class = "pi-stat-label", label),
      shiny::strong(
        class = "pi-stat-value",
        shiny::textOutput(ns(output_id), inline = TRUE)
      )
    )
  }
  
  signal_row <- function(
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
        class = "pi-cba-link"
      )
    } else {
      shiny::span(label)
    }
    
    shiny::div(
      class = "pi-signal-row",
      rendered_label,
      shiny::strong(
        shiny::textOutput(ns(output_id), inline = TRUE)
      )
    )
  }
  
  shiny::div(
    class = "tbi-module-page pi-page",
    `data-tbi-subtab-input` = ns("active_subtab"),
    
    # --------------------------------------------------------
    # Scoped styling
    # --------------------------------------------------------
    
    shiny::tags$style(
      shiny::HTML(
        "
        .pi-page {
          display: grid;
          gap: 12px;
          width: 100%;
        }

        .pi-cba-link {
          color:#72adff !important;
          font-weight:800 !important;
          text-decoration:none !important;
        }

        .pi-cba-link::after {
          content:'  ↗';
          color:#5f9fee;
          font-size:.66em;
          opacity:.78;
        }

        .pi-cba-link:hover {
          color:#a8ceff !important;
          text-decoration:underline !important;
          text-underline-offset:2px;
        }


        .pi-selector-row {
          display: flex;
          align-items: end;
          justify-content: space-between;
          gap: 18px;
          margin-bottom: 2px;
        }

        .pi-selector-wrap {
          width: min(420px, 100%);
        }

        .pi-selector-label,
        .pi-kicker,
        .pi-panel-title,
        .pi-stat-label,
        .pi-metric-label {
          color: #718198;
          font-size: .54rem;
          font-weight: 850;
          letter-spacing: .10em;
          text-transform: uppercase;
        }

        .pi-panel {
          border: 1px solid rgba(148,163,184,.14);
          border-radius: 13px;
          background: linear-gradient(145deg, rgba(17,24,36,.98), rgba(12,18,28,.98));
          box-shadow: 0 10px 28px rgba(0,0,0,.12);
          overflow: hidden;
        }

        .pi-panel-head {
          min-height: 42px;
          padding: 0 14px;
          display: flex;
          align-items: center;
          justify-content: space-between;
          gap: 12px;
          border-bottom: 1px solid rgba(148,163,184,.11);
        }

        .pi-panel-title {
          color: #61a8ff;
          display: flex;
          align-items: center;
          gap: 8px;
        }

        .pi-panel-title svg {
          width: 14px;
          height: 14px;
        }

        .pi-profile-card {
          display: grid;
          grid-template-columns: minmax(330px,1.35fr) minmax(300px,.85fr);
          gap: 0;
          min-height: 150px;
        }

        .pi-profile-main {
          padding: 18px;
          display: grid;
          grid-template-columns: 82px minmax(0,1fr);
          gap: 17px;
          align-items: center;
        }

        .pi-avatar {
          width: 82px;
          height: 82px;
          display: grid;
          place-items: center;
          border: 1px solid rgba(96,165,250,.28);
          border-radius: 50%;
          background:
            radial-gradient(circle at 35% 30%, rgba(96,165,250,.18), transparent 55%),
            #111b2a;
          color: #7db6ff;
          font-size: 1.45rem;
          font-weight: 900;
          letter-spacing: .04em;
        }

        .pi-name-row {
          display: flex;
          flex-wrap: wrap;
          align-items: center;
          gap: 9px;
        }

        .pi-player-name {
          margin: 0;
          color: #f8fafc;
          font-size: clamp(1.55rem,2.6vw,2.15rem);
          font-weight: 800;
          letter-spacing: -.035em;
        }

        .pi-role-pill {
          padding: 5px 9px;
          border: 1px solid rgba(52,211,153,.22);
          border-radius: 999px;
          color: #34d399;
          background: rgba(16,185,129,.07);
          font-size: .55rem;
          font-weight: 850;
          letter-spacing: .07em;
        }

        .pi-player-subtitle {
          margin-top: 5px;
          color: #95a4b7;
          font-size: .78rem;
        }

        .pi-player-detail-line {
          margin-top: 10px;
          color: #73849a;
          font-size: .63rem;
          font-weight: 700;
          letter-spacing: .04em;
        }

        .pi-profile-meta {
          padding: 16px;
          display: grid;
          grid-template-columns: repeat(3, minmax(0,1fr));
          gap: 9px;
          align-content: center;
          border-left: 1px solid rgba(148,163,184,.11);
        }

        .pi-metric-box {
          min-height: 64px;
          padding: 10px 11px;
          display: flex;
          flex-direction: column;
          justify-content: center;
          gap: 4px;
          border: 1px solid rgba(148,163,184,.11);
          border-radius: 9px;
          background: rgba(255,255,255,.015);
        }

        .pi-metric-value {
          color: #edf3f9;
          font-size: .92rem;
          font-weight: 780;
        }

        .pi-metric-suffix {
          margin-left: 3px;
          color: #718198;
          font-size: .6rem;
        }

        .pi-main-grid {
          display: grid;
          grid-template-columns: minmax(0,1fr) 310px;
          gap: 12px;
          align-items: start;
        }

        .pi-left-grid {
          display: grid;
          gap: 12px;
          min-width: 0;
        }

        .pi-three-grid {
          display: grid;
          grid-template-columns: 1.08fr 1.08fr .82fr;
          gap: 12px;
        }

        .pi-three-grid-secondary {
          display: grid;
          grid-template-columns: .92fr 1fr 1.08fr;
          gap: 12px;
        }

        .pi-panel-body {
          padding: 13px 14px;
        }

        .pi-stats-grid {
          display: grid;
          grid-template-columns: repeat(5,minmax(0,1fr));
          border-bottom: 1px solid rgba(148,163,184,.10);
        }

        .pi-stats-grid.secondary {
          margin-top: 4px;
          border-bottom: 0;
        }

        .pi-stat-cell {
          min-height: 58px;
          padding: 9px 8px;
          display: flex;
          flex-direction: column;
          gap: 4px;
          border-right: 1px solid rgba(148,163,184,.09);
        }

        .pi-stat-cell:last-child {
          border-right: 0;
        }

        .pi-stat-value {
          color: #eef3f8;
          font-size: .94rem;
        }

        .pi-empty-note {
          color: #73839a;
          font-size: .63rem;
          line-height: 1.5;
        }

        .pi-role-layout {
          display: grid;
          grid-template-columns: .72fr 1.28fr;
          gap: 12px;
        }

        .pi-role-primary {
          padding-right: 11px;
          border-right: 1px solid rgba(148,163,184,.10);
        }

        .pi-role-name {
          margin: 5px 0 3px;
          color: #64aaff;
          font-size: 1.05rem;
          font-weight: 800;
        }

        .pi-role-tier {
          margin-top: 12px;
          color: #f0f4f8;
          font-size: .72rem;
          font-weight: 750;
        }

        .pi-role-caption {
          color: #7d8da3;
          font-size: .59rem;
        }

        .pi-list-title {
          margin-bottom: 6px;
          color: #7d8da3;
          font-size: .53rem;
          font-weight: 850;
          letter-spacing: .08em;
          text-transform: uppercase;
        }

        .pi-list-title.green { color:#34d399; }
        .pi-list-title.orange { color:#f59e0b; }

        .pi-bullet {
          display: flex;
          gap: 7px;
          margin: 5px 0;
          color: #cbd5e1;
          font-size: .61rem;
          line-height: 1.35;
        }

        .pi-bullet-dot {
          width: 6px;
          height: 6px;
          margin-top: 4px;
          flex: 0 0 6px;
          border-radius: 50%;
          background: #34d399;
        }

        .pi-bullet-dot.orange { background:#f59e0b; }
        .pi-bullet-dot.red { background:#fb7185; }
        .pi-bullet-dot.blue { background:#60a5fa; }

        .pi-signal-row {
          min-height: 34px;
          padding: 6px 0;
          display: flex;
          align-items: center;
          justify-content: space-between;
          gap: 14px;
          border-bottom: 1px solid rgba(148,163,184,.085);
          color: #8493a8;
          font-size: .62rem;
        }

        .pi-signal-row:last-child { border-bottom:0; }

        .pi-signal-row strong {
          color: #f0f4f8;
          text-align: right;
          font-size: .68rem;
        }

        .pi-projection-grid {
          display: grid;
          grid-template-columns: repeat(3,minmax(0,1fr));
          gap: 8px;
        }

        .pi-projection-card {
          min-height: 74px;
          padding: 10px;
          border: 1px solid rgba(148,163,184,.10);
          border-radius: 8px;
          background: rgba(255,255,255,.012);
        }

        .pi-projection-card span {
          color: #728197;
          font-size: .52rem;
          font-weight: 800;
          letter-spacing: .08em;
          text-transform: uppercase;
        }

        .pi-projection-card strong {
          display: block;
          margin-top: 6px;
          color: #34d399;
          font-size: .84rem;
        }

        .pi-flag-row {
          min-height: 29px;
          display: flex;
          justify-content: space-between;
          gap: 10px;
          align-items: center;
          border-bottom: 1px solid rgba(148,163,184,.075);
          font-size: .61rem;
        }

        .pi-flag-row:last-child { border-bottom:0; }
        .pi-flag-row span { color:#a7b3c2; }
        .pi-flag-row strong.yes { color:#34d399; }
        .pi-flag-row strong.no { color:#fb7185; }
        .pi-flag-row strong.review { color:#f59e0b; }

        .pi-development-meter {
          height: 6px;
          margin-top: 7px;
          overflow: hidden;
          border-radius: 99px;
          background: #1d2938;
        }

        .pi-development-fill {
          height: 100%;
          border-radius: inherit;
          background: #4c9aff;
        }

        .pi-right-rail {
          display: grid;
          gap: 12px;
        }

        .pi-summary-status {
          color:#34d399;
          font-size:.55rem;
          font-weight:850;
          letter-spacing:.08em;
        }

        .pi-summary-grid {
          display:grid;
          grid-template-columns:repeat(2,minmax(0,1fr));
          gap:8px;
        }

        .pi-risk {
          padding:10px;
          border:1px solid rgba(251,113,133,.20);
          border-radius:8px;
          background:rgba(127,29,29,.07);
        }

        .pi-opportunity {
          padding:10px;
          border:1px solid rgba(52,211,153,.20);
          border-radius:8px;
          background:rgba(6,78,59,.08);
        }

        .pi-risk strong { color:#fb7185; font-size:.60rem; }
        .pi-opportunity strong { color:#34d399; font-size:.60rem; }

        .pi-risk p,
        .pi-opportunity p {
          margin:4px 0 0;
          color:#9aa8ba;
          font-size:.60rem;
          line-height:1.4;
        }

        .pi-recommendation {
          display:grid;
          grid-template-columns:220px minmax(0,1fr) minmax(0,1fr);
          gap:18px;
          padding:15px 17px;
        }

        .pi-rec-label {
          color:#34d399;
          font-size:1.55rem;
          font-weight:850;
          letter-spacing:-.03em;
        }

        .pi-rec-sub {
          margin-top:2px;
          color:#7d8da3;
          font-size:.61rem;
        }

        .pi-rec-list {
          margin:0;
          padding-left:16px;
          color:#cbd5e1;
          font-size:.62rem;
          line-height:1.6;
        }

        .pi-data-note {
          padding:10px 13px;
          color:#718198;
          font-size:.58rem;
          line-height:1.45;
        }

        .pi-trade-banner {
          display:flex;
          align-items:center;
          justify-content:space-between;
          gap:14px;
          padding:10px 13px;
          border:1px solid rgba(96,165,250,.24);
          border-radius:10px;
          background:linear-gradient(
            90deg,
            rgba(59,130,246,.10),
            rgba(59,130,246,.03)
          );
        }

        .pi-trade-banner-copy {
          min-width:0;
          color:#a9b8ca;
          font-size:.60rem;
          line-height:1.45;
        }

        .pi-trade-chip {
          display:inline-flex;
          align-items:center;
          gap:6px;
          margin-right:8px;
          padding:5px 8px;
          border-radius:999px;
          background:rgba(59,130,246,.12);
          color:#72adff;
          font-size:.52rem;
          font-weight:900;
          letter-spacing:.08em;
          white-space:nowrap;
        }

        .pi-trade-summary {
          color:#f4f8fc;
          font-size:.60rem;
          font-weight:850;
          white-space:nowrap;
        }


        .pi-bie-panel {
          border-color:rgba(96,165,250,.20);
          background:
            linear-gradient(
              145deg,
              rgba(14,29,47,.98),
              rgba(10,20,33,.98)
            );
        }

        .pi-bie-chip {
          display:inline-flex;
          align-items:center;
          padding:4px 7px;
          border:1px solid rgba(96,165,250,.22);
          border-radius:999px;
          background:rgba(59,130,246,.07);
          color:#8fc0ff;
          font-size:.47rem;
          font-weight:900;
          letter-spacing:.07em;
        }

        .pi-bie-layout {
          display:grid;
          grid-template-columns:190px minmax(0,1fr);
          gap:14px;
        }

        .pi-bie-score-card {
          display:flex;
          flex-direction:column;
          justify-content:center;
          min-height:155px;
          padding:14px;
          border:1px solid rgba(96,165,250,.14);
          border-radius:10px;
          background:rgba(59,130,246,.035);
        }

        .pi-bie-score-card span {
          color:#70849d;
          font-size:.48rem;
          font-weight:900;
          letter-spacing:.09em;
          text-transform:uppercase;
        }

        .pi-bie-score {
          margin-top:4px;
          color:#f5f9fd;
          font-size:2rem;
          font-weight:900;
          letter-spacing:-.04em;
        }

        .pi-bie-grade {
          color:#60a5fa;
          font-size:.78rem;
          font-weight:900;
        }

        .pi-bie-confidence {
          margin-top:8px;
          color:#8ca0ba;
          font-size:.50rem;
          font-weight:800;
        }

        .pi-bie-rank {
          margin-top:4px;
          color:#9db0c6;
          font-size:.51rem;
        }

        .pi-bie-profile {
          display:grid;
          gap:10px;
        }

        .pi-bie-profile-top {
          display:grid;
          grid-template-columns:repeat(4,minmax(0,1fr));
          gap:7px;
        }

        .pi-bie-profile-item {
          min-width:0;
          padding:8px 9px;
          border:1px solid rgba(148,163,184,.09);
          border-radius:8px;
          background:rgba(255,255,255,.016);
        }

        .pi-bie-profile-item span {
          display:block;
          color:#6f839c;
          font-size:.43rem;
          font-weight:900;
          letter-spacing:.07em;
          text-transform:uppercase;
        }

        .pi-bie-profile-item strong {
          display:block;
          margin-top:3px;
          color:#eaf1f8;
          font-size:.59rem;
          font-weight:850;
          line-height:1.3;
        }

        .pi-bie-component-grid {
          display:grid;
          grid-template-columns:repeat(6,minmax(0,1fr));
          gap:6px;
        }

        .pi-bie-component {
          padding:8px 7px;
          text-align:center;
          border:1px solid rgba(148,163,184,.08);
          border-radius:8px;
          background:rgba(255,255,255,.012);
        }

        .pi-bie-component span {
          display:block;
          color:#6f839c;
          font-size:.40rem;
          font-weight:900;
          letter-spacing:.05em;
          text-transform:uppercase;
        }

        .pi-bie-component strong {
          display:block;
          margin-top:4px;
          color:#edf4fb;
          font-size:.62rem;
          font-weight:900;
        }

        .pi-bie-notes {
          display:grid;
          grid-template-columns:1fr 1fr;
          gap:9px;
        }

        .pi-bie-note-box {
          padding:9px 10px;
          border:1px solid rgba(148,163,184,.08);
          border-radius:8px;
          background:rgba(255,255,255,.012);
        }

        .pi-bie-note-title {
          display:block;
          margin-bottom:6px;
          color:#71859e;
          font-size:.43rem;
          font-weight:900;
          letter-spacing:.08em;
          text-transform:uppercase;
        }

        .pi-bie-note-item {
          display:flex;
          gap:6px;
          margin-bottom:5px;
          color:#9fb0c5;
          font-size:.50rem;
          line-height:1.4;
        }

        .pi-bie-note-item:last-child {
          margin-bottom:0;
        }

        .pi-bie-note-dot {
          width:5px;
          height:5px;
          flex:0 0 auto;
          margin-top:5px;
          border-radius:50%;
          background:#34d399;
        }

        .pi-bie-note-dot.concern {
          background:#fbbf24;
        }

        .pi-bie-scope {
          color:#788da7;
          font-size:.50rem;
          line-height:1.45;
        }

        @media(max-width:1000px) {
          .pi-bie-layout {
            grid-template-columns:1fr;
          }

          .pi-bie-score-card {
            min-height:auto;
          }

          .pi-bie-profile-top {
            grid-template-columns:1fr 1fr;
          }

          .pi-bie-component-grid {
            grid-template-columns:repeat(3,1fr);
          }
        }

        @media(max-width:680px) {
          .pi-bie-profile-top,
          .pi-bie-notes {
            grid-template-columns:1fr;
          }

          .pi-bie-component-grid {
            grid-template-columns:1fr 1fr;
          }
        }

        @media (max-width: 1250px) {
          .pi-main-grid { grid-template-columns:1fr; }
          .pi-right-rail { grid-template-columns:repeat(3,minmax(0,1fr)); }
        }

        @media (max-width: 1000px) {
          .pi-three-grid,
          .pi-three-grid-secondary { grid-template-columns:1fr; }
          .pi-profile-card { grid-template-columns:1fr; }
          .pi-profile-meta { border-left:0; border-top:1px solid rgba(148,163,184,.11); }
          .pi-recommendation { grid-template-columns:1fr; }
          .pi-right-rail { grid-template-columns:1fr; }
        }

        @media (max-width: 680px) {
          .pi-profile-main { grid-template-columns:1fr; }
          .pi-avatar { width:64px; height:64px; }
          .pi-profile-meta { grid-template-columns:repeat(2,minmax(0,1fr)); }
          .pi-stats-grid { grid-template-columns:repeat(2,minmax(0,1fr)); }
          .pi-selector-row { align-items:stretch; flex-direction:column; }
        }
        "
      )
    ),
    
    # --------------------------------------------------------
    # Player selector
    # --------------------------------------------------------
    
    shiny::div(
      class = "pi-selector-row",
      
      shiny::div(
        shiny::div(
          class = "tbi-page-eyebrow",
          "PLAYER EVALUATION"
        ),
        shiny::h2(
          class = "tbi-v2-module-title",
          "Player Intelligence"
        ),
        shiny::p(
          class = "tbi-v2-module-subtitle",
          paste(
            "Integrate roster role, contract control, current production,",
            "CBA flags, development context, and future outlook."
          )
        )
      ),
      
      shiny::div(
        class = "pi-selector-wrap",
        shiny::div(
          class = "pi-selector-label",
          "SELECT PLAYER"
        ),
        shiny::selectInput(
          ns("selected_player"),
          label = NULL,
          choices = NULL,
          width = "100%"
        )
      )
    ),
    
    shiny::uiOutput(
      ns("player_trade_scenario_banner")
    ),
    
    # --------------------------------------------------------
    # Player identity card
    # --------------------------------------------------------
    
    shiny::tags$section(
      class = "pi-panel pi-profile-card",
      
      shiny::div(
        class = "pi-profile-main",
        
        shiny::div(
          class = "pi-avatar",
          shiny::textOutput(
            ns("player_initials"),
            inline = TRUE
          )
        ),
        
        shiny::div(
          shiny::div(
            class = "pi-name-row",
            
            shiny::h3(
              class = "pi-player-name",
              shiny::textOutput(
                ns("player_name"),
                inline = TRUE
              )
            ),
            
            shiny::span(
              class = "pi-role-pill",
              shiny::textOutput(
                ns("role_badge"),
                inline = TRUE
              )
            )
          ),
          
          shiny::div(
            class = "pi-player-subtitle",
            shiny::textOutput(
              ns("player_subtitle"),
              inline = TRUE
            )
          ),
          
          shiny::div(
            class = "pi-player-detail-line",
            shiny::textOutput(
              ns("player_detail_line"),
              inline = TRUE
            )
          )
        )
      ),
      
      shiny::div(
        class = "pi-profile-meta",
        metric_box("HEIGHT", "height"),
        metric_box("WEIGHT", "weight"),
        metric_box("AGE", "age"),
        metric_box("CAP HIT", "cap_hit"),
        metric_box("CONTRACT THROUGH", "contract_through"),
        metric_box("TEAM CONTROL", "team_control")
      )
    ),
    
    # --------------------------------------------------------
    # Main body
    # --------------------------------------------------------
    
    shiny::div(
      class = "pi-main-grid",
      
      # LEFT CONTENT
      shiny::div(
        class = "pi-left-grid",
        
        shiny::div(
          class = "pi-three-grid",
          
          # Current performance
          shiny::tags$section(
            class = "pi-panel",
            
            shiny::div(
              class = "pi-panel-head",
              shiny::div(
                class = "pi-panel-title",
                bsicons::bs_icon("graph-up-arrow"),
                "CURRENT PERFORMANCE"
              ),
              shiny::span(
                class = "pi-kicker",
                shiny::textOutput(
                  ns("performance_season_label"),
                  inline = TRUE
                )
              )
            ),
            
            shiny::div(
              class = "pi-panel-body",
              
              shiny::div(
                class = "pi-stats-grid",
                stat_cell("PTS", "pts"),
                stat_cell("REB", "reb"),
                stat_cell("AST", "ast"),
                stat_cell("BLK", "blk"),
                stat_cell("STL", "stl")
              ),
              
              shiny::div(
                class = "pi-stats-grid secondary",
                stat_cell("TS%", "ts_pct"),
                stat_cell("USG%", "usg_pct"),
                stat_cell("OREB%", "oreb_pct"),
                stat_cell("DREB%", "dreb_pct"),
                stat_cell("MIN", "minutes")
              ),
              
              shiny::div(
                class = "pi-empty-note",
                style = "margin-top:9px;",
                shiny::textOutput(
                  ns("stats_data_status"),
                  inline = TRUE
                )
              )
            )
          ),
          
          # Role & value
          shiny::tags$section(
            class = "pi-panel",
            
            shiny::div(
              class = "pi-panel-head",
              shiny::div(
                class = "pi-panel-title",
                bsicons::bs_icon("person-badge"),
                "ROLE & VALUE"
              )
            ),
            
            shiny::div(
              class = "pi-panel-body pi-role-layout",
              
              shiny::div(
                class = "pi-role-primary",
                
                shiny::div(
                  class = "pi-kicker",
                  "CURRENT ROLE"
                ),
                
                shiny::div(
                  class = "pi-role-name",
                  shiny::textOutput(
                    ns("current_role"),
                    inline = TRUE
                  )
                ),
                
                shiny::div(
                  class = "pi-role-caption",
                  shiny::textOutput(
                    ns("role_context"),
                    inline = TRUE
                  )
                ),
                
                shiny::div(
                  class = "pi-role-tier",
                  shiny::textOutput(
                    ns("player_tier"),
                    inline = TRUE
                  )
                ),
                
                shiny::div(
                  class = "pi-role-caption",
                  "Roster / contract-based classification"
                )
              ),
              
              shiny::div(
                shiny::div(
                  class = "pi-list-title green",
                  "STRENGTHS"
                ),
                shiny::uiOutput(
                  ns("strengths")
                ),
                
                shiny::div(
                  class = "pi-list-title orange",
                  style = "margin-top:10px;",
                  "AREAS TO REVIEW"
                ),
                shiny::uiOutput(
                  ns("development_areas")
                )
              )
            )
          ),
          
          # Advanced impact
          shiny::tags$section(
            class = "pi-panel",
            
            shiny::div(
              class = "pi-panel-head",
              shiny::div(
                class = "pi-panel-title",
                bsicons::bs_icon("bullseye"),
                "ADVANCED IMPACT"
              )
            ),
            
            shiny::div(
              class = "pi-panel-body",
              
              shiny::div(
                class = "pi-summary-grid",
                metric_box("OFF RTG", "off_rtg"),
                metric_box("DEF RTG", "def_rtg"),
                metric_box("NET RTG", "net_rtg"),
                metric_box("IMPACT", "impact_metric")
              ),
              
              shiny::div(
                class = "pi-empty-note",
                style = "margin-top:9px;",
                shiny::textOutput(
                  ns("advanced_data_status"),
                  inline = TRUE
                )
              )
            )
          )
        ),
        
        # ----------------------------------------------------
        # BIE Player Fit Intelligence
        # ----------------------------------------------------
        
        shiny::tags$section(
          class = "pi-panel pi-bie-panel",
          
          shiny::div(
            class = "pi-panel-head",
            
            shiny::div(
              class = "pi-panel-title",
              bsicons::bs_icon("cpu"),
              "PLAYER VALUE & FIT INTELLIGENCE"
            ),
            
            shiny::span(
              class = "pi-bie-chip",
              "BIE v3.4"
            )
          ),
          
          shiny::div(
            class = "pi-panel-body",
            
            shiny::div(
              class = "pi-bie-layout",
              
              shiny::div(
                class = "pi-bie-score-card",
                
                shiny::span(
                  "BIE PERFORMANCE SCORE"
                ),
                
                shiny::div(
                  class = "pi-bie-score",
                  shiny::textOutput(
                    ns("bie_player_score"),
                    inline = TRUE
                  )
                ),
                
                shiny::div(
                  class = "pi-bie-grade",
                  shiny::textOutput(
                    ns("bie_player_grade"),
                    inline = TRUE
                  )
                ),
                
                shiny::div(
                  class = "pi-bie-confidence",
                  shiny::textOutput(
                    ns("bie_player_confidence"),
                    inline = TRUE
                  )
                ),
                
                shiny::div(
                  class = "pi-bie-rank",
                  shiny::textOutput(
                    ns("bie_player_rank"),
                    inline = TRUE
                  )
                )
              ),
              
              shiny::div(
                class = "pi-bie-profile",
                
                shiny::div(
                  class = "pi-bie-profile-top",
                  
                  shiny::div(
                    class = "pi-bie-profile-item",
                    shiny::span("ARCHETYPE"),
                    shiny::strong(
                      shiny::textOutput(
                        ns("bie_archetype"),
                        inline = TRUE
                      )
                    )
                  ),
                  
                  shiny::div(
                    class = "pi-bie-profile-item",
                    shiny::span("BEST POSITION"),
                    shiny::strong(
                      shiny::textOutput(
                        ns("bie_best_position"),
                        inline = TRUE
                      )
                    )
                  ),
                  
                  shiny::div(
                    class = "pi-bie-profile-item",
                    shiny::span("SECONDARY"),
                    shiny::strong(
                      shiny::textOutput(
                        ns("bie_secondary_positions"),
                        inline = TRUE
                      )
                    )
                  ),
                  
                  shiny::div(
                    class = "pi-bie-profile-item",
                    shiny::span("AGE CURVE"),
                    shiny::strong(
                      shiny::textOutput(
                        ns("bie_age_curve"),
                        inline = TRUE
                      )
                    )
                  )
                ),
                
                shiny::div(
                  class = "pi-bie-component-grid",
                  
                  shiny::div(
                    class = "pi-bie-component",
                    shiny::span("IMPACT"),
                    shiny::strong(
                      shiny::textOutput(
                        ns("bie_impact"),
                        inline = TRUE
                      )
                    )
                  ),
                  
                  shiny::div(
                    class = "pi-bie-component",
                    shiny::span("OFFENSE"),
                    shiny::strong(
                      shiny::textOutput(
                        ns("bie_offense"),
                        inline = TRUE
                      )
                    )
                  ),
                  
                  shiny::div(
                    class = "pi-bie-component",
                    shiny::span("DEFENSE"),
                    shiny::strong(
                      shiny::textOutput(
                        ns("bie_defense"),
                        inline = TRUE
                      )
                    )
                  ),
                  
                  shiny::div(
                    class = "pi-bie-component",
                    shiny::span("EFFICIENCY"),
                    shiny::strong(
                      shiny::textOutput(
                        ns("bie_efficiency"),
                        inline = TRUE
                      )
                    )
                  ),
                  
                  shiny::div(
                    class = "pi-bie-component",
                    shiny::span("PLAYMAKING"),
                    shiny::strong(
                      shiny::textOutput(
                        ns("bie_playmaking"),
                        inline = TRUE
                      )
                    )
                  ),
                  
                  shiny::div(
                    class = "pi-bie-component",
                    shiny::span("REBOUNDING"),
                    shiny::strong(
                      shiny::textOutput(
                        ns("bie_rebounding"),
                        inline = TRUE
                      )
                    )
                  )
                ),
                
                shiny::div(
                  class = "pi-bie-notes",
                  
                  shiny::div(
                    class = "pi-bie-note-box",
                    shiny::span(
                      class = "pi-bie-note-title",
                      "BIE STRENGTHS"
                    ),
                    shiny::uiOutput(
                      ns("bie_player_strengths")
                    )
                  ),
                  
                  shiny::div(
                    class = "pi-bie-note-box",
                    shiny::span(
                      class = "pi-bie-note-title",
                      "BIE CONCERNS"
                    ),
                    shiny::uiOutput(
                      ns("bie_player_concerns")
                    )
                  )
                ),
                
                shiny::div(
                  class = "pi-bie-scope",
                  shiny::textOutput(
                    ns("bie_player_scope"),
                    inline = TRUE
                  )
                )
              )
            )
          )
        ),
        
        # ----------------------------------------------------
        # Secondary row
        # ----------------------------------------------------
        
        shiny::div(
          class = "pi-three-grid-secondary",
          
          # Future projection
          shiny::tags$section(
            class = "pi-panel",
            
            shiny::div(
              class = "pi-panel-head",
              shiny::div(
                class = "pi-panel-title",
                bsicons::bs_icon("graph-up-arrow"),
                "FUTURE PROJECTION"
              )
            ),
            
            shiny::div(
              class = "pi-panel-body",
              
              shiny::div(
                class = "pi-projection-grid",
                
                shiny::div(
                  class = "pi-projection-card",
                  shiny::span("1-YEAR OUTLOOK"),
                  shiny::strong(
                    shiny::textOutput(
                      ns("projection_1y"),
                      inline = TRUE
                    )
                  )
                ),
                
                shiny::div(
                  class = "pi-projection-card",
                  shiny::span("3-YEAR OUTLOOK"),
                  shiny::strong(
                    shiny::textOutput(
                      ns("projection_3y"),
                      inline = TRUE
                    )
                  )
                ),
                
                shiny::div(
                  class = "pi-projection-card",
                  shiny::span("TRAJECTORY"),
                  shiny::strong(
                    shiny::textOutput(
                      ns("trajectory"),
                      inline = TRUE
                    )
                  )
                )
              ),
              
              shiny::div(
                class = "pi-empty-note",
                style = "margin-top:9px;",
                shiny::textOutput(
                  ns("projection_data_status"),
                  inline = TRUE
                )
              )
            )
          ),
          
          # CBA flags
          shiny::tags$section(
            class = "pi-panel",
            
            shiny::div(
              class = "pi-panel-head",
              shiny::div(
                class = "pi-panel-title",
                bsicons::bs_icon("exclamation-triangle"),
                "CBA FLAGS & ELIGIBILITY"
              )
            ),
            
            shiny::div(
              class = "pi-panel-body",
              shiny::uiOutput(
                ns("cba_flags")
              )
            )
          ),
          
          # Development
          shiny::tags$section(
            class = "pi-panel",
            
            shiny::div(
              class = "pi-panel-head",
              shiny::div(
                class = "pi-panel-title",
                bsicons::bs_icon("people"),
                "DEVELOPMENT INSIGHTS"
              )
            ),
            
            shiny::div(
              class = "pi-panel-body",
              
              shiny::div(
                class = "pi-kicker",
                "DEVELOPMENT FOCUS"
              ),
              
              shiny::p(
                style = "margin:6px 0 12px; color:#cbd5e1; font-size:.64rem; line-height:1.5;",
                shiny::textOutput(
                  ns("development_focus"),
                  inline = TRUE
                )
              ),
              
              shiny::div(
                class = "pi-kicker",
                "DEVELOPMENT UPSIDE"
              ),
              
              shiny::div(
                class = "pi-development-meter",
                shiny::div(
                  class = "pi-development-fill",
                  shiny::uiOutput(
                    ns("development_meter")
                  )
                )
              ),
              
              shiny::div(
                style = "display:grid; grid-template-columns:1fr 1fr; gap:8px; margin-top:12px;",
                metric_box("RISK LEVEL", "development_risk"),
                metric_box("AGE CURVE", "age_curve")
              )
            )
          )
        ),
        
        # ----------------------------------------------------
        # Executive recommendation
        # ----------------------------------------------------
        
        shiny::tags$section(
          class = "pi-panel",
          
          shiny::div(
            class = "pi-panel-head",
            shiny::div(
              class = "pi-panel-title",
              bsicons::bs_icon("bullseye"),
              "EXECUTIVE RECOMMENDATION"
            ),
            shiny::span(
              class = "pi-kicker",
              "DECISION SUPPORT"
            )
          ),
          
          shiny::div(
            class = "pi-recommendation",
            
            shiny::div(
              shiny::div(
                class = "pi-rec-label",
                shiny::textOutput(
                  ns("recommendation"),
                  inline = TRUE
                )
              ),
              shiny::div(
                class = "pi-rec-sub",
                shiny::textOutput(
                  ns("recommendation_subtitle"),
                  inline = TRUE
                )
              )
            ),
            
            shiny::div(
              shiny::div(
                class = "pi-kicker",
                style = "margin-bottom:5px;",
                "RATIONALE"
              ),
              shiny::uiOutput(
                ns("recommendation_rationale")
              )
            ),
            
            shiny::div(
              shiny::div(
                class = "pi-kicker",
                style = "margin-bottom:5px;",
                "RECOMMENDED ACTIONS"
              ),
              shiny::uiOutput(
                ns("recommended_actions")
              )
            )
          )
        ),
        
        shiny::div(
          class = "pi-panel pi-data-note",
          shiny::textOutput(
            ns("model_scope_note"),
            inline = TRUE
          )
        )
      ),
      
      # RIGHT RAIL
      shiny::tags$aside(
        class = "pi-right-rail",
        
        # Player summary
        shiny::tags$section(
          class = "pi-panel",
          
          shiny::div(
            class = "pi-panel-head",
            shiny::div(
              class = "pi-panel-title",
              "PLAYER SUMMARY"
            ),
            shiny::span(
              class = "pi-summary-status",
              "ACTIVE"
            )
          ),
          
          shiny::div(
            class = "pi-panel-body",
            
            shiny::div(
              class = "pi-summary-grid",
              metric_box("POSITION", "summary_position"),
              metric_box("ROLE", "summary_role"),
              metric_box("STATUS", "summary_status"),
              metric_box("MINUTES", "summary_minutes")
            ),
            
            shiny::div(
              style = "margin-top:9px;",
              signal_row("Overall view", "overall_view"),
              signal_row("Timeline", "timeline"),
              signal_row("Roster category", "roster_category")
            )
          )
        ),
        
        # Contract intelligence
        shiny::tags$section(
          class = "pi-panel",
          
          shiny::div(
            class = "pi-panel-head",
            shiny::div(
              class = "pi-panel-title",
              bsicons::bs_icon("cash-stack"),
              "CONTRACT INTELLIGENCE"
            )
          ),
          
          shiny::div(
            class = "pi-panel-body",
            signal_row("Cap hit", "contract_cap_hit"),
            signal_row("Years remaining", "years_remaining"),
            signal_row("Total value", "total_value"),
            signal_row("Guaranteed", "guaranteed_value"),
            signal_row("Contract type", "contract_type"),
            signal_row(
              "Bird rights",
              "bird_rights",
              cba_term = "Bird Exception"
            ),
            signal_row("Option", "option_type"),
            signal_row("Free agency", "free_agent_year")
          )
        ),
        
        # Risk opportunity
        shiny::tags$section(
          class = "pi-panel",
          
          shiny::div(
            class = "pi-panel-head",
            shiny::div(
              class = "pi-panel-title",
              bsicons::bs_icon("exclamation-triangle"),
              "RISK & OPPORTUNITY"
            )
          ),
          
          shiny::div(
            class = "pi-panel-body",
            
            shiny::div(
              class = "pi-risk",
              shiny::strong("KEY RISK"),
              shiny::p(
                shiny::textOutput(
                  ns("key_risk"),
                  inline = TRUE
                )
              )
            ),
            
            shiny::div(
              class = "pi-opportunity",
              style = "margin-top:9px;",
              shiny::strong("KEY OPPORTUNITY"),
              shiny::p(
                shiny::textOutput(
                  ns("key_opportunity"),
                  inline = TRUE
                )
              )
            )
          )
        )
      )
    )
  )
}


# ============================================================
# SERVER
# ============================================================

#' Player Intelligence server
#'
#' @param id Internal module ID.
#' @param selected_team Reactive selected organization.
#' @param selected_season Reactive selected season.
#' @noRd
mod_player_manager_server <- function(
    id,
    selected_team,
    selected_season,
    transaction_state = NULL) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      subtab_seen <- shiny::reactiveValues(
        overview = TRUE,
        value = FALSE,
        development = FALSE,
        contract = FALSE,
        recommendation = FALSE
      )

      shiny::observeEvent(
        input$active_subtab,
        {
          tab <- as.character(input$active_subtab)
          valid_tabs <- c(
            "overview",
            "value",
            "development",
            "contract",
            "recommendation"
          )

          if (length(tab) == 1L && tab %in% valid_tabs) {
            subtab_seen[[tab]] <- TRUE
          }
        },
        ignoreInit = FALSE
      )

      subtab_ready <- function(tab) {
        shiny::req(isTRUE(subtab_seen[[tab]]))
        invisible(TRUE)
      }
      
      # ------------------------------------------------------
      # Generic helpers
      # ------------------------------------------------------
      
      money <- function(x) {
        x <- suppressWarnings(as.numeric(x))
        
        if (!length(x) || is.na(x)) {
          return("—")
        }
        
        if (abs(x) >= 1e9) {
          return(sprintf("$%.2fB", x / 1e9))
        }
        
        if (abs(x) >= 1e6) {
          return(sprintf("$%.1fM", x / 1e6))
        }
        
        if (abs(x) >= 1e3) {
          return(sprintf("$%.0fK", x / 1e3))
        }
        
        paste0(
          "$",
          format(
            round(x),
            big.mark = ",",
            scientific = FALSE
          )
        )
      }
      
      text_value <- function(x, fallback = "—") {
        if (
          is.null(x) ||
          !length(x) ||
          is.na(x[[1]]) ||
          !nzchar(trimws(as.character(x[[1]])))
        ) {
          fallback
        } else {
          as.character(x[[1]])
        }
      }
      
      num_value <- function(x, fallback = NA_real_) {
        value <- suppressWarnings(as.numeric(x))
        
        if (!length(value) || is.na(value[[1]])) {
          fallback
        } else {
          value[[1]]
        }
      }
      
      first_existing <- function(d, names, fallback = NA) {
        if (is.null(d) || !nrow(d)) {
          return(fallback)
        }
        
        available <- names[names %in% colnames(d)]
        
        if (!length(available)) {
          return(fallback)
        }
        
        for (nm in available) {
          value <- d[[nm]][[1]]
          
          if (
            !is.null(value) &&
            length(value) &&
            !is.na(value)
          ) {
            return(value)
          }
        }
        
        fallback
      }
      
      format_inches <- function(x) {
        x <- num_value(x)
        
        if (is.na(x)) {
          return("—")
        }
        
        feet <- floor(x / 12)
        inches <- round(x - feet * 12)
        
        paste0(
          feet,
          "'",
          inches,
          "\""
        )
      }
      
      flag_row <- function(
    label,
    value,
    state = "review",
    cba_term = NULL) {
        
        rendered_label <- if (
          !is.null(cba_term) &&
          nzchar(as.character(cba_term)) &&
          exists("tbi_cba_link", mode = "function")
        ) {
          tbi_cba_link(
            term = cba_term,
            label = label,
            class = "pi-cba-link"
          )
        } else {
          shiny::span(label)
        }
        
        shiny::div(
          class = "pi-flag-row",
          rendered_label,
          shiny::strong(
            class = state,
            value
          )
        )
      }
      
      extension_cba_term <- function(contract_type) {
        
        value <- tolower(
          as.character(
            contract_type %||% ""
          )
        )
        
        if (grepl("rookie", value, fixed = TRUE)) {
          return("Rookie-Scale Extension")
        }
        
        if (grepl("veteran", value, fixed = TRUE)) {
          return("Veteran Extension")
        }
        
        "Veteran Extension"
      }
      
      bullet_list <- function(items, tone = "green") {
        if (!length(items)) {
          return(
            shiny::div(
              class = "pi-empty-note",
              "No loaded signal."
            )
          )
        }
        
        shiny::tagList(
          lapply(
            items,
            function(item) {
              shiny::div(
                class = "pi-bullet",
                shiny::span(
                  class = paste(
                    "pi-bullet-dot",
                    tone
                  )
                ),
                shiny::span(item)
              )
            }
          )
        )
      }
      
      # ------------------------------------------------------
      # Database metadata helpers
      # ------------------------------------------------------
      
      table_exists <- function(con, table_name) {
        table_name %in% DBI::dbListTables(con)
      }
      
      table_fields <- function(con, table_name) {
        if (!table_exists(con, table_name)) {
          return(character())
        }
        
        DBI::dbListFields(
          con,
          table_name
        )
      }
      
      
      # ------------------------------------------------------
      # Performance-season / evidence helpers
      # ------------------------------------------------------
      
      season_start_year <- function(x) {
        
        value <- suppressWarnings(
          as.integer(
            substr(
              as.character(x),
              1,
              4
            )
          )
        )
        
        if (
          !length(value) ||
          is.na(value[[1]])
        ) {
          NA_integer_
        } else {
          value[[1]]
        }
      }
      
      
      latest_player_performance_season <- function(
    con,
    player_id,
    roster_season) {
        
        if (
          !"player_season_stats" %in%
          DBI::dbListTables(con)
        ) {
          return(
            as.character(
              roster_season
            )
          )
        }
        
        fields <- DBI::dbListFields(
          con,
          "player_season_stats"
        )
        
        if (
          !"player_id" %in% fields ||
          !"season" %in% fields
        ) {
          return(
            as.character(
              roster_season
            )
          )
        }
        
        game_filter <- if (
          "games_played" %in% fields
        ) {
          "AND COALESCE(games_played, 0) > 0"
        } else {
          ""
        }
        
        result <- tryCatch(
          DBI::dbGetQuery(
            con,
            paste0(
              "
              SELECT season
              FROM player_season_stats
              WHERE player_id = ?
                AND season <= ?
              ",
              game_filter,
              "
              GROUP BY season
              ORDER BY season DESC
              LIMIT 1
              "
            ),
            params = list(
              as.integer(player_id),
              as.character(roster_season)
            )
          ),
          error = function(e) {
            data.frame()
          }
        )
        
        if (
          !nrow(result) ||
          is.na(result$season[[1]])
        ) {
          return(
            as.character(
              roster_season
            )
          )
        }
        
        as.character(
          result$season[[1]]
        )
      }
      
      
      latest_row_for_player <- function(
    con,
    table_name,
    player_id,
    performance_season) {
        
        if (
          !table_name %in%
          DBI::dbListTables(con)
        ) {
          return(
            data.frame()
          )
        }
        
        fields <- DBI::dbListFields(
          con,
          table_name
        )
        
        if (
          !"player_id" %in% fields
        ) {
          return(
            data.frame()
          )
        }
        
        where <- c(
          "player_id = ?"
        )
        
        params <- list(
          as.integer(
            player_id
          )
        )
        
        if (
          "season" %in% fields
        ) {
          where <- c(
            where,
            "season = ?"
          )
          
          params <- c(
            params,
            list(
              as.character(
                performance_season
              )
            )
          )
        }
        
        order_clause <- if (
          "minutes" %in% fields
        ) {
          " ORDER BY COALESCE(minutes, 0) DESC "
        } else if (
          "updated_at" %in% fields
        ) {
          " ORDER BY updated_at DESC "
        } else {
          ""
        }
        
        tryCatch(
          DBI::dbGetQuery(
            con,
            paste0(
              "SELECT * FROM ",
              table_name,
              " WHERE ",
              paste(
                where,
                collapse = " AND "
              ),
              order_clause,
              " LIMIT 1"
            ),
            params = params
          ),
          error = function(e) {
            data.frame()
          }
        )
      }
      
      
      merge_evidence_row <- function(
    base,
    extra,
    prefix = NULL) {
        
        if (
          is.null(extra) ||
          !is.data.frame(extra) ||
          !nrow(extra)
        ) {
          return(base)
        }
        
        extra <- extra[
          1,
          ,
          drop = FALSE
        ]
        
        key_columns <- c(
          "player_id",
          "team_id",
          "season",
          "source_name",
          "source_player_id",
          "imported_at",
          "updated_at",
          "metric_version"
        )
        
        for (
          column in
          setdiff(
            names(extra),
            key_columns
          )
        ) {
          
          target <- column
          
          if (
            target %in%
            names(base) &&
            !is.null(prefix)
          ) {
            target <- paste0(
              prefix,
              column
            )
          }
          
          if (
            !target %in%
            names(base)
          ) {
            base[[target]] <-
              extra[[column]][[1]]
          } else {
            
            current <-
              base[[target]][[1]]
            
            replacement <-
              extra[[column]][[1]]
            
            if (
              (
                is.null(current) ||
                !length(current) ||
                is.na(current)
              ) &&
              !is.null(replacement) &&
              length(replacement) &&
              !is.na(replacement)
            ) {
              base[[target]][[1]] <-
                replacement
            }
          }
        }
        
        base
      }
      
      # ------------------------------------------------------
      # Shared transaction scenario
      # ------------------------------------------------------
      
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
          !identical(as.character(scenario$scenario_type), "trade") ||
          !identical(as.character(scenario$season), as.character(selected_season()))
        ) {
          return(NULL)
        }
        
        selected <- as.character(selected_team())
        primary_team <- as.character(scenario$team)
        partner_team_name <- as.character(scenario$partner_team)
        
        if (!selected %in% c(primary_team, partner_team_name)) {
          return(NULL)
        }
        
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
        }
        
        scenario
      })
      
      
      scenario_player_ids <- function(x) {
        
        if (
          is.null(x) ||
          !is.data.frame(x) ||
          !nrow(x) ||
          !"player_id" %in% names(x)
        ) {
          return(integer())
        }
        
        ids <- suppressWarnings(
          as.integer(x$player_id)
        )
        
        ids[!is.na(ids)]
      }
      
      
      # ------------------------------------------------------
      # Player pool
      # ------------------------------------------------------
      
      base_player_pool <- shiny::reactive({
        shiny::req(
          selected_team(),
          selected_season()
        )
        
        con <- connect_db()
        
        on.exit(
          disconnect_db(con),
          add = TRUE
        )
        
        DBI::dbGetQuery(
          con,
          "
          SELECT DISTINCT
            p.player_id,
            p.player_name,
            p.primary_position,
            p.player_age,
            p.height_inches,
            p.weight_lbs,
            t.team_name,
            t.abbreviation,
            rh.roster_status,
            COALESCE(rh.two_way_flag, 0) AS two_way_flag,
            rh.jersey_number,
            cy.cap_hit,
            cy.base_salary,
            cy.guaranteed_amount,
            cy.option_type,
            c.contract_type,
            c.contract_start_season,
            c.contract_end_season,
            c.total_value,
            c.guaranteed_value,
            c.free_agent_year,
            c.bird_rights,
            c.trade_bonus_percent

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
      })
      
      base_player_pool <- shiny::bindCache(
        base_player_pool,
        selected_team(),
        selected_season(),
        cache = "session"
      )

      player_pool <- shiny::reactive({
        
        current <- base_player_pool()
        scenario <- active_trade_scenario()
        
        if (is.null(scenario)) {
          if (nrow(current)) {
            current$transaction_role <- "CURRENT"
          }
          return(current)
        }
        
        outgoing_ids <- scenario_player_ids(
          scenario$outgoing_players
        )
        
        incoming_ids <- scenario_player_ids(
          scenario$incoming_players
        )
        
        preview <- current[
          !current$player_id %in% outgoing_ids,
          ,
          drop = FALSE
        ]
        
        if (nrow(preview)) {
          preview$transaction_role <- "CURRENT"
        }
        
        if (length(incoming_ids)) {
          
          con <- connect_db()
          
          on.exit(
            disconnect_db(con),
            add = TRUE
          )
          
          placeholders <- paste(
            rep("?", length(incoming_ids)),
            collapse = ","
          )
          
          query <- paste0(
            "
            SELECT DISTINCT
              p.player_id,
              p.player_name,
              p.primary_position,
              p.player_age,
              p.height_inches,
              p.weight_lbs,
              t.team_name,
              t.abbreviation,
              rh.roster_status,
              COALESCE(rh.two_way_flag, 0) AS two_way_flag,
              rh.jersey_number,
              cy.cap_hit,
              cy.base_salary,
              cy.guaranteed_amount,
              cy.option_type,
              c.contract_type,
              c.contract_start_season,
              c.contract_end_season,
              c.total_value,
              c.guaranteed_value,
              c.free_agent_year,
              c.bird_rights,
              c.trade_bonus_percent
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
              AND p.player_id IN (",
            placeholders,
            ")
            ORDER BY
              COALESCE(cy.cap_hit, 0) DESC,
              p.player_name
            "
          )
          
          incoming <- tryCatch(
            DBI::dbGetQuery(
              con,
              query,
              params = c(
                list(
                  scenario$partner_team,
                  selected_season()
                ),
                as.list(incoming_ids)
              )
            ),
            error = function(e) data.frame()
          )
          
          if (nrow(incoming)) {
            
            incoming$team_name <- selected_team()
            incoming$transaction_role <- "TRADE IN"
            
            if (!nrow(preview)) {
              preview <- incoming
            } else {
              
              common <- intersect(
                names(preview),
                names(incoming)
              )
              
              incoming <- incoming[
                ,
                common,
                drop = FALSE
              ]
              
              for (
                nm in setdiff(
                  names(preview),
                  names(incoming)
                )
              ) {
                incoming[[nm]] <- NA
              }
              
              incoming <- incoming[
                ,
                names(preview),
                drop = FALSE
              ]
              
              preview <- rbind(
                preview,
                incoming
              )
            }
          }
        }
        
        if (nrow(preview)) {
          
          cap_sort <- suppressWarnings(
            as.numeric(preview$cap_hit)
          )
          
          cap_sort[is.na(cap_sort)] <- 0
          
          preview <- preview[
            order(
              -cap_sort,
              preview$player_name
            ),
            ,
            drop = FALSE
          ]
        }
        
        rownames(preview) <- NULL
        preview
      })
      
      
      output$player_trade_scenario_banner <- shiny::renderUI({
        
        scenario <- active_trade_scenario()
        
        if (is.null(scenario)) {
          return(NULL)
        }
        
        outgoing_count <- if (
          is.data.frame(scenario$outgoing_players)
        ) {
          nrow(scenario$outgoing_players)
        } else {
          0
        }
        
        incoming_count <- if (
          is.data.frame(scenario$incoming_players)
        ) {
          nrow(scenario$incoming_players)
        } else {
          0
        }
        
        shiny::div(
          class = "pi-trade-banner",
          
          shiny::div(
            class = "pi-trade-banner-copy",
            
            shiny::span(
              class = "pi-trade-chip",
              bsicons::bs_icon("arrow-left-right"),
              "TRADE SCENARIO"
            ),
            
            paste0(
              selected_team(),
              " versus ",
              scenario$partner_team,
              ": Player Management is using the proposed roster. ",
              outgoing_count,
              " out / ",
              incoming_count,
              " in."
            )
          ),
          
          shiny::span(
            class = "pi-trade-summary",
            "PREVIEW ONLY"
          )
        )
      })
      
      
      shiny::observe({
        d <- player_pool()
        
        if (!nrow(d)) {
          shiny::updateSelectInput(
            session,
            "selected_player",
            choices = character(),
            selected = character()
          )
          
          return()
        }
        
        trade_suffix <- if (
          "transaction_role" %in% names(d)
        ) {
          ifelse(
            d$transaction_role == "TRADE IN",
            " • TRADE IN",
            ""
          )
        } else {
          ""
        }
        
        choices <- stats::setNames(
          as.character(d$player_id),
          paste0(
            d$player_name,
            " — ",
            ifelse(
              is.na(d$primary_position),
              "—",
              d$primary_position
            ),
            trade_suffix
          )
        )
        
        current <- isolate(
          input$selected_player
        )
        
        selected_value <- if (
          !is.null(current) &&
          current %in% choices
        ) {
          current
        } else {
          choices[[1]]
        }
        
        shiny::updateSelectInput(
          session,
          "selected_player",
          choices = choices,
          selected = selected_value
        )
      })
      
      selected_player <- shiny::reactive({
        d <- player_pool()
        
        shiny::req(
          nrow(d),
          input$selected_player
        )
        
        player_id <- suppressWarnings(
          as.integer(
            input$selected_player
          )
        )
        
        row <- d[
          d$player_id == player_id,
          ,
          drop = FALSE
        ]
        
        shiny::req(
          nrow(row)
        )
        
        row[1, , drop = FALSE]
      })
      
      
      # ------------------------------------------------------
      # Latest available performance season
      #
      # Roster/contract context remains selected_season().
      # Performance evidence rolls to the selected season only
      # when that player has actual games in that season.
      # ------------------------------------------------------
      
      performance_season <- shiny::reactive({
        
        shiny::req(
          input$selected_player,
          selected_season()
        )
        
        con <- connect_db(
          read_only = TRUE
        )
        
        on.exit(
          disconnect_db(con),
          add = TRUE
        )
        
        latest_player_performance_season(
          con = con,
          player_id =
            suppressWarnings(
              as.integer(
                input$selected_player
              )
            ),
          roster_season =
            selected_season()
        )
      })
      
      
      output$performance_season_label <- shiny::renderText({
        
        perf <-
          performance_season()
        
        roster <-
          as.character(
            selected_season()
          )
        
        if (
          identical(
            perf,
            roster
          )
        ) {
          paste0(
            perf,
            " • CURRENT"
          )
        } else {
          paste0(
            perf,
            " • LATEST AVAILABLE"
          )
        }
      })
      
      
      # ------------------------------------------------------
      # Performance evidence package
      # ------------------------------------------------------
      
      player_stats <- shiny::reactive({
        
        shiny::req(
          input$selected_player,
          performance_season()
        )
        
        con <- connect_db(
          read_only = TRUE
        )
        
        on.exit(
          disconnect_db(con),
          add = TRUE
        )
        
        player_id <- suppressWarnings(
          as.integer(
            input$selected_player
          )
        )
        
        perf_season <-
          performance_season()
        
        base <- latest_row_for_player(
          con = con,
          table_name =
            "player_season_stats",
          player_id =
            player_id,
          performance_season =
            perf_season
        )
        
        if (!nrow(base)) {
          return(NULL)
        }
        
        # Preserve the actual evidence source/team when a player
        # changed organizations between seasons.
        if (
          "team_id" %in%
          names(base) &&
          "teams" %in%
          DBI::dbListTables(con)
        ) {
          
          source_team <- tryCatch(
            DBI::dbGetQuery(
              con,
              "
              SELECT team_name
              FROM teams
              WHERE team_id = ?
              LIMIT 1
              ",
              params = list(
                suppressWarnings(
                  as.integer(
                    base$team_id[[1]]
                  )
                )
              )
            ),
            error = function(e) {
              data.frame()
            }
          )
          
          if (nrow(source_team)) {
            base$performance_source_team <-
              source_team$team_name[[1]]
          }
        }
        
        evidence_tables <- list(
          advanced =
            "player_season_advanced",
          shooting =
            "player_season_shooting",
          playmaking =
            "player_season_playmaking",
          defense =
            "player_season_defense_rebounding",
          roles =
            "player_season_roles",
          impact =
            "player_season_impact"
        )
        
        for (
          evidence_name in
          names(evidence_tables)
        ) {
          
          extra <- latest_row_for_player(
            con = con,
            table_name =
              evidence_tables[[evidence_name]],
            player_id =
              player_id,
            performance_season =
              perf_season
          )
          
          base <- merge_evidence_row(
            base = base,
            extra = extra,
            prefix = paste0(
              evidence_name,
              "_"
            )
          )
        }
        
        # Aliases used by the existing Player Management UI.
        # These do not fabricate values; they only map the new
        # Phase-3 field names to the established display names.
        
        alias_if_available <- function(
    target,
    candidates) {
          
          if (
            target %in%
            names(base) &&
            !is.na(
              base[[target]][[1]]
            )
          ) {
            return(
              invisible(NULL)
            )
          }
          
          for (candidate in candidates) {
            
            if (
              candidate %in%
              names(base) &&
              length(
                base[[candidate]]
              ) &&
              !is.na(
                base[[candidate]][[1]]
              )
            ) {
              base[[target]] <<-
                base[[candidate]][[1]]
              
              return(
                invisible(NULL)
              )
            }
          }
          
          invisible(NULL)
        }
        
        alias_if_available(
          "true_shooting_pct",
          c(
            "advanced_true_shooting_pct"
          )
        )
        
        alias_if_available(
          "usage_rate",
          c(
            "advanced_usage_rate"
          )
        )
        
        alias_if_available(
          "offensive_rebound_pct",
          c(
            "advanced_offensive_rebound_pct"
          )
        )
        
        alias_if_available(
          "defensive_rebound_pct",
          c(
            "advanced_defensive_rebound_pct"
          )
        )
        
        alias_if_available(
          "offensive_rating",
          c(
            "advanced_offensive_rating"
          )
        )
        
        alias_if_available(
          "defensive_rating",
          c(
            "advanced_defensive_rating"
          )
        )
        
        alias_if_available(
          "net_rating",
          c(
            "advanced_net_rating",
            "advanced_on_court_net_rating"
          )
        )
        
        alias_if_available(
          "bpm",
          c(
            "advanced_box_plus_minus"
          )
        )
        
        alias_if_available(
          "vorp",
          c(
            "advanced_value_over_replacement"
          )
        )
        
        alias_if_available(
          "impact",
          c(
            "all_around_impact_score",
            "impact_all_around_impact_score",
            "bie_performance_rating",
            "impact_bie_performance_rating"
          )
        )
        
        alias_if_available(
          "bie_performance_rating",
          c(
            "impact_bie_performance_rating"
          )
        )
        
        alias_if_available(
          "bie_performance_percentile",
          c(
            "impact_bie_performance_percentile"
          )
        )
        
        alias_if_available(
          "offensive_impact_score",
          c(
            "impact_offensive_impact_score"
          )
        )
        
        alias_if_available(
          "defensive_impact_score",
          c(
            "impact_defensive_impact_score"
          )
        )
        
        alias_if_available(
          "all_around_impact_score",
          c(
            "impact_all_around_impact_score"
          )
        )
        
        alias_if_available(
          "primary_role",
          c(
            "roles_primary_role"
          )
        )
        
        alias_if_available(
          "archetype",
          c(
            "roles_archetype"
          )
        )
        
        base$performance_season_used <-
          perf_season
        
        base
      })
      
      
      # ------------------------------------------------------
      # BIE team-wide player data
      # ------------------------------------------------------
      
      bie_team_player_data <- shiny::reactive({
        
        pool <- player_pool()
        
        if (!nrow(pool)) {
          return(pool)
        }
        
        con <- connect_db(
          read_only = TRUE
        )
        
        on.exit(
          disconnect_db(con),
          add = TRUE
        )
        
        rows <- lapply(
          seq_len(
            nrow(pool)
          ),
          function(i) {
            
            roster_row <- pool[
              i,
              ,
              drop = FALSE
            ]
            
            player_id <- suppressWarnings(
              as.integer(
                roster_row$player_id[[1]]
              )
            )
            
            perf_season <-
              latest_player_performance_season(
                con = con,
                player_id =
                  player_id,
                roster_season =
                  selected_season()
              )
            
            stats <- latest_row_for_player(
              con = con,
              table_name =
                "player_season_stats",
              player_id =
                player_id,
              performance_season =
                perf_season
            )
            
            impact <- latest_row_for_player(
              con = con,
              table_name =
                "player_season_impact",
              player_id =
                player_id,
              performance_season =
                perf_season
            )
            
            roles <- latest_row_for_player(
              con = con,
              table_name =
                "player_season_roles",
              player_id =
                player_id,
              performance_season =
                perf_season
            )
            
            shooting <- latest_row_for_player(
              con = con,
              table_name =
                "player_season_shooting",
              player_id =
                player_id,
              performance_season =
                perf_season
            )
            
            playmaking <- latest_row_for_player(
              con = con,
              table_name =
                "player_season_playmaking",
              player_id =
                player_id,
              performance_season =
                perf_season
            )
            
            defense <- latest_row_for_player(
              con = con,
              table_name =
                "player_season_defense_rebounding",
              player_id =
                player_id,
              performance_season =
                perf_season
            )
            
            row <- roster_row
            
            row <- merge_evidence_row(
              row,
              stats,
              "stats_"
            )
            
            row <- merge_evidence_row(
              row,
              shooting,
              "shooting_"
            )
            
            row <- merge_evidence_row(
              row,
              playmaking,
              "playmaking_"
            )
            
            row <- merge_evidence_row(
              row,
              defense,
              "defense_"
            )
            
            row <- merge_evidence_row(
              row,
              roles,
              "roles_"
            )
            
            row <- merge_evidence_row(
              row,
              impact,
              "impact_"
            )
            
            # Map Phase-3 performance outputs into the BIE
            # Phase-2-compatible component names consumed by
            # the existing dark Player Fit panel.
            get_first_numeric <- function(candidates) {
              
              for (nm in candidates) {
                
                if (
                  nm %in% names(row)
                ) {
                  value <- suppressWarnings(
                    as.numeric(
                      row[[nm]][[1]]
                    )
                  )
                  
                  if (
                    length(value) &&
                    is.finite(value)
                  ) {
                    return(
                      value
                    )
                  }
                }
              }
              
              NA_real_
            }
            
            rating <- get_first_numeric(
              c(
                "bie_performance_rating",
                "impact_bie_performance_rating"
              )
            )
            
            all_around <- get_first_numeric(
              c(
                "all_around_impact_score",
                "impact_all_around_impact_score"
              )
            )
            
            offense <- get_first_numeric(
              c(
                "offensive_impact_score",
                "impact_offensive_impact_score"
              )
            )
            
            defense_score <- get_first_numeric(
              c(
                "defensive_impact_score",
                "impact_defensive_impact_score"
              )
            )
            
            shooting_score <- get_first_numeric(
              c(
                "shooting_component",
                "impact_shooting_component",
                "shooting_efficiency_score",
                "shooting_shooting_efficiency_score"
              )
            )
            
            playmaking_score <- get_first_numeric(
              c(
                "creation_component",
                "impact_creation_component",
                "creation_score",
                "playmaking_creation_score"
              )
            )
            
            rebounding_score <- get_first_numeric(
              c(
                "rebounding_component",
                "impact_rebounding_component",
                "rebounding_score",
                "defense_rebounding_score"
              )
            )
            
            row$bie_player_score <-
              rating
            
            row$bie_impact_score <-
              all_around
            
            row$bie_offense_score <-
              offense
            
            row$bie_defense_score <-
              defense_score
            
            row$bie_efficiency_score <-
              shooting_score
            
            row$bie_playmaking_score <-
              playmaking_score
            
            row$bie_rebounding_score <-
              rebounding_score
            
            components <- c(
              all_around,
              offense,
              defense_score,
              shooting_score,
              playmaking_score,
              rebounding_score
            )
            
            row$bie_metric_components <-
              sum(
                is.finite(
                  components
                )
              )
            
            row$bie_score_source <- if (
              is.finite(
                rating
              )
            ) {
              "PERFORMANCE_DATA"
            } else {
              "FOUNDATION"
            }
            
            row$performance_season_used <-
              perf_season
            
            row
          }
        )
        
        # Normalize all rows to a common set of columns.
        all_names <- unique(
          unlist(
            lapply(
              rows,
              names
            )
          )
        )
        
        normalized <- lapply(
          rows,
          function(row) {
            
            missing <- setdiff(
              all_names,
              names(row)
            )
            
            for (nm in missing) {
              row[[nm]] <- NA
            }
            
            row[
              ,
              all_names,
              drop = FALSE
            ]
          }
        )
        
        do.call(
          rbind,
          normalized
        )
      })
      
      
      bie_evaluated_roster <- shiny::reactive({
        
        d <- bie_team_player_data()
        
        if (
          !nrow(d) ||
          !exists(
            "evaluate_bie_players",
            mode = "function"
          )
        ) {
          return(NULL)
        }
        
        tryCatch(
          evaluate_bie_players(d),
          error = function(e) NULL
        )
      })
      
      
      bie_player_profile <- shiny::reactive({
        
        shiny::req(
          input$selected_player
        )
        
        # Phase 3.3 rule:
        # Do NOT run the already-calibrated BIE components back
        # through roster-relative percentile scoring.
        #
        # bie_team_player_data() already carries the real
        # Phase 3 performance rating and component scores:
        #   bie_player_score
        #   bie_impact_score
        #   bie_offense_score
        #   bie_defense_score
        #   bie_efficiency_score
        #   bie_playmaking_score
        #   bie_rebounding_score
        
        d <- bie_team_player_data()
        
        if (
          is.null(d) ||
          !is.data.frame(d) ||
          !nrow(d)
        ) {
          return(NULL)
        }
        
        player_id <- suppressWarnings(
          as.integer(
            input$selected_player
          )
        )
        
        row <- d[
          suppressWarnings(
            as.integer(
              d$player_id
            )
          ) == player_id,
          ,
          drop = FALSE
        ]
        
        if (!nrow(row)) {
          return(NULL)
        }
        
        row <- row[
          1,
          ,
          drop = FALSE
        ]
        
        get_num <- function(candidates) {
          
          for (nm in candidates) {
            
            if (
              nm %in% names(row) &&
              length(row[[nm]])
            ) {
              
              value <- suppressWarnings(
                as.numeric(
                  row[[nm]][[1]]
                )
              )
              
              if (
                length(value) &&
                is.finite(value)
              ) {
                return(value)
              }
            }
          }
          
          NA_real_
        }
        
        score <- get_num(
          c(
            "bie_player_score",
            "bie_performance_rating",
            "impact_bie_performance_rating"
          )
        )
        
        impact <- get_num(
          c(
            "bie_impact_score",
            "all_around_impact_score",
            "impact_all_around_impact_score"
          )
        )
        
        offense <- get_num(
          c(
            "bie_offense_score",
            "offensive_impact_score",
            "impact_offensive_impact_score"
          )
        )
        
        defense <- get_num(
          c(
            "bie_defense_score",
            "defensive_impact_score",
            "impact_defensive_impact_score",
            "defense_proxy_score"
          )
        )
        
        efficiency <- get_num(
          c(
            "bie_efficiency_score",
            "shooting_component",
            "shooting_efficiency_score",
            "spacing_score"
          )
        )
        
        playmaking <- get_num(
          c(
            "bie_playmaking_score",
            "creation_component",
            "creation_score",
            "playmaking_creation_score"
          )
        )
        
        rebounding <- get_num(
          c(
            "bie_rebounding_score",
            "rebounding_component",
            "rebounding_score",
            "defense_rebounding_score"
          )
        )
        
        availability <- get_num(
          c(
            "bie_availability_score",
            "availability_component"
          )
        )
        
        # Rank actual BIE performance ratings across the current
        # roster. This is a performance rank, not a second model.
        roster_scores <- suppressWarnings(
          as.numeric(
            d$bie_player_score
          )
        )
        
        valid_rank <-
          is.finite(
            roster_scores
          )
        
        rated_players <- sum(
          valid_rank,
          na.rm = TRUE
        )
        
        roster_rank <- NA_integer_
        
        if (
          is.finite(score) &&
          rated_players > 0
        ) {
          
          ranked_ids <- suppressWarnings(
            as.integer(
              d$player_id[
                valid_rank
              ]
            )
          )
          
          ranked_scores <- roster_scores[
            valid_rank
          ]
          
          ord <- order(
            -ranked_scores,
            ranked_ids,
            na.last = TRUE
          )
          
          roster_rank <- match(
            player_id,
            ranked_ids[
              ord
            ]
          )
        }
        
        # Use the existing BIE grade function when available.
        grade <- if (
          exists(
            "bie_player_grade",
            mode = "function"
          )
        ) {
          bie_player_grade(
            score
          )
        } else if (
          is.finite(score) &&
          score >= 90
        ) {
          "A+"
        } else if (
          is.finite(score) &&
          score >= 80
        ) {
          "A"
        } else if (
          is.finite(score) &&
          score >= 70
        ) {
          "B+"
        } else if (
          is.finite(score) &&
          score >= 60
        ) {
          "B"
        } else if (
          is.finite(score) &&
          score >= 50
        ) {
          "C"
        } else {
          "UNRATED"
        }
        
        components <- c(
          Impact = impact,
          Offense = offense,
          Defense = defense,
          Efficiency = efficiency,
          Playmaking = playmaking,
          Rebounding = rebounding,
          Availability = availability
        )
        
        finite_components <- components[
          is.finite(
            components
          )
        ]
        
        metric_components <- length(
          finite_components
        )
        
        confidence <- if (
          metric_components >= 6
        ) {
          "HIGH"
        } else if (
          metric_components >= 3
        ) {
          "MODERATE"
        } else if (
          metric_components >= 1
        ) {
          "LIMITED"
        } else {
          "FOUNDATION"
        }
        
        strengths <- character()
        concerns <- character()
        
        if (length(finite_components)) {
          
          for (nm in names(finite_components)) {
            
            value <- finite_components[[nm]]
            
            if (value >= 75) {
              strengths <- c(
                strengths,
                paste0(
                  nm,
                  " is a Phase 3.3 strength (",
                  round(value, 1),
                  ")."
                )
              )
            }
            
            if (value < 40) {
              concerns <- c(
                concerns,
                paste0(
                  nm,
                  " is below the preferred Phase 3.3 range (",
                  round(value, 1),
                  ")."
                )
              )
            }
          }
        }
        
        if (!length(strengths)) {
          strengths <-
            "No Phase 3.3 component currently clears the strength threshold."
        }
        
        if (!length(concerns)) {
          concerns <-
            "No major Phase 3.3 performance concern is identified."
        }
        
        positions <- unique(
          na.omit(
            c(
              if ("primary_position" %in% names(row)) {
                as.character(
                  row$primary_position[[1]]
                )
              } else {
                NA_character_
              },
              if ("secondary_position" %in% names(row)) {
                as.character(
                  row$secondary_position[[1]]
                )
              } else {
                NA_character_
              }
            )
          )
        )
        
        primary_position <- if (
          length(positions)
        ) {
          positions[[1]]
        } else {
          "—"
        }
        
        secondary_positions <- if (
          length(positions) > 1
        ) {
          paste(
            positions[-1],
            collapse = " / "
          )
        } else {
          "None loaded"
        }
        
        age <- if (
          "player_age" %in% names(row)
        ) {
          suppressWarnings(
            as.numeric(
              row$player_age[[1]]
            )
          )
        } else {
          NA_real_
        }
        
        age_curve <- if (
          is.finite(age) &&
          age <= 23
        ) {
          "DEVELOPMENT"
        } else if (
          is.finite(age) &&
          age <= 29
        ) {
          "PRIME"
        } else if (
          is.finite(age) &&
          age <= 32
        ) {
          "VETERAN PRIME"
        } else if (
          is.finite(age)
        ) {
          "LATE VETERAN"
        } else {
          "AGE PENDING"
        }
        
        archetype <- if (
          "archetype" %in% names(row) &&
          length(row$archetype) &&
          !is.na(row$archetype[[1]]) &&
          nzchar(
            trimws(
              as.character(
                row$archetype[[1]]
              )
            )
          )
        ) {
          as.character(
            row$archetype[[1]]
          )
        } else if (
          is.finite(offense) &&
          offense >= 75 &&
          is.finite(playmaking) &&
          playmaking >= 70
        ) {
          "PRIMARY OFFENSIVE ENGINE"
        } else if (
          is.finite(defense) &&
          defense >= 75
        ) {
          "DEFENSIVE IMPACT PLAYER"
        } else if (
          is.finite(efficiency) &&
          efficiency >= 75
        ) {
          "SCORING / SPACING IMPACT"
        } else if (
          is.finite(rebounding) &&
          rebounding >= 75
        ) {
          "REBOUNDING IMPACT"
        } else {
          "BALANCED ROTATION PROFILE"
        }
        
        list(
          status = "OK",
          score = score,
          grade = grade,
          confidence = confidence,
          performance_available =
            is.finite(score),
          metric_components =
            metric_components,
          roster_rank =
            roster_rank,
          rated_players =
            rated_players,
          primary_position =
            primary_position,
          secondary_positions =
            secondary_positions,
          age_curve =
            age_curve,
          archetype =
            archetype,
          impact =
            impact,
          offense =
            offense,
          defense =
            defense,
          efficiency =
            efficiency,
          playmaking =
            playmaking,
          rebounding =
            rebounding,
          availability =
            availability,
          strengths =
            unique(strengths),
          concerns =
            unique(concerns),
          explanation =
            paste(
              "Phase 3.3 BIE performance scores are displayed directly.",
              "The panel does not re-percentile calibrated components against the roster."
            ),
          model_label =
            "BIE Player Value v3.3"
        )
      })
      
      
      # ------------------------------------------------------
      # Optional projections table
      # ------------------------------------------------------
      
      player_projection <- shiny::reactive({
        
        shiny::req(
          input$selected_player
        )
        
        con <- connect_db(
          read_only = TRUE
        )
        
        on.exit(
          disconnect_db(con),
          add = TRUE
        )
        
        candidates <- c(
          "player_projection_intelligence",
          "player_projections",
          "player_projection",
          "projection_results"
        )
        
        available <- candidates[
          candidates %in%
            DBI::dbListTables(con)
        ]
        
        if (!length(available)) {
          return(NULL)
        }
        
        table_name <- available[[1]]
        
        fields <- DBI::dbListFields(
          con,
          table_name
        )
        
        player_id <- suppressWarnings(
          as.integer(
            input$selected_player
          )
        )
        
        if (
          !"player_id" %in%
          fields
        ) {
          return(NULL)
        }
        
        where <-
          "player_id = ?"
        
        params <-
          list(
            player_id
          )
        
        # New Phase 3.3 projection table stores the performance
        # season as latest_season rather than the roster season.
        perf <- tryCatch(
          performance_season(),
          error = function(e) {
            NULL
          }
        )
        
        if (
          "latest_season" %in%
          fields &&
          !is.null(perf)
        ) {
          
          where <- paste0(
            where,
            " AND latest_season = ?"
          )
          
          params <- c(
            params,
            list(
              perf
            )
          )
          
        } else if (
          "season" %in%
          fields
        ) {
          
          where <- paste0(
            where,
            " AND season = ?"
          )
          
          params <- c(
            params,
            list(
              selected_season()
            )
          )
        }
        
        result <- DBI::dbGetQuery(
          con,
          paste0(
            "SELECT * FROM ",
            table_name,
            " WHERE ",
            where,
            " LIMIT 1"
          ),
          params = params
        )
        
        # If a historical/current-season filter did not match,
        # fall back to the player's newest available projection.
        if (
          !nrow(result) &&
          identical(
            table_name,
            "player_projection_intelligence"
          )
        ) {
          
          order_sql <- if (
            "latest_season" %in%
            fields
          ) {
            " ORDER BY latest_season DESC"
          } else {
            ""
          }
          
          result <- DBI::dbGetQuery(
            con,
            paste0(
              "SELECT * FROM ",
              table_name,
              " WHERE player_id = ?",
              order_sql,
              " LIMIT 1"
            ),
            params =
              list(
                player_id
              )
          )
        }
        
        if (!nrow(result)) {
          NULL
        } else {
          result
        }
      })
      
      # ------------------------------------------------------
      # Derived player context
      # ------------------------------------------------------
      
      derived_context <- shiny::reactive({
        p <- selected_player()
        
        age <- num_value(
          p$player_age
        )
        
        cap_hit <- num_value(
          p$cap_hit,
          0
        )
        
        two_way <- num_value(
          p$two_way_flag,
          0
        ) == 1
        
        contract_type <- tolower(
          text_value(
            p$contract_type,
            ""
          )
        )
        
        roster_status <- text_value(
          p$roster_status,
          "Active"
        )
        
        # Phase 3.4:
        # basketball role is performance-based;
        # contract tier remains contract-based.
        profile <- tryCatch(
          bie_player_profile(),
          error = function(e) NULL
        )
        
        bie_score <- if (
          !is.null(profile) &&
          is.finite(
            suppressWarnings(
              as.numeric(
                profile$score
              )
            )
          )
        ) {
          suppressWarnings(
            as.numeric(
              profile$score
            )
          )
        } else {
          NA_real_
        }
        
        perf <- tryCatch(
          performance_data(),
          error = function(e) NULL
        )
        
        mpg <- if (
          !is.null(perf) &&
          is.data.frame(perf) &&
          nrow(perf)
        ) {
          num_value(
            first_existing(
              perf,
              c(
                "minutes_per_game",
                "mpg",
                "min"
              ),
              NA_real_
            )
          )
        } else {
          NA_real_
        }
        
        role <- if (two_way) {
          "Two-Way"
        } else if (
          grepl(
            "exhibit",
            contract_type,
            fixed = TRUE
          )
        ) {
          "Camp / Exhibit"
        } else if (
          is.finite(bie_score) &&
          bie_score >= 75
        ) {
          "Core Rotation"
        } else if (
          is.finite(mpg) &&
          mpg >= 28
        ) {
          "Core Rotation"
        } else if (
          is.finite(bie_score) &&
          bie_score >= 60
        ) {
          "Rotation"
        } else if (
          is.finite(mpg) &&
          mpg >= 16
        ) {
          "Rotation"
        } else if (
          is.finite(bie_score) &&
          bie_score >= 45
        ) {
          "Roster Depth"
        } else {
          "Development / Depth"
        }
        
        tier <- if (
          cap_hit >= 35e6
        ) {
          "Premium Contract Role"
        } else if (
          cap_hit >= 15e6
        ) {
          "Established Contract"
        } else if (
          two_way
        ) {
          "Development Contract"
        } else if (
          is.finite(age) &&
          age >= 30
        ) {
          "Veteran Value Contract"
        } else if (
          is.finite(age) &&
          age <= 23
        ) {
          "Development Contract"
        } else {
          "Cost-Controlled Contract"
        }
        
        timeline <- if (
          is.na(age)
        ) {
          "Age data pending"
        } else if (
          age <= 23
        ) {
          "Development"
        } else if (
          age <= 29
        ) {
          "Prime"
        } else if (
          age <= 32
        ) {
          "Veteran Prime"
        } else {
          "Late Veteran"
        }
        
        list(
          age = age,
          cap_hit = cap_hit,
          two_way = two_way,
          role = role,
          tier = tier,
          timeline = timeline,
          roster_status = roster_status
        )
      })
      
      # ------------------------------------------------------
      # BIE Player Fit outputs
      # ------------------------------------------------------
      
      bie_score_text <- function(value) {
        
        value <- num_value(
          value,
          NA_real_
        )
        
        if (is.na(value)) {
          "UNRATED"
        } else {
          sprintf(
            "%.1f",
            value
          )
        }
      }
      
      
      output$bie_player_score <- shiny::renderText({
        subtab_ready("value")
        
        profile <- bie_player_profile()
        
        if (is.null(profile)) {
          return("UNRATED")
        }
        
        bie_score_text(
          profile$score
        )
      })
      
      
      output$bie_player_grade <- shiny::renderText({
        subtab_ready("value")
        
        profile <- bie_player_profile()
        
        if (is.null(profile)) {
          return(
            "Performance grade pending"
          )
        }
        
        if (
          identical(
            profile$grade,
            "UNRATED"
          )
        ) {
          "Performance grade pending"
        } else {
          paste0(
            "GRADE ",
            profile$grade
          )
        }
      })
      
      
      output$bie_player_confidence <- shiny::renderText({
        subtab_ready("value")
        
        profile <- bie_player_profile()
        
        confidence <- if (
          is.null(profile)
        ) {
          "FOUNDATION"
        } else {
          text_value(
            profile$confidence,
            "FOUNDATION"
          )
        }
        
        paste0(
          "CONFIDENCE: ",
          confidence
        )
      })
      
      
      output$bie_player_rank <- shiny::renderText({
        subtab_ready("value")
        
        profile <- bie_player_profile()
        
        if (
          is.null(profile) ||
          is.null(
            profile$roster_rank
          ) ||
          is.na(
            profile$roster_rank
          ) ||
          is.null(
            profile$rated_players
          ) ||
          profile$rated_players <= 0
        ) {
          return(
            "Roster rank pending performance data"
          )
        }
        
        paste0(
          "Rated roster rank: ",
          profile$roster_rank,
          " / ",
          profile$rated_players
        )
      })
      
      
      output$bie_archetype <- shiny::renderText({
        subtab_ready("value")
        
        profile <- bie_player_profile()
        
        if (is.null(profile)) {
          "Pending"
        } else {
          text_value(
            profile$archetype,
            "Pending"
          )
        }
      })
      
      
      output$bie_best_position <- shiny::renderText({
        subtab_ready("value")
        
        profile <- bie_player_profile()
        
        if (is.null(profile)) {
          "—"
        } else {
          text_value(
            profile$primary_position
          )
        }
      })
      
      
      output$bie_secondary_positions <- shiny::renderText({
        subtab_ready("value")
        
        profile <- bie_player_profile()
        
        if (is.null(profile)) {
          "—"
        } else {
          text_value(
            profile$secondary_positions
          )
        }
      })
      
      
      output$bie_age_curve <- shiny::renderText({
        subtab_ready("value")
        
        profile <- bie_player_profile()
        
        if (is.null(profile)) {
          derived_context()$timeline
        } else {
          text_value(
            profile$age_curve,
            derived_context()$timeline
          )
        }
      })
      
      
      bie_component_text <- function(component) {
        
        profile <- bie_player_profile()
        
        if (
          is.null(profile) ||
          is.null(
            profile[[component]]
          )
        ) {
          return("—")
        }
        
        bie_score_text(
          profile[[component]]
        )
      }
      
      
      output$bie_impact <- shiny::renderText({
        subtab_ready("value")
        bie_component_text("impact")
      })
      
      output$bie_offense <- shiny::renderText({
        subtab_ready("value")
        bie_component_text("offense")
      })
      
      output$bie_defense <- shiny::renderText({
        subtab_ready("value")
        bie_component_text("defense")
      })
      
      output$bie_efficiency <- shiny::renderText({
        subtab_ready("value")
        bie_component_text("efficiency")
      })
      
      output$bie_playmaking <- shiny::renderText({
        subtab_ready("value")
        bie_component_text("playmaking")
      })
      
      output$bie_rebounding <- shiny::renderText({
        subtab_ready("value")
        bie_component_text("rebounding")
      })
      
      
      bie_note_items <- function(
    items,
    concern = FALSE) {
        
        if (
          is.null(items) ||
          !length(items)
        ) {
          items <- if (concern) {
            "No BIE concern available."
          } else {
            "No BIE strength available."
          }
        }
        
        shiny::tagList(
          lapply(
            items,
            function(item) {
              shiny::div(
                class = "pi-bie-note-item",
                shiny::span(
                  class = paste(
                    "pi-bie-note-dot",
                    if (
                      concern
                    ) {
                      "concern"
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
      
      
      output$bie_player_strengths <- shiny::renderUI({
        subtab_ready("value")
        
        profile <- bie_player_profile()
        
        bie_note_items(
          if (
            is.null(profile)
          ) {
            NULL
          } else {
            profile$strengths
          },
          concern = FALSE
        )
      })
      
      
      output$bie_player_concerns <- shiny::renderUI({
        subtab_ready("value")
        
        profile <- bie_player_profile()
        
        bie_note_items(
          if (
            is.null(profile)
          ) {
            NULL
          } else {
            profile$concerns
          },
          concern = TRUE
        )
      })
      
      
      output$bie_player_scope <- shiny::renderText({
        subtab_ready("value")
        
        perf <- tryCatch(
          performance_season(),
          error = function(e) {
            selected_season()
          }
        )
        
        paste0(
          "Performance basis: ",
          perf,
          " • Roster / contract context: ",
          selected_season(),
          " • Phase 3.3 calibrated component scores are shown directly; they are not re-percentiled against the roster."
        )
      })
      
      
      # ------------------------------------------------------
      # Header outputs
      # ------------------------------------------------------
      
      output$player_initials <- shiny::renderText({
        name <- text_value(
          selected_player()$player_name,
          "Player"
        )
        
        parts <- strsplit(
          trimws(name),
          "\\s+"
        )[[1]]
        
        paste0(
          substr(
            utils::head(parts, 1),
            1,
            1
          ),
          substr(
            utils::tail(parts, 1),
            1,
            1
          )
        )
      })
      
      output$player_name <- shiny::renderText({
        text_value(
          selected_player()$player_name
        )
      })
      
      output$role_badge <- shiny::renderText({
        
        p <- selected_player()
        
        if (
          "transaction_role" %in% names(p) &&
          identical(
            as.character(p$transaction_role[[1]]),
            "TRADE IN"
          )
        ) {
          return(
            paste0(
              "TRADE IN • ",
              derived_context()$role
            )
          )
        }
        
        derived_context()$role
      })
      
      output$player_subtitle <- shiny::renderText({
        p <- selected_player()
        
        number <- text_value(
          p$jersey_number,
          "—"
        )
        
        paste0(
          text_value(
            p$primary_position
          ),
          "  •  #",
          number,
          "  •  ",
          text_value(
            p$team_name
          )
        )
      })
      
      output$player_detail_line <- shiny::renderText({
        p <- selected_player()
        
        age <- text_value(
          p$player_age
        )
        
        height <- format_inches(
          p$height_inches
        )
        
        weight <- if (
          is.na(
            num_value(
              p$weight_lbs
            )
          )
        ) {
          "—"
        } else {
          paste0(
            round(
              num_value(
                p$weight_lbs
              )
            ),
            " lbs"
          )
        }
        
        paste(
          paste0(age, " YEARS OLD"),
          height,
          weight,
          derived_context()$timeline,
          sep = "  •  "
        )
      })
      
      output$height <- shiny::renderText({
        format_inches(
          selected_player()$height_inches
        )
      })
      
      output$weight <- shiny::renderText({
        value <- num_value(
          selected_player()$weight_lbs
        )
        
        if (is.na(value)) {
          "—"
        } else {
          paste0(
            round(value),
            " lbs"
          )
        }
      })
      
      output$age <- shiny::renderText({
        text_value(
          selected_player()$player_age
        )
      })
      
      output$cap_hit <- shiny::renderText({
        money(
          selected_player()$cap_hit
        )
      })
      
      output$contract_through <- shiny::renderText({
        text_value(
          selected_player()$contract_end_season
        )
      })
      
      output$team_control <- shiny::renderText({
        p <- selected_player()
        
        fa_year <- num_value(
          p$free_agent_year
        )
        
        season_start <- suppressWarnings(
          as.numeric(
            substr(
              selected_season(),
              1,
              4
            )
          )
        )
        
        if (
          is.na(fa_year) ||
          is.na(season_start)
        ) {
          "—"
        } else {
          paste0(
            max(
              0,
              fa_year - season_start
            ),
            " yrs"
          )
        }
      })
      
      # ------------------------------------------------------
      # Stats outputs
      # ------------------------------------------------------
      
      stat_render <- function(output_id, candidates, digits = 1, pct = FALSE) {
        output[[output_id]] <- shiny::renderText({
          d <- player_stats()
          
          if (is.null(d)) {
            return("—")
          }
          
          value <- first_existing(
            d,
            candidates,
            NA_real_
          )
          
          value <- suppressWarnings(
            as.numeric(value)
          )
          
          if (is.na(value)) {
            return("—")
          }
          
          if (pct) {
            if (abs(value) <= 1.5) {
              value <- value * 100
            }
            
            return(
              paste0(
                format(
                  round(
                    value,
                    digits
                  ),
                  nsmall = digits,
                  trim = TRUE
                ),
                "%"
              )
            )
          }
          
          format(
            round(
              value,
              digits
            ),
            nsmall = digits,
            trim = TRUE
          )
        })
      }
      
      stat_render(
        "pts",
        c(
          "pts",
          "ppg",
          "points_per_game"
        )
      )
      
      stat_render(
        "reb",
        c(
          "reb",
          "rpg",
          "rebounds_per_game"
        )
      )
      
      stat_render(
        "ast",
        c(
          "ast",
          "apg",
          "assists_per_game"
        )
      )
      
      stat_render(
        "blk",
        c(
          "blk",
          "bpg",
          "blocks_per_game"
        )
      )
      
      stat_render(
        "stl",
        c(
          "stl",
          "spg",
          "steals_per_game"
        )
      )
      
      stat_render(
        "ts_pct",
        c(
          "ts_pct",
          "true_shooting_pct",
          "true_shooting"
        ),
        pct = TRUE
      )
      
      stat_render(
        "usg_pct",
        c(
          "usg_pct",
          "usage_rate",
          "usage_pct",
          "usage",
          "advanced_usage_rate"
        ),
        pct = TRUE
      )
      
      stat_render(
        "oreb_pct",
        c(
          "oreb_pct",
          "offensive_rebound_pct"
        ),
        pct = TRUE
      )
      
      stat_render(
        "dreb_pct",
        c(
          "dreb_pct",
          "defensive_rebound_pct"
        ),
        pct = TRUE
      )
      
      stat_render(
        "minutes",
        c(
          "min",
          "mpg",
          "minutes_per_game"
        )
      )
      
      stat_render(
        "off_rtg",
        c(
          "off_rtg",
          "ortg",
          "offensive_rating",
          "advanced_offensive_rating"
        )
      )
      
      stat_render(
        "def_rtg",
        c(
          "def_rtg",
          "drtg",
          "defensive_rating",
          "advanced_defensive_rating"
        )
      )
      
      stat_render(
        "net_rtg",
        c(
          "net_rtg",
          "net_rating",
          "advanced_net_rating",
          "advanced_on_court_net_rating"
        )
      )
      
      output$impact_metric <- shiny::renderText({
        d <- player_stats()
        
        if (is.null(d)) {
          return("—")
        }
        
        value <- first_existing(
          d,
          c(
            "bie_performance_rating",
            "all_around_impact_score",
            "impact",
            "war",
            "vorp",
            "bpm",
            "epm"
          ),
          NA_real_
        )
        
        value <- suppressWarnings(
          as.numeric(value)
        )
        
        if (is.na(value)) {
          "—"
        } else {
          format(
            round(
              value,
              1
            ),
            nsmall = 1,
            trim = TRUE
          )
        }
      })
      
      output$summary_minutes <- shiny::renderText({
        d <- player_stats()
        
        if (is.null(d)) {
          return("—")
        }
        
        value <- first_existing(
          d,
          c(
            "min",
            "mpg",
            "minutes_per_game"
          ),
          NA_real_
        )
        
        value <- suppressWarnings(
          as.numeric(value)
        )
        
        if (is.na(value)) {
          "—"
        } else {
          format(
            round(value, 1),
            nsmall = 1
          )
        }
      })
      
      output$stats_data_status <- shiny::renderText({
        if (is.null(player_stats())) {
          "Player-season performance table not loaded yet."
        } else {
          "Loaded from player-season performance data."
        }
      })
      
      output$advanced_data_status <- shiny::renderText({
        
        d <- player_stats()
        
        perf <- tryCatch(
          performance_season(),
          error = function(e) {
            selected_season()
          }
        )
        
        if (is.null(d)) {
          return(
            paste0(
              "No NBA performance evidence is loaded through ",
              perf,
              "."
            )
          )
        }
        
        advanced_candidates <- c(
          "offensive_rating",
          "defensive_rating",
          "net_rating",
          "bie_performance_rating",
          "all_around_impact_score",
          "bpm",
          "vorp"
        )
        
        available <- advanced_candidates[
          advanced_candidates %in%
            names(d)
        ]
        
        has_advanced <- any(
          vapply(
            available,
            function(nm) {
              value <- suppressWarnings(
                as.numeric(
                  d[[nm]][[1]]
                )
              )
              
              length(value) &&
                is.finite(value)
            },
            logical(1)
          )
        )
        
        if (has_advanced) {
          paste0(
            "Performance evidence: ",
            perf,
            ". Roster / contract context: ",
            selected_season(),
            "."
          )
        } else {
          paste0(
            "Performance evidence: ",
            perf,
            ". Some advanced metrics are not supplied by the loaded source and remain unrated."
          )
        }
      })
      
      # ------------------------------------------------------
      # Role / value
      # ------------------------------------------------------
      
      output$current_role <- shiny::renderText({
        derived_context()$role
      })
      
      output$role_context <- shiny::renderText({
        paste(
          text_value(
            selected_player()$primary_position
          ),
          "•",
          derived_context()$roster_status
        )
      })
      
      output$player_tier <- shiny::renderText({
        derived_context()$tier
      })
      
      output$strengths <- shiny::renderUI({
        p <- selected_player()
        context <- derived_context()
        
        strengths <- character()
        
        if (
          context$cap_hit > 0 &&
          context$cap_hit < 15e6
        ) {
          strengths <- c(
            strengths,
            "Cost-controlled current cap hit"
          )
        }
        
        if (
          !is.na(context$age) &&
          context$age <= 27
        ) {
          strengths <- c(
            strengths,
            "Development-age timeline"
          )
        }
        
        if (
          grepl(
            "team",
            tolower(
              text_value(
                p$option_type,
                ""
              )
            ),
            fixed = TRUE
          )
        ) {
          strengths <- c(
            strengths,
            "Team-controlled option structure"
          )
        }
        
        if (
          grepl(
            "bird",
            tolower(
              text_value(
                p$bird_rights,
                ""
              )
            ),
            fixed = TRUE
          )
        ) {
          strengths <- c(
            strengths,
            "Bird-rights retention pathway"
          )
        }
        
        if (!length(strengths)) {
          strengths <- c(
            strengths,
            "Roster role and contract status are currently loaded"
          )
        }
        
        bullet_list(
          strengths,
          "green"
        )
      })
      
      output$development_areas <- shiny::renderUI({
        subtab_ready("development")
        p <- selected_player()
        context <- derived_context()
        
        areas <- character()
        
        if (
          !is.na(context$age) &&
          context$age >= 31
        ) {
          areas <- c(
            areas,
            "Age-curve monitoring"
          )
        }
        
        if (
          context$cap_hit >= 30e6
        ) {
          areas <- c(
            areas,
            "High salary concentration"
          )
        }
        
        if (
          is.null(
            player_stats()
          )
        ) {
          areas <- c(
            areas,
            "Performance evaluation pending stats load"
          )
        }
        
        if (
          is.na(
            num_value(
              p$guaranteed_amount
            )
          ) ||
          num_value(
            p$guaranteed_amount,
            0
          ) == 0
        ) {
          areas <- c(
            areas,
            "Guarantee data should be verified"
          )
        }
        
        if (!length(areas)) {
          areas <- "No immediate contract-structure concern identified."
        }
        
        bullet_list(
          areas,
          "orange"
        )
      })
      
      # ------------------------------------------------------
      # Projection outputs — Phase 3.3
      # ------------------------------------------------------
      
      projection_num <- function(
    d,
    candidates,
    fallback = NA_real_) {
        
        if (
          is.null(d) ||
          !is.data.frame(d) ||
          !nrow(d)
        ) {
          return(fallback)
        }
        
        for (nm in candidates) {
          
          if (
            nm %in% names(d) &&
            length(d[[nm]])
          ) {
            
            value <- suppressWarnings(
              as.numeric(
                d[[nm]][[1]]
              )
            )
            
            if (
              length(value) &&
              is.finite(value)
            ) {
              return(value)
            }
          }
        }
        
        fallback
      }
      
      
      projection_tier <- function(value) {
        
        if (!is.finite(value)) {
          return("UNRATED")
        }
        
        if (value >= 85) {
          "ELITE / ALL-NBA IMPACT"
        } else if (value >= 75) {
          "PLUS STARTER IMPACT"
        } else if (value >= 65) {
          "STARTER / HIGH-END ROTATION"
        } else if (value >= 55) {
          "ROTATION IMPACT"
        } else if (value >= 45) {
          "DEPTH IMPACT"
        } else {
          "LIMITED IMPACT"
        }
      }
      
      
      projection_value_text <- function(
    value) {
        
        if (!is.finite(value)) {
          return("PENDING")
        }
        
        paste0(
          format(
            round(
              value,
              1
            ),
            nsmall = 1,
            trim = TRUE
          ),
          " • ",
          projection_tier(
            value
          )
        )
      }
      
      
      projection_1y_value <- shiny::reactive({
        
        d <- player_projection()
        
        if (is.null(d)) {
          return(NA_real_)
        }
        
        projection_num(
          d,
          c(
            "projected_bie_1y",
            "projected_bie_rating",
            "projection_1y",
            "one_year_projection"
          )
        )
      })
      
      
      projection_3y_value <- shiny::reactive({
        
        d <- player_projection()
        
        if (is.null(d)) {
          return(NA_real_)
        }
        
        # Legacy tables may already provide a direct 3-year value.
        direct <- projection_num(
          d,
          c(
            "projected_bie_3y",
            "projection_3y",
            "three_year_projection"
          )
        )
        
        if (is.finite(direct)) {
          return(direct)
        }
        
        baseline <- projection_num(
          d,
          c(
            "weighted_baseline",
            "latest_bie_rating",
            "projected_bie_rating"
          )
        )
        
        trend <- projection_num(
          d,
          c(
            "annual_trend"
          ),
          0
        )
        
        if (!is.finite(baseline)) {
          return(NA_real_)
        }
        
        # The Phase 3.3 trend engine deliberately damps
        # extrapolation. Three-year continuation is also capped
        # so one noisy historical slope cannot create an
        # unrealistic long-range forecast.
        trend_3y <- max(
          -10,
          min(
            10,
            trend * 1.50
          )
        )
        
        max(
          0,
          min(
            100,
            baseline +
              trend_3y
          )
        )
      })
      
      
      output$projection_1y <- shiny::renderText({
        subtab_ready("development")
        
        projection_value_text(
          projection_1y_value()
        )
      })
      
      
      output$projection_3y <- shiny::renderText({
        subtab_ready("development")
        
        projection_value_text(
          projection_3y_value()
        )
      })
      
      
      output$trajectory <- shiny::renderText({
        subtab_ready("development")
        
        d <- player_projection()
        
        if (is.null(d)) {
          
          age <- derived_context()$age
          
          if (is.na(age)) {
            return("PENDING")
          }
          
          return(
            if (age <= 23) {
              "DEVELOPING"
            } else if (age <= 29) {
              "PRIME"
            } else if (age <= 32) {
              "VETERAN PRIME"
            } else {
              "AGE CURVE"
            }
          )
        }
        
        stored_trajectory <- first_existing(
          d,
          c(
            "projection_trajectory",
            "trajectory",
            "trend",
            "projection_trend"
          ),
          NA_character_
        )
        
        if (
          !is.na(stored_trajectory) &&
          nzchar(
            trimws(
              as.character(
                stored_trajectory
              )
            )
          )
        ) {
          return(
            toupper(
              as.character(
                stored_trajectory
              )
            )
          )
        }
        
        trend <- projection_num(
          d,
          c(
            "annual_trend"
          ),
          NA_real_
        )
        
        if (!is.finite(trend)) {
          
          legacy <- first_existing(
            d,
            c(
              "trajectory",
              "trend",
              "projection_trend"
            ),
            NA_character_
          )
          
          if (
            !is.na(legacy) &&
            nzchar(
              trimws(
                as.character(
                  legacy
                )
              )
            )
          ) {
            return(
              toupper(
                as.character(
                  legacy
                )
              )
            )
          }
          
          return("STABLE")
        }
        
        if (trend >= 2.0) {
          "RISING"
        } else if (trend <= -2.0) {
          "DECLINING"
        } else {
          "STABLE"
        }
      })
      
      
      output$projection_data_status <- shiny::renderText({
        subtab_ready("development")
        
        d <- player_projection()
        
        if (is.null(d)) {
          return(
            paste(
              "Projection table is not loaded for this player.",
              "Trajectory falls back to age/timeline context."
            )
          )
        }
        
        seasons_used <- projection_num(
          d,
          c(
            "seasons_used"
          ),
          NA_real_
        )
        
        confidence <- text_value(
          first_existing(
            d,
            c(
              "projection_confidence",
              "confidence"
            ),
            "AVAILABLE"
          ),
          "AVAILABLE"
        )
        
        version <- text_value(
          first_existing(
            d,
            c(
              "projection_version",
              "model_version"
            ),
            "Phase 3.3"
          ),
          "Phase 3.3"
        )
        
        paste0(
          "Five-season projection evidence",
          if (
            is.finite(
              seasons_used
            )
          ) {
            paste0(
              ": ",
              as.integer(
                seasons_used
              ),
              " season",
              if (
                as.integer(
                  seasons_used
                ) == 1
              ) "" else "s"
            )
          } else {
            ""
          },
          " • Confidence: ",
          toupper(
            confidence
          ),
          " • ",
          version
        )
      })
      
      # ------------------------------------------------------
      # CBA flags
      # ------------------------------------------------------
      
      output$cba_flags <- shiny::renderUI({
        p <- selected_player()
        context <- derived_context()
        
        option <- tolower(
          text_value(
            p$option_type,
            ""
          )
        )
        
        contract_type <- tolower(
          text_value(
            p$contract_type,
            ""
          )
        )
        
        fa_year <- num_value(
          p$free_agent_year
        )
        
        season_year <- suppressWarnings(
          as.numeric(
            substr(
              selected_season(),
              1,
              4
            )
          )
        )
        
        extension_review <- if (
          grepl(
            "rookie",
            contract_type,
            fixed = TRUE
          ) ||
          grepl(
            "veteran",
            contract_type,
            fixed = TRUE
          )
        ) {
          "Review"
        } else {
          "Not flagged"
        }
        
        trade_eligible <- if (
          context$two_way
        ) {
          "Review"
        } else {
          "Screen"
        }
        
        shiny::tagList(
          flag_row(
            "Extension eligibility",
            extension_review,
            "review",
            cba_term = extension_cba_term(
              contract_type
            )
          ),
          flag_row(
            "Trade eligibility",
            trade_eligible,
            "review"
          ),
          flag_row(
            "Two-way contract",
            if (context$two_way) "Yes" else "No",
            if (context$two_way) "yes" else "no",
            cba_term = "Two-Way Contract"
          ),
          flag_row(
            "Team option",
            if (
              grepl(
                "team",
                option,
                fixed = TRUE
              )
            ) {
              "Yes"
            } else {
              "No"
            },
            if (
              grepl(
                "team",
                option,
                fixed = TRUE
              )
            ) {
              "yes"
            } else {
              "no"
            },
            cba_term = "Team Option"
          ),
          flag_row(
            "Player option",
            if (
              grepl(
                "player",
                option,
                fixed = TRUE
              )
            ) {
              "Yes"
            } else {
              "No"
            },
            if (
              grepl(
                "player",
                option,
                fixed = TRUE
              )
            ) {
              "yes"
            } else {
              "no"
            },
            cba_term = "Player Option"
          ),
          flag_row(
            "Free agency proximity",
            if (
              !is.na(fa_year) &&
              !is.na(season_year) &&
              fa_year <= season_year + 1
            ) {
              "Near-term"
            } else {
              "No flag"
            },
            if (
              !is.na(fa_year) &&
              !is.na(season_year) &&
              fa_year <= season_year + 1
            ) {
              "review"
            } else {
              "yes"
            }
          )
        )
      })
      
      # ------------------------------------------------------
      # Development
      # ------------------------------------------------------
      
      output$development_focus <- shiny::renderText({
        subtab_ready("development")
        age <- derived_context()$age
        role <- derived_context()$role
        
        if (is.na(age)) {
          return(
            "Load performance and development data to establish a player-specific development plan."
          )
        }
        
        if (age <= 23) {
          paste(
            "Prioritize role clarity, skill development,",
            "and sustainable rotation growth."
          )
        } else if (age <= 27) {
          paste(
            "Maximize prime-development years while refining",
            "role-specific impact and consistency."
          )
        } else if (age <= 31) {
          paste(
            "Maintain role efficiency, availability, and",
            "skills that preserve lineup value."
          )
        } else {
          paste(
            "Monitor workload, role efficiency, and age-curve",
            "risk while preserving specialized value."
          )
        }
      })
      
      development_upside_value <- shiny::reactive({
        age <- derived_context()$age
        
        if (is.na(age)) {
          50
        } else if (age <= 22) {
          88
        } else if (age <= 24) {
          78
        } else if (age <= 27) {
          68
        } else if (age <= 30) {
          55
        } else {
          38
        }
      })
      
      output$development_meter <- shiny::renderUI({
        subtab_ready("development")
        shiny::tags$style(
          shiny::HTML(
            paste0(
              ".pi-development-fill{width:",
              development_upside_value(),
              "%;}"
            )
          )
        )
      })
      
      output$development_risk <- shiny::renderText({
        subtab_ready("development")
        age <- derived_context()$age
        
        if (is.na(age)) {
          "Review"
        } else if (age <= 29) {
          "Low"
        } else if (age <= 32) {
          "Moderate"
        } else {
          "Elevated"
        }
      })
      
      output$age_curve <- shiny::renderText({
        derived_context()$timeline
      })
      
      # ------------------------------------------------------
      # Right rail
      # ------------------------------------------------------
      
      output$summary_position <- shiny::renderText({
        text_value(
          selected_player()$primary_position
        )
      })
      
      output$summary_role <- shiny::renderText({
        derived_context()$role
      })
      
      output$summary_status <- shiny::renderText({
        derived_context()$roster_status
      })
      
      output$overall_view <- shiny::renderText({
        if (!is.null(player_stats())) {
          "Data-backed"
        } else {
          "Contract / roster"
        }
      })
      
      output$timeline <- shiny::renderText({
        derived_context()$timeline
      })
      
      output$roster_category <- shiny::renderText({
        p <- selected_player()
        
        if (derived_context()$two_way) {
          "Two-Way"
        } else if (
          grepl(
            "exhibit",
            tolower(
              text_value(
                p$contract_type,
                ""
              )
            ),
            fixed = TRUE
          )
        ) {
          "Exhibit"
        } else {
          "Standard"
        }
      })
      
      output$contract_cap_hit <- shiny::renderText({
        money(
          selected_player()$cap_hit
        )
      })
      
      output$years_remaining <- shiny::renderText({
        p <- selected_player()
        
        fa <- num_value(
          p$free_agent_year
        )
        
        season_start <- suppressWarnings(
          as.numeric(
            substr(
              selected_season(),
              1,
              4
            )
          )
        )
        
        if (
          is.na(fa) ||
          is.na(season_start)
        ) {
          "—"
        } else {
          as.character(
            max(
              0,
              fa - season_start
            )
          )
        }
      })
      
      output$total_value <- shiny::renderText({
        money(
          selected_player()$total_value
        )
      })
      
      output$guaranteed_value <- shiny::renderText({
        value <- num_value(
          selected_player()$guaranteed_value
        )
        
        if (is.na(value)) {
          value <- num_value(
            selected_player()$guaranteed_amount
          )
        }
        
        money(value)
      })
      
      output$contract_type <- shiny::renderText({
        text_value(
          selected_player()$contract_type,
          "Not classified"
        )
      })
      
      output$bird_rights <- shiny::renderText({
        text_value(
          selected_player()$bird_rights
        )
      })
      
      output$option_type <- shiny::renderText({
        text_value(
          selected_player()$option_type
        )
      })
      
      output$free_agent_year <- shiny::renderText({
        text_value(
          selected_player()$free_agent_year
        )
      })
      
      # ------------------------------------------------------
      # Risk and opportunity
      # ------------------------------------------------------
      
      output$key_risk <- shiny::renderText({
        p <- selected_player()
        context <- derived_context()
        
        if (
          is.null(
            player_stats()
          )
        ) {
          return(
            "Performance data is not yet connected, limiting basketball-impact evaluation."
          )
        }
        
        if (
          !is.na(context$age) &&
          context$age >= 32
        ) {
          return(
            "Age-curve decline could reduce future role and contract value."
          )
        }
        
        if (
          context$cap_hit >= 30e6
        ) {
          return(
            "High salary concentration raises the cost of performance variance."
          )
        }
        
        if (
          num_value(
            p$guaranteed_amount,
            0
          ) == 0
        ) {
          return(
            "Loaded guarantee information should be verified before final contract decisions."
          )
        }
        
        "No major structural risk is identified from the currently loaded player and contract inputs."
      })
      
      output$key_opportunity <- shiny::renderText({
        p <- selected_player()
        context <- derived_context()
        
        if (
          !is.na(context$age) &&
          context$age <= 24
        ) {
          return(
            "Development runway creates potential for role growth and surplus value."
          )
        }
        
        if (
          context$cap_hit > 0 &&
          context$cap_hit < 15e6
        ) {
          return(
            "Current salary level can provide useful roster value if role performance is sustained."
          )
        }
        
        if (
          grepl(
            "team",
            tolower(
              text_value(
                p$option_type,
                ""
              )
            ),
            fixed = TRUE
          )
        ) {
          return(
            "Team-controlled option structure preserves future roster flexibility."
          )
        }
        
        "Preserve optionality while comparing development, extension, and transaction pathways."
      })
      
      # ------------------------------------------------------
      # Recommendation
      # ------------------------------------------------------
      
      recommendation_context <- shiny::reactive({
        p <- selected_player()
        context <- derived_context()
        
        near_fa <- FALSE
        
        fa <- num_value(
          p$free_agent_year
        )
        
        season_start <- suppressWarnings(
          as.numeric(
            substr(
              selected_season(),
              1,
              4
            )
          )
        )
        
        if (
          !is.na(fa) &&
          !is.na(season_start)
        ) {
          near_fa <- fa <= season_start + 1
        }
        
        if (
          context$two_way
        ) {
          label <- "DEVELOP"
          subtitle <- "Evaluate conversion pathway"
        } else if (
          !is.na(context$age) &&
          context$age <= 24
        ) {
          label <- "DEVELOP"
          subtitle <- "Preserve growth runway"
        } else if (
          near_fa
        ) {
          label <- "REASSESS"
          subtitle <- "Contract decision window approaching"
        } else if (
          context$cap_hit >= 30e6 &&
          is.null(
            player_stats()
          )
        ) {
          label <- "REVIEW"
          subtitle <- "Impact evidence incomplete"
        } else {
          label <- "HOLD"
          subtitle <- "Maintain role and reassess"
        }
        
        list(
          label = label,
          subtitle = subtitle
        )
      })
      
      output$recommendation <- shiny::renderText({
        recommendation_context()$label
      })
      
      output$recommendation_subtitle <- shiny::renderText({
        recommendation_context()$subtitle
      })
      
      output$recommendation_rationale <- shiny::renderUI({
        p <- selected_player()
        context <- derived_context()
        
        rationale <- c(
          paste0(
            "Current role: ",
            context$role,
            "."
          ),
          paste0(
            "Contract type: ",
            text_value(
              p$contract_type,
              "not classified"
            ),
            "."
          ),
          paste0(
            "Timeline: ",
            context$timeline,
            "."
          )
        )
        
        if (is.null(player_stats())) {
          rationale <- c(
            rationale,
            "Performance data is still pending."
          )
        }
        
        shiny::tags$ul(
          class = "pi-rec-list",
          lapply(
            rationale,
            shiny::tags$li
          )
        )
      })
      
      output$recommended_actions <- shiny::renderUI({
        rec <- recommendation_context()$label
        
        actions <- switch(
          rec,
          "DEVELOP" = c(
            "Define role-specific development priorities.",
            "Track performance and minutes growth.",
            "Reassess contract pathway after updated production."
          ),
          "REASSESS" = c(
            "Review extension and free-agency timing.",
            "Compare internal replacement alternatives.",
            "Run transaction scenarios before the decision window."
          ),
          "REVIEW" = c(
            "Load current performance data.",
            "Verify guarantee and option details.",
            "Re-run value decision with complete inputs."
          ),
          c(
            "Maintain current roster role.",
            "Monitor performance and contract value.",
            "Compare extension and transaction alternatives as conditions change."
          )
        )
        
        shiny::tags$ul(
          class = "pi-rec-list",
          lapply(
            actions,
            shiny::tags$li
          )
        )
      })
      
      output$model_scope_note <- shiny::renderText({
        paste(
          "Player Intelligence is a decision-support workspace.",
          "Roster and contract outputs use loaded team data.",
          "Performance and projection outputs only populate when corresponding player-season",
          "and projection tables are available; missing values are not imputed or invented."
        )
      })
    }
  )
}


# ============================================================
# PLAYER MANAGEMENT — PERFORMANCE-SEASON HEALTHCHECK
# ============================================================

player_management_performance_context_healthcheck <- function() {
  
  body_text <- paste(
    deparse(
      body(
        mod_player_manager_server
      )
    ),
    collapse = "\n"
  )
  
  required <- c(
    "latest_player_performance_season",
    "performance_season",
    "player_season_impact",
    "LATEST AVAILABLE"
  )
  
  missing <- required[
    !vapply(
      required,
      function(x) {
        grepl(
          x,
          body_text,
          fixed = TRUE
        )
      },
      logical(1)
    )
  ]
  
  list(
    module =
      "Player Management",
    status = if (
      !length(missing)
    ) {
      "READY"
    } else {
      "REVIEW"
    },
    missing =
      missing,
    rule =
      "ROSTER SEASON IS SEPARATE FROM LATEST AVAILABLE PERFORMANCE EVIDENCE"
  )
}
