# ============================================================
# STEP 5.1 INTEGRITY-VERIFIED BUILD
# Required exports:
#   mod_depth_chart_ui
#   mod_depth_chart_server
# ============================================================

# ============================================================
# TBI NBA Basketball Operations Platform
# Roster Intelligence — BIE Phase 2 Performance Pass 6.1
# Cached Lineup + Rotation Intelligence
# ============================================================

# ------------------------------------------------------------
# Module: Roster Intelligence - Depth Chart
# Version 2.1 Final Front-Office Board
# ------------------------------------------------------------

# ============================================================
# UI
# ============================================================

#' Depth Chart UI
#'
#' @param id Internal module ID.
#' @noRd
mod_depth_chart_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    class = "depth-v21-page",
    
    shiny::tags$style(
      shiny::HTML(
        "
        /* ====================================================
           ROSTER INTELLIGENCE V2.1
           ==================================================== */

        .depth-v21-page {
          width:100%;
          padding:20px 22px 8px;
        }

        .depth-v21-shell {
          display:grid;
          grid-template-columns:minmax(0,1fr) 286px;
          gap:14px;
          align-items:start;
          max-width:1320px;
          margin:0 auto;
        }

        /* ---------- main board ---------- */

        .depth-v21-board {
          grid-column:1;
          grid-row:1;
          min-width:0;
          overflow:hidden;
          border:1px solid rgba(96,165,250,.20);
          border-radius:15px;
          background:
            linear-gradient(145deg,rgba(18,32,52,.98),rgba(12,24,40,.98));
          box-shadow:0 14px 34px rgba(0,0,0,.16);
        }

        .depth-v21-board-head {
          padding:18px 20px 14px;
          display:flex;
          align-items:flex-end;
          justify-content:space-between;
          gap:18px;
          border-bottom:1px solid rgba(148,163,184,.10);
        }

        .depth-v21-eyebrow {
          margin-bottom:4px;
          color:#7fa7dc;
          font-size:.58rem;
          font-weight:850;
          letter-spacing:.11em;
          text-transform:uppercase;
        }

        .depth-v21-title {
          margin:0 !important;
          color:#f7f9fc !important;
          font-size:1.58rem !important;
          font-weight:760 !important;
          letter-spacing:-.035em !important;
          line-height:1.08 !important;
        }

        .depth-v21-subtitle {
          margin:4px 0 0;
          color:#8ea0b7;
          font-size:.70rem;
          line-height:1.4;
        }

        .depth-v21-count {
          flex:0 0 auto;
          padding:6px 11px;
          border:1px solid rgba(96,165,250,.30);
          border-radius:999px;
          color:#7db5ff;
          background:rgba(59,130,246,.08);
          font-size:.61rem;
          font-weight:850;
        }

        .depth-v21-columns {
          display:grid;
          grid-template-columns:repeat(5,minmax(0,1fr));
          gap:0;
          padding:0;
        }

        .depth-v21-column {
          min-width:0;
          min-height:350px;
          padding:0 10px 14px;
          border-right:1px solid rgba(148,163,184,.095);
        }

        .depth-v21-column:last-child {
          border-right:0;
        }

        .depth-v21-column-head {
          height:54px;
          display:flex;
          align-items:center;
          justify-content:space-between;
          gap:8px;
          margin:0 -10px 10px;
          padding:0 12px;
          border-bottom:1px solid rgba(148,163,184,.10);
          background:rgba(255,255,255,.012);
        }

        .depth-v21-position-wrap {
          display:flex;
          align-items:center;
          gap:8px;
          min-width:0;
        }

        .depth-v21-position {
          min-width:34px;
          height:30px;
          padding:0 8px;
          display:grid;
          place-items:center;
          border:1px solid rgba(59,130,246,.12);
          border-radius:7px;
          color:#69aaff;
          background:rgba(37,99,235,.13);
          font-size:.69rem;
          font-weight:900;
        }

        .depth-v21-position-name {
          display:none;
          color:#d9e3ef;
          font-size:.66rem;
          font-weight:800;
        }

        .depth-v21-position-count {
          color:#7190b7;
          font-size:.56rem;
          font-weight:750;
          white-space:nowrap;
        }

        /* ---------- board player ---------- */

        .depth-v21-player {
          position:relative;
          width:100%;
          min-height:64px;
          margin:0 0 9px;
          padding:10px 9px 9px 11px;
          overflow:hidden;
          cursor:pointer;
          border:1px solid rgba(96,165,250,.18);
          border-left:2px solid rgba(96,165,250,.34);
          border-radius:9px;
          background:rgba(10,24,42,.54);
          transition:
            transform .14s ease,
            border-color .14s ease,
            background .14s ease,
            box-shadow .14s ease;
          user-select:none;
        }

        .depth-v21-player:hover {
          transform:translateY(-1px);
          border-color:rgba(96,165,250,.35);
          background:rgba(20,39,65,.72);
        }

        .depth-v21-player.starter {
          border-left-color:#34d399;
          background:
            linear-gradient(
              90deg,
              rgba(16,185,129,.055),
              rgba(10,24,42,.55) 34%
            );
        }

        .depth-v21-player.selected {
          border-color:#579cff;
          border-left-color:#579cff;
          box-shadow:
            0 0 0 1px rgba(87,156,255,.26),
            0 7px 18px rgba(25,88,170,.12);
          background:rgba(26,50,82,.80);
        }

        .depth-v21-player-top {
          display:flex;
          align-items:center;
          justify-content:space-between;
          gap:6px;
          margin-bottom:5px;
        }

        .depth-v21-role {
          overflow:hidden;
          color:#7d9bc2;
          font-size:.49rem;
          font-weight:900;
          letter-spacing:.09em;
          text-overflow:ellipsis;
          text-transform:uppercase;
          white-space:nowrap;
        }

        .depth-v21-player.starter .depth-v21-role {
          color:#44dda8;
        }

        .depth-v21-mini-pos {
          color:#6f89aa;
          font-size:.49rem;
          font-weight:850;
        }

        .depth-v21-player-name {
          display:block;
          overflow:hidden;
          color:#f2f6fb;
          font-size:.72rem;
          font-weight:800;
          letter-spacing:-.015em;
          line-height:1.15;
          text-overflow:ellipsis;
          white-space:nowrap;
        }

        .depth-v21-player-meta {
          margin-top:6px;
          display:flex;
          align-items:center;
          justify-content:space-between;
          gap:6px;
          color:#7d92ad;
          font-size:.53rem;
          white-space:nowrap;
        }

        .depth-v21-player-age {
          overflow:hidden;
          text-overflow:ellipsis;
        }

        .depth-v21-player-salary {
          flex:0 0 auto;
          color:#9fb4ce;
          font-weight:750;
        }

        .depth-v21-empty {
          padding:16px 7px;
          color:#667990;
          font-size:.59rem;
          text-align:center;
        }


        /* ---------- starting five court ---------- */

        .depth-v21-court-panel {
          grid-column:1;
          grid-row:2;
          overflow:hidden;
          border:1px solid rgba(96,165,250,.18);
          border-radius:15px;
          background:
            linear-gradient(145deg,rgba(16,28,45,.98),rgba(10,20,34,.98));
          box-shadow:0 14px 34px rgba(0,0,0,.13);
        }

        .depth-v21-court-head {
          min-height:52px;
          padding:0 17px;
          display:flex;
          align-items:center;
          justify-content:space-between;
          gap:12px;
          border-bottom:1px solid rgba(148,163,184,.09);
        }

        .depth-v21-court-title-wrap {
          display:flex;
          flex-direction:column;
          gap:2px;
        }

        .depth-v21-court-kicker {
          color:#6f87a4;
          font-size:.48rem;
          font-weight:900;
          letter-spacing:.10em;
          text-transform:uppercase;
        }

        .depth-v21-court-title {
          color:#eff4fa;
          font-size:.82rem;
          font-weight:800;
        }

        .depth-v21-court-live {
          display:flex;
          align-items:center;
          gap:6px;
          color:#6f87a4;
          font-size:.49rem;
          font-weight:800;
          letter-spacing:.07em;
          text-transform:uppercase;
        }

        .depth-v21-court-live-dot {
          width:6px;
          height:6px;
          border-radius:50%;
          background:#34d399;
          box-shadow:0 0 10px rgba(52,211,153,.48);
        }

        .depth-v21-court-stage {
          position:relative;
          width:100%;
          height:280px;
          overflow:hidden;
          background:
            radial-gradient(circle at 50% 8%,rgba(64,120,190,.08),transparent 30%),
            linear-gradient(180deg,#0e1b2b,#0a1726);
        }

        .depth-v21-court-svg {
          position:absolute;
          inset:0;
          width:100%;
          height:100%;
          pointer-events:none;
        }

        .depth-v21-court-svg line,
        .depth-v21-court-svg path,
        .depth-v21-court-svg rect,
        .depth-v21-court-svg circle {
          fill:none;
          stroke:rgba(125,170,221,.24);
          stroke-width:1.4;
          vector-effect:non-scaling-stroke;
        }

        .depth-v21-court-svg .court-strong {
          stroke:rgba(125,170,221,.34);
        }

        .depth-v21-court-player {
          position:absolute;
          width:112px;
          transform:translate(-50%,-50%);
          cursor:pointer;
          text-align:center;
          transition:
            transform .14s ease,
            filter .14s ease;
        }

        .depth-v21-court-player:hover {
          transform:translate(-50%,-50%) scale(1.045);
          filter:brightness(1.08);
        }

        .depth-v21-court-player.selected .depth-v21-court-avatar {
          border-color:#5da3ff;
          box-shadow:
            0 0 0 3px rgba(93,163,255,.12),
            0 7px 18px rgba(41,110,200,.18);
        }

        .depth-v21-court-avatar {
          width:48px;
          height:48px;
          margin:0 auto;
          display:grid;
          place-items:center;
          border:1px solid rgba(52,211,153,.42);
          border-radius:50%;
          background:
            radial-gradient(circle at 35% 30%,rgba(52,211,153,.14),transparent 60%),
            #102237;
          color:#ecf7f3;
          font-size:.68rem;
          font-weight:900;
          letter-spacing:.02em;
          box-shadow:0 7px 17px rgba(0,0,0,.22);
        }

        .depth-v21-court-name {
          margin-top:6px;
          overflow:hidden;
          color:#f0f5fa;
          font-size:.56rem;
          font-weight:800;
          line-height:1.15;
          text-overflow:ellipsis;
          white-space:nowrap;
          text-shadow:0 2px 6px rgba(0,0,0,.7);
        }

        .depth-v21-court-role {
          margin-top:2px;
          color:#76a9e5;
          font-size:.46rem;
          font-weight:850;
          letter-spacing:.08em;
        }

        .depth-v21-court-note {
          padding:8px 15px 10px;
          color:#687b92;
          font-size:.52rem;
          line-height:1.4;
          border-top:1px solid rgba(148,163,184,.07);
        }

        .depth-v21-court-empty {
          position:absolute;
          inset:0;
          display:grid;
          place-items:center;
          color:#70849b;
          font-size:.62rem;
        }


        /* ---------- Starting Five 2.0 scenario workspace ---------- */

        .depth-v23-court-panel {
          grid-column:1;
          grid-row:2;
          overflow:hidden;
          border:1px solid rgba(96,165,250,.18);
          border-radius:15px;
          background:linear-gradient(145deg,rgba(16,28,45,.98),rgba(10,20,34,.98));
          box-shadow:0 14px 34px rgba(0,0,0,.13);
        }

        .depth-v23-court-head {
          min-height:58px;
          padding:0 17px;
          display:flex;
          align-items:center;
          justify-content:space-between;
          gap:12px;
          border-bottom:1px solid rgba(148,163,184,.09);
        }

        .depth-v23-court-head-actions {
          display:flex;
          align-items:center;
          gap:8px;
        }

        .depth-v23-scenario-chip {
          display:flex;
          align-items:center;
          gap:6px;
          padding:5px 8px;
          border:1px solid rgba(96,165,250,.18);
          border-radius:999px;
          color:#7d96b5;
          background:rgba(59,130,246,.04);
          font-size:.47rem;
          font-weight:850;
          letter-spacing:.07em;
          text-transform:uppercase;
        }

        .depth-v23-scenario-chip.active {
          border-color:rgba(245,158,11,.25);
          color:#f1ad3f;
          background:rgba(245,158,11,.06);
        }

        .depth-v23-scenario-dot {
          width:6px;
          height:6px;
          border-radius:50%;
          background:#34d399;
          box-shadow:0 0 10px rgba(52,211,153,.48);
        }

        .depth-v23-scenario-chip.active .depth-v23-scenario-dot {
          background:#f59e0b;
          box-shadow:0 0 10px rgba(245,158,11,.40);
        }

        .depth-v23-intel-strip {
          display:grid;
          grid-template-columns:repeat(4,minmax(0,1fr));
          border-bottom:1px solid rgba(148,163,184,.08);
          background:rgba(255,255,255,.012);
        }

        .depth-v23-intel-metric {
          min-height:57px;
          padding:10px 13px;
          display:flex;
          flex-direction:column;
          justify-content:center;
          gap:3px;
          border-right:1px solid rgba(148,163,184,.08);
        }

        .depth-v23-intel-metric:last-child { border-right:0; }

        .depth-v23-intel-metric span {
          color:#6f839c;
          font-size:.47rem;
          font-weight:900;
          letter-spacing:.08em;
          text-transform:uppercase;
        }

        .depth-v23-intel-metric strong {
          color:#edf3f9;
          font-size:.72rem;
          font-weight:820;
        }

        .depth-bie-toolbar {
          display:grid;
          grid-template-columns:minmax(0,1fr) auto;
          gap:14px;
          align-items:center;
          padding:10px 13px;
          border-bottom:1px solid rgba(148,163,184,.08);
          background:linear-gradient(90deg,rgba(59,130,246,.055),rgba(59,130,246,.018));
        }

        .depth-bie-toolbar-copy { min-width:0; }

        .depth-bie-eyebrow {
          display:block;
          margin-bottom:4px;
          color:#60a5fa;
          font-size:.46rem;
          font-weight:900;
          letter-spacing:.10em;
          text-transform:uppercase;
        }

        .depth-bie-summary {
          display:flex;
          flex-wrap:wrap;
          gap:5px 12px;
          align-items:center;
          color:#8498b2;
          font-size:.54rem;
          line-height:1.45;
        }

        .depth-bie-summary strong {
          color:#edf4fb;
          font-size:.60rem;
          font-weight:850;
        }

        .depth-bie-summary .positive { color:#34d399; }
        .depth-bie-summary .warning { color:#fbbf24; }

        .depth-bie-confidence {
          display:inline-flex;
          align-items:center;
          padding:3px 6px;
          border:1px solid rgba(96,165,250,.20);
          border-radius:999px;
          background:rgba(59,130,246,.07);
          color:#8fc0ff !important;
          font-size:.45rem !important;
          font-weight:900 !important;
          letter-spacing:.07em;
        }

        .depth-bie-actions {
          display:flex;
          align-items:center;
          gap:6px;
          flex-wrap:wrap;
          justify-content:flex-end;
        }

        .depth-bie-actions .btn {
          min-height:31px;
          padding:5px 9px;
          border-radius:8px;
          font-size:.53rem;
          font-weight:850;
        }

        .depth-bie-view-btn {
          border:1px solid rgba(148,163,184,.16) !important;
          background:rgba(255,255,255,.025) !important;
          color:#a9b8ca !important;
        }

        .depth-bie-use-btn {
          border:1px solid rgba(52,211,153,.25) !important;
          background:rgba(16,185,129,.08) !important;
          color:#6ee7b7 !important;
        }


        .depth-bie-explain {
          display:grid;
          grid-template-columns:1.15fr .85fr;
          border-bottom:1px solid rgba(148,163,184,.08);
          background:rgba(5,15,27,.28);
        }

        .depth-bie-explain-main {
          padding:12px 13px;
          border-right:1px solid rgba(148,163,184,.08);
        }

        .depth-bie-explain-side {
          padding:12px 13px;
        }

        .depth-bie-explain-title {
          margin:0 0 4px;
          color:#edf4fb;
          font-size:.68rem;
          font-weight:900;
        }

        .depth-bie-explain-copy {
          margin:0 0 10px;
          color:#8297b1;
          font-size:.53rem;
          line-height:1.5;
        }

        .depth-bie-driver-grid {
          display:grid;
          grid-template-columns:repeat(3,minmax(0,1fr));
          gap:7px;
        }

        .depth-bie-driver {
          min-width:0;
          padding:8px 9px;
          border:1px solid rgba(148,163,184,.09);
          border-radius:8px;
          background:rgba(255,255,255,.018);
        }

        .depth-bie-driver span {
          display:block;
          color:#6f839c;
          font-size:.43rem;
          font-weight:900;
          letter-spacing:.07em;
          text-transform:uppercase;
        }

        .depth-bie-driver strong {
          display:block;
          margin-top:3px;
          color:#ecf3fa;
          font-size:.64rem;
          font-weight:880;
        }

        .depth-bie-driver strong.positive {
          color:#34d399;
        }

        .depth-bie-driver strong.warning {
          color:#fbbf24;
        }

        .depth-bie-driver em {
          display:block;
          margin-top:2px;
          color:#657b96;
          font-size:.44rem;
          font-style:normal;
        }

        .depth-bie-explain-section + .depth-bie-explain-section {
          margin-top:10px;
        }

        .depth-bie-explain-label {
          display:block;
          margin-bottom:5px;
          color:#6f839c;
          font-size:.44rem;
          font-weight:900;
          letter-spacing:.08em;
          text-transform:uppercase;
        }

        .depth-bie-explain-item {
          display:flex;
          gap:6px;
          align-items:flex-start;
          margin-bottom:5px;
          color:#9fb0c5;
          font-size:.51rem;
          line-height:1.4;
        }

        .depth-bie-explain-item:last-child {
          margin-bottom:0;
        }

        .depth-bie-explain-dot {
          width:5px;
          height:5px;
          margin-top:5px;
          flex:0 0 auto;
          border-radius:50%;
          background:#34d399;
        }

        .depth-bie-explain-dot.concern {
          background:#fbbf24;
        }

        .depth-bie-change-summary {
          margin-top:9px;
          padding-top:8px;
          border-top:1px solid rgba(148,163,184,.07);
          color:#8ea1b8;
          font-size:.50rem;
          line-height:1.45;
        }


        .depth-bie-rotation-shell {
          border-top:1px solid rgba(148,163,184,.08);
          border-bottom:1px solid rgba(148,163,184,.08);
          background:rgba(5,15,27,.22);
        }

        .depth-bie-rotation-head {
          display:flex;
          align-items:center;
          justify-content:space-between;
          gap:12px;
          padding:11px 13px 8px;
        }

        .depth-bie-rotation-title {
          display:block;
          color:#edf4fb;
          font-size:.68rem;
          font-weight:900;
        }


        .depth-bie-rotation-baseline-note {
          display:block;
          margin-top:3px;
          color:#7188a4;
          font-size:.46rem;
          line-height:1.35;
        }

        .depth-bie-rotation-controls
        .shiny-input-container {
          margin:0 !important;
        }

        .depth-bie-rotation-controls
        .form-group {
          margin-bottom:0 !important;
        }

        .depth-bie-rotation-controls select,
        .depth-bie-rotation-controls .form-select {
          min-height:31px !important;
          height:31px !important;
          padding-top:3px !important;
          padding-bottom:3px !important;
          border-color:rgba(148,163,184,.15) !important;
          background-color:rgba(255,255,255,.025) !important;
          color:#dce7f4 !important;
          font-size:.52rem !important;
        }

        .depth-bie-rotation-summary {
          display:flex;
          flex-wrap:wrap;
          gap:7px 14px;
          padding:0 13px 10px;
          color:#8094ad;
          font-size:.51rem;
          line-height:1.4;
        }

        .depth-bie-rotation-summary strong {
          color:#edf4fb;
          font-weight:900;
        }

        .depth-bie-rotation-summary
        .confidence {
          color:#8fc0ff;
        }

        .depth-bie-rotation-table {
          display:grid;
          grid-template-columns:
            48px minmax(140px,1.4fr)
            70px 70px 80px;
          align-items:center;
          padding:0 13px;
        }

        .depth-bie-rotation-row {
          display:contents;
        }

        .depth-bie-rotation-cell {
          min-width:0;
          padding:8px 7px;
          border-top:1px solid rgba(148,163,184,.07);
          color:#9fb0c5;
          font-size:.52rem;
        }

        .depth-bie-rotation-header {
          color:#687d97;
          font-size:.43rem;
          font-weight:900;
          letter-spacing:.07em;
          text-transform:uppercase;
        }

        .depth-bie-rotation-player {
          color:#edf4fb;
          font-weight:850;
          white-space:nowrap;
          overflow:hidden;
          text-overflow:ellipsis;
        }

        .depth-bie-rotation-role {
          display:inline-flex;
          padding:3px 5px;
          border-radius:999px;
          background:rgba(59,130,246,.07);
          color:#8fc0ff;
          font-size:.42rem;
          font-weight:900;
          letter-spacing:.05em;
        }

        .depth-bie-rotation-role.bench {
          background:rgba(148,163,184,.06);
          color:#9fb0c5;
        }

        .depth-bie-rotation-minutes {
          color:#34d399;
          font-weight:900;
        }

        .depth-bie-rotation-note {
          padding:9px 13px 11px;
          border-top:1px solid rgba(148,163,184,.07);
          color:#7389a4;
          font-size:.48rem;
          line-height:1.45;
        }

        @media(max-width:760px) {
          .depth-bie-rotation-table {
            grid-template-columns:
              42px minmax(110px,1.4fr)
              58px 58px 66px;
            padding:0 8px;
          }

          .depth-bie-rotation-cell {
            padding:7px 4px;
            font-size:.47rem;
          }
        }

        @media(max-width:1050px) {
          .depth-bie-explain {
            grid-template-columns:1fr;
          }

          .depth-bie-explain-main {
            border-right:0;
            border-bottom:1px solid rgba(148,163,184,.08);
          }
        }

        @media(max-width:720px) {
          .depth-bie-driver-grid {
            grid-template-columns:1fr 1fr;
          }
        }

        @media(max-width:900px) {
          .depth-bie-toolbar { grid-template-columns:1fr; }
          .depth-bie-actions { justify-content:flex-start; }
        }

        .depth-v23-court-stage {
          position:relative;
          width:100%;
          height:305px;
          overflow:hidden;
          background:
            radial-gradient(circle at 50% 8%,rgba(64,120,190,.08),transparent 30%),
            linear-gradient(180deg,#0e1b2b,#0a1726);
        }

        .depth-v23-court-slot {
          position:absolute;
          width:132px;
          min-height:84px;
          transform:translate(-50%,-50%);
          border:1px dashed rgba(96,165,250,.16);
          border-radius:14px;
          transition:border-color .12s ease,background .12s ease,box-shadow .12s ease;
        }

        .depth-v23-court-slot.drag-over {
          border-color:#61a8ff;
          background:rgba(59,130,246,.08);
          box-shadow:0 0 0 3px rgba(59,130,246,.07);
        }

        .depth-v23-court-token {
          position:absolute;
          inset:0;
          padding:8px 7px 7px;
          cursor:grab;
          text-align:center;
          border:1px solid rgba(52,211,153,.22);
          border-radius:13px;
          background:linear-gradient(145deg,rgba(17,39,61,.96),rgba(10,26,43,.98));
          box-shadow:0 8px 20px rgba(0,0,0,.20);
          transition:transform .12s ease,border-color .12s ease,box-shadow .12s ease;
        }

        .depth-v23-court-token:active { cursor:grabbing; }

        .depth-v23-court-token:hover {
          transform:translateY(-2px);
          border-color:rgba(96,165,250,.42);
        }

        .depth-v23-court-token.selected {
          border-color:#5da3ff;
          box-shadow:0 0 0 2px rgba(93,163,255,.15),0 9px 22px rgba(41,110,200,.18);
        }

        .depth-v23-court-token.out-of-position {
          border-color:rgba(245,158,11,.38);
          background:linear-gradient(145deg,rgba(67,45,17,.34),rgba(10,26,43,.98));
        }

        .depth-v23-token-top {
          display:flex;
          align-items:center;
          justify-content:center;
          gap:7px;
        }

        .depth-v23-token-avatar {
          width:34px;
          height:34px;
          display:grid;
          place-items:center;
          border:1px solid rgba(96,165,250,.28);
          border-radius:50%;
          color:#eaf3fc;
          background:radial-gradient(circle at 35% 30%,rgba(96,165,250,.16),transparent 60%),#11253c;
          font-size:.53rem;
          font-weight:900;
        }

        .depth-v23-token-logo-slot {
          width:18px;
          height:18px;
          display:grid;
          place-items:center;
          border:1px solid rgba(148,163,184,.14);
          border-radius:5px;
          color:#607892;
          font-size:.36rem;
          font-weight:900;
          background:rgba(255,255,255,.015);
        }

        .depth-v23-token-name {
          margin-top:6px;
          overflow:hidden;
          color:#f3f7fb;
          font-size:.57rem;
          font-weight:820;
          line-height:1.15;
          text-overflow:ellipsis;
          white-space:nowrap;
        }

        .depth-v23-token-meta {
          margin-top:4px;
          display:flex;
          align-items:center;
          justify-content:center;
          gap:7px;
          color:#7fa7d7;
          font-size:.45rem;
          font-weight:750;
        }

        .depth-v23-oop-label {
          margin-top:3px;
          color:#f3af3f;
          font-size:.39rem;
          font-weight:900;
          letter-spacing:.06em;
          text-transform:uppercase;
        }

        .depth-v23-bench {
          border-top:1px solid rgba(148,163,184,.08);
          background:rgba(8,18,30,.36);
        }

        .depth-v23-bench-head {
          min-height:38px;
          padding:0 14px;
          display:flex;
          align-items:center;
          justify-content:space-between;
          gap:10px;
          border-bottom:1px solid rgba(148,163,184,.07);
        }

        .depth-v23-bench-title {
          color:#7d94b0;
          font-size:.49rem;
          font-weight:900;
          letter-spacing:.09em;
          text-transform:uppercase;
        }

        .depth-v23-bench-count {
          color:#657b95;
          font-size:.47rem;
          font-weight:800;
        }

        .depth-v23-bench-scroll {
          padding:10px 12px 12px;
          display:flex;
          gap:8px;
          overflow-x:auto;
          scrollbar-width:thin;
        }

        .depth-v23-bench-token {
          flex:0 0 142px;
          min-height:62px;
          padding:8px 9px;
          cursor:grab;
          border:1px solid rgba(96,165,250,.15);
          border-radius:9px;
          background:rgba(12,29,48,.68);
          transition:transform .12s ease,border-color .12s ease,background .12s ease;
        }

        .depth-v23-bench-token:hover {
          transform:translateY(-1px);
          border-color:rgba(96,165,250,.32);
          background:rgba(18,39,64,.76);
        }

        .depth-v23-bench-token:active { cursor:grabbing; }

        .depth-v23-bench-token.selected {
          border-color:#5da3ff;
          box-shadow:0 0 0 1px rgba(93,163,255,.14);
        }

        .depth-v23-bench-name {
          overflow:hidden;
          color:#edf3f9;
          font-size:.58rem;
          font-weight:800;
          text-overflow:ellipsis;
          white-space:nowrap;
        }

        .depth-v23-bench-meta {
          margin-top:5px;
          display:flex;
          justify-content:space-between;
          gap:6px;
          color:#718aa8;
          font-size:.45rem;
        }

        .depth-v23-controls {
          min-height:54px;
          padding:9px 13px;
          display:flex;
          align-items:center;
          justify-content:space-between;
          gap:12px;
          border-top:1px solid rgba(148,163,184,.08);
          background:rgba(255,255,255,.012);
        }

        .depth-v23-control-copy {
          color:#6e829a;
          font-size:.50rem;
          line-height:1.45;
        }

        .depth-v23-control-copy strong { color:#aebed0; }

        .depth-v23-buttons {
          flex:0 0 auto;
          display:flex;
          align-items:center;
          gap:8px;
        }

        .depth-v23-buttons .btn {
          min-height:34px;
          padding:0 11px !important;
          border-radius:8px !important;
          font-size:.52rem !important;
          font-weight:850 !important;
        }

        .depth-v23-buttons .btn-primary {
          border-color:#327ee8 !important;
          background:linear-gradient(180deg,#398cf5,#286fd4) !important;
        }

        .depth-v23-buttons .btn-default {
          border-color:rgba(148,163,184,.15) !important;
          background:#111c2c !important;
          color:#aebac9 !important;
        }

        .depth-v23-shared-trade-banner {
          display:flex;
          align-items:center;
          justify-content:space-between;
          gap:14px;
          margin-bottom:10px;
          padding:10px 13px;
          border:1px solid rgba(96,165,250,.24);
          border-radius:10px;
          background:linear-gradient(90deg,rgba(59,130,246,.10),rgba(59,130,246,.03));
        }

        .depth-v23-shared-trade-copy {
          display:flex;
          align-items:center;
          gap:9px;
          color:#a9b8ca;
          font-size:.60rem;
          line-height:1.45;
        }

        .depth-v23-shared-trade-badge {
          display:inline-flex;
          align-items:center;
          gap:6px;
          padding:5px 8px;
          border-radius:999px;
          background:rgba(59,130,246,.12);
          color:#72adff;
          font-size:.53rem;
          font-weight:900;
          letter-spacing:.08em;
          white-space:nowrap;
        }

        .depth-v23-shared-trade-note {
          color:#fbbf24;
          font-size:.54rem;
          font-weight:800;
          white-space:nowrap;
        }

        .depth-v21-player.trade-incoming {
          border-color:rgba(52,211,153,.34);
          border-left-color:#34d399;
          background:
            linear-gradient(
              90deg,
              rgba(16,185,129,.08),
              rgba(10,24,42,.55) 42%
            );
        }

        .depth-v23-bench-token.trade-incoming {
          border-color:rgba(52,211,153,.34);
          background:rgba(16,185,129,.065);
        }

        @media(max-width:760px) {
          .depth-v23-intel-strip {
            grid-template-columns:repeat(2,minmax(0,1fr));
          }

          .depth-v23-court-stage { height:420px; }
          .depth-v23-court-slot { width:116px; }

          .depth-v23-controls {
            align-items:flex-start;
            flex-direction:column;
          }
        }

        /* ---------- player rail ---------- */

        .depth-v21-rail {
          grid-column:2;
          grid-row:1 / span 2;
          overflow:hidden;
          border:1px solid rgba(96,165,250,.20);
          border-radius:15px;
          background:
            linear-gradient(155deg,rgba(18,33,54,.99),rgba(12,25,42,.99));
          box-shadow:0 14px 34px rgba(0,0,0,.16);
        }

        .depth-v21-rail-head {
          padding:16px 16px 13px;
          border-bottom:1px solid rgba(148,163,184,.10);
        }

        .depth-v21-profile {
          display:grid;
          grid-template-columns:52px minmax(0,1fr);
          gap:11px;
          align-items:center;
        }

        .depth-v21-avatar {
          width:52px;
          height:52px;
          display:grid;
          place-items:center;
          border:1px solid rgba(96,165,250,.28);
          border-radius:14px;
          color:#78b3ff;
          background:
            radial-gradient(
              circle at 35% 30%,
              rgba(96,165,250,.18),
              transparent 60%
            ),
            #0e1c30;
          font-size:.92rem;
          font-weight:900;
          letter-spacing:.03em;
        }

        .depth-v21-player-title {
          margin:0 !important;
          overflow:hidden;
          color:#f7f9fc !important;
          font-size:1.03rem !important;
          font-weight:780 !important;
          letter-spacing:-.025em !important;
          line-height:1.15 !important;
          text-overflow:ellipsis;
          white-space:nowrap;
        }

        .depth-v21-profile-sub {
          margin-top:4px;
          color:#89a0bd;
          font-size:.59rem;
          font-weight:700;
        }

        .depth-v21-profile-badges {
          margin-top:8px;
          display:flex;
          gap:6px;
          flex-wrap:wrap;
        }

        .depth-v21-badge {
          display:inline-flex;
          align-items:center;
          min-height:23px;
          padding:3px 7px;
          border:1px solid rgba(96,165,250,.18);
          border-radius:999px;
          color:#8db7e9;
          background:rgba(59,130,246,.06);
          font-size:.48rem;
          font-weight:850;
          letter-spacing:.05em;
          text-transform:uppercase;
        }

        .depth-v21-badge.starter {
          border-color:rgba(52,211,153,.20);
          color:#3edda5;
          background:rgba(16,185,129,.06);
        }

        .depth-v21-badge.override {
          border-color:rgba(245,158,11,.20);
          color:#f7b23a;
          background:rgba(245,158,11,.06);
        }

        .depth-v21-metrics {
          padding:12px 14px 4px;
          display:grid;
          grid-template-columns:repeat(2,minmax(0,1fr));
          gap:8px;
        }

        .depth-v21-metric {
          min-height:58px;
          padding:9px 10px;
          display:flex;
          flex-direction:column;
          justify-content:center;
          gap:3px;
          border:1px solid rgba(148,163,184,.10);
          border-radius:8px;
          background:rgba(255,255,255,.012);
        }

        .depth-v21-metric span {
          color:#71839a;
          font-size:.48rem;
          font-weight:850;
          letter-spacing:.08em;
          text-transform:uppercase;
        }

        .depth-v21-metric strong {
          overflow:hidden;
          color:#eef3f8;
          font-size:.76rem;
          font-weight:800;
          text-overflow:ellipsis;
          white-space:nowrap;
        }

        /* ---------- assignment ---------- */

        .depth-v21-assignment {
          margin:8px 14px 12px;
          padding:12px;
          border:1px solid rgba(148,163,184,.10);
          border-radius:10px;
          background:rgba(7,17,29,.28);
        }

        .depth-v21-section-title {
          margin-bottom:10px;
          color:#78899f;
          font-size:.50rem;
          font-weight:900;
          letter-spacing:.10em;
          text-transform:uppercase;
        }

        .depth-v21-eligibility {
          margin-bottom:8px;
          color:#d3dbe5;
          font-size:.62rem;
          line-height:1.4;
        }

        .depth-v21-assignment .form-group {
          margin-bottom:9px !important;
        }

        .depth-v21-assignment label {
          margin-bottom:4px !important;
          color:#778aa2 !important;
          font-size:.52rem !important;
          font-weight:800 !important;
        }

        .depth-v21-assignment .form-control,
        .depth-v21-assignment .selectize-input {
          min-height:36px !important;
          border-color:rgba(148,163,184,.15) !important;
          border-radius:8px !important;
          background:#101b2b !important;
          color:#edf3f9 !important;
          font-size:.62rem !important;
        }

        .depth-v21-assignment .form-check {
          margin:5px 0 7px;
        }

        .depth-v21-assignment .form-check-label {
          color:#b5c1d0 !important;
          font-size:.58rem !important;
          line-height:1.35 !important;
        }

        .depth-v21-override-reason {
          margin-top:7px;
        }

        /* TBI_NATURAL_DEPTH_PRESENTATION_V2 */
        .depth-v21-eligibility,
        .depth-v21-override-reason,
        .depth-v21-assignment .form-check:has(input[id$='allow_position_override']),
        .depth-v21-assignment .form-group:has(input[id$='allow_position_override']),
        .depth-v21-assignment .shiny-input-container:has(input[id$='allow_position_override']) {
          display:none !important;
        }

        .depth-v23-oop-label {
          display:none !important;
        }

        .depth-v21-actions {
          display:grid;
          grid-template-columns:minmax(0,1fr) 74px;
          gap:8px;
          margin-top:10px;
        }

        .depth-v21-actions .btn {
          min-height:36px;
          border-radius:8px !important;
          font-size:.58rem !important;
          font-weight:800 !important;
        }

        .depth-v21-actions .btn-primary {
          border-color:#327ee8 !important;
          background:linear-gradient(180deg,#398cf5,#286fd4) !important;
        }

        .depth-v21-actions .btn-default {
          border-color:rgba(148,163,184,.15) !important;
          background:#111c2c !important;
          color:#aebac9 !important;
        }

        /* ---------- contract footer ---------- */

        .depth-v21-contract {
          margin:0 14px 14px;
          padding:11px 12px;
          border:1px solid rgba(148,163,184,.09);
          border-radius:9px;
          background:rgba(255,255,255,.012);
        }

        .depth-v21-contract-label {
          color:#71839a;
          font-size:.48rem;
          font-weight:900;
          letter-spacing:.09em;
          text-transform:uppercase;
        }

        .depth-v21-contract strong {
          display:block;
          margin-top:4px;
          color:#e9eff6;
          font-size:.70rem;
        }

        .depth-v21-contract span {
          display:block;
          margin-top:3px;
          color:#75879d;
          font-size:.55rem;
        }

        /* ---------- notifications ---------- */

        .depth-v21-message {
          margin:0 14px 12px;
          padding:8px 10px;
          border:1px solid rgba(96,165,250,.16);
          border-radius:8px;
          color:#9db5d1;
          background:rgba(59,130,246,.05);
          font-size:.56rem;
          line-height:1.4;
        }

        /* ---------- responsive ---------- */

        @media(max-width:1180px) {
          .depth-v21-shell {
            grid-template-columns:1fr;
          }

          .depth-v21-rail {
            grid-column:1;
            grid-row:auto;
            display:grid;
            grid-template-columns:minmax(250px,.8fr) minmax(0,1.2fr);
          }

          .depth-v21-court-panel {
            grid-column:1;
            grid-row:auto;
          }

          .depth-v21-rail-head {
            border-right:1px solid rgba(148,163,184,.10);
          }

          .depth-v21-metrics {
            align-content:start;
          }

          .depth-v21-assignment,
          .depth-v21-contract {
            grid-column:2;
          }
        }

        @media(max-width:920px) {
          .depth-v21-columns {
            grid-template-columns:repeat(2,minmax(0,1fr));
          }

          .depth-v21-column {
            border-bottom:1px solid rgba(148,163,184,.09);
          }
        }

        @media(max-width:650px) {
          .depth-v21-page {
            padding:12px 10px;
          }

          .depth-v21-columns,
          .depth-v21-rail {
            grid-template-columns:1fr;
          }

          .depth-v21-column {
            min-height:auto;
            border-right:0;
          }

          .depth-v21-assignment,
          .depth-v21-contract {
            grid-column:auto;
          }
        }
        "
      )
    ),
    
    shiny::tags$script(
      shiny::HTML(
        "
        document.addEventListener('dragstart', function(e) {
          var token = e.target.closest('[data-lineup-player]');
          if (!token) return;

          e.dataTransfer.setData(
            'text/plain',
            token.getAttribute('data-lineup-player')
          );

          e.dataTransfer.effectAllowed = 'move';
        });

        document.addEventListener('dragover', function(e) {
          var slot = e.target.closest('[data-lineup-slot]');
          if (!slot) return;

          e.preventDefault();
          e.dataTransfer.dropEffect = 'move';
          slot.classList.add('drag-over');
        });

        document.addEventListener('dragleave', function(e) {
          var slot = e.target.closest('[data-lineup-slot]');
          if (!slot) return;
          slot.classList.remove('drag-over');
        });

        document.addEventListener('drop', function(e) {
          var slot = e.target.closest('[data-lineup-slot]');
          if (!slot) return;

          e.preventDefault();
          slot.classList.remove('drag-over');

          var playerId = e.dataTransfer.getData('text/plain');
          var position = slot.getAttribute('data-lineup-slot');

          var panel = slot.closest('[data-lineup-input]');
          if (!panel) return;

          var inputId = panel.getAttribute('data-lineup-input');

          if (!playerId || !position || !inputId) return;

          Shiny.setInputValue(
            inputId,
            {
              player_id: parseInt(playerId),
              position: position,
              nonce: Date.now()
            },
            {priority:'event'}
          );
        });
        "
      )
    ),
    
    shiny::uiOutput(
      ns("shared_trade_scenario_banner")
    ),

    shiny::div(
      class = "depth-v21-shell",
      
      # ------------------------------------------------------
      # Depth chart board
      # ------------------------------------------------------
      
      shiny::div(
        class = "depth-v21-board",
        
        shiny::div(
          class = "depth-v21-board-head",
          
          shiny::div(
            shiny::div(
              class = "depth-v21-eyebrow",
              "ROTATION BOARD"
            ),
            shiny::h2(
              class = "depth-v21-title",
              "Depth Chart"
            ),
            shiny::p(
              class = "depth-v21-subtitle",
              "Select a player to review or edit their lineup assignment."
            )
          ),
          
          shiny::div(
            class = "depth-v21-count",
            shiny::textOutput(
              ns("player_count"),
              inline = TRUE
            )
          )
        ),
        
        shiny::uiOutput(
          ns("depth_chart_board")
        )
      ),
      
      # ------------------------------------------------------
      # Starting Five 2.0 scenario workspace
      # ------------------------------------------------------
      
      shiny::div(
        class = "depth-v21-right-workspace",

        shiny::div(
          class = "depth-v23-court-panel",
          `data-lineup-input` = ns("lineup_drop"),

          shiny::tags$details(
            class = "depth-lineup-editor-disclosure",
            open = NA,
            shiny::tags$summary(
              "Edit Working Lineup"
            ),

        shiny::div(
          class = "depth-v23-court-head",
          
          shiny::div(
            class = "depth-v21-court-title-wrap",
            shiny::span(
              class = "depth-v21-court-kicker",
              "LINEUP SCENARIO"
            ),
            shiny::strong(
              class = "depth-v21-court-title",
              "Current Starting Five"
            )
          ),
          
          shiny::div(
            class = "depth-v23-court-head-actions",
            shiny::uiOutput(
              ns("scenario_status_chip")
            )
          )
        ),
        
        shiny::uiOutput(
          ns("lineup_intelligence_strip")
        ),
        
        shiny::div(
          class = "depth-bie-toolbar",
          
          shiny::div(
            class = "depth-bie-toolbar-copy",
            shiny::span(
              class = "depth-bie-eyebrow",
              "BASKETBALL INTELLIGENCE ENGINE"
            ),
            shiny::uiOutput(
              ns("bie_lineup_summary")
            )
          ),
          
          shiny::div(
            class = "depth-bie-actions",
            
            shiny::actionButton(
              ns("view_working_lineup"),
              "Working Lineup",
              class = "depth-bie-view-btn"
            ),
            
            shiny::actionButton(
              ns("view_bie_lineup"),
              "BIE Recommended",
              class = "depth-bie-view-btn"
            ),
            
            shiny::actionButton(
              ns("use_bie_lineup"),
              "Use Recommendation",
              class = "depth-bie-use-btn"
            )
          )
        ),
        
        shiny::uiOutput(
          ns("bie_explanation_panel")
        ),
        
        shiny::uiOutput(
          ns("starting_five_court")
        ),
        
        shiny::uiOutput(
          ns("bench_strip")
        ),
        
        shiny::div(
          class = "depth-bie-rotation-shell",
          
          shiny::div(
            class = "depth-bie-rotation-head",
            
            shiny::div(
              shiny::span(
                class = "depth-bie-eyebrow",
                "ROTATION INTELLIGENCE"
              ),
              shiny::strong(
                class = "depth-bie-rotation-title",
                "Rotation Around Working Five"
              ),
              shiny::span(
                class = "depth-bie-rotation-baseline-note",
                "Starters preserved • BIE selects bench + minutes"
              )
            ),
            
            shiny::div(
              class = "depth-bie-rotation-controls",
              
              shiny::selectInput(
                ns("bie_rotation_size"),
                label = NULL,
                choices = c(
                  "8-Man" = "8",
                  "9-Man" = "9",
                  "10-Man" = "10"
                ),
                selected = "9",
                width = "92px"
              )
            )
          ),
          
          shiny::uiOutput(
            ns("bie_rotation_summary")
          ),
          
          shiny::uiOutput(
            ns("bie_rotation_table")
          ),
          
          shiny::uiOutput(
            ns("phase11_lineup_optimization_panel")
          ),
          
          shiny::uiOutput(
            ns("phase12_scenario_comparison_panel")
          )
        ),
        
          shiny::div(
            class = "depth-v23-controls",
          
          shiny::div(
            class = "depth-v23-control-copy",
            shiny::HTML(
              paste(
                "<strong>Drag any available player onto a court position</strong>",
                "to adjust the proposed Starting Five.",
                "Pending trades automatically refill vacated starter spots;",
                "Apply Lineup confirms the working preview without changing the official database."
              )
            )
          ),
          
          shiny::div(
            class = "depth-v23-buttons",
            
            shiny::actionButton(
              ns("reset_scenario"),
              "Reset Scenario"
            ),
            
            shiny::actionButton(
              ns("apply_scenario"),
              "Apply Lineup",
              class = "btn-primary"
            )
            )
          )
          )
        ),
      
      # ------------------------------------------------------
      # Player intelligence rail
      # ------------------------------------------------------
      
        shiny::div(
          class = "depth-v21-rail",
        
        shiny::div(
          class = "depth-v21-rail-head",
          shiny::div(
            class = "depth-v21-rail-toolbar",
            shiny::div(
              class = "depth-v21-eyebrow",
              "PLAYER DETAIL"
            ),
            shiny::actionLink(
              ns("clear_player_detail"),
              "Clear player detail",
              class = "depth-v21-clear-player"
            )
          ),
          
          shiny::uiOutput(
            ns("player_profile_header")
          )
        ),
        
        shiny::uiOutput(
          ns("player_metric_grid")
        ),
        
        shiny::uiOutput(
          ns("assignment_panel")
        ),
        
        shiny::uiOutput(
          ns("save_message")
        ),
        
          shiny::uiOutput(
            ns("contract_panel"),
            class = "depth-v21-contract-output"
          )
        )
      )
    ),

    shiny::uiOutput(
      ns("v2_development_intelligence"),
      class = "tbi-depth-v2-workspace"
    )
  )
}


# ============================================================
# DEPTH CHART PERFORMANCE HELPERS
# ============================================================

#' Merge one evidence row using the frozen compatibility precedence
#' @noRd
depth_chart_merge_bie_evidence <- function(base, extra, prefix) {
  if (is.null(extra) || !is.data.frame(extra) || !nrow(extra)) {
    return(base)
  }

  extra <- extra[1, , drop = FALSE]
  key_columns <- c(
    "player_id",
    "team_id",
    "season",
    "source_name",
    "source_player_id",
    "source_team",
    "imported_at",
    "updated_at",
    "metric_version"
  )
  evidence_columns <- setdiff(names(extra), key_columns)

  for (column in evidence_columns) {
    value <- extra[[column]][[1]]
    prefixed_name <- paste0(prefix, column)
    base[[prefixed_name]] <- value

    if (!column %in% names(base)) {
      base[[column]] <- value
    } else {
      current <- base[[column]]
      current_missing <-
        is.null(current) || !length(current) || all(is.na(current))
      value_usable <-
        !is.null(value) && length(value) && !all(is.na(value))

      if (current_missing && value_usable) {
        base[[column]] <- value
      }
    }
  }

  base
}


#' Resolve official positions for a roster with one read-only query
#' @noRd
depth_chart_batched_official_positions <- function(roster) {
  if (is.null(roster) || !is.data.frame(roster) || !nrow(roster)) {
    return(list())
  }

  if (!all(c("player_id", "primary_position") %in% names(roster))) {
    stop(
      "Roster must contain player_id and primary_position.",
      call. = FALSE
    )
  }

  player_ids <- unique(suppressWarnings(as.integer(roster$player_id)))
  player_ids <- player_ids[!is.na(player_ids)]

  if (!length(player_ids)) {
    return(list())
  }

  con <- connect_db(read_only = TRUE)
  on.exit(disconnect_db(con), add = TRUE)

  placeholders <- paste(rep("?", length(player_ids)), collapse = ",")
  explicit <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT player_id, position ",
      "FROM player_positions ",
      "WHERE player_id IN (", placeholders, ") ",
      "ORDER BY player_id, eligibility_rank, position"
    ),
    params = as.list(player_ids)
  )

  valid_positions <- c("PG", "SG", "SF", "PF", "C")
  roster_first <- roster[
    !duplicated(suppressWarnings(as.integer(roster$player_id))),
    ,
    drop = FALSE
  ]

  result <- stats::setNames(
    vector("list", nrow(roster_first)),
    as.character(suppressWarnings(as.integer(roster_first$player_id)))
  )

  for (i in seq_len(nrow(roster_first))) {
    player_id <- suppressWarnings(as.integer(roster_first$player_id[[i]]))
    primary_position <- roster_first$primary_position[[i]]
    explicit_rows <- explicit[
      suppressWarnings(as.integer(explicit$player_id)) == player_id,
      ,
      drop = FALSE
    ]

    eligible <- unique(vapply(
      explicit_rows$position,
      normalize_depth_position,
      character(1)
    ))
    eligible <- eligible[eligible %in% valid_positions]

    if (!length(eligible)) {
      position <- toupper(trimws(as.character(primary_position %||% "")))
      tokens <- unlist(strsplit(
        gsub("[^A-Z/\\-]", "", position),
        "[/\\-]"
      ))
      tokens <- unique(vapply(
        tokens[nzchar(tokens)],
        normalize_depth_position,
        character(1)
      ))
      tokens <- tokens[tokens %in% valid_positions]

      if (grepl("G", position, fixed = TRUE) && !length(tokens)) {
        tokens <- c("PG", "SG")
      }
      if (grepl("F", position, fixed = TRUE) && !length(tokens)) {
        tokens <- c("SF", "PF")
      }
      eligible <- tokens
    }

    if (!length(eligible)) {
      eligible <- normalize_depth_position(primary_position)
    }
    eligible <- eligible[eligible %in% valid_positions]
    if (!length(eligible)) {
      eligible <- valid_positions
    }

    result[[as.character(player_id)]] <- unique(eligible)
  }

  result
}


#' Batch the read-only evidence used by frozen BIE roster enrichment
#' @noRd
depth_chart_batched_bie_enrich_roster <- function(
    roster,
    roster_season = NULL) {
  if (is.null(roster) || !is.data.frame(roster) || !nrow(roster)) {
    return(roster)
  }

  if (
    is.null(roster_season) ||
      !length(roster_season) ||
      is.na(roster_season[[1]]) ||
      !nzchar(trimws(as.character(roster_season[[1]])))
  ) {
    season_values <- if ("season" %in% names(roster)) {
      unique(as.character(roster$season))
    } else {
      character()
    }
    season_values <- season_values[
      !is.na(season_values) & nzchar(trimws(season_values))
    ]
    roster_season <- if (length(season_values)) {
      season_values[[1]]
    } else {
      "2026-27"
    }
  }
  roster_season <- as.character(roster_season[[1]])

  required_helpers <- c(
    "player_manager_batched_metadata",
    "player_manager_batched_performance_seasons",
    "player_manager_batched_evidence_rows",
    "player_manager_batched_latest_row"
  )

  if (!all(vapply(required_helpers, exists, logical(1), mode = "function"))) {
    stop(
      "Batched player-evidence helpers are required for Depth Chart enrichment.",
      call. = FALSE
    )
  }

  con <- connect_db(read_only = TRUE)
  on.exit(disconnect_db(con), add = TRUE)

  evidence_tables <- c(
    stats = "player_season_stats",
    advanced = "player_season_advanced",
    shooting = "player_season_shooting",
    playmaking = "player_season_playmaking",
    defense = "player_season_defense_rebounding",
    roles = "player_season_roles",
    impact = "player_season_impact"
  )
  metadata <- player_manager_batched_metadata(
    con,
    unname(evidence_tables)
  )
  player_ids <- suppressWarnings(as.integer(roster$player_id))
  performance_seasons <- player_manager_batched_performance_seasons(
    con = con,
    player_ids = player_ids,
    roster_season = roster_season,
    metadata = metadata
  )
  evidence <- stats::setNames(
    lapply(
      evidence_tables,
      function(table_name) {
        player_manager_batched_evidence_rows(
          con = con,
          table_name = table_name,
          player_ids = player_ids,
          performance_seasons = performance_seasons,
          metadata = metadata
        )
      }
    ),
    names(evidence_tables)
  )

  rows <- lapply(
    seq_len(nrow(roster)),
    function(index) {
      row <- roster[index, , drop = FALSE]
      player_id <- player_ids[[index]]
      performance_season <- performance_seasons[[index]]
      selected_rows <- stats::setNames(
        lapply(
          names(evidence_tables),
          function(evidence_name) {
            table_name <- evidence_tables[[evidence_name]]

            player_manager_batched_latest_row(
              evidence = evidence[[evidence_name]],
              fields = metadata$fields[[table_name]],
              player_id = player_id,
              performance_season = performance_season
            )
          }
        ),
        names(evidence_tables)
      )
      stats_row <- selected_rows[["stats"]]

      row$tbi_performance_available <- nrow(stats_row) > 0L
      row$performance_season_used <- if (nrow(stats_row)) {
        performance_season
      } else {
        NA_character_
      }

      for (evidence_name in names(evidence_tables)) {
        row <- depth_chart_merge_bie_evidence(
          base = row,
          extra = selected_rows[[evidence_name]],
          prefix = paste0(evidence_name, "_")
        )
      }

      row
    }
  )
  all_names <- unique(unlist(lapply(rows, names)))
  normalized <- lapply(
    rows,
    function(row) {
      missing <- setdiff(all_names, names(row))

      for (column in missing) {
        row[[column]] <- NA
      }

      row[, all_names, drop = FALSE]
    }
  )

  do.call(rbind, normalized)
}


#' Execute the frozen lineup search with position fits prepared once
#' @noRd
depth_chart_prepared_bie_starting_five <- function(
    players,
    compact_players,
    candidate_limit = 8L,
    player_weight = 0.65,
    position_weight = 0.25,
    balance_weight = 0.10,
    locks = NULL) {
  candidate_limit <- suppressWarnings(as.integer(candidate_limit))

  if (
    !length(candidate_limit) ||
      is.na(candidate_limit) ||
      candidate_limit < 2L
  ) {
    candidate_limit <- 8L
  }

  lineup_weights <- suppressWarnings(as.numeric(c(
    player_weight,
    position_weight,
    balance_weight
  )))

  if (
    length(lineup_weights) != 3L ||
      any(!is.finite(lineup_weights)) ||
      any(lineup_weights < 0) ||
      sum(lineup_weights) <= 0
  ) {
    lineup_weights <- c(0.65, 0.25, 0.10)
  }

  lineup_weights <- lineup_weights / sum(lineup_weights)
  player_weight <- lineup_weights[[1]]
  position_weight <- lineup_weights[[2]]
  balance_weight <- lineup_weights[[3]]
  candidate_total <- player_weight + position_weight
  candidate_player_weight <- player_weight / candidate_total
  candidate_position_weight <- position_weight / candidate_total
  positions <- c("PG", "SG", "SF", "PF", "C")
  quality <- suppressWarnings(
    as.numeric(compact_players$bie_selection_score)
  )

  resolved_locks <- locks

  if (
    is.null(resolved_locks) ||
      !is.data.frame(resolved_locks) ||
      !nrow(resolved_locks)
  ) {
    resolved_locks <- bie_default_starting_five_locks(compact_players)
  }

  if (
    is.null(resolved_locks) ||
      !is.data.frame(resolved_locks) ||
      !nrow(resolved_locks)
  ) {
    resolved_locks <- data.frame(
      player_id = integer(),
      player_name = character(),
      position = character(),
      stringsAsFactors = FALSE
    )
  }

  if (nrow(resolved_locks) && !"position" %in% names(resolved_locks)) {
    stop("locks must contain a position column.", call. = FALSE)
  }

  if (nrow(resolved_locks)) {
    resolved_locks$position <- vapply(
      resolved_locks$position,
      bie_normalize_position,
      character(1)
    )

    if (any(duplicated(resolved_locks$position))) {
      stop(
        "Only one lock may be assigned to each lineup position.",
        call. = FALSE
      )
    }

    resolved_locks$resolved_index <- NA_integer_

    for (index in seq_len(nrow(resolved_locks))) {
      player_index <- NA_integer_

      if ("player_id" %in% names(resolved_locks)) {
        lock_id <- suppressWarnings(
          as.integer(resolved_locks$player_id[[index]])
        )

        if (length(lock_id) && is.finite(lock_id)) {
          player_index <- match(
            lock_id,
            suppressWarnings(as.integer(compact_players$player_id))
          )
        }
      }

      if (
        is.na(player_index) &&
          "player_name" %in% names(resolved_locks)
      ) {
        lock_name <- as.character(
          resolved_locks$player_name[[index]]
        )
        hits <- which(
          tolower(trimws(compact_players$player_name)) ==
            tolower(trimws(lock_name))
        )

        if (length(hits) == 1L) {
          player_index <- hits[[1]]
        }
      }

      if (is.na(player_index)) {
        return(list(
          status = "LOCKED_PLAYER_NOT_FOUND",
          lineup = data.frame(),
          score = NA_real_
        ))
      }

      resolved_locks$resolved_index[[index]] <- player_index
    }

    if (any(duplicated(resolved_locks$resolved_index))) {
      stop(
        "The same player cannot be locked into multiple positions.",
        call. = FALSE
      )
    }
  }

  locked_indices <- if (nrow(resolved_locks)) {
    as.integer(resolved_locks$resolved_index)
  } else {
    integer()
  }
  position_fit <- vapply(
    positions,
    function(position) {
      vapply(
        seq_len(nrow(compact_players)),
        function(index) {
          bie_position_fit_score(
            compact_players[index, , drop = FALSE],
            position
          )
        },
        numeric(1)
      )
    },
    numeric(nrow(compact_players))
  )
  colnames(position_fit) <- positions
  candidates <- vector("list", length(positions))
  names(candidates) <- positions

  for (position in positions) {
    position_lock <- if (nrow(resolved_locks)) {
      resolved_locks[
        resolved_locks$position == position,
        ,
        drop = FALSE
      ]
    } else {
      data.frame()
    }

    if (nrow(position_lock) == 1L) {
      player_index <- as.integer(position_lock$resolved_index[[1]])

      if (!is.finite(position_fit[player_index, position])) {
        return(list(
          status = "LOCKED_PLAYER_POSITION_ILLEGAL",
          lineup = data.frame(),
          score = NA_real_
        ))
      }

      candidates[[position]] <- player_index
      next
    }

    fit <- position_fit[, position]
    eligible <- is.finite(fit)

    if (length(locked_indices)) {
      eligible[locked_indices] <- FALSE
    }

    performance_eligible <-
      eligible &
      compact_players$bie_score_source == "PERFORMANCE_DATA" &
      is.finite(quality)
    pool <- if (any(performance_eligible)) {
      performance_eligible
    } else {
      eligible & is.finite(quality)
    }

    if (!any(pool)) {
      candidates[[position]] <- integer()
      next
    }

    candidate_score <-
      candidate_player_weight * quality +
      candidate_position_weight * fit
    indices <- which(pool)
    indices <- indices[order(
      -candidate_score[indices],
      compact_players$player_name[indices],
      na.last = TRUE
    )]
    candidates[[position]] <- head(indices, candidate_limit)
  }

  missing_positions <- positions[
    !vapply(candidates, length, integer(1))
  ]

  if (length(missing_positions)) {
    return(list(
      status = "INCOMPLETE_POSITION_COVERAGE",
      lineup = data.frame(),
      score = NA_real_,
      missing_positions = missing_positions
    ))
  }

  best_score <- -Inf
  best_assignment <- NULL
  evaluated_lineups <- 0L

  search_assignment <- function(
      position_index,
      chosen_indices,
      assigned_positions) {
    if (position_index > length(positions)) {
      evaluated_lineups <<- evaluated_lineups + 1L
      lineup <- compact_players[chosen_indices, , drop = FALSE]
      lineup$bie_lineup_position <- assigned_positions
      lineup_quality <- suppressWarnings(
        as.numeric(lineup$bie_selection_score)
      )
      player_quality <- mean(lineup_quality, na.rm = TRUE)
      raw_bie_quality <- mean(
        suppressWarnings(as.numeric(lineup$bie_player_score)),
        na.rm = TRUE
      )
      fit_columns <- match(assigned_positions, positions)
      lineup_position_fit <- mean(
        position_fit[cbind(chosen_indices, fit_columns)],
        na.rm = TRUE
      )
      balance <- bie_lineup_balance(lineup)
      score <-
        player_weight * player_quality +
        position_weight * lineup_position_fit +
        balance_weight * balance$score
      score <- pmin(100, pmax(0, score))

      if (score > best_score) {
        best_score <<- score
        best_assignment <<- list(
          indices = chosen_indices,
          positions = assigned_positions,
          player_quality = player_quality,
          raw_bie_quality = raw_bie_quality,
          position_fit = lineup_position_fit,
          balance = balance
        )
      }

      return(invisible(NULL))
    }

    position <- positions[[position_index]]

    for (candidate_index in candidates[[position]]) {
      if (candidate_index %in% chosen_indices) {
        next
      }

      search_assignment(
        position_index = position_index + 1L,
        chosen_indices = c(chosen_indices, candidate_index),
        assigned_positions = c(assigned_positions, position)
      )
    }

    invisible(NULL)
  }

  search_assignment(
    position_index = 1L,
    chosen_indices = integer(),
    assigned_positions = character()
  )

  if (is.null(best_assignment)) {
    return(list(
      status = "NO_LEGAL_LINEUP",
      lineup = data.frame(),
      score = NA_real_,
      evaluated_lineups = evaluated_lineups
    ))
  }

  lineup <- players[best_assignment$indices, , drop = FALSE]
  lineup$bie_lineup_position <- best_assignment$positions
  lineup <- lineup[
    match(positions, lineup$bie_lineup_position),
    ,
    drop = FALSE
  ]
  rownames(lineup) <- NULL
  source_counts <- table(lineup$bie_score_source)
  performance_players <- if (
    "PERFORMANCE_DATA" %in% names(source_counts)
  ) {
    as.integer(source_counts[["PERFORMANCE_DATA"]])
  } else {
    0L
  }
  confidence <- if (performance_players == 5L) {
    "HIGH"
  } else if (performance_players >= 3L) {
    "MODERATE"
  } else {
    "FOUNDATION"
  }

  list(
    status = "OK",
    lineup = lineup,
    score = best_score,
    player_quality = best_assignment$player_quality,
    raw_bie_quality = best_assignment$raw_bie_quality,
    position_fit = best_assignment$position_fit,
    balance = best_assignment$balance,
    evaluated_lineups = evaluated_lineups,
    performance_data_players = performance_players,
    confidence = confidence,
    explanation = paste0(
      "BIE vNEXT Starting Five: ",
      round(100 * player_weight),
      "% player quality / ",
      round(100 * position_weight),
      "% position fit / ",
      round(100 * balance_weight),
      "% lineup balance. Performance-backed legal candidates take priority over fallback."
    ),
    model_label = "BIE Starting Five vNEXT LOCKED"
  )
}


#' Run the frozen optimizer on only columns it reads, then rehydrate
#' @noRd
depth_chart_optimize_bie_starting_five <- function(
    players,
    candidate_limit = 8L,
    player_weight = 0.65,
    position_weight = 0.25,
    balance_weight = 0.10,
    locks = NULL) {
  if (is.null(players) || !is.data.frame(players) || !nrow(players)) {
    return(
      optimize_bie_starting_five(
        players = players,
        candidate_limit = candidate_limit,
        player_weight = player_weight,
        position_weight = position_weight,
        balance_weight = balance_weight,
        locks = locks
      )
    )
  }

  identity_and_position_columns <- c(
    "player_id",
    "player_name",
    "team_id",
    "team_name",
    "current_team_name",
    "team",
    "season",
    "roster_season",
    "position",
    "primary_position",
    "eligible_positions",
    "official_positions",
    "positions",
    "pos"
  )
  required_bie_columns <- c(
    "bie_player_score",
    "bie_score_source",
    "bie_metric_components",
    "bie_selection_score",
    "bie_model_version",
    "bie_efficiency_score",
    "bie_playmaking_score",
    "bie_defense_score",
    "bie_rebounding_score"
  )

  if (
    !all(required_bie_columns %in% names(players)) ||
      !all(as.character(players$bie_model_version) == "BIE vNEXT LOCKED")
  ) {
    return(
      optimize_bie_starting_five(
        players = players,
        candidate_limit = candidate_limit,
        player_weight = player_weight,
        position_weight = position_weight,
        balance_weight = balance_weight,
        locks = locks
      )
    )
  }

  optimizer_columns <- unique(c(
    intersect(identity_and_position_columns, names(players)),
    required_bie_columns
  ))
  compact_players <- players[, optimizer_columns, drop = FALSE]
  depth_chart_prepared_bie_starting_five(
    players = players,
    compact_players = compact_players,
    candidate_limit = candidate_limit,
    player_weight = player_weight,
    position_weight = position_weight,
    balance_weight = balance_weight,
    locks = locks
  )
}


#' Build Phase 11 lineup variants from one immutable candidate pool
#' @noRd
depth_chart_build_phase11_lineup_result <- function(
    rotation_result,
    pool_size = NULL) {
  required_helpers <- c(
    "resolve_lineup_optimization_rules",
    "get_lineup_candidate_pool",
    "enumerate_lineup_candidates",
    "score_lineup",
    "summarize_lineup"
  )
  if (!all(vapply(required_helpers, exists, logical(1), mode = "function"))) {
    stop(
      "Phase 9 Lineup Optimization Engine is not loaded.",
      call. = FALSE
    )
  }

  if (
    is.null(rotation_result) ||
      !identical(rotation_result$status, "OK")
  ) {
    return(NULL)
  }

  if (is.null(pool_size)) {
    pool_size <- rotation_result$rotation_size
  }

  rules <- resolve_lineup_optimization_rules()
  pool <- get_lineup_candidate_pool(
    rotation_result$phase8_result,
    pool_size = pool_size
  )
  candidates <- enumerate_lineup_candidates(pool)

  optimize_type <- function(lineup_type) {
    scores <- vapply(
      candidates,
      function(candidate) {
        score_lineup(candidate, lineup_type = lineup_type)$score
      },
      numeric(1)
    )
    best_index <- which.max(scores)
    best_lineup <- candidates[[best_index]]
    best_score <- score_lineup(
      best_lineup,
      lineup_type = lineup_type
    )
    summary <- summarize_lineup(best_lineup, best_score)
    summary$candidate_count <- length(candidates)
    summary
  }

  list(
    balanced = optimize_type("balanced"),
    offense = optimize_type("offense"),
    defense = optimize_type("defense"),
    closing = optimize_type("closing"),
    model_label = rules$model_label
  )
}


#' Resolve the canonical trade preview for the active Depth Chart context.
#'
#' This is intentionally pure so scenario publication/reset behavior can be
#' regression-tested without mounting the full module. Only the protected V1
#' two-team preview is supported here; V2 organizational impact remains
#' development-visible through its own governed presentation contracts.
#'
#' @noRd
depth_chart_active_trade_scenario <- function(
    scenario,
    selected_team,
    selected_season) {
  if (
    !is.list(scenario) ||
    !isTRUE(scenario$active) ||
    !tbi_scenario_is_shared_supported(scenario) ||
    !identical(as.character(scenario$scenario_type), "trade") ||
    !identical(as.character(scenario$season), as.character(selected_season))
  ) {
    return(NULL)
  }

  selected <- as.character(selected_team)
  primary_team <- as.character(scenario$team)
  partner_team_name <- as.character(scenario$partner_team)

  if (!selected %in% c(primary_team, partner_team_name)) {
    return(NULL)
  }

  # Normalize the same pending transaction to whichever organization is open.
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
    scenario$salary_delta <- scenario$incoming_salary - scenario$outgoing_salary
  }

  scenario
}


#' Resolve one canonical team context for the V2 shadow evaluator.
#'
#' @noRd
depth_chart_v2_shadow_team <- function(roster, selected_team) {
  fallback <- as.character(selected_team)
  if (!is.data.frame(roster) || !nrow(roster) || !"team_id" %in% names(roster)) {
    return(fallback)
  }
  values <- unique(as.character(roster$team_id))
  values <- values[!is.na(values) & nzchar(trimws(values))]
  if (length(values) == 1L) values[[1L]] else fallback
}


# ============================================================
# SERVER
# ============================================================

#' Depth Chart server
#'
#' @param id Internal module ID.
#' @param selected_team Reactive selected team.
#' @param selected_season Reactive selected season.
#' @noRd
mod_depth_chart_server <- function(
    id,
    selected_team,
    selected_season,
    transaction_state = NULL,
    rotation_route = tbi_rotation_route()) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      # ------------------------------------------------------
      # Helpers
      # ------------------------------------------------------
      
      safe_num <- function(x, default = NA_real_) {
        value <- suppressWarnings(
          as.numeric(x)
        )
        
        if (
          !length(value) ||
          is.na(value[[1]]) ||
          !is.finite(value[[1]])
        ) {
          default
        } else {
          value[[1]]
        }
      }
      
      text_or <- function(x, fallback = "—") {
        if (
          is.null(x) ||
          !length(x) ||
          is.na(x[[1]]) ||
          !nzchar(
            trimws(
              as.character(
                x[[1]]
              )
            )
          )
        ) {
          fallback
        } else {
          as.character(
            x[[1]]
          )
        }
      }
      
      money <- function(x) {
        value <- safe_num(
          x,
          NA_real_
        )
        
        if (is.na(value)) {
          return("—")
        }
        
        if (value >= 1e9) {
          return(
            sprintf(
              "$%.2fB",
              value / 1e9
            )
          )
        }
        
        if (value >= 1e6) {
          return(
            sprintf(
              "$%.1fM",
              value / 1e6
            )
          )
        }
        
        if (value >= 1e3) {
          return(
            sprintf(
              "$%.0fK",
              value / 1e3
            )
          )
        }
        
        paste0(
          "$",
          format(
            round(value),
            big.mark = ",",
            scientific = FALSE
          )
        )
      }
      
      format_height <- function(x) {
        value <- safe_num(
          x,
          NA_real_
        )
        
        if (is.na(value)) {
          return("—")
        }
        
        feet <- floor(
          value / 12
        )
        
        inches <- round(
          value -
            feet *
            12
        )
        
        paste0(
          feet,
          "'",
          inches,
          "\""
        )
      }
      
      initials <- function(name) {
        name <- text_or(
          name,
          "Player"
        )
        
        parts <- strsplit(
          trimws(name),
          "\\s+"
        )[[1]]
        
        paste0(
          substr(
            utils::head(
              parts,
              1
            ),
            1,
            1
          ),
          substr(
            utils::tail(
              parts,
              1
            ),
            1,
            1
          )
        )
      }
      
      official_positions_lookup <- NULL
      official_positions_session_cache <- new.env(parent = emptyenv())

      official_positions <- function(row) {
        player_id <- suppressWarnings(as.integer(row$player_id[[1]]))
        batched <- tryCatch(
          {
            if (is.function(official_positions_lookup)) {
              official_positions_lookup()
            } else {
              NULL
            }
          },
          error = function(e) NULL
        )
        positions <- if (
          !is.null(batched) &&
            !is.na(player_id) &&
            !is.null(batched[[as.character(player_id)]])
        ) {
          batched[[as.character(player_id)]]
        } else {
          tryCatch(
            get_player_eligible_positions(
              player_id = row$player_id[[1]],
              primary_position = row$primary_position[[1]]
            ),
            error = function(e) character()
          )
        }
        
        positions <- unique(
          positions[
            positions %in%
              c(
                "PG",
                "SG",
                "SF",
                "PF",
                "C"
              )
          ]
        )
        
        if (!length(positions)) {
          fallback <- tryCatch(
            normalize_depth_position(
              row$primary_position[[1]]
            ),
            error = function(e) "OTHER"
          )
          
          if (
            fallback %in%
            c(
              "PG",
              "SG",
              "SF",
              "PF",
              "C"
            )
          ) {
            positions <- fallback
          }
        }
        
        positions
      }
      
      # ------------------------------------------------------
      # Data
      # ------------------------------------------------------
      
      refresh_key <- shiny::reactiveVal(0L)
      save_message_value <- shiny::reactiveVal(NULL)
      
      base_depth_data <- shiny::reactive({
        refresh_key()
        
        shiny::req(
          selected_team(),
          selected_season()
        )
        
        result <- tryCatch(
          get_depth_chart_records(
            team_value = selected_team(),
            season = selected_season()
          ),
          error = function(e) data.frame()
        )
        
        if (is.null(result)) data.frame() else result
      })
      
      
      current_transaction_scenario <- function() {
        if (is.null(transaction_state)) {
          return(list(active = FALSE))
        }

        if (!is.function(transaction_state$snapshot)) {
          return(list(
            active = TRUE,
            state_unavailable = TRUE
          ))
        }

        scenario <- tryCatch(
          transaction_state$snapshot(),
          error = function(e) list(
            active = TRUE,
            state_unavailable = TRUE
          )
        )

        if (is.list(scenario)) {
          scenario
        } else {
          list(
            active = TRUE,
            state_unavailable = TRUE
          )
        }
      }

      active_trade_scenario <- shiny::reactive({
        depth_chart_active_trade_scenario(
          current_transaction_scenario(),
          selected_team(),
          selected_season()
        )
      })

      official_write_decision <- function(operation) {
        tbi_authorize_official_write(
          current_transaction_scenario(),
          operation = operation
        )
      }

      block_official_write <- function(operation) {
        decision <- official_write_decision(operation)

        if (isTRUE(decision$allowed)) {
          return(FALSE)
        }

        save_message_value(decision)
        TRUE
      }
      
      
      trade_incoming_ids <- shiny::reactive({
        
        scenario <- active_trade_scenario()
        
        if (
          is.null(scenario) ||
          !is.data.frame(scenario$incoming_players) ||
          !nrow(scenario$incoming_players) ||
          !"player_id" %in% names(scenario$incoming_players)
        ) {
          return(integer())
        }
        
        ids <- suppressWarnings(
          as.integer(
            scenario$incoming_players$player_id
          )
        )
        
        ids[!is.na(ids)]
      })
      
      
      is_trade_incoming <- function(player_id) {
        
        as.integer(player_id) %in%
          trade_incoming_ids()
      }
      
      
      player_eligible_for_position <- function(
    row,
    position) {
        
        if (
          is.null(row) ||
          !nrow(row)
        ) {
          return(FALSE)
        }
        
        eligible <- tryCatch(
          official_positions(row),
          error = function(e) character()
        )
        
        position %in% eligible
      }
      
      
      build_smart_lineup <- function(d) {
        
        positions <- c("PG", "SG", "SF", "PF", "C")
        
        lineup <- stats::setNames(
          rep(NA_integer_, length(positions)),
          positions
        )
        
        if (
          is.null(d) ||
          !is.data.frame(d) ||
          !nrow(d)
        ) {
          return(lineup)
        }
        
        if (!"trade_source_starter" %in% names(d)) {
          d$trade_source_starter <- 0
        }
        
        used <- integer()
        
        # Pass 1: keep every surviving starter in place.
        for (pos in positions) {
          
          rows <- d[
            d$position == pos &
              suppressWarnings(
                as.numeric(d$is_starter)
              ) == 1,
            ,
            drop = FALSE
          ]
          
          if (!nrow(rows)) {
            next
          }
          
          depth_rank <- suppressWarnings(
            as.numeric(rows$depth_order)
          )
          depth_rank[is.na(depth_rank)] <- 999
          
          salary_rank <- suppressWarnings(
            as.numeric(rows$salary)
          )
          salary_rank[is.na(salary_rank)] <- 0
          
          rows <- rows[
            order(
              depth_rank,
              -salary_rank,
              rows$player_name,
              na.last = TRUE
            ),
            ,
            drop = FALSE
          ]
          
          rows <- rows[
            !rows$player_id %in% used,
            ,
            drop = FALSE
          ]
          
          if (!nrow(rows)) {
            next
          }
          
          player_id <- as.integer(
            rows$player_id[[1]]
          )
          
          lineup[[pos]] <- player_id
          used <- c(used, player_id)
        }
        
        # Pass 2: refill every vacancy from the proposed roster.
        # Before the BIE is added, ranking uses:
        # natural position > incoming source starter > depth order > salary.
        for (pos in positions) {
          
          if (!is.na(lineup[[pos]])) {
            next
          }
          
          candidate_rows <- list()
          
          for (i in seq_len(nrow(d))) {
            
            row <- d[i, , drop = FALSE]
            
            player_id <- suppressWarnings(
              as.integer(row$player_id[[1]])
            )
            
            if (
              is.na(player_id) ||
              player_id %in% used
            ) {
              next
            }
            
            if (
              !player_eligible_for_position(
                row,
                pos
              )
            ) {
              next
            }
            
            exact_position <- identical(
              as.character(row$position[[1]]),
              pos
            )
            
            source_starter <- suppressWarnings(
              as.numeric(
                row$trade_source_starter[[1]]
              )
            )
            if (is.na(source_starter)) {
              source_starter <- 0
            }
            
            depth_rank <- suppressWarnings(
              as.numeric(row$depth_order[[1]])
            )
            if (is.na(depth_rank)) {
              depth_rank <- 999
            }
            
            salary_rank <- suppressWarnings(
              as.numeric(row$salary[[1]])
            )
            if (is.na(salary_rank)) {
              salary_rank <- 0
            }
            
            candidate_rows[[length(candidate_rows) + 1L]] <-
              data.frame(
                player_id = player_id,
                exact_position =
                  if (exact_position) 1 else 0,
                source_starter = source_starter,
                depth_order = depth_rank,
                salary = salary_rank,
                player_name =
                  as.character(row$player_name[[1]]),
                stringsAsFactors = FALSE
              )
          }
          
          if (!length(candidate_rows)) {
            next
          }
          
          candidates <- do.call(
            rbind,
            candidate_rows
          )
          
          candidates <- candidates[
            order(
              -candidates$exact_position,
              -candidates$source_starter,
              candidates$depth_order,
              -candidates$salary,
              candidates$player_name,
              na.last = TRUE
            ),
            ,
            drop = FALSE
          ]
          
          player_id <- as.integer(
            candidates$player_id[[1]]
          )
          
          lineup[[pos]] <- player_id
          used <- c(used, player_id)
        }
        
        lineup
      }
      
      
      normalize_preview_depth <- function(d) {
        
        if (
          is.null(d) ||
          !is.data.frame(d) ||
          !nrow(d)
        ) {
          return(d)
        }
        
        positions <- c("PG", "SG", "SF", "PF", "C")
        
        for (pos in positions) {
          
          idx <- which(d$position == pos)
          
          if (!length(idx)) {
            next
          }
          
          current_starter <- suppressWarnings(
            as.numeric(d$is_starter[idx])
          )
          current_starter[is.na(current_starter)] <- 0
          
          source_starter <- if (
            "trade_source_starter" %in% names(d)
          ) {
            suppressWarnings(
              as.numeric(
                d$trade_source_starter[idx]
              )
            )
          } else {
            rep(0, length(idx))
          }
          source_starter[is.na(source_starter)] <- 0
          
          depth_rank <- suppressWarnings(
            as.numeric(d$depth_order[idx])
          )
          depth_rank[is.na(depth_rank)] <- 999
          
          salary_rank <- suppressWarnings(
            as.numeric(d$salary[idx])
          )
          salary_rank[is.na(salary_rank)] <- 0
          
          ord <- order(
            -current_starter,
            -source_starter,
            depth_rank,
            -salary_rank,
            d$player_name[idx],
            na.last = TRUE
          )
          
          ordered_idx <- idx[ord]
          
          d$depth_order[ordered_idx] <-
            seq_along(ordered_idx)
        }
        
        d
      }
      
      
      depth_data <- shiny::reactive({
        
        current <- base_depth_data()
        scenario <- active_trade_scenario()
        
        if (is.null(scenario) || !nrow(current)) {
          return(current)
        }
        
        outgoing_ids <- integer()
        
        if (
          is.data.frame(scenario$outgoing_players) &&
          nrow(scenario$outgoing_players) &&
          "player_id" %in% names(scenario$outgoing_players)
        ) {
          outgoing_ids <- suppressWarnings(
            as.integer(scenario$outgoing_players$player_id)
          )
          outgoing_ids <- outgoing_ids[!is.na(outgoing_ids)]
        }
        
        preview <- current[
          !current$player_id %in% outgoing_ids,
          ,
          drop = FALSE
        ]
        
        preview$trade_source_starter <- 0
        preview$trade_source_depth <- NA_real_
        
        incoming_ids <- integer()
        
        if (
          is.data.frame(scenario$incoming_players) &&
          nrow(scenario$incoming_players) &&
          "player_id" %in% names(scenario$incoming_players)
        ) {
          incoming_ids <- suppressWarnings(
            as.integer(scenario$incoming_players$player_id)
          )
          incoming_ids <- incoming_ids[!is.na(incoming_ids)]
        }
        
        if (length(incoming_ids)) {
          
          partner_depth <- tryCatch(
            get_depth_chart_records(
              team_value = scenario$partner_team,
              season = selected_season()
            ),
            error = function(e) data.frame()
          )
          
          incoming_rows <- if (
            is.data.frame(partner_depth) &&
            nrow(partner_depth)
          ) {
            partner_depth[
              partner_depth$player_id %in% incoming_ids,
              ,
              drop = FALSE
            ]
          } else {
            data.frame()
          }
          
          if (nrow(incoming_rows)) {
            current_team_ids <- unique(as.character(current$team_id))
            current_team_ids <- current_team_ids[!is.na(current_team_ids) & nzchar(current_team_ids)]
            if (length(current_team_ids) == 1L && "team_id" %in% names(incoming_rows)) {
              # Scenario previews model the incoming player as a member of the
              # selected organization. Retaining the source team's identifier
              # would create a mixed-team roster and correctly block V2.
              incoming_rows$team_id <- current_team_ids[[1L]]
            }
            
            incoming_rows$trade_source_starter <-
              suppressWarnings(
                as.numeric(
                  incoming_rows$is_starter
                )
              )
            
            incoming_rows$trade_source_depth <-
              suppressWarnings(
                as.numeric(
                  incoming_rows$depth_order
                )
              )
            
            incoming_rows$is_starter <- 0
            
            for (i in seq_len(nrow(incoming_rows))) {
              
              pos <- as.character(
                incoming_rows$position[[i]]
              )
              
              existing <- suppressWarnings(
                as.numeric(
                  preview$depth_order[
                    preview$position == pos
                  ]
                )
              )
              
              existing <- existing[!is.na(existing)]
              
              incoming_rows$depth_order[[i]] <-
                if (length(existing)) {
                  max(existing) + 1
                } else {
                  1
                }
            }
            
            common <- intersect(
              names(preview),
              names(incoming_rows)
            )
            
            incoming_rows <- incoming_rows[, common, drop = FALSE]
            
            for (
              nm in setdiff(
                names(preview),
                names(incoming_rows)
              )
            ) {
              incoming_rows[[nm]] <- NA
            }
            
            incoming_rows <- incoming_rows[
              ,
              names(preview),
              drop = FALSE
            ]
            
            preview <- rbind(
              preview,
              incoming_rows
            )
          }
        }
        
        preview <- normalize_preview_depth(
          preview
        )
        
        rownames(preview) <- NULL
        preview
      })


      official_positions_lookup <- shiny::reactive({
        roster <- depth_data()

        if (is.null(roster) || !is.data.frame(roster) || !nrow(roster)) {
          return(list())
        }

        ids <- suppressWarnings(as.integer(roster$player_id))
        primary <- as.character(roster$primary_position)
        order_index <- order(ids, primary, na.last = TRUE)
        cache_key <- paste(
          as.character(selected_team()),
          as.character(selected_season()),
          paste(ids[order_index], primary[order_index], sep = ":", collapse = "|"),
          sep = "||"
        )

        if (exists(cache_key, envir = official_positions_session_cache, inherits = FALSE)) {
          return(get(
            cache_key,
            envir = official_positions_session_cache,
            inherits = FALSE
          ))
        }

        positions <- tryCatch(
          depth_chart_batched_official_positions(roster),
          error = function(e) NULL
        )

        if (!is.null(positions)) {
          assign(
            cache_key,
            positions,
            envir = official_positions_session_cache
          )
        }

        positions
      })
      
      
      output$shared_trade_scenario_banner <- shiny::renderUI({
        
        scenario <- active_trade_scenario()
        
        if (is.null(scenario)) {
          return(NULL)
        }
        
        outgoing_names <- if (
          is.data.frame(scenario$outgoing_players) &&
          nrow(scenario$outgoing_players) &&
          "player_name" %in% names(scenario$outgoing_players)
        ) {
          paste(
            scenario$outgoing_players$player_name,
            collapse = ", "
          )
        } else {
          "Outgoing players"
        }
        
        incoming_names <- if (
          is.data.frame(scenario$incoming_players) &&
          nrow(scenario$incoming_players) &&
          "player_name" %in% names(scenario$incoming_players)
        ) {
          paste(
            scenario$incoming_players$player_name,
            collapse = ", "
          )
        } else {
          "Incoming players"
        }
        
        shiny::div(
          class = "depth-v23-shared-trade-banner",
          
          shiny::div(
            class = "depth-v23-shared-trade-copy",
            shiny::span(
              class = "depth-v23-shared-trade-badge",
              bsicons::bs_icon("arrow-left-right"),
              "TRADE PREVIEW"
            ),
            shiny::span(
              paste0(
                outgoing_names,
                " OUT • ",
                incoming_names,
                " IN from ",
                scenario$partner_team,
                ". Depth Chart and Starting Five reflect the proposed roster."
              )
            )
          ),
          
          shiny::span(
            class = "depth-v23-shared-trade-note",
            "PREVIEW ONLY"
          )
        )
      })
      
      selected_player_id <- shiny::reactiveVal(
        NULL
      )
      
      # Receive click from board cards.
      shiny::observeEvent(
        input$selected_player,
        {
          id <- suppressWarnings(
            as.integer(
              input$selected_player
            )
          )
          
          if (!is.na(id)) {
            selected_player_id(id)
          }
        },
        ignoreInit = TRUE
      )

      shiny::observeEvent(
        input$clear_player_detail,
        selected_player_id(NULL),
        ignoreInit = TRUE
      )
      
      # Select first player on team/season change.
      shiny::observeEvent(
        list(
          selected_team(),
          selected_season(),
          active_trade_scenario()
        ),
        {
          d <- depth_data()
          
          if (!nrow(d)) {
            selected_player_id(NULL)
            return()
          }
          
          current <-
            selected_player_id()
          
          if (
            is.null(current) ||
            !current %in%
            d$player_id
          ) {
            starter_rows <- d[
              d$is_starter == 1,
              ,
              drop = FALSE
            ]
            
            selected_player_id(
              if (
                nrow(
                  starter_rows
                )
              ) {
                starter_rows$player_id[[1]]
              } else {
                d$player_id[[1]]
              }
            )
          }
        },
        ignoreInit = FALSE
      )
      
      selected_player <- shiny::reactive({
        d <- depth_data()
        id <- selected_player_id()
        
        if (
          !nrow(d) ||
          is.null(id)
        ) {
          return(NULL)
        }
        
        row <- d[
          d$player_id == id,
          ,
          drop = FALSE
        ]
        
        if (!nrow(row)) {
          return(NULL)
        }
        
        row[
          1,
          ,
          drop = FALSE
        ]
      })
      
      
      # ------------------------------------------------------
      # Working lineup scenario
      # ------------------------------------------------------
      
      baseline_lineup <- shiny::reactive({
        
        build_smart_lineup(
          depth_data()
        )
      })
      
      scenario_lineup <- shiny::reactiveVal(NULL)
      
      shiny::observeEvent(
        list(
          selected_team(),
          selected_season(),
          refresh_key(),
          active_trade_scenario()
        ),
        {
          scenario_lineup(
            baseline_lineup()
          )
        },
        ignoreInit = FALSE
      )
      
      active_lineup <- shiny::reactive({
        lineup <- scenario_lineup()
        
        if (is.null(lineup)) {
          lineup <- baseline_lineup()
        }
        
        lineup
      })
      
      scenario_changed <- shiny::reactive({
        baseline <- baseline_lineup()
        scenario <- active_lineup()
        
        !identical(
          unname(baseline),
          unname(scenario)
        )
      })
      
      # ------------------------------------------------------
      # Basketball Intelligence Engine — Starting Five
      # ------------------------------------------------------
      
      lineup_view_mode <- shiny::reactiveVal("working")
      
      # ------------------------------------------------------
      # BIE evaluated-roster cache
      # ------------------------------------------------------
      # Expensive player evaluation runs only when the actual roster
      # signature changes. Lineup drag/drop does not force a full
      # player re-evaluation.
      # ------------------------------------------------------
      
      bie_roster_cache <- shiny::reactiveVal(
        list(
          key = NULL,
          value = NULL
        )
      )
      
      
      bie_roster <- shiny::reactive({
        
        d <- depth_chart_batched_bie_enrich_roster(depth_data())
        
        if (
          !nrow(d) ||
          !exists(
            "evaluate_bie_players",
            mode = "function"
          )
        ) {
          return(NULL)
        }
        
        key <- if (
          exists(
            "phase13_roster_signature",
            mode = "function"
          )
        ) {
          phase13_roster_signature(d)
        } else if (
          exists(
            "bie_roster_signature",
            mode = "function"
          )
        ) {
          bie_roster_signature(d)
        } else {
          paste(
            nrow(d),
            paste(
              sort(
                suppressWarnings(
                  as.integer(
                    d$player_id
                  )
                )
              ),
              collapse = ","
            ),
            sep = "|"
          )
        }
        
        cached <- shiny::isolate(bie_roster_cache())
        
        if (
          !is.null(cached$key) &&
          identical(
            cached$key,
            key
          ) &&
          !is.null(cached$value)
        ) {
          return(
            cached$value
          )
        }
        
        evaluated <- tryCatch(
          evaluate_bie_players(d),
          error = function(e) NULL
        )
        
        bie_roster_cache(
          list(
            key = key,
            value = evaluated
          )
        )
        
        evaluated
      })

      # V2 Phase 1C remains comparison-only. The observer is constructed only
      # for the explicit server-side shadow route, so V1 never executes V2.
      v2_shadow_diagnostic <- shiny::reactiveVal(list(
        execution_status = "DISABLED",
        route = rotation_route
      ))

      if (identical(rotation_route$model, "v2_shadow")) {
        shiny::observeEvent(
          list(
            selected_team(),
            selected_season(),
            depth_data(),
            active_lineup(),
            active_trade_scenario(),
            bie_roster()
          ),
          {
            roster <- bie_roster()
            if (is.null(roster) || !nrow(roster)) roster <- depth_data()
            shadow_team <- depth_chart_v2_shadow_team(roster, selected_team())
            v1_reference <- list(
              baseline_lineup = baseline_lineup(),
              active_lineup = active_lineup(),
              roster_signature = v2_input_signature(depth_data())
            )
            v2_shadow_diagnostic(run_v2_rotation_shadow(
              rotation_model = "v2_shadow",
              team = shadow_team,
              season = selected_season(),
              roster = roster,
              approved_lineup = active_lineup(),
              scenario = active_trade_scenario(),
              v1_reference = v1_reference
            ))
          },
          ignoreInit = FALSE
        )
      }

      output$v2_development_intelligence <- shiny::renderUI({
        v2_ui_depth_intelligence(
          v2_shadow_diagnostic(),
          lineup_working_ui = shiny::uiOutput(
            ns("compact_working_lineup_card"),
            class = "tbi-p3-working-lineup-slot"
          )
        )
      })
      
      bie_starting_five_cache <- shiny::reactiveVal(
        list(
          key = NULL,
          value = NULL
        )
      )
      
      
      bie_recommended_result <- shiny::reactive({
        
        d <- bie_roster()
        
        if (
          is.null(d) ||
          !nrow(d) ||
          !exists(
            "optimize_bie_starting_five",
            mode = "function"
          )
        ) {
          return(NULL)
        }
        
        key <- if (
          exists(
            "bie_roster_signature",
            mode = "function"
          )
        ) {
          bie_roster_signature(d)
        } else {
          paste(
            nrow(d),
            collapse = "|"
          )
        }
        
        cached <- shiny::isolate(bie_starting_five_cache())
        
        if (
          !is.null(cached$key) &&
          identical(
            cached$key,
            key
          ) &&
          !is.null(cached$value)
        ) {
          return(
            cached$value
          )
        }
        
        result <- tryCatch(
          depth_chart_optimize_bie_starting_five(d),
          error = function(e) {
            list(
              status = "ERROR",
              lineup = data.frame(),
              score = NA_real_,
              confidence = "UNAVAILABLE",
              explanation =
                conditionMessage(e)
            )
          }
        )
        
        bie_starting_five_cache(
          list(
            key = key,
            value = result
          )
        )
        
        result
      })
      
      bie_recommended_lineup <- shiny::reactive({
        result <- bie_recommended_result()
        positions <- c("PG","SG","SF","PF","C")
        output <- stats::setNames(
          rep(NA_integer_, length(positions)),
          positions
        )
        
        if (
          is.null(result) ||
          !identical(result$status, "OK") ||
          !is.data.frame(result$lineup) ||
          !nrow(result$lineup) ||
          !"player_id" %in% names(result$lineup) ||
          !"bie_lineup_position" %in% names(result$lineup)
        ) {
          return(output)
        }
        
        for (i in seq_len(nrow(result$lineup))) {
          pos <- as.character(
            result$lineup$bie_lineup_position[[i]]
          )
          player_id <- suppressWarnings(
            as.integer(result$lineup$player_id[[i]])
          )
          
          if (pos %in% positions && !is.na(player_id)) {
            output[[pos]] <- player_id
          }
        }
        
        output
      })
      
      score_bie_lineup <- function(lineup) {
        d <- bie_roster()
        positions <- c("PG","SG","SF","PF","C")
        
        if (
          is.null(d) ||
          !nrow(d) ||
          is.null(lineup) ||
          any(is.na(lineup[positions]))
        ) {
          return(NULL)
        }
        
        indices <- match(
          as.integer(lineup[positions]),
          as.integer(d$player_id)
        )
        
        if (any(is.na(indices))) {
          return(NULL)
        }
        
        lineup_rows <- d[indices, , drop = FALSE]
        lineup_rows$bie_lineup_position <- positions
        
        player_quality <- mean(
          as.numeric(lineup_rows$bie_player_score),
          na.rm = TRUE
        )
        
        position_fit <- mean(
          vapply(
            seq_len(nrow(lineup_rows)),
            function(i) {
              if (
                exists("bie_position_fit_score", mode = "function")
              ) {
                bie_position_fit_score(
                  lineup_rows[i, , drop = FALSE],
                  positions[[i]]
                )
              } else {
                100
              }
            },
            numeric(1)
          ),
          na.rm = TRUE
        )
        
        balance <- if (
          exists("bie_lineup_balance", mode = "function")
        ) {
          bie_lineup_balance(lineup_rows)
        } else {
          list(score = 50)
        }
        
        score <-
          0.72 * player_quality +
          0.18 * position_fit +
          0.10 * safe_num(balance$score, 50)
        
        if (
          exists("basketball_intel_clamp", mode = "function")
        ) {
          score <- basketball_intel_clamp(score)
        }
        
        performance_players <- if (
          "bie_score_source" %in% names(lineup_rows)
        ) {
          sum(
            lineup_rows$bie_score_source == "PERFORMANCE_DATA",
            na.rm = TRUE
          )
        } else {
          0L
        }
        
        confidence <- if (performance_players == 5L) {
          "HIGH"
        } else if (performance_players >= 3L) {
          "MODERATE"
        } else {
          "FOUNDATION"
        }
        
        list(
          score = score,
          player_quality = player_quality,
          position_fit = position_fit,
          balance = balance,
          confidence = confidence,
          performance_data_players = performance_players
        )
      }
      
      bie_working_score <- shiny::reactive({
        score_bie_lineup(active_lineup())
      })
      
      bie_working_result <- shiny::reactive({
        
        current <- bie_working_score()
        
        if (is.null(current)) {
          return(NULL)
        }
        
        list(
          status = "OK",
          score = current$score,
          player_quality =
            current$player_quality,
          position_fit =
            current$position_fit,
          balance =
            current$balance,
          confidence =
            current$confidence,
          performance_data_players =
            current$performance_data_players
        )
      })
      
      
      bie_explanation <- shiny::reactive({
        
        recommended <-
          bie_recommended_result()
        
        if (
          is.null(recommended) ||
          !identical(
            recommended$status,
            "OK"
          ) ||
          !exists(
            "explain_bie_lineup",
            mode = "function"
          )
        ) {
          return(NULL)
        }
        
        tryCatch(
          explain_bie_lineup(
            recommended
          ),
          error = function(e) NULL
        )
      })
      
      
      bie_change_explanation <- shiny::reactive({
        
        current <- bie_working_result()
        recommended <-
          bie_recommended_result()
        
        if (
          is.null(current) ||
          is.null(recommended) ||
          !identical(
            recommended$status,
            "OK"
          ) ||
          !exists(
            "explain_bie_lineup_change",
            mode = "function"
          )
        ) {
          return(NULL)
        }
        
        tryCatch(
          explain_bie_lineup_change(
            current_result = current,
            recommended_result =
              recommended
          ),
          error = function(e) NULL
        )
      })
      
      
      bie_lineup_comparison <- shiny::reactive({
        current <- bie_working_score()
        recommended <- bie_recommended_result()
        
        if (
          is.null(current) ||
          is.null(recommended) ||
          !identical(recommended$status, "OK")
        ) {
          return(NULL)
        }
        
        current_score <- safe_num(current$score, NA_real_)
        recommended_score <- safe_num(recommended$score, NA_real_)
        
        if (is.na(current_score) || is.na(recommended_score)) {
          return(NULL)
        }
        
        list(
          current_score = current_score,
          recommended_score = recommended_score,
          delta = recommended_score - current_score,
          confidence = recommended$confidence %||% "FOUNDATION"
        )
      })

      output$compact_working_lineup_card <- shiny::renderUI({
        positions <- c("PG", "SG", "SF", "PF", "C")
        lineup <- displayed_lineup()
        roster <- depth_data()
        ids <- suppressWarnings(as.integer(lineup[positions]))
        indices <- match(ids, suppressWarnings(as.integer(roster$player_id)))
        names <- rep("Unassigned", length(positions))
        resolved <- !is.na(indices)
        names[resolved] <- as.character(roster$player_name[indices[resolved]])

        recommended_mode <- identical(lineup_view_mode(), "recommended")
        recommended <- bie_recommended_result()
        comparison <- bie_lineup_comparison()
        complete <- all(!is.na(ids)) && length(unique(ids)) == length(positions)
        status <- if (complete) "PASS" else "REVIEW"

        note <- if (
          is.null(recommended) ||
          !identical(recommended$status, "OK")
        ) {
          v2_ui_text(
            recommended$explanation %||% NULL,
            "BIE recommendation is unavailable until all five positions can be covered."
          )
        } else if (recommended_mode) {
          v2_ui_text(
            recommended$explanation,
            "Showing the governed BIE-recommended five."
          )
        } else if (!is.null(comparison)) {
          sprintf(
            "Working BIE %.1f · recommended %.1f · change %+.1f.",
            comparison$current_score,
            comparison$recommended_score,
            comparison$delta
          )
        } else {
          "Working lineup is active; a comparable BIE recommendation is unavailable."
        }

        shiny::div(
          class = "tbi-p3-lineup-card tbi-p3-working-lineup-card",
          `data-lineup-type` = "BIE_WORKING",
          shiny::div(
            class = "tbi-p3-lineup-head",
            shiny::div(
              shiny::strong("BIE Recommended / Working Lineup"),
              shiny::span(
                class = "tbi-p3-working-lineup-mode",
                if (recommended_mode) "Showing BIE Recommended" else "Showing Working Lineup"
              )
            ),
            v2_ui_status_chip(status)
          ),
          shiny::div(
            class = "tbi-p3-lineup-members",
            lapply(seq_along(names), function(i) {
              shiny::span(paste0(positions[[i]], " · ", names[[i]]))
            })
          ),
          shiny::tags$small(
            class = "tbi-p3-lineup-note",
            v2_ui_concise_note(note, "Working-lineup summary is unavailable.")
          )
        )
      })
      
      displayed_lineup <- shiny::reactive({
        if (identical(lineup_view_mode(), "recommended")) {
          recommended <- bie_recommended_lineup()
          
          if (!any(is.na(recommended))) {
            return(recommended)
          }
        }
        
        active_lineup()
      })
      
      bie_rotation_cache <- shiny::reactiveVal(
        list(
          key = NULL,
          value = NULL
        )
      )
      
      
      bie_rotation_result <- shiny::reactive({
        
        # Reuse the already-evaluated roster. This prevents Rotation
        # Intelligence from running evaluate_bie_players() a second time.
        d <- bie_roster()
        
        if (
          is.null(d) ||
          !nrow(d) ||
          !exists(
            "build_phase11_rotation_result",
            mode = "function"
          )
        ) {
          return(NULL)
        }
        
        rotation_size <- suppressWarnings(
          as.integer(
            input$bie_rotation_size %||%
              "9"
          )
        )
        
        if (
          is.na(rotation_size)
        ) {
          rotation_size <- 9L
        }
        
        approved <- active_lineup()
        
        roster_key <- if (
          exists(
            "phase13_roster_signature",
            mode = "function"
          )
        ) {
          phase13_roster_signature(d)
        } else if (
          exists(
            "bie_roster_signature",
            mode = "function"
          )
        ) {
          bie_roster_signature(d)
        } else {
          as.character(
            nrow(d)
          )
        }
        
        lineup_key <- paste(
          names(approved),
          suppressWarnings(
            as.integer(
              approved
            )
          ),
          sep = "=",
          collapse = "|"
        )
        
        key <- paste(
          roster_key,
          lineup_key,
          rotation_size,
          sep = "||"
        )
        
        cached <- shiny::isolate(bie_rotation_cache())
        
        if (
          !is.null(cached$key) &&
          identical(
            cached$key,
            key
          ) &&
          !is.null(cached$value)
        ) {
          return(
            cached$value
          )
        }
        
        result <- tryCatch(
          build_phase11_rotation_result(
            players = d,
            rotation_size =
              rotation_size,
            total_minutes = 240L,
            approved_lineup =
              approved
          ),
          error = function(e) {
            list(
              status = "ERROR",
              rotation = data.frame(),
              score = NA_real_,
              confidence = "UNAVAILABLE",
              explanation =
                conditionMessage(e)
            )
          }
        )
        
        bie_rotation_cache(
          list(
            key = key,
            value = result
          )
        )
        
        result
      })
      
      
      phase11_lineup_cache <- shiny::reactiveVal(
        phase13_cache_new()
      )
      
      
      phase11_lineup_result <- shiny::reactive({
        
        rotation_result <- bie_rotation_result()
        
        if (
          is.null(rotation_result) ||
          !identical(
            rotation_result$status,
            "OK"
          ) ||
          !exists(
            "build_phase11_lineup_result",
            mode = "function"
          )
        ) {
          return(NULL)
        }
        
        rotation_key <- paste(
          phase13_roster_signature(
            rotation_result$rotation
          ),
          rotation_result$rotation_size,
          rotation_result$total_minutes,
          sep = "||"
        )
        
        cached <- shiny::isolate(phase11_lineup_cache())
        value <- phase13_cache_get(
          cached,
          rotation_key
        )
        
        if (!is.null(value)) {
          phase11_lineup_cache(
            phase13_cache_hit(
              cached
            )
          )
          
          return(value)
        }
        
        result <- tryCatch(
          depth_chart_build_phase11_lineup_result(
            rotation_result =
              rotation_result,
            pool_size =
              rotation_result$rotation_size
          ),
          error = function(e) {
            NULL
          }
        )
        
        phase11_lineup_cache(
          phase13_cache_store(
            cached,
            rotation_key,
            result
          )
        )
        
        result
      })
      
      
      output$phase11_lineup_optimization_panel <-
        shiny::renderUI({
          
          result <- phase11_lineup_result()
          
          if (
            !exists(
              "build_phase11_lineup_panel",
              mode = "function"
            )
          ) {
            return(NULL)
          }
          
          build_phase11_lineup_panel(
            result
          )
        })
      
      
      phase12_base_roster_cache <- shiny::reactiveVal(
        phase13_cache_new()
      )
      
      
      phase12_base_bie_roster <- shiny::reactive({
        
        scenario <- active_trade_scenario()
        
        if (
          is.null(scenario) ||
          !exists(
            "evaluate_bie_players",
            mode = "function"
          )
        ) {
          return(NULL)
        }
        
        d <- base_depth_data()
        
        if (
          is.null(d) ||
          !nrow(d)
        ) {
          return(NULL)
        }
        
        key <- phase13_roster_signature(d)
        
        cached <- shiny::isolate(phase12_base_roster_cache())
        value <- phase13_cache_get(
          cached,
          key
        )
        
        if (!is.null(value)) {
          phase12_base_roster_cache(
            phase13_cache_hit(
              cached
            )
          )
          
          return(value)
        }
        
        evaluated <- tryCatch(
          evaluate_bie_players(d),
          error = function(e) NULL
        )
        
        phase12_base_roster_cache(
          phase13_cache_store(
            cached,
            key,
            evaluated
          )
        )
        
        evaluated
      })
      
      
      phase12_scenario_cache <- shiny::reactiveVal(
        phase13_cache_new()
      )
      
      
      phase12_scenario_result <- shiny::reactive({
        
        scenario <- active_trade_scenario()
        
        if (
          is.null(scenario) ||
          !exists(
            "build_scenario_comparison",
            mode = "function"
          )
        ) {
          return(NULL)
        }
        
        base_roster <-
          phase12_base_bie_roster()
        
        proposed_roster <-
          bie_roster()
        
        if (
          is.null(base_roster) ||
          !nrow(base_roster) ||
          is.null(proposed_roster) ||
          !nrow(proposed_roster)
        ) {
          return(NULL)
        }
        
        rotation_size <- suppressWarnings(
          as.integer(
            input$bie_rotation_size %||%
              "9"
          )
        )
        
        if (is.na(rotation_size)) {
          rotation_size <- 9L
        }
        
        scenario_name <- paste0(
          selected_team(),
          " trade with ",
          scenario$partner_team
        )
        
        key <- paste(
          phase13_roster_signature(
            base_roster
          ),
          phase13_roster_signature(
            proposed_roster
          ),
          phase13_trade_signature(
            scenario
          ),
          rotation_size,
          scenario_name,
          sep = "|||"
        )
        
        cached <- shiny::isolate(phase12_scenario_cache())
        value <- phase13_cache_get(
          cached,
          key
        )
        
        if (!is.null(value)) {
          phase12_scenario_cache(
            phase13_cache_hit(
              cached
            )
          )
          
          return(value)
        }
        
        result <- tryCatch(
          build_scenario_comparison(
            base_roster = base_roster,
            scenario_roster =
              proposed_roster,
            scenario_name =
              scenario_name,
            rotation_size =
              rotation_size
          ),
          error = function(e) {
            NULL
          }
        )
        
        phase12_scenario_cache(
          phase13_cache_store(
            cached,
            key,
            result
          )
        )
        
        result
      })
      
      
      output$phase12_scenario_comparison_panel <-
        shiny::renderUI({
          
          scenario <- active_trade_scenario()
          
          if (
            is.null(scenario) ||
            !exists(
              "build_phase12_scenario_panel",
              mode = "function"
            )
          ) {
            return(NULL)
          }
          
          result <-
            phase12_scenario_result()
          
          if (is.null(result)) {
            return(NULL)
          }
          
          build_phase12_scenario_panel(
            result = result,
            scenario = scenario
          )
        })
      
      
      output$bie_rotation_summary <- shiny::renderUI({
        
        result <- bie_rotation_result()
        
        if (
          is.null(result) ||
          !identical(
            result$status,
            "OK"
          )
        ) {
          return(
            shiny::div(
              class =
                "depth-bie-rotation-summary",
              shiny::span(
                if (
                  !is.null(result) &&
                  !is.null(
                    result$explanation
                  )
                ) {
                  result$explanation
                } else {
                  "Rotation Intelligence is unavailable."
                }
              )
            )
          )
        }
        
        shiny::div(
          class =
            "depth-bie-rotation-summary",
          
          shiny::span(
            "Rotation ",
            shiny::strong(
              paste0(
                result$rotation_size,
                " players"
              )
            )
          ),
          
          shiny::span(
            "Baseline ",
            shiny::strong(
              if (
                identical(
                  result$starting_five_source,
                  "WORKING_LINEUP"
                )
              ) {
                "Working Starting Five"
              } else {
                "BIE Generated"
              }
            )
          ),
          
          shiny::span(
            "Minutes ",
            shiny::strong(
              sprintf(
                "%.0f / 240",
                result$total_minutes
              )
            )
          ),
          
          shiny::span(
            "Rotation Score ",
            shiny::strong(
              if (
                is.na(
                  safe_num(
                    result$score,
                    NA_real_
                  )
                )
              ) {
                "—"
              } else {
                sprintf(
                  "%.1f",
                  result$score
                )
              }
            )
          ),
          
          shiny::span(
            class = "confidence",
            paste0(
              "CONFIDENCE: ",
              result$confidence %||%
                "FOUNDATION"
            )
          )
        )
      })
      
      
      output$bie_rotation_table <- shiny::renderUI({
        
        result <- bie_rotation_result()
        
        if (
          is.null(result) ||
          !identical(
            result$status,
            "OK"
          ) ||
          !is.data.frame(
            result$rotation
          ) ||
          !nrow(
            result$rotation
          )
        ) {
          return(NULL)
        }
        
        rotation <- result$rotation
        
        header <- list(
          shiny::div(
            class =
              "depth-bie-rotation-cell depth-bie-rotation-header",
            "#"
          ),
          shiny::div(
            class =
              "depth-bie-rotation-cell depth-bie-rotation-header",
            "PLAYER"
          ),
          shiny::div(
            class =
              "depth-bie-rotation-cell depth-bie-rotation-header",
            "ROLE"
          ),
          shiny::div(
            class =
              "depth-bie-rotation-cell depth-bie-rotation-header",
            "SLOT"
          ),
          shiny::div(
            class =
              "depth-bie-rotation-cell depth-bie-rotation-header",
            "MIN"
          )
        )
        
        rows <- unlist(
          lapply(
            seq_len(
              nrow(rotation)
            ),
            function(i) {
              
              row <- rotation[
                i,
                ,
                drop = FALSE
              ]
              
              role <- text_or(
                row$bie_rotation_role,
                "BENCH"
              )
              
              list(
                shiny::div(
                  class =
                    "depth-bie-rotation-cell",
                  i
                ),
                
                shiny::div(
                  class =
                    "depth-bie-rotation-cell depth-bie-rotation-player",
                  text_or(
                    row$player_name
                  )
                ),
                
                shiny::div(
                  class =
                    "depth-bie-rotation-cell",
                  shiny::span(
                    class = paste(
                      "depth-bie-rotation-role",
                      if (
                        identical(
                          role,
                          "BENCH"
                        )
                      ) {
                        "bench"
                      } else {
                        ""
                      }
                    ),
                    role
                  )
                ),
                
                shiny::div(
                  class =
                    "depth-bie-rotation-cell",
                  text_or(
                    row$bie_rotation_slot
                  )
                ),
                
                shiny::div(
                  class =
                    "depth-bie-rotation-cell depth-bie-rotation-minutes",
                  sprintf(
                    "%.1f",
                    safe_num(
                      row$
                        bie_recommended_minutes,
                      0
                    )
                  )
                )
              )
            }
          ),
          recursive = FALSE
        )
        
        shiny::tagList(
          shiny::div(
            class =
              "depth-bie-rotation-table",
            c(
              header,
              rows
            )
          ),
          
          shiny::div(
            class =
              "depth-bie-rotation-note",
            result$explanation
          )
        )
      })
      
      
      output$bie_explanation_panel <- shiny::renderUI({
        
        explanation <- bie_explanation()
        change <- bie_change_explanation()
        
        if (is.null(explanation)) {
          return(NULL)
        }
        
        component_table <-
          explanation$component_table
        
        change_table <- if (
          !is.null(change)
        ) {
          change$component_table
        } else {
          data.frame()
        }
        
        component_delta <- function(
    component) {
          
          if (
            !is.data.frame(
              change_table
            ) ||
            !nrow(change_table)
          ) {
            return(NA_real_)
          }
          
          row <- change_table[
            change_table$component ==
              component &
              change_table$available,
            ,
            drop = FALSE
          ]
          
          if (!nrow(row)) {
            return(NA_real_)
          }
          
          safe_num(
            row$delta[[1]],
            NA_real_
          )
        }
        
        
        driver <- function(
    component,
    label) {
          
          row <- component_table[
            component_table$component ==
              component,
            ,
            drop = FALSE
          ]
          
          score <- if (
            nrow(row)
          ) {
            safe_num(
              row$score[[1]],
              NA_real_
            )
          } else {
            NA_real_
          }
          
          band <- if (
            nrow(row)
          ) {
            as.character(
              row$band[[1]]
            )
          } else {
            "Unavailable"
          }
          
          delta <- component_delta(
            component
          )
          
          delta_class <- if (
            is.na(delta)
          ) {
            ""
          } else if (
            delta >= 0
          ) {
            "positive"
          } else {
            "warning"
          }
          
          shiny::div(
            class = "depth-bie-driver",
            shiny::span(label),
            shiny::strong(
              if (
                is.na(score)
              ) {
                "—"
              } else {
                sprintf(
                  "%.1f",
                  score
                )
              }
            ),
            shiny::em(
              paste0(
                band,
                if (
                  is.na(delta)
                ) {
                  ""
                } else {
                  paste0(
                    " • ",
                    sprintf(
                      "%+.1f",
                      delta
                    )
                  )
                }
              ),
              class = delta_class
            )
          )
        }
        
        
        explain_items <- function(
    items,
    concern = FALSE) {
          
          shiny::tagList(
            lapply(
              items,
              function(item) {
                shiny::div(
                  class =
                    "depth-bie-explain-item",
                  shiny::span(
                    class = paste(
                      "depth-bie-explain-dot",
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
        
        
        shiny::div(
          class = "depth-bie-explain",
          
          shiny::div(
            class = "depth-bie-explain-main",
            
            shiny::h4(
              class =
                "depth-bie-explain-title",
              "Why BIE Recommends This Five"
            ),
            
            shiny::p(
              class =
                "depth-bie-explain-copy",
              explanation$explanation
            ),
            
            shiny::div(
              class = "depth-bie-driver-grid",
              
              driver(
                "player_quality",
                "Player Quality"
              ),
              
              driver(
                "position_fit",
                "Position Fit"
              ),
              
              driver(
                "spacing",
                "Efficiency / Spacing"
              ),
              
              driver(
                "playmaking",
                "Playmaking"
              ),
              
              driver(
                "defense",
                "Defense"
              ),
              
              driver(
                "rebounding",
                "Rebounding"
              )
            ),
            
            if (
              !is.null(change) &&
              !is.null(
                change$summary
              )
            ) {
              shiny::div(
                class =
                  "depth-bie-change-summary",
                shiny::strong(
                  "WHY THE SCORE CHANGED: "
                ),
                change$summary
              )
            }
          ),
          
          shiny::div(
            class = "depth-bie-explain-side",
            
            shiny::div(
              class =
                "depth-bie-explain-section",
              
              shiny::span(
                class =
                  "depth-bie-explain-label",
                "Strengths"
              ),
              
              explain_items(
                explanation$strengths,
                concern = FALSE
              )
            ),
            
            shiny::div(
              class =
                "depth-bie-explain-section",
              
              shiny::span(
                class =
                  "depth-bie-explain-label",
                "Potential Concerns"
              ),
              
              explain_items(
                explanation$concerns,
                concern = TRUE
              )
            )
          )
        )
      })
      
      
      output$bie_lineup_summary <- shiny::renderUI({
        result <- bie_recommended_result()
        comparison <- bie_lineup_comparison()
        
        if (
          is.null(result) ||
          !identical(result$status, "OK")
        ) {
          message <- if (
            !is.null(result) &&
            !is.null(result$explanation)
          ) {
            result$explanation
          } else {
            "BIE recommendation is unavailable until all five positions can be covered."
          }
          
          return(
            shiny::div(
              class = "depth-bie-summary",
              shiny::span(message)
            )
          )
        }
        
        delta <- if (is.null(comparison)) {
          NA_real_
        } else {
          safe_num(comparison$delta, NA_real_)
        }
        
        delta_class <- if (is.na(delta)) {
          ""
        } else if (delta >= 0) {
          "positive"
        } else {
          "warning"
        }
        
        shiny::div(
          class = "depth-bie-summary",
          
          shiny::span(
            "Working ",
            shiny::strong(
              if (is.null(comparison)) {
                "—"
              } else {
                sprintf("%.1f", comparison$current_score)
              }
            )
          ),
          
          shiny::span(
            "Recommended ",
            shiny::strong(
              sprintf("%.1f", safe_num(result$score, 0))
            )
          ),
          
          shiny::span(
            "Change ",
            shiny::strong(
              class = delta_class,
              if (is.na(delta)) {
                "—"
              } else {
                sprintf("%+.1f", delta)
              }
            )
          ),
          
          shiny::strong(
            class = "depth-bie-confidence",
            paste0(
              "CONFIDENCE: ",
              result$confidence %||% "FOUNDATION"
            )
          ),
          
          shiny::span(result$explanation)
        )
      })
      
      shiny::observeEvent(
        input$view_working_lineup,
        {
          lineup_view_mode("working")
        }
      )
      
      shiny::observeEvent(
        input$view_bie_lineup,
        {
          result <- bie_recommended_result()
          
          if (
            !is.null(result) &&
            identical(result$status, "OK")
          ) {
            lineup_view_mode("recommended")
          }
        }
      )
      
      shiny::observeEvent(
        input$use_bie_lineup,
        {
          result <- bie_recommended_result()
          
          if (
            is.null(result) ||
            !identical(result$status, "OK")
          ) {
            return()
          }
          
          recommendation <- bie_recommended_lineup()
          
          if (any(is.na(recommendation))) {
            return()
          }
          
          scenario_lineup(recommendation)
          lineup_view_mode("working")
        }
      )
      
      lineup_player_row <- function(player_id) {
        d <- depth_data()
        
        if (
          !nrow(d) ||
          is.na(player_id)
        ) {
          return(NULL)
        }
        
        row <- d[
          d$player_id == player_id,
          ,
          drop = FALSE
        ]
        
        if (!nrow(row)) {
          return(NULL)
        }
        
        row[1, , drop = FALSE]
      }
      
      scenario_out_of_position <- function(
    player_id,
    position) {
        
        row <- lineup_player_row(
          player_id
        )
        
        if (is.null(row)) {
          return(FALSE)
        }
        
        eligible <- official_positions(
          row
        )
        
        !position %in% eligible
      }
      
      shiny::observeEvent(
        input$lineup_drop,
        {
          drop <- input$lineup_drop
          
          if (
            is.null(drop$player_id) ||
            is.null(drop$position)
          ) {
            return()
          }
          
          player_id <- suppressWarnings(
            as.integer(
              drop$player_id
            )
          )
          
          target_position <- toupper(
            trimws(
              as.character(
                drop$position
              )
            )
          )
          
          if (
            is.na(player_id) ||
            !target_position %in%
            c("PG", "SG", "SF", "PF", "C")
          ) {
            return()
          }
          
          current <- active_lineup()
          
          source_position <- names(current)[
            current == player_id
          ]
          
          target_player <- current[[target_position]]
          
          if (length(source_position)) {
            source_position <- source_position[[1]]
            current[[target_position]] <- player_id
            current[[source_position]] <- target_player
          } else {
            current[[target_position]] <- player_id
          }
          
          scenario_lineup(current)
          lineup_view_mode("working")
          selected_player_id(player_id)
        },
        ignoreInit = TRUE
      )
      
      shiny::observeEvent(
        input$reset_scenario,
        {
          scenario_lineup(
            baseline_lineup()
          )
          lineup_view_mode("working")
        }
      )
      
      # ------------------------------------------------------
      # Board
      # ------------------------------------------------------
      
      output$player_count <- shiny::renderText({
        paste0(
          nrow(
            depth_data()
          ),
          " Players"
        )
      })
      
      output$depth_chart_board <- shiny::renderUI({
        d <- depth_data()
        
        if (!nrow(d)) {
          return(
            shiny::div(
              class = "depth-v21-empty",
              paste(
                "No depth-chart records are available for",
                selected_team(),
                selected_season()
              )
            )
          )
        }
        
        current_id <-
          selected_player_id()
        
        positions <- c(
          "PG",
          "SG",
          "SF",
          "PF",
          "C"
        )
        
        position_names <- c(
          PG = "Point Guard",
          SG = "Shooting Guard",
          SF = "Small Forward",
          PF = "Power Forward",
          C = "Center"
        )
        
        columns <- lapply(
          positions,
          function(position) {
            
            rows <- d[
              d$position ==
                position,
              ,
              drop = FALSE
            ]
            
            if (nrow(rows)) {
              rows <- rows[
                order(
                  -rows$is_starter,
                  rows$depth_order,
                  -rows$salary,
                  rows$player_name
                ),
                ,
                drop = FALSE
              ]
            }
            
            player_nodes <- if (
              !nrow(rows)
            ) {
              list(
                shiny::div(
                  class = "depth-v21-empty",
                  "No player assigned"
                )
              )
            } else {
              lapply(
                seq_len(
                  nrow(rows)
                ),
                function(i) {
                  
                  row <- rows[
                    i,
                    ,
                    drop = FALSE
                  ]
                  
                  player_id <-
                    as.integer(
                      row$player_id[[1]]
                    )
                  
                  starter <-
                    safe_num(
                      row$is_starter,
                      0
                    ) ==
                    1
                  
                  selected <-
                    !is.null(
                      current_id
                    ) &&
                    identical(
                      as.integer(
                        current_id
                      ),
                      player_id
                    )
                  
                  depth_number <-
                    safe_num(
                      row$depth_order,
                      i
                    )
                  
                  depth_label <- if (starter) "S" else as.character(depth_number)
                  
                  click_js <- sprintf(
                    "Shiny.setInputValue('%s', %d, {priority:'event'});",
                    ns(
                      "selected_player"
                    ),
                    player_id
                  )
                  
                  shiny::div(
                    class = paste(
                      "depth-v21-player",
                      if (
                        starter
                      ) {
                        "starter"
                      } else {
                        ""
                      },
                      if (
                        selected
                      ) {
                        "selected"
                      } else {
                        ""
                      },
                      if (
                        is_trade_incoming(
                          player_id
                        )
                      ) {
                        "trade-incoming"
                      } else {
                        ""
                      }
                    ),
                    onclick = click_js,
                    role = "button",
                    tabindex = "0",
                    onkeydown = "if(event.key==='Enter'||event.key===' '){event.preventDefault();this.click();}",

                    shiny::span(
                      class = "depth-v21-player-depth",
                      title = if (starter) "Starter" else paste("Depth", depth_number),
                      depth_label
                    ),

                    shiny::span(
                      class = "depth-v21-player-name",
                      title =
                        text_or(
                          row$player_name
                        ),
                      text_or(
                        row$player_name
                      )
                    ),

                    shiny::span(
                      class = "depth-v21-mini-pos",
                      position
                    ),

                    shiny::span(
                      class = "depth-v21-player-salary",
                      money(
                        row$salary
                      )
                    )
                  )
                }
              )
            }
            
            shiny::div(
              class = "depth-v21-column",
              
              shiny::div(
                class = "depth-v21-column-head",
                
                shiny::div(
                  class = "depth-v21-position-wrap",
                  
                  shiny::span(
                    class = "depth-v21-position",
                    position
                  ),
                  
                  shiny::span(
                    class = "depth-v21-position-name",
                    position_names[[position]]
                  )
                ),
                
                shiny::span(
                  class = "depth-v21-position-count",
                  paste0(
                    nrow(rows),
                    if (
                      nrow(rows) ==
                      1
                    ) {
                      " player"
                    } else {
                      " players"
                    }
                  )
                )
              ),
              
              player_nodes
            )
          }
        )
        
        shiny::div(
          class = "depth-v21-columns",
          columns
        )
      })
      
      # ------------------------------------------------------
      # Starting Five 2.0
      # ------------------------------------------------------
      
      output$scenario_status_chip <- shiny::renderUI({
        
        changed <- scenario_changed()
        trade_preview <- !is.null(
          active_trade_scenario()
        )
        
        label <- if (trade_preview) {
          if (changed) {
            "TRADE LINEUP PREVIEW"
          } else {
            "AUTO-REFILLED TRADE LINEUP"
          }
        } else if (changed) {
          "UNSAVED SCENARIO"
        } else {
          "SAVED LINEUP"
        }
        
        shiny::div(
          class = paste(
            "depth-v23-scenario-chip",
            if (
              changed ||
              trade_preview
            ) {
              "active"
            } else {
              ""
            }
          ),
          shiny::span(
            class = "depth-v23-scenario-dot"
          ),
          shiny::span(label)
        )
      })
      
      output$lineup_intelligence_strip <- shiny::renderUI({
        lineup <- displayed_lineup()
        
        ids <- as.integer(
          lineup[
            !is.na(lineup)
          ]
        )
        
        rows <- lapply(
          ids,
          lineup_player_row
        )
        
        rows <- rows[
          !vapply(
            rows,
            is.null,
            logical(1)
          )
        ]
        
        starter_salary <- if (length(rows)) {
          sum(
            vapply(
              rows,
              function(row) {
                safe_num(
                  row$salary,
                  0
                )
              },
              numeric(1)
            ),
            na.rm = TRUE
          )
        } else {
          0
        }
        
        ages <- if (length(rows)) {
          vapply(
            rows,
            function(row) {
              safe_num(
                row$player_age,
                NA_real_
              )
            },
            numeric(1)
          )
        } else {
          numeric()
        }
        
        avg_age <- if (
          length(ages) &&
          !all(is.na(ages))
        ) {
          mean(
            ages,
            na.rm = TRUE
          )
        } else {
          NA_real_
        }
        
        oop <- sum(
          vapply(
            names(lineup),
            function(position) {
              player_id <- lineup[[position]]
              
              if (is.na(player_id)) {
                return(FALSE)
              }
              
              scenario_out_of_position(
                player_id,
                position
              )
            },
            logical(1)
          )
        )
        
        metric <- function(label, value) {
          shiny::div(
            class = "depth-v23-intel-metric",
            shiny::span(label),
            shiny::strong(value)
          )
        }
        
        shiny::div(
          class = "depth-v23-intel-strip",
          
          metric(
            "STARTERS",
            paste0(
              sum(!is.na(lineup)),
              " / 5"
            )
          ),
          
          metric(
            "STARTER SALARY",
            money(starter_salary)
          ),
          
          metric(
            "AVG STARTER AGE",
            if (
              is.na(avg_age)
            ) {
              "—"
            } else {
              sprintf("%.1f", avg_age)
            }
          ),
          
          metric(
            "POSITION FLAGS",
            if (
              oop == 0
            ) {
              "None"
            } else {
              paste0(
                oop,
                " review"
              )
            }
          )
        )
      })
      
      output$starting_five_court <- shiny::renderUI({
        d <- depth_data()
        lineup <- displayed_lineup()
        
        if (!nrow(d)) {
          return(
            shiny::div(
              class = "depth-v23-court-stage",
              shiny::div(
                class = "depth-v21-court-empty",
                "Starting lineup is unavailable."
              )
            )
          )
        }
        
        court_positions <- list(
          PG = c(left = 31, top = 76),
          SG = c(left = 69, top = 76),
          SF = c(left = 76, top = 47),
          PF = c(left = 24, top = 47),
          C  = c(left = 50, top = 27)
        )
        
        current_id <- selected_player_id()
        
        player_nodes <- lapply(
          c("PG", "SG", "SF", "PF", "C"),
          function(pos) {
            player_id <- lineup[[pos]]
            coords <- court_positions[[pos]]
            
            if (is.na(player_id)) {
              return(
                shiny::div(
                  class = "depth-v23-court-slot",
                  `data-lineup-slot` = pos,
                  style = paste0(
                    "left:", coords[["left"]], "%;",
                    "top:", coords[["top"]], "%;"
                  )
                )
              )
            }
            
            row <- lineup_player_row(player_id)
            
            if (is.null(row)) {
              return(NULL)
            }
            
            click_js <- sprintf(
              "Shiny.setInputValue('%s', %d, {priority:'event'});",
              ns("selected_player"),
              player_id
            )
            
            selected <- !is.null(current_id) &&
              identical(
                as.integer(current_id),
                player_id
              )
            
            oop <- scenario_out_of_position(
              player_id,
              pos
            )
            
            shiny::div(
              class = "depth-v23-court-slot",
              `data-lineup-slot` = pos,
              style = paste0(
                "left:", coords[["left"]], "%;",
                "top:", coords[["top"]], "%;"
              ),
              
              shiny::div(
                class = paste(
                  "depth-v23-court-token",
                  if (selected) "selected" else "",
                  ""
                ),
                draggable = "true",
                `data-lineup-player` = player_id,
                onclick = click_js,
                
                shiny::div(
                  class = "depth-v23-token-top",
                  
                  shiny::div(
                    class = "depth-v23-token-logo-slot",
                    "LOGO"
                  ),
                  
                  shiny::div(
                    class = "depth-v23-token-avatar",
                    initials(
                      row$player_name
                    )
                  )
                ),
                
                shiny::div(
                  class = "depth-v23-token-name",
                  title = text_or(
                    row$player_name
                  ),
                  text_or(
                    row$player_name
                  )
                ),
                
                shiny::div(
                  class = "depth-v23-token-meta",
                  shiny::span(pos),
                  shiny::span(
                    money(
                      row$salary
                    )
                  )
                ),
                
                if (oop) {
                  shiny::div(
                    class = "depth-v23-oop-label",
                    "POSITION REVIEW"
                  )
                }
              )
            )
          }
        )
        
        shiny::div(
          class = "depth-v23-court-stage",
          
          shiny::tags$svg(
            class = "depth-v21-court-svg",
            viewBox = "0 0 1000 500",
            preserveAspectRatio = "none",
            
            shiny::tags$rect(
              x = "70",
              y = "25",
              width = "860",
              height = "450",
              rx = "2",
              class = "court-strong"
            ),
            
            shiny::tags$rect(
              x = "365",
              y = "25",
              width = "270",
              height = "190"
            ),
            
            shiny::tags$circle(
              cx = "500",
              cy = "215",
              r = "70"
            ),
            
            shiny::tags$circle(
              cx = "500",
              cy = "74",
              r = "13",
              class = "court-strong"
            ),
            
            shiny::tags$line(
              x1 = "455",
              y1 = "55",
              x2 = "545",
              y2 = "55",
              class = "court-strong"
            ),
            
            shiny::tags$path(
              d = "M 445 92 A 55 55 0 0 0 555 92"
            ),
            
            shiny::tags$path(
              d = "M 155 25 L 155 120 A 385 385 0 0 0 845 120 L 845 25",
              class = "court-strong"
            ),
            
            shiny::tags$line(
              x1 = "500",
              y1 = "215",
              x2 = "500",
              y2 = "475",
              style = "stroke:rgba(125,170,221,.08);stroke-dasharray:4 6;"
            )
          ),
          
          player_nodes
        )
      })
      
      output$bench_strip <- shiny::renderUI({
        d <- depth_data()
        lineup <- displayed_lineup()
        
        if (!nrow(d)) {
          return(NULL)
        }
        
        starter_ids <- as.integer(
          lineup[
            !is.na(lineup)
          ]
        )
        
        bench <- d[
          !d$player_id %in%
            starter_ids,
          ,
          drop = FALSE
        ]
        
        if (nrow(bench)) {
          bench <- bench[
            !duplicated(
              bench$player_id
            ),
            ,
            drop = FALSE
          ]
          
          bench <- bench[
            order(
              bench$depth_order,
              -bench$salary,
              bench$player_name
            ),
            ,
            drop = FALSE
          ]
        }
        
        current_id <- selected_player_id()
        
        tokens <- if (!nrow(bench)) {
          list(
            shiny::div(
              class = "depth-v21-empty",
              "No bench players are available."
            )
          )
        } else {
          lapply(
            seq_len(
              nrow(bench)
            ),
            function(i) {
              row <- bench[
                i,
                ,
                drop = FALSE
              ]
              
              player_id <- as.integer(
                row$player_id[[1]]
              )
              
              click_js <- sprintf(
                "Shiny.setInputValue('%s', %d, {priority:'event'});",
                ns("selected_player"),
                player_id
              )
              
              selected <- !is.null(current_id) &&
                identical(
                  as.integer(current_id),
                  player_id
                )
              
              shiny::div(
                class = paste(
                  "depth-v23-bench-token",
                  if (selected) "selected" else "",
                  if (is_trade_incoming(player_id)) {
                    "trade-incoming"
                  } else {
                    ""
                  }
                ),
                draggable = "true",
                `data-lineup-player` = player_id,
                onclick = click_js,
                
                shiny::div(
                  class = "depth-v23-bench-name",
                  title = text_or(
                    row$player_name
                  ),
                  text_or(
                    row$player_name
                  )
                ),
                
                shiny::div(
                  class = "depth-v23-bench-meta",
                  shiny::span(
                    text_or(
                      row$primary_position,
                      row$position
                    )
                  ),
                  shiny::span(
                    money(
                      row$salary
                    )
                  )
                )
              )
            }
          )
        }
        
        shiny::div(
          class = "depth-v23-bench",
          
          shiny::div(
            class = "depth-v23-bench-head",
            
            shiny::span(
              class = "depth-v23-bench-title",
              "BENCH + RESERVES"
            ),
            
            shiny::span(
              class = "depth-v23-bench-count",
              paste0(
                nrow(bench),
                " players"
              )
            )
          ),
          
          shiny::div(
            class = "depth-v23-bench-scroll",
            tokens
          )
        )
      })
      
      # ------------------------------------------------------
      # Player profile
      # ------------------------------------------------------
      
      output$player_profile_header <- shiny::renderUI({
        row <- selected_player()
        
        if (is.null(row)) {
          return(
            shiny::div(
              class = "depth-v21-empty",
              "Select a player to inspect or edit their depth-chart assignment."
            )
          )
        }
        
        starter <-
          safe_num(
            row$is_starter,
            0
          ) ==
          1
        
        override <-
          safe_num(
            row$has_override,
            0
          ) ==
          1
        
        shiny::tagList(
          shiny::div(
            class = "depth-v21-profile",
            
            shiny::div(
              class = "depth-v21-avatar",
              initials(
                row$player_name
              )
            ),
            
            shiny::div(
              shiny::h3(
                class = "depth-v21-player-title",
                title =
                  text_or(
                    row$player_name
                  ),
                text_or(
                  row$player_name
                )
              ),
              
              shiny::div(
                class = "depth-v21-profile-sub",
                paste0(
                  text_or(
                    row$position
                  ),
                  " • Depth ",
                  safe_num(
                    row$depth_order,
                    1
                  )
                )
              ),
              
              shiny::div(
                class = "depth-v21-profile-badges",
                
                if (
                  starter
                ) {
                  shiny::span(
                    class = "depth-v21-badge starter",
                    "Starter"
                  )
                } else {
                  shiny::span(
                    class = "depth-v21-badge",
                    "Rotation"
                  )
                }
              )
            )
          )
        )
      })
      
      output$player_metric_grid <- shiny::renderUI({
        row <- selected_player()
        
        if (is.null(row)) {
          return(NULL)
        }
        
        age <- safe_num(
          row$player_age,
          NA_real_
        )
        
        weight <- safe_num(
          row$weight_lbs,
          NA_real_
        )
        
        metric <- function(
    label,
    value) {
          
          shiny::div(
            class = "depth-v21-metric",
            shiny::span(label),
            shiny::strong(value)
          )
        }
        
        control_value <- if (
          "contract_type" %in%
          names(row)
        ) {
          text_or(
            row$contract_type,
            "—"
          )
        } else {
          "—"
        }
        
        shiny::div(
          class = "depth-v21-metrics",
          
          metric(
            "AGE",
            if (
              is.na(age)
            ) {
              "—"
            } else {
              as.character(
                round(age)
              )
            }
          ),
          
          metric(
            "HEIGHT",
            format_height(
              row$height_inches
            )
          ),
          
          metric(
            "WEIGHT",
            if (
              is.na(weight)
            ) {
              "—"
            } else {
              paste0(
                round(weight),
                " lbs"
              )
            }
          ),

          metric(
            "SALARY",
            money(
              row$salary
            )
          ),

          metric(
            "CONTRACT",
            control_value
          )
        )
      })
      
      # ------------------------------------------------------
      # Assignment form
      # ------------------------------------------------------
      
      output$assignment_panel <- shiny::renderUI({
        row <- selected_player()
        
        if (is.null(row)) {
          return(NULL)
        }
        
        eligible <-
          official_positions(
            row
          )
        
        all_positions <- c(
          "PG",
          "SG",
          "SF",
          "PF",
          "C"
        )
        
        current_position <-
          text_or(
            row$position,
            eligible[[1]] %||%
              "PG"
          )
        
        is_official <-
          current_position %in%
          eligible
        
        override_active <-
          !is_official ||
          (
            "is_position_override" %in%
              names(row) &&
              safe_num(
                row$is_position_override,
                0
              ) ==
              1
          )
        
        choices <- if (
          override_active
        ) {
          all_positions
        } else {
          eligible
        }
        
        if (
          !current_position %in%
          choices
        ) {
          choices <- unique(
            c(
              choices,
              current_position
            )
          )
        }
        
        reason_value <- ""
        
        if (
          "position_override_reason" %in%
          names(row)
        ) {
          reason_value <-
            text_or(
              row$position_override_reason,
              ""
            )
        }
        
        shiny::div(
          class = "depth-v21-assignment",
          
          shiny::div(
            class = "depth-v21-section-title",
            "EDIT LINEUP ASSIGNMENT"
          ),
          
          shiny::div(
            class = "depth-v21-eligibility",
            paste0(
              "Official eligibility: ",
              if (
                length(eligible)
              ) {
                paste(
                  eligible,
                  collapse = ", "
                )
              } else {
                "Not loaded"
              }
            )
          ),
          
          shiny::checkboxInput(
            ns(
              "allow_position_override"
            ),
            paste(
              "Allow lineup assignment outside",
              "official eligibility"
            ),
            value =
              override_active
          ),

          shiny::div(
            class = "depth-v21-assignment-fields",

            shiny::selectInput(
            ns(
              "assignment_position"
            ),
            "Position",
            choices =
              if (
                override_active
              ) {
                all_positions
              } else {
                eligible
              },
            selected =
              current_position,
            width = "100%"
            ),

            shiny::numericInput(
            ns(
              "assignment_depth"
            ),
            "Depth order",
            value =
              max(
                1,
                safe_num(
                  row$depth_order,
                  1
                )
              ),
            min = 1,
            max = 20,
            step = 1,
            width = "100%"
            ),

            shiny::checkboxInput(
            ns(
              "assignment_starter"
            ),
            "Starter",
            value =
              safe_num(
                row$is_starter,
                0
              ) ==
              1
            )
          ),
          
          shiny::conditionalPanel(
            condition =
              sprintf(
                "input['%s'] === true",
                ns(
                  "allow_position_override"
                )
              ),
            
            shiny::div(
              class = "depth-v21-override-reason",
              
              shiny::textInput(
                ns(
                  "override_reason"
                ),
                "Override reason",
                value =
                  reason_value,
                placeholder =
                  "Basketball rationale for non-standard assignment"
              )
            )
          ),
          
          shiny::div(
            class = "depth-v21-actions",
            
            shiny::actionButton(
              ns(
                "save_assignment"
              ),
              "Save assignment",
              class = "btn-primary"
            ),
            
            shiny::actionButton(
              ns(
                "reset_assignment"
              ),
              "Reset"
            )
          )
        )
      })
      
      # When override checkbox changes, update position choices.
      shiny::observeEvent(
        input$allow_position_override,
        {
          row <- selected_player()
          
          if (is.null(row)) {
            return()
          }
          
          eligible <-
            official_positions(
              row
            )
          
          choices <- if (
            isTRUE(
              input$allow_position_override
            )
          ) {
            c(
              "PG",
              "SG",
              "SF",
              "PF",
              "C"
            )
          } else {
            eligible
          }
          
          current <-
            isolate(
              input$assignment_position
            )
          
          if (
            is.null(current) ||
            !current %in%
            choices
          ) {
            current <-
              choices[[1]]
          }
          
          shiny::updateSelectInput(
            session,
            "assignment_position",
            choices =
              choices,
            selected =
              current
          )
        },
        ignoreInit = TRUE
      )
      
      # ------------------------------------------------------
      # Apply working lineup scenario
      # ------------------------------------------------------
      
      shiny::observeEvent(
        input$apply_scenario,
        {
          
          trade_preview <- !is.null(
            active_trade_scenario()
          )

          if (
            !trade_preview &&
            block_official_write("Apply Starting Five")
          ) {
            return()
          }
          
          lineup <- active_lineup()
          
          if (
            any(
              is.na(
                lineup
              )
            )
          ) {
            save_message_value(
              list(
                ok = FALSE,
                message = "Assign a starter at all five positions before applying the lineup."
              )
            )
            return()
          }
          
          if (trade_preview) {
            
            scenario_lineup(
              lineup
            )
            
            save_message_value(
              list(
                ok = TRUE,
                message = paste(
                  "Pending-trade Starting Five applied to the working scenario.",
                  "Vacated starter spots were auto-refilled and incoming players remain available for manual lineup changes.",
                  "No official depth-chart records were changed."
                )
              )
            )
            
            return()
          }
          
          result <- tryCatch(
            {
              for (position in names(lineup)) {
                player_id <- as.integer(
                  lineup[[position]]
                )
                
                row <- lineup_player_row(
                  player_id
                )
                
                if (is.null(row)) {
                  stop(
                    paste(
                      "Could not resolve the player assigned at",
                      position
                    ),
                    call. = FALSE
                  )
                }
                
                eligible <- official_positions(
                  row
                )
                
                non_official <- !position %in%
                  eligible
                
                save_assignment_to_db(
                  row = row,
                  position = position,
                  depth_order = 1,
                  is_starter = TRUE,
                  allow_override = TRUE,
                  override_reason = if (
                    non_official
                  ) {
                    "Starting Five 2.0 lineup scenario override"
                  } else {
                    ""
                  }
                )
              }
              
              list(
                ok = TRUE,
                message = "Starting Five scenario applied to the saved depth chart."
              )
            },
            error = function(e) {
              list(
                ok = FALSE,
                message = conditionMessage(e)
              )
            }
          )
          
          save_message_value(
            result
          )
          
          if (isTRUE(result$ok)) {
            refresh_key(
              refresh_key() + 1L
            )
          }
        }
      )
      
      # ------------------------------------------------------
      # Save / reset
      # ------------------------------------------------------
      
      save_assignment_to_db <- function(
    row,
    position,
    depth_order,
    is_starter,
    allow_override,
    override_reason) {
        authorization <- official_write_decision(
          "Save depth-chart assignment"
        )

        if (!isTRUE(authorization$allowed)) {
          stop(authorization$message, call. = FALSE)
        }
        
        eligible <-
          official_positions(
            row
          )
        
        position <-
          toupper(
            trimws(
              position
            )
          )
        
        non_official <-
          !position %in%
          eligible
        
        if (
          non_official &&
          !isTRUE(
            allow_override
          )
        ) {
          stop(
            paste(
              position,
              "is outside the player's official eligibility.",
              "Enable the lineup override first."
            ),
            call. = FALSE
          )
        }
        
        if (
          non_official &&
          !nzchar(
            trimws(
              override_reason
            )
          )
        ) {
          stop(
            "Enter an override reason for a non-standard position assignment.",
            call. = FALSE
          )
        }
        
        # Prefer direct DB write so both legacy and upgraded
        # depth_chart_overrides schemas remain supported.
        if (
          exists(
            "connect_db",
            mode = "function"
          )
        ) {
          con <- connect_db()
          
          on.exit(
            disconnect_db(
              con
            ),
            add = TRUE
          )
          
          if (
            exists(
              "ensure_tbi_support_tables",
              mode = "function"
            )
          ) {
            ensure_tbi_support_tables(
              con
            )
          }
          
          team <- DBI::dbGetQuery(
            con,
            "
            SELECT team_id
            FROM teams
            WHERE team_name = ?
               OR abbreviation = ?
               OR CAST(team_id AS TEXT) = CAST(? AS TEXT)
            LIMIT 1
            ",
            params = list(
              selected_team(),
              selected_team(),
              selected_team()
            )
          )
          
          if (!nrow(team)) {
            stop(
              "Selected team was not found.",
              call. = FALSE
            )
          }
          
          fields <- DBI::dbListFields(
            con,
            "depth_chart_overrides"
          )
          
          has_override_fields <-
            all(
              c(
                "is_position_override",
                "position_override_reason"
              ) %in%
                fields
            )
          
          if (
            has_override_fields
          ) {
            DBI::dbExecute(
              con,
              "
              INSERT INTO depth_chart_overrides
                (
                  player_id,
                  team_id,
                  season,
                  position,
                  depth_order,
                  is_starter,
                  is_position_override,
                  position_override_reason,
                  position_override_updated_at,
                  notes,
                  updated_at
                )
              VALUES
                (?, ?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP,
                 'User depth-chart edit', CURRENT_TIMESTAMP)
              ON CONFLICT(player_id, team_id, season)
              DO UPDATE SET
                position = excluded.position,
                depth_order = excluded.depth_order,
                is_starter = excluded.is_starter,
                is_position_override = excluded.is_position_override,
                position_override_reason = excluded.position_override_reason,
                position_override_updated_at = excluded.position_override_updated_at,
                notes = excluded.notes,
                updated_at = CURRENT_TIMESTAMP
              ",
              params = list(
                as.integer(
                  row$player_id[[1]]
                ),
                as.integer(
                  team$team_id[[1]]
                ),
                selected_season(),
                position,
                as.integer(
                  max(
                    1,
                    depth_order
                  )
                ),
                as.integer(
                  isTRUE(
                    is_starter
                  )
                ),
                as.integer(
                  non_official
                ),
                if (
                  non_official
                ) {
                  trimws(
                    override_reason
                  )
                } else {
                  NA_character_
                }
              )
            )
          } else {
            DBI::dbExecute(
              con,
              "
              INSERT INTO depth_chart_overrides
                (
                  player_id,
                  team_id,
                  season,
                  position,
                  depth_order,
                  is_starter,
                  notes,
                  updated_at
                )
              VALUES
                (?, ?, ?, ?, ?, ?, 'User depth-chart edit', CURRENT_TIMESTAMP)
              ON CONFLICT(player_id, team_id, season)
              DO UPDATE SET
                position = excluded.position,
                depth_order = excluded.depth_order,
                is_starter = excluded.is_starter,
                notes = excluded.notes,
                updated_at = CURRENT_TIMESTAMP
              ",
              params = list(
                as.integer(
                  row$player_id[[1]]
                ),
                as.integer(
                  team$team_id[[1]]
                ),
                selected_season(),
                position,
                as.integer(
                  max(
                    1,
                    depth_order
                  )
                ),
                as.integer(
                  isTRUE(
                    is_starter
                  )
                )
              )
            )
          }
          
          return(
            invisible(
              TRUE
            )
          )
        }
        
        # Fallback to helper when direct connection helper is absent.
        save_depth_chart_override(
          player_id =
            row$player_id[[1]],
          team_value =
            selected_team(),
          season =
            selected_season(),
          position =
            position,
          depth_order =
            max(
              1,
              depth_order
            ),
          is_starter =
            isTRUE(
              is_starter
            )
        )
      }
      
      shiny::observeEvent(
        input$save_assignment,
        {
          
          if (!is.null(active_trade_scenario())) {
            decision <- official_write_decision(
              "Save depth-chart assignment"
            )
            decision$message <- paste(
              "Trade preview is active.",
              "Incoming players can be used and the proposed Starting Five can be adjusted,",
              "but individual assignments cannot be written to the official depth chart until the trade is cleared or completed."
            )
            save_message_value(
              decision
            )
            return()
          }

          if (block_official_write("Save depth-chart assignment")) {
            return()
          }
          
          row <- selected_player()
          
          shiny::req(
            !is.null(row),
            input$assignment_position
          )
          
          result <- tryCatch(
            {
              save_assignment_to_db(
                row =
                  row,
                position =
                  input$assignment_position,
                depth_order =
                  safe_num(
                    input$assignment_depth,
                    1
                  ),
                is_starter =
                  isTRUE(
                    input$assignment_starter
                  ),
                allow_override =
                  isTRUE(
                    input$allow_position_override
                  ),
                override_reason =
                  text_or(
                    input$override_reason,
                    ""
                  )
              )
              
              list(
                ok = TRUE,
                message =
                  paste0(
                    text_or(
                      row$player_name
                    ),
                    " assignment saved."
                  )
              )
            },
            error = function(e) {
              list(
                ok = FALSE,
                message =
                  conditionMessage(
                    e
                  )
              )
            }
          )
          
          save_message_value(
            result
          )
          
          if (
            isTRUE(
              result$ok
            )
          ) {
            refresh_key(
              refresh_key() +
                1L
            )
          }
        }
      )
      
      shiny::observeEvent(
        input$reset_assignment,
        {
          if (block_official_write("Reset depth-chart assignment")) {
            return()
          }

          row <- selected_player()
          
          shiny::req(
            !is.null(row)
          )
          
          result <- tryCatch(
            {
              reset_depth_chart_override(
                player_id =
                  row$player_id[[1]],
                team_value =
                  selected_team(),
                season =
                  selected_season()
              )
              
              list(
                ok = TRUE,
                message =
                  paste0(
                    text_or(
                      row$player_name
                    ),
                    " reset to the baseline depth chart."
                  )
              )
            },
            error = function(e) {
              list(
                ok = FALSE,
                message =
                  conditionMessage(
                    e
                  )
              )
            }
          )
          
          save_message_value(
            result
          )
          
          if (
            isTRUE(
              result$ok
            )
          ) {
            refresh_key(
              refresh_key() +
                1L
            )
          }
        }
      )
      
      output$save_message <- shiny::renderUI({
        message <-
          save_message_value()
        
        if (
          is.null(message)
        ) {
          return(NULL)
        }
        
        shiny::div(
          class = "depth-v21-message",
          style =
            if (
              isTRUE(
                message$ok
              )
            ) {
              NULL
            } else {
              paste(
                "border-color:rgba(251,113,133,.22);",
                "color:#f1a0ae;",
                "background:rgba(127,29,29,.07);"
              )
            },
          message$message
        )
      })
      
      # ------------------------------------------------------
      # Contract footer
      # ------------------------------------------------------
      
      output$contract_panel <- shiny::renderUI({
        row <- selected_player()
        
        if (is.null(row)) {
          return(NULL)
        }
        
        shiny::div(
          class = "depth-v21-contract",
          
          shiny::div(
            class = "depth-v21-contract-label",
            "CONTRACT"
          ),
          
          shiny::strong(
            text_or(
              row$contract_type,
              "Not loaded"
            )
          ),
          
          shiny::span(
            paste0(
              "Season ",
              selected_season()
            )
          )
        )
      })

      list(v2_rotation_shadow = v2_shadow_diagnostic)
    }
  )
}
