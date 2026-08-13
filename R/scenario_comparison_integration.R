# ============================================================
# Thompson's Basketball Intelligence
# Phase 12: Scenario Comparison UI Integration
# ============================================================

phase12_number <- function(x, default = 0) {
  if (is.null(x) || length(x) == 0L) {
    return(default)
  }
  
  value <- suppressWarnings(
    as.numeric(x[[1]])
  )
  
  if (!is.finite(value)) {
    return(default)
  }
  
  value
}


phase12_change_text <- function(x,
                                digits = 1) {
  value <- phase12_number(x, 0)
  
  paste0(
    if (value > 0) "+" else "",
    format(
      round(value, digits),
      nsmall = digits,
      trim = TRUE
    )
  )
}


phase12_recommendation_class <- function(label) {
  label <- toupper(
    as.character(label %||% "")
  )
  
  if (grepl("STRONGLY FAVOR SCENARIO", label, fixed = TRUE)) {
    return("positive-strong")
  }
  
  if (grepl("FAVOR SCENARIO", label, fixed = TRUE)) {
    return("positive")
  }
  
  if (grepl("STRONGLY FAVOR BASE", label, fixed = TRUE)) {
    return("negative-strong")
  }
  
  if (grepl("FAVOR BASE", label, fixed = TRUE)) {
    return("negative")
  }
  
  "neutral"
}


phase12_metric_card <- function(label,
                                value) {
  shiny::div(
    class = "depth-phase12-metric-card",
    
    shiny::span(
      class = "depth-phase12-metric-label",
      label
    ),
    
    shiny::strong(
      class = paste(
        "depth-phase12-metric-value",
        if (
          phase12_number(value, 0) > 0
        ) {
          "is-positive"
        } else if (
          phase12_number(value, 0) < 0
        ) {
          "is-negative"
        } else {
          "is-neutral"
        }
      ),
      phase12_change_text(value)
    )
  )
}


phase12_top_minute_changes <- function(result,
                                       n = 5L) {
  d <- result$minute_comparison
  
  if (is.null(d) ||
      !is.data.frame(d) ||
      nrow(d) == 0L) {
    return(
      shiny::div(
        class = "depth-phase12-empty",
        "No player minute changes."
      )
    )
  }
  
  d <- d[
    order(
      -abs(d$minute_change),
      d$player_name
    ),
    ,
    drop = FALSE
  ]
  
  d <- head(
    d,
    n
  )
  
  rows <- lapply(
    seq_len(nrow(d)),
    function(i) {
      value <- phase12_number(
        d$minute_change[[i]],
        0
      )
      
      shiny::div(
        class = "depth-phase12-change-row",
        
        shiny::span(
          class = "depth-phase12-change-name",
          as.character(
            d$player_name[[i]]
          )
        ),
        
        shiny::span(
          class = paste(
            "depth-phase12-change-value",
            if (value > 0) {
              "is-positive"
            } else if (value < 0) {
              "is-negative"
            } else {
              "is-neutral"
            }
          ),
          paste0(
            phase12_change_text(
              value,
              digits = 0
            ),
            " MIN"
          )
        )
      )
    }
  )
  
  do.call(
    shiny::tagList,
    rows
  )
}


phase12_lineup_changes <- function(result) {
  d <- result$lineup_comparison
  
  if (is.null(d) ||
      !is.data.frame(d) ||
      nrow(d) == 0L) {
    return(
      shiny::div(
        class = "depth-phase12-empty",
        "No lineup comparison available."
      )
    )
  }
  
  rows <- lapply(
    seq_len(nrow(d)),
    function(i) {
      changed <- isTRUE(
        d$lineup_changed[[i]]
      )
      
      delta <- phase12_number(
        d$score_change[[i]],
        0
      )
      
      shiny::div(
        class = "depth-phase12-lineup-row",
        
        shiny::div(
          class = "depth-phase12-lineup-row-head",
          
          shiny::span(
            class = "depth-phase12-lineup-type",
            toupper(
              as.character(
                d$lineup_type[[i]]
              )
            )
          ),
          
          shiny::span(
            class = paste(
              "depth-phase12-change-value",
              if (delta > 0) {
                "is-positive"
              } else if (delta < 0) {
                "is-negative"
              } else {
                "is-neutral"
              }
            ),
            phase12_change_text(delta)
          )
        ),
        
        shiny::div(
          class = "depth-phase12-lineup-copy",
          
          shiny::span(
            paste0(
              "Base: ",
              d$players_base[[i]]
            )
          ),
          
          shiny::span(
            paste0(
              "Scenario: ",
              d$players_scenario[[i]]
            )
          )
        ),
        
        if (changed) {
          shiny::span(
            class = "depth-phase12-lineup-chip",
            "LINEUP CHANGED"
          )
        } else {
          shiny::span(
            class =
              "depth-phase12-lineup-chip is-stable",
            "SAME FIVE"
          )
        }
      )
    }
  )
  
  do.call(
    shiny::tagList,
    rows
  )
}


build_phase12_scenario_panel <- function(result,
                                         scenario = NULL) {
  if (!requireNamespace(
    "shiny",
    quietly = TRUE
  )) {
    stop(
      "The shiny package is required for Phase 12 UI.",
      call. = FALSE
    )
  }
  
  if (is.null(result)) {
    return(NULL)
  }
  
  detail <- result$score_detail
  
  partner <- if (
    !is.null(scenario) &&
    !is.null(
      scenario$partner_team
    )
  ) {
    as.character(
      scenario$partner_team
    )
  } else {
    "Proposed transaction"
  }
  
  rec_class <-
    phase12_recommendation_class(
      result$recommendation
    )
  
  reasons <- if (
    length(result$reasons)
  ) {
    lapply(
      result$reasons,
      function(reason) {
        shiny::tags$li(
          as.character(reason)
        )
      }
    )
  } else {
    list(
      shiny::tags$li(
        "No material basketball-impact change was detected."
      )
    )
  }
  
  shiny::div(
    class = "depth-phase12-shell",
    
    shiny::tags$style(
      shiny::HTML(
        "
        .depth-phase12-shell{
          margin-top:14px;
          padding:14px;
          border:1px solid rgba(111,171,255,.20);
          border-radius:14px;
          background:linear-gradient(180deg,rgba(10,24,44,.90),rgba(7,17,31,.92));
        }
        .depth-phase12-head{
          display:flex;
          align-items:flex-start;
          justify-content:space-between;
          gap:12px;
          margin-bottom:12px;
        }
        .depth-phase12-kicker{
          display:block;
          font-size:10px;
          letter-spacing:.12em;
          font-weight:800;
          color:#7da7dc;
          margin-bottom:3px;
        }
        .depth-phase12-title{
          display:block;
          color:#f4f8ff;
          font-size:15px;
        }
        .depth-phase12-subtitle{
          display:block;
          margin-top:3px;
          color:#91a9c7;
          font-size:11px;
        }
        .depth-phase12-rec{
          white-space:nowrap;
          border-radius:999px;
          padding:6px 9px;
          font-size:10px;
          font-weight:900;
          letter-spacing:.05em;
          border:1px solid rgba(255,255,255,.10);
        }
        .depth-phase12-rec.positive,
        .depth-phase12-rec.positive-strong{
          color:#72e6ad;
          background:rgba(38,160,101,.12);
          border-color:rgba(70,210,142,.25);
        }
        .depth-phase12-rec.negative,
        .depth-phase12-rec.negative-strong{
          color:#ff9a9a;
          background:rgba(191,64,64,.12);
          border-color:rgba(255,111,111,.25);
        }
        .depth-phase12-rec.neutral{
          color:#d7e4f6;
          background:rgba(110,140,180,.10);
        }
        .depth-phase12-scoregrid{
          display:grid;
          grid-template-columns:repeat(6,minmax(0,1fr));
          gap:8px;
          margin-bottom:12px;
        }
        .depth-phase12-metric-card{
          min-width:0;
          padding:9px 10px;
          border-radius:10px;
          border:1px solid rgba(130,173,229,.12);
          background:rgba(13,28,50,.70);
        }
        .depth-phase12-metric-label{
          display:block;
          color:#89a7cc;
          font-size:9px;
          font-weight:800;
          letter-spacing:.06em;
          margin-bottom:4px;
        }
        .depth-phase12-metric-value{
          font-size:15px;
        }
        .is-positive{color:#6ee2a9!important;}
        .is-negative{color:#ff9292!important;}
        .is-neutral{color:#e5edf8!important;}
        .depth-phase12-body{
          display:grid;
          grid-template-columns:minmax(0,.9fr) minmax(0,1.35fr);
          gap:10px;
        }
        .depth-phase12-box{
          min-width:0;
          padding:11px 12px;
          border-radius:11px;
          border:1px solid rgba(130,173,229,.12);
          background:rgba(8,20,38,.62);
        }
        .depth-phase12-box-title{
          display:block;
          margin-bottom:8px;
          color:#b7cceb;
          font-size:10px;
          font-weight:900;
          letter-spacing:.08em;
        }
        .depth-phase12-change-row,
        .depth-phase12-lineup-row-head{
          display:flex;
          align-items:center;
          justify-content:space-between;
          gap:8px;
        }
        .depth-phase12-change-row{
          padding:5px 0;
          border-bottom:1px solid rgba(255,255,255,.05);
        }
        .depth-phase12-change-name{
          color:#edf4ff;
          font-size:11px;
        }
        .depth-phase12-change-value{
          font-size:11px;
          font-weight:900;
        }
        .depth-phase12-lineup-row{
          padding:7px 0;
          border-bottom:1px solid rgba(255,255,255,.05);
        }
        .depth-phase12-lineup-type{
          color:#b9cff0;
          font-size:10px;
          font-weight:900;
        }
        .depth-phase12-lineup-copy{
          display:grid;
          gap:2px;
          margin-top:4px;
          color:#93a9c5;
          font-size:10px;
          line-height:1.35;
        }
        .depth-phase12-lineup-chip{
          display:inline-block;
          width:max-content;
          margin-top:5px;
          padding:3px 6px;
          border-radius:999px;
          color:#ffd277;
          background:rgba(205,151,42,.10);
          font-size:8px;
          font-weight:900;
          letter-spacing:.05em;
        }
        .depth-phase12-lineup-chip.is-stable{
          color:#8cc9aa;
          background:rgba(62,154,108,.10);
        }
        .depth-phase12-reasons{
          margin:10px 0 0 18px;
          padding:0;
          color:#9fb4cf;
          font-size:10px;
          line-height:1.45;
        }
        .depth-phase12-empty{
          color:#8fa6c4;
          font-size:11px;
        }
        @media(max-width:1250px){
          .depth-phase12-scoregrid{
            grid-template-columns:repeat(3,minmax(0,1fr));
          }
        }
        @media(max-width:950px){
          .depth-phase12-body{
            grid-template-columns:1fr;
          }
        }
        "
      )
    ),
    
    shiny::div(
      class = "depth-phase12-head",
      
      shiny::div(
        shiny::span(
          class = "depth-phase12-kicker",
          "PHASE 10 • SCENARIO COMPARISON"
        ),
        
        shiny::strong(
          class = "depth-phase12-title",
          "Basketball Impact — Before vs After"
        ),
        
        shiny::span(
          class = "depth-phase12-subtitle",
          paste0(
            "Active trade preview versus ",
            partner,
            "."
          )
        )
      ),
      
      shiny::span(
        class = paste(
          "depth-phase12-rec",
          rec_class
        ),
        result$recommendation
      )
    ),
    
    shiny::div(
      class = "depth-phase12-scoregrid",
      
      phase12_metric_card(
        "COMPOSITE",
        result$composite_score
      ),
      
      phase12_metric_card(
        "ROTATION",
        detail$rotation_quality_change
      ),
      
      phase12_metric_card(
        "BALANCED",
        detail$balanced_change
      ),
      
      phase12_metric_card(
        "OFFENSE",
        detail$offense_change
      ),
      
      phase12_metric_card(
        "DEFENSE",
        detail$defense_change
      ),
      
      phase12_metric_card(
        "CLOSING",
        detail$closing_change
      )
    ),
    
    shiny::div(
      class = "depth-phase12-body",
      
      shiny::div(
        class = "depth-phase12-box",
        
        shiny::span(
          class = "depth-phase12-box-title",
          "TOP MINUTE CHANGES"
        ),
        
        phase12_top_minute_changes(
          result
        ),
        
        shiny::tags$ul(
          class = "depth-phase12-reasons",
          reasons
        )
      ),
      
      shiny::div(
        class = "depth-phase12-box",
        
        shiny::span(
          class = "depth-phase12-box-title",
          "OPTIMIZED LINEUP IMPACT"
        ),
        
        phase12_lineup_changes(
          result
        )
      )
    )
  )
}