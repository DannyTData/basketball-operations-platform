# ------------------------------------------------------------
# Module: Executive Dashboard
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
  
  kpi_tile <- function(label, output_id, icon) {
    shiny::div(
      class = "terminal-kpi",
      shiny::div(
        class = "terminal-kpi-top",
        shiny::span(class = "terminal-kpi-label", label),
        shiny::span(
          class = "terminal-kpi-icon",
          bsicons::bs_icon(icon)
        )
      ),
      shiny::div(
        class = "terminal-kpi-value",
        shiny::textOutput(ns(output_id), inline = TRUE)
      )
    )
  }
  
  metric_cell <- function(label, output_id) {
    shiny::div(
      class = "metric-cell",
      shiny::div(
        class = "metric-cell-label",
        label
      ),
      shiny::div(
        class = "metric-cell-value",
        shiny::textOutput(ns(output_id), inline = TRUE)
      )
    )
  }
  
  shiny::div(
    class = "executive-dashboard terminal-dashboard",
    
    shiny::div(
      class = "terminal-command-bar",
      
      shiny::div(
        class = "terminal-command-left",
        shiny::span(
          class = "terminal-code",
          "TBI / EXEC"
        ),
        shiny::span(class = "terminal-divider"),
        shiny::span(class = "terminal-live-dot"),
        shiny::span(
          class = "terminal-command-copy",
          "LIVE DECISION ENVIRONMENT"
        )
      ),
      
      shiny::div(
        class = "terminal-command-right",
        shiny::span("STANDINGS"),
        shiny::span("PERFORMANCE"),
        shiny::span("CONTEXT")
      )
    ),
    
    shiny::div(
      class = "executive-header-row",
      
      shiny::div(
        class = "executive-header-copy",
        
        shiny::div(
          class = "tbi-page-eyebrow",
          "FRONT OFFICE COMMAND CENTER"
        ),
        
        shiny::h1(
          class = "executive-main-title",
          shiny::textOutput(
            ns("dashboard_title"),
            inline = TRUE
          )
        ),
        
        shiny::p(
          class = "executive-subtitle",
          paste(
            "Competitive position, organizational signals,",
            "and conference context"
          )
        )
      ),
      
      shiny::div(
        class = "executive-header-badge",
        shiny::span(
          class = "header-badge-label",
          "MODE"
        ),
        shiny::strong("EXECUTIVE")
      )
    ),
    
    shiny::uiOutput(ns("executive_status")),
    
    shiny::div(
      class = "terminal-kpi-grid terminal-kpi-grid-six",
      
      kpi_tile("Team Payroll", "team_payroll", "currency-dollar"),
      
      kpi_tile("Payroll Rank", "payroll_rank", "list-ol"),
      
      kpi_tile("Contracts", "contract_count", "people"),
      
      kpi_tile(
        "Conference Rank",
        "conference_rank",
        "list-ol"
      ),
      
      kpi_tile(
        "Highest Paid Player",
        "highest_paid_player",
        "person-badge"
      ),
      
      kpi_tile(
        "Scoring",
        "kpi_scoring",
        "bullseye"
      )
    ),
    
    shiny::div(
      class = "terminal-main-grid",
      
      shiny::tags$section(
        class = "terminal-panel snapshot-panel",
        
        shiny::div(
          class = "terminal-panel-header",
          
          shiny::div(
            shiny::div(
              class = "terminal-panel-kicker",
              "TEAM PROFILE"
            ),
            shiny::h3("Team Snapshot")
          ),
          
          shiny::span(
            class = "terminal-panel-tag",
            "CURRENT"
          )
        ),
        
        shiny::div(
          class = "metric-cell-grid",
          
          metric_cell(
            "Conference",
            "snapshot_conference"
          ),
          
          metric_cell(
            "Conference Position",
            "snapshot_conference_rank"
          ),
          
          metric_cell(
            "Division Position",
            "snapshot_division_rank"
          ),
          
          metric_cell(
            "Scoring Average",
            "snapshot_scoring"
          )
        )
      ),
      
      shiny::tags$section(
        class = "terminal-panel outlook-panel",
        
        shiny::div(
          class = "terminal-panel-header",
          
          shiny::div(
            shiny::div(
              class = "terminal-panel-kicker",
              "DECISION SUPPORT"
            ),
            
            shiny::h3(
              shiny::textOutput(
                ns("outlook_heading"),
                inline = TRUE
              )
            )
          ),
          
          shiny::span(
            class = paste(
              "terminal-panel-tag",
              "terminal-panel-tag-accent"
            ),
            "ASSESSMENT"
          )
        ),
        
        shiny::div(
          class = "outlook-summary-terminal",
          shiny::textOutput(
            ns("outlook_summary"),
            inline = TRUE
          )
        ),
        
        shiny::div(
          class = "outlook-signal-grid",
          
          metric_cell(
            "Competitive Position",
            "competitive_position"
          ),
          
          metric_cell(
            "Division Position",
            "division_position"
          ),
          
          metric_cell(
            "Scoring Profile",
            "scoring_profile"
          )
        )
      )
    ),
    
    shiny::tags$section(
      class = "terminal-panel standings-panel",
      
      shiny::div(
        class = "terminal-panel-header",
        
        shiny::div(
          shiny::div(
            class = "terminal-panel-kicker",
            "LEAGUE CONTEXT"
          ),
          
          shiny::h3(
            shiny::textOutput(
              ns("standings_heading"),
              inline = TRUE
            )
          )
        ),
        
        shiny::span(
          class = "terminal-panel-tag",
          "LIVE TABLE"
        )
      ),
      
      shiny::div(
        class = "terminal-table-wrap",
        reactable::reactableOutput(
          ns("conference_standings")
        )
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
#' @param selected_team Reactive expression containing the
#' selected NBA team name.
#'
#' @noRd
mod_executive_dashboard_server <- function(id, selected_team) {
  
  shiny::moduleServer(id, function(input, output, session) {
    
    database_path <- local({
      development_path <- file.path("inst", "app", "data", "basketball_ops.duckdb")
      if (file.exists(development_path)) {
        normalizePath(development_path, winslash = "/", mustWork = TRUE)
      } else {
        packaged_path <- tryCatch(
          app_sys("app/data/basketball_ops.duckdb"),
          error = function(e) ""
        )
        if (nzchar(packaged_path) && file.exists(packaged_path)) {
          packaged_path
        } else {
          system.file(
            "app/data/basketball_ops.duckdb",
            package = utils::packageName(),
            mustWork = FALSE
          )
        }
      }
    })
    
    # --------------------------------------------------------
    # Load standings once and reuse the data
    # --------------------------------------------------------
    
    standings_table <- shiny::reactive({
      
      shiny::req(selected_team())
      
      if (!file.exists(database_path) || !requireNamespace("duckdb", quietly = TRUE)) {
        return(NULL)
      }
      
      con <- tryCatch(
        DBI::dbConnect(
          duckdb::duckdb(),
          dbdir = database_path,
          read_only = TRUE
        ),
        error = function(e) NULL
      )
      if (is.null(con)) return(NULL)
      
      on.exit(
        DBI::dbDisconnect(
          con,
          shutdown = TRUE
        ),
        add = TRUE
      )
      
      standings <- DBI::dbGetQuery(
        con,
        "
        SELECT
          team_name,
          wins,
          losses,
          win_pct,
          conference_rank,
          division_rank,
          points_per_game,
          point_diff,
          conference
        FROM standings
        "
      )

      if (nrow(standings) == 0) return(standings)

      # Some planning-season feeds do not publish conference_rank yet.
      # Build a deterministic rank from available results so the dashboard
      # never displays a blank card.
      standings$conference_rank <- suppressWarnings(as.numeric(standings$conference_rank))
      for (conference_name in unique(standings$conference)) {
        idx <- which(standings$conference == conference_name)
        current_rank <- standings$conference_rank[idx]
        if (all(is.na(current_rank)) || any(is.na(current_rank))) {
          win_pct <- suppressWarnings(as.numeric(standings$win_pct[idx]))
          wins <- suppressWarnings(as.numeric(standings$wins[idx]))
          point_diff <- suppressWarnings(as.numeric(standings$point_diff[idx]))
          win_pct[is.na(win_pct)] <- -Inf
          wins[is.na(wins)] <- -Inf
          point_diff[is.na(point_diff)] <- -Inf
          ord <- order(-win_pct, -wins, -point_diff, standings$team_name[idx])
          computed <- integer(length(idx))
          computed[ord] <- seq_along(ord)
          missing <- is.na(current_rank)
          current_rank[missing] <- computed[missing]
          standings$conference_rank[idx] <- current_rank
        }
      }

      standings[order(standings$conference, standings$conference_rank), , drop = FALSE]
    })
      
    # --------------------------------------------------------
    # Selected team
    # --------------------------------------------------------
    
    team_data <- shiny::reactive({
      
      standings <- standings_table()
      
      if (is.null(standings) || nrow(standings) == 0) {
        return(NULL)
      }
      
      team <- standings[
        standings$team_name == selected_team(),
        ,
        drop = FALSE
      ]
      
      if (nrow(team) == 0) {
        return(NULL)
      }
      
      team
    })
    
    # --------------------------------------------------------
    # Conference standings
    # --------------------------------------------------------
    
    conference_data <- shiny::reactive({
      
      standings <- standings_table()
      team <- team_data()
      
      if (
        is.null(standings) ||
        is.null(team) ||
        nrow(team) == 0
      ) {
        return(NULL)
      }
      
      conference_name <- team$conference[[1]]
      
      conference_standings <- standings[
        standings$conference == conference_name,
        c(
          "conference_rank",
          "team_name",
          "wins",
          "losses",
          "win_pct",
          "point_diff"
        ),
        drop = FALSE
      ]
      
      conference_standings <- conference_standings[
        order(
          as.numeric(conference_standings$conference_rank),
          -as.numeric(conference_standings$wins)
        ),
        ,
        drop = FALSE
      ]
      
      conference_standings
    })
  
# --------------------------------------------------------
# Executive assessment
# --------------------------------------------------------
      
    executive_assessment <- shiny::reactive({
      
      rank <- get_team_payroll_rank(
        selected_team()
      )
      
      if (
        length(rank) == 0 ||
        is.na(rank)
      ) {
        return(NULL)
      }
      
      status <- dplyr::case_when(
        rank <= 5 ~ "Top-Five Payroll",
        rank <= 10 ~ "High Payroll Commitment",
        rank <= 20 ~ "Middle-Tier Payroll",
        TRUE ~ "Lower-Tier Payroll"
      )
      
      grade <- dplyr::case_when(
        rank <= 5 ~ "TIER 1",
        rank <= 10 ~ "TIER 2",
        rank <= 20 ~ "TIER 3",
        TRUE ~ "TIER 4"
      )
      
      recommendation <- dplyr::case_when(
        rank <= 5 ~ "MAXIMIZE",
        rank <= 10 ~ "CONTEND",
        rank <= 20 ~ "EVALUATE",
        TRUE ~ "BUILD"
      )
      
      window <- dplyr::case_when(
        rank <= 10 ~ "NOW",
        rank <= 20 ~ "FLEXIBLE",
        TRUE ~ "FUTURE"
      )
      
      color <- dplyr::case_when(
        rank <= 5 ~ "#8b1e2d",
        rank <= 10 ~ "#8a5a00",
        rank <= 20 ~ "#174a7e",
        TRUE ~ "#176b46"
      )
      
      list(
        rank = rank,
        status = status,
        grade = grade,
        recommendation = recommendation,
        window = window,
        color = color
      )
    })
      
      # --------------------------------------------------------
      # Dashboard title
      # --------------------------------------------------------
      
      output$dashboard_title <- shiny::renderText({
        
        paste(
          selected_team(),
          "Executive Dashboard"
        )
      })
      
      
      # --------------------------------------------------------
      # Compact executive status strip
      # --------------------------------------------------------
      
      output$executive_status <- shiny::renderUI({
        
        assessment <- executive_assessment()
        
        payroll <- get_team_payroll(
          selected_team()
        )
        
        rank <- get_team_payroll_rank(
          selected_team()
        )
        
        highest_paid <- get_highest_paid_player(
          selected_team()
        )
        
        if (
          is.null(assessment) ||
          nrow(payroll) == 0 ||
          length(rank) == 0 ||
          is.na(rank) ||
          nrow(highest_paid) == 0
        ) {
          return(NULL)
        }
        
        total_payroll <- as.numeric(
          payroll$cap_hit[[1]]
        )
        
        contract_count <- as.integer(
          payroll$contracts[[1]]
        )
        
        top_player <- highest_paid$player_name[[1]]
        
        recommendation <- dplyr::case_when(
          rank <= 5 ~ "MAXIMIZE",
          rank <= 10 ~ "CONTEND",
          rank <= 20 ~ "EVALUATE",
          TRUE ~ "BUILD"
        )
        
        decision_window <- dplyr::case_when(
          rank <= 5 ~ "NOW",
          rank <= 10 ~ "CURRENT",
          rank <= 20 ~ "FLEXIBLE",
          TRUE ~ "FUTURE"
        )
        
        shiny::div(
          class = "executive-status-strip",
          
          style = paste0(
            "--status-accent:",
            assessment$color,
            ";"
          ),
          
          shiny::div(
            class = "executive-status-primary",
            
            shiny::div(
              class = "executive-status-label",
              "Payroll Status"
            ),
            
            shiny::div(
              class = "executive-status-name",
              assessment$status
            ),
            
            shiny::div(
              class = "executive-status-context",
              
              paste0(
                "#",
                rank,
                " NBA Payroll"
              ),
              
              shiny::span(
                class = "executive-status-dot",
                "•"
              ),
              
              paste0(
                "$",
                format(
                  round(total_payroll),
                  big.mark = ",",
                  scientific = FALSE
                ),
                " Cap Hits"
              ),
              
              shiny::span(
                class = "executive-status-dot",
                "•"
              ),
              
              paste0(
                contract_count,
                " Contracts"
              ),
              
              shiny::span(
                class = "executive-status-dot",
                "•"
              ),
              
              paste0(
                "Top: ",
                top_player
              )
            )
          ),
          
          shiny::div(
            class = "executive-status-metrics",
            
            shiny::div(
              class = "executive-status-metric",
              
              shiny::div(
                class = "executive-status-label",
                "Payroll Tier"
              ),
              
              shiny::div(
                class = "executive-status-metric-value",
                assessment$grade
              )
            ),
            
            shiny::div(
              class = "executive-status-metric",
              
              shiny::div(
                class = "executive-status-label",
                "Recommendation"
              ),
              
              shiny::div(
                class = "executive-status-metric-value",
                recommendation
              )
            ),
            
            shiny::div(
              class = "executive-status-metric",
              
              shiny::div(
                class = "executive-status-label",
                "Window"
              ),
              
              shiny::div(
                class = "executive-status-metric-value",
                decision_window
              )
            )
          )
        )
      })
  
      # --------------------------------------------------------
      # KPI outputs
      # --------------------------------------------------------
      
      output$team_payroll <- shiny::renderText({
        
        payroll <- get_team_payroll(
          selected_team()
        )
        
        if (nrow(payroll) == 0) {
          return("Unavailable")
        }
        
        paste0(
          "$",
          format(
            round(payroll$cap_hit[[1]]),
            big.mark = ",",
            scientific = FALSE
          )
        )
      })
      
      
      output$payroll_rank <- shiny::renderText({
        
        rank <- get_team_payroll_rank(selected_team())
        
        if (is.na(rank)) {
          return("Unavailable")
        }
        
        paste0("#", rank)
      })
      
      
      output$contract_count <- shiny::renderText({
        
        payroll <- get_team_payroll(
          selected_team()
        )
        
        if (nrow(payroll) == 0) {
          return("Unavailable")
        }
        
        payroll$contracts[[1]]
      })
      
      
      output$highest_paid_player <- shiny::renderText({
        
        player <- get_highest_paid_player(
          selected_team()
        )
        
        if (nrow(player) == 0) {
          return("Unavailable")
        }
        
        player$player_name[[1]]
      })
      
      
      # --------------------------------------------------------
      # Team Snapshot outputs
      # --------------------------------------------------------
      
      output$snapshot_conference <- shiny::renderText({
        
        team <- team_data()
        
        if (is.null(team)) {
          return("Unavailable")
        }
        
        team$conference[[1]]
      })
      
      
      output$snapshot_conference_rank <- shiny::renderText({
        
        team <- team_data()
        
        if (is.null(team)) {
          return("Unavailable")
        }
        
        rank_value <- suppressWarnings(as.integer(team$conference_rank[[1]]))
        if (is.na(rank_value)) "Not available" else paste0("#", rank_value)
      })
      
      
      output$division_position <- shiny::renderText({
        
        team <- team_data()
        
        if (is.null(team) || nrow(team) == 0) {
          return("Unavailable")
        }
        
        paste0("#", team$division_rank[[1]])
      })
      
      
      output$scoring_average <- shiny::renderText({
        
        team <- team_data()
        
        if (is.null(team) || nrow(team) == 0) {
          return("Unavailable")
        }
        
        paste0(
          sprintf(
            "%.1f",
            as.numeric(team$points_per_game[[1]])
          ),
          " PPG"
        )
      })
      
      
      # --------------------------------------------------------
      # Decision Outlook
      # --------------------------------------------------------
      
      output$outlook_heading <- shiny::renderText({
        
        paste(
          selected_team(),
          "Decision Outlook"
        )
      })
      
      
      output$outlook_summary <- shiny::renderText({
        
        team <- team_data()
        assessment <- executive_assessment()
        
        if (
          is.null(team) ||
          is.null(assessment)
        ) {
          return(
            paste(
              "Standings data is unavailable for",
              selected_team()
            )
          )
        }
        
        win_pct <- as.numeric(
          team$win_pct[[1]]
        )
        
        point_diff <- as.numeric(
          team$point_diff[[1]]
        )
        
        conference_rank <- as.numeric(
          team$conference_rank[[1]]
        )
        
        competitive_tier <- dplyr::case_when(
          conference_rank <= 3 ~
            "a top-tier conference contender",
          
          conference_rank <= 6 ~
            "a playoff-position team",
          
          conference_rank <= 10 ~
            "a play-in-position team",
          
          TRUE ~
            "outside the current postseason field"
        )
        
        performance_signal <- dplyr::case_when(
          point_diff >= 5 ~
            "strong underlying performance",
          
          point_diff >= 0 ~
            "a positive scoring margin",
          
          point_diff >= -3 ~
            "a slightly negative scoring margin",
          
          TRUE ~
            "a significant negative scoring margin"
        )
        
        strategic_direction <- dplyr::case_when(
          assessment$rank <= 5 ~
            paste(
              "The front office should prioritize",
              "championship-level upgrades while",
              "protecting long-term flexibility"
            ),
          
          assessment$rank <= 10 ~
            paste(
              "The front office should pursue",
              "targeted upgrades without overcommitting",
              "premium long-term assets"
            ),
          
          assessment$rank <= 20 ~
            paste(
              "The front office should evaluate",
              "whether short-term investment can",
              "meaningfully raise the team's ceiling"
            ),
          
          TRUE ~
            paste(
              "The front office should emphasize",
              "player development, asset accumulation,",
              "and future flexibility"
            )
        )
        
        paste0(
          selected_team(),
          " is currently ",
          competitive_tier,
          ", ranked #",
          conference_rank,
          " in the conference with a ",
          sprintf("%.3f", as.numeric(team$win_pct)),
          " winning percentage and a ",
          sprintf("%+.1f", as.numeric(point_diff)),
          " point differential. The current profile reflects ",
          performance_signal,
          ". ",
          strategic_direction,
          "."
        )
      })
      
      
      output$competitive_position <- shiny::renderText({
        
        team <- team_data()
        
        if (is.null(team)) {
          return("Unavailable")
        }
        
        rank <- as.numeric(
          team$conference_rank[[1]]
        )
        
        label <- dplyr::case_when(
          rank <= 3 ~ "Contender tier",
          rank <= 6 ~ "Playoff position",
          rank <= 10 ~ "Play-in position",
          TRUE ~ "Outside postseason field"
        )
        
        paste0(
          "#",
          rank,
          " in conference — ",
          label
        )
      })
      output$kpi_division_rank <- shiny::renderText({
        
        team <- team_data()
        
        if (is.null(team) || nrow(team) == 0) {
          return("Unavailable")
        }
        
        paste0("#", team$division_rank[[1]])
      })
      
      output$kpi_scoring <- shiny::renderText({
        
        team <- team_data()
        
        if (is.null(team) || nrow(team) == 0) {
          return("Unavailable")
        }
        
        paste0(
          sprintf("%.1f", as.numeric(team$points_per_game[[1]])),
          " PPG"
        )
      })
      output$snapshot_division_rank <- shiny::renderText({
        
        team <- team_data()
        
        if (is.null(team) || nrow(team) == 0) {
          return("Unavailable")
        }
        
        paste0("#", team$division_rank[[1]])
      })
      
      output$snapshot_scoring <- shiny::renderText({
        
        team <- team_data()
        
        if (is.null(team) || nrow(team) == 0) {
          return("Unavailable")
        }
        
        paste0(
          sprintf("%.1f", as.numeric(team$points_per_game[[1]])),
          " PPG"
        )
      })
   
      output$scoring_profile <- shiny::renderText({
        
        team <- team_data()
        
        if (is.null(team)) {
          return("Unavailable")
        }
        
        points_per_game <- as.numeric(
          team$points_per_game[[1]]
        )
        
        point_diff <- as.numeric(
          team$point_diff[[1]]
        )
        
        paste0(
          sprintf("%.1f", as.numeric(points_per_game)),
          " PPG | ",
          sprintf("%+.1f", as.numeric(point_diff)),
          " differential"
        )
      })
      
      
      # --------------------------------------------------------
      # Conference Standings
      # --------------------------------------------------------
      
      output$standings_heading <- shiny::renderText({
        
        team <- team_data()
        
        if (is.null(team)) {
          return("Conference Standings")
        }
        
        paste(
          team$conference[[1]],
          "Conference Standings"
        )
      })
      
      
      output$conference_standings <- reactable::renderReactable({
        
        standings <- conference_data()
        
        shiny::validate(
          
          shiny::need(
            
            !is.null(standings) &&
              nrow(standings) > 0,
            
            "Conference standings are unavailable."
          )
        )
        
        standings$conference_rank <- as.numeric(
          standings$conference_rank
        )
        
        standings$wins <- as.numeric(
          standings$wins
        )
        
        standings$losses <- as.numeric(
          standings$losses
        )
        
        standings$win_pct <- as.numeric(
          standings$win_pct
        )
        
        standings$point_diff <- as.numeric(
          standings$point_diff
        )
        
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
            
            if (
              row$team_name[[1]] ==
              selected_team()
            ) {
              
              return(
                
                list(
                  
                  fontWeight = "700",
                  
                  background = "#122033",
                  
                  color = "white",
                  
                  borderLeft =
                    "4px solid #4f8cff"
                )
              )
            }
            
            list(
              
              background = "#0b1422",
              
              color = "#d8e2ef",
              
              borderBottom =
                "1px solid #1d2c40"
            )
          },
          
          defaultColDef = reactable::colDef(
            
            headerStyle = list(
              
              background = "#08111f",
              
              color = "#9fb3c8",
              
              fontWeight = 600,
              
              borderBottom =
                "1px solid #243244"
            ),
            
            style = list(
              
              fontSize = "13px"
            )
          ),
          
          columns = list(
            
            conference_rank =
              reactable::colDef(
                
                name = "Rank",
                
                align = "center",
                
                width = 70
              ),
            
            team_name =
              reactable::colDef(
                
                name = "Team",
                
                minWidth = 180
              ),
            
            wins =
              reactable::colDef(
                
                name = "W",
                
                align = "center",
                
                width = 60
              ),
            
            losses =
              reactable::colDef(
                
                name = "L",
                
                align = "center",
                
                width = 60
              ),
            
            win_pct =
              reactable::colDef(
                
                name = "Win %",
                
                align = "center",
                
                width = 90,
                
                format =
                  reactable::colFormat(
                    digits = 3
                  )
              ),
            
            point_diff =
              reactable::colDef(
                
                name = "+/-",
                
                align = "center",
                
                width = 80,
                
                format =
                  reactable::colFormat(
                    digits = 1
                  )
              )
          )
        )
      })  
      })
      }
      