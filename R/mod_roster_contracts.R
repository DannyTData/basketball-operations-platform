#' Roster and contracts UI Function
#' @noRd
mod_roster_contracts_ui <- function(id) {
  
  ns <- shiny::NS(id)
  
  shiny::div(
    class = "roster-intelligence-page",
    
    # ----------------------------------------------------------
    # Page heading
    # ----------------------------------------------------------
    
    shiny::div(
      class = "roster-page-heading",
      
      shiny::div(
        class = "roster-page-eyebrow",
        "ROSTER CONSTRUCTION"
      ),
      
      shiny::h2(
        class = "roster-page-title",
        "Roster Intelligence"
      ),
      
      shiny::p(
        class = "roster-page-description",
        paste(
          "Evaluate roster balance, player timelines,",
          "contract structure, and positional depth."
        )
      )
    ),
    
    # ----------------------------------------------------------
    # Dynamic summary cards
    # ----------------------------------------------------------
    
    shiny::div(
      class = "roster-summary-grid",
      
      roster_summary_card(
        icon = "award",
        label = "ROSTER GRADE",
        value = shiny::textOutput(
          ns("roster_grade"),
          inline = TRUE
        ),
        detail = shiny::textOutput(
          ns("roster_grade_detail"),
          inline = TRUE
        )
      ),
      
      roster_summary_card(
        icon = "calendar3",
        label = "AVERAGE AGE",
        value = shiny::textOutput(
          ns("average_age"),
          inline = TRUE
        ),
        detail = shiny::textOutput(
          ns("average_age_detail"),
          inline = TRUE
        )
      ),
      
      roster_summary_card(
        icon = "cash-stack",
        label = "TOTAL PAYROLL",
        value = shiny::textOutput(
          ns("total_payroll"),
          inline = TRUE
        ),
        detail = shiny::textOutput(
          ns("payroll_detail"),
          inline = TRUE
        )
      ),
      
      roster_summary_card(
        icon = "people",
        label = "ACTIVE PLAYERS",
        value = shiny::textOutput(
          ns("active_players"),
          inline = TRUE
        ),
        detail = shiny::textOutput(
          ns("active_players_detail"),
          inline = TRUE
        )
      )
    ),
    
    # ----------------------------------------------------------
    # Main roster and depth-chart area
    # ----------------------------------------------------------
    
    shiny::div(
      class = "roster-main-grid",
      
      shiny::div(
        class = "roster-panel roster-table-panel",
        
        shiny::div(
          class = "roster-panel-header",
          
          shiny::div(
            shiny::div(
              class = "roster-panel-eyebrow",
              "PERSONNEL"
            ),
            
            shiny::h3(
              class = "roster-panel-title",
              "Active Roster"
            )
          ),
          
          shiny::uiOutput(
            ns("roster_count_badge")
          )
        ),
        
        shiny::div(
          class = "roster-table-wrapper",
          
          shiny::tableOutput(
            ns("roster_table")
          )
        )
      ),
      
      shiny::div(
        class = "roster-panel roster-depth-panel",
        
        shiny::div(
          class = "roster-panel-header",
          
          shiny::div(
            shiny::div(
              class = "roster-panel-eyebrow",
              "ROTATION"
            ),
            
            shiny::h3(
              class = "roster-panel-title",
              "Depth Chart"
            )
          )
        ),
        
        shiny::div(
          class = "depth-chart",
          
          depth_chart_row(
            position = "PG",
            starter = "Starting Guard",
            reserve = "Primary Reserve"
          ),
          
          depth_chart_row(
            position = "SG",
            starter = "Starting Guard",
            reserve = "Primary Reserve"
          ),
          
          depth_chart_row(
            position = "SF",
            starter = "Starting Wing",
            reserve = "Primary Reserve"
          ),
          
          depth_chart_row(
            position = "PF",
            starter = "Starting Forward",
            reserve = "Primary Reserve"
          ),
          
          depth_chart_row(
            position = "C",
            starter = "Starting Center",
            reserve = "Primary Reserve"
          )
        )
      )
    ),
    
    # ----------------------------------------------------------
    # Bottom analysis panels
    # ----------------------------------------------------------
    
    shiny::div(
      class = "roster-bottom-grid",
      
      shiny::div(
        class = "roster-panel",
        
        shiny::div(
          class = "roster-panel-header",
          
          shiny::div(
            shiny::div(
              class = "roster-panel-eyebrow",
              "POSITIONAL ANALYSIS"
            ),
            
            shiny::h3(
              class = "roster-panel-title",
              "Position Strength"
            )
          )
        ),
        
        position_strength_row(
          position = "Point Guard",
          score = 82
        ),
        
        position_strength_row(
          position = "Shooting Guard",
          score = 88
        ),
        
        position_strength_row(
          position = "Small Forward",
          score = 92
        ),
        
        position_strength_row(
          position = "Power Forward",
          score = 79
        ),
        
        position_strength_row(
          position = "Center",
          score = 68
        )
      ),
      
      shiny::div(
        class = "roster-panel",
        
        shiny::div(
          class = "roster-panel-header",
          
          shiny::div(
            shiny::div(
              class = "roster-panel-eyebrow",
              "EXECUTIVE BRIEF"
            ),
            
            shiny::h3(
              class = "roster-panel-title",
              "Roster Assessment"
            )
          )
        ),
        
        shiny::tags$ul(
          class = "roster-assessment-list",
          
          shiny::tags$li(
            "The current core is positioned inside its prime competitive window."
          ),
          
          shiny::tags$li(
            "Wing depth projects as the strongest area of the roster."
          ),
          
          shiny::tags$li(
            "Frontcourt depth should remain a priority in future transactions."
          ),
          
          shiny::tags$li(
            "Payroll commitments limit near-term flexibility."
          )
        )
      )
    )
  )
}
#' Roster and contracts Server Functions
#' @noRd
mod_roster_contracts_server <- function(
    id,
    selected_team,
    selected_season
) {
  
  shiny::moduleServer(id, function(input, output, session) {
    
    roster_master <- data.frame(
      team_name = c(
        rep("Boston Celtics", 6),
        rep("New York Knicks", 6),
        rep("Los Angeles Lakers", 6)
      ),
      
      season = rep("2025-26", 18),
      
      player = c(
        "Boston Player One",
        "Boston Player Two",
        "Boston Player Three",
        "Boston Player Four",
        "Boston Player Five",
        "Boston Player Six",
        
        "New York Player One",
        "New York Player Two",
        "New York Player Three",
        "New York Player Four",
        "New York Player Five",
        "New York Player Six",
        
        "Los Angeles Player One",
        "Los Angeles Player Two",
        "Los Angeles Player Three",
        "Los Angeles Player Four",
        "Los Angeles Player Five",
        "Los Angeles Player Six"
      ),
      
      position = rep(
        c("PG", "SG", "SF", "PF", "C", "G"),
        3
      ),
      
      age = c(
        27, 28, 26, 25, 30, 23,
        28, 27, 26, 29, 25, 23,
        31, 27, 29, 26, 30, 22
      ),
      
      salary = c(
        34500000, 29800000, 25400000,
        18200000, 12600000, 4100000,
        
        42000000, 31000000, 24100000,
        17800000, 10500000, 3900000,
        
        48500000, 39600000, 27100000,
        15400000, 9200000, 3100000
      ),
      
      contract = c(
        "Guaranteed",
        "Guaranteed",
        "Guaranteed",
        "Team Option",
        "Expiring",
        "Rookie Scale",
        
        "Guaranteed",
        "Guaranteed",
        "Guaranteed",
        "Guaranteed",
        "Expiring",
        "Rookie Scale",
        
        "Guaranteed",
        "Guaranteed",
        "Player Option",
        "Guaranteed",
        "Expiring",
        "Rookie Scale"
      ),
      
      status = rep("Active", 18),
      
      stringsAsFactors = FALSE
    )
    
    selected_roster <- shiny::reactive({
      
      shiny::req(
        selected_team(),
        selected_season()
      )
      
      roster_master[
        roster_master$team_name == selected_team() &
          roster_master$season == selected_season(),
        ,
        drop = FALSE
      ]
    })
    
    output$active_players <- shiny::renderText({
      nrow(selected_roster())
    })
    
    output$active_players_detail <- shiny::renderText({
      
      player_count <- nrow(selected_roster())
      
      if (player_count == 0) {
        return("No roster data available")
      }
      
      paste(
        max(0, 18 - player_count),
        "roster openings"
      )
    })
    
    output$average_age <- shiny::renderText({
      
      roster <- selected_roster()
      
      if (nrow(roster) == 0) {
        return("--")
      }
      
      sprintf(
        "%.1f",
        mean(roster$age, na.rm = TRUE)
      )
    })
    
    output$average_age_detail <- shiny::renderText({
      
      roster <- selected_roster()
      
      if (nrow(roster) == 0) {
        return("Age data unavailable")
      }
      
      average_age <- mean(
        roster$age,
        na.rm = TRUE
      )
      
      if (average_age < 25) {
        "Young development timeline"
      } else if (average_age < 28) {
        "Prime competitive window"
      } else {
        "Veteran roster timeline"
      }
    })
    
    output$total_payroll <- shiny::renderText({
      
      roster <- selected_roster()
      
      if (nrow(roster) == 0) {
        return("$0.0M")
      }
      
      payroll <- sum(
        roster$salary,
        na.rm = TRUE
      )
      
      paste0(
        "$",
        format(
          round(payroll / 1000000, 1),
          nsmall = 1
        ),
        "M"
      )
    })
    
    output$payroll_detail <- shiny::renderText({
      
      roster <- selected_roster()
      
      if (nrow(roster) == 0) {
        return("Payroll data unavailable")
      }
      
      payroll <- sum(
        roster$salary,
        na.rm = TRUE
      )
      
      if (payroll >= 180000000) {
        "High payroll commitment"
      } else if (payroll >= 140000000) {
        "Moderate payroll flexibility"
      } else {
        "Strong payroll flexibility"
      }
    })
    
    output$roster_grade <- shiny::renderText({
      
      roster <- selected_roster()
      
      if (nrow(roster) == 0) {
        return("--")
      }
      
      average_age <- mean(
        roster$age,
        na.rm = TRUE
      )
      
      payroll <- sum(
        roster$salary,
        na.rm = TRUE
      )
      
      if (
        average_age >= 24 &&
        average_age <= 28 &&
        payroll >= 140000000
      ) {
        "A-"
      } else if (
        average_age <= 29 &&
        payroll >= 110000000
      ) {
        "B+"
      } else {
        "B"
      }
    })
    
    output$roster_grade_detail <- shiny::renderText({
      
      roster <- selected_roster()
      
      if (nrow(roster) == 0) {
        return("Roster evaluation unavailable")
      }
      
      average_age <- mean(
        roster$age,
        na.rm = TRUE
      )
      
      if (average_age <= 27) {
        "Balanced competitive timeline"
      } else {
        "Veteran-focused construction"
      }
    })
    
    output$roster_count_badge <- shiny::renderUI({
      
      shiny::span(
        class = "roster-panel-badge",
        paste(
          nrow(selected_roster()),
          "Players"
        )
      )
    })
    
    output$roster_table <- shiny::renderTable({
      
      roster <- selected_roster()
      
      shiny::validate(
        shiny::need(
          nrow(roster) > 0,
          paste(
            "No roster data is currently available for",
            selected_team(),
            "during",
            selected_season()
          )
        )
      )
      
      data.frame(
        Player = roster$player,
        Position = roster$position,
        Age = roster$age,
        
        Salary = paste0(
          "$",
          format(
            round(roster$salary / 1000000, 1),
            nsmall = 1
          ),
          "M"
        ),
        
        Contract = roster$contract,
        Status = roster$status,
        check.names = FALSE
      )
      
    },
    striped = FALSE,
    bordered = FALSE,
    hover = TRUE,
    spacing = "s",
    width = "100%"
    )
  })
}