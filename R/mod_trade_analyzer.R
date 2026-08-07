#' Trade analyzer UI Function
#' @noRd
mod_trade_analyzer_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    class = "tbi-module-page",
    
    shiny::div(
      class = "tbi-page-header",
      
      shiny::div(
        shiny::div(
          class = "tbi-page-eyebrow",
          "TRANSACTION STRATEGY"
        ),
        shiny::h2("Trade Intelligence"),
        shiny::p(
          paste(
            "Build a two-team trade framework and run a first-pass",
            "CBA salary-matching and apron-restriction screen."
          )
        )
      ),
      
      shiny::div(
        class = "tbi-page-icon",
        bsicons::bs_icon("arrow-left-right")
      )
    ),
    
    bslib::layout_columns(
      col_widths = c(5, 2, 5),
      
      shiny::div(
        class = "tbi-panel tbi-framework-panel",
        
        shiny::div(
          class = "tbi-panel-kicker",
          "YOUR ORGANIZATION"
        ),
        
        shiny::h3(
          shiny::textOutput(
            ns("team_a_name"),
            inline = TRUE
          )
        ),
        
        shiny::p("Select outgoing players"),
        
        shiny::uiOutput(
          ns("team_a_players")
        ),
        
        shiny::div(
          class = "tbi-trade-total",
          shiny::span("Outgoing salary"),
          shiny::strong(
            shiny::textOutput(
              ns("outgoing_salary"),
              inline = TRUE
            )
          )
        )
      ),
      
      shiny::div(
        class = "tbi-trade-center",
        bsicons::bs_icon("arrow-left-right"),
        shiny::span("SCENARIO")
      ),
      
      shiny::div(
        class = "tbi-panel tbi-framework-panel",
        
        shiny::div(
          class = "tbi-panel-kicker",
          "TRADE PARTNER"
        ),
        
        shiny::selectInput(
          ns("partner_team"),
          "Organization",
          choices = NULL
        ),
        
        shiny::p("Select incoming players"),
        
        shiny::uiOutput(
          ns("team_b_players")
        ),
        
        shiny::div(
          class = "tbi-trade-total",
          shiny::span("Incoming salary"),
          shiny::strong(
            shiny::textOutput(
              ns("incoming_salary"),
              inline = TRUE
            )
          )
        )
      )
    ),
    
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      
      shiny::div(
        class = "tbi-framework-kpi",
        shiny::span("SALARY DIFFERENCE"),
        shiny::strong(
          shiny::textOutput(
            ns("salary_difference"),
            inline = TRUE
          )
        )
      ),
      
      shiny::div(
        class = "tbi-framework-kpi",
        shiny::span("SALARY RATIO"),
        shiny::strong(
          shiny::textOutput(
            ns("salary_ratio"),
            inline = TRUE
          )
        )
      ),
      
      shiny::div(
        class = "tbi-framework-kpi",
        shiny::span("CBA SCREEN RESULT"),
        shiny::strong(
          shiny::textOutput(
            ns("framework_result"),
            inline = TRUE
          )
        )
      )
    ),
    
    shiny::uiOutput(
      ns("cba_summary")
    ),
    
    shiny::div(
      class = "tbi-panel tbi-framework-panel",
      
      shiny::div(
        class = "tbi-panel-kicker",
        "DECISION NOTES"
      ),
      
      shiny::h3("Transaction review framework"),
      
      shiny::uiOutput(
        ns("trade_notes")
      ),
      
      shiny::tags$small(
        paste(
          "This is a first-pass CBA salary-matching and selected",
          "apron-restriction screen. Sign-and-trade timing,",
          "recently traded restrictions, Base Year Compensation,",
          "poison-pill treatment, trade kickers, and use of specific",
          "trade exceptions still require final review."
        )
      )
    )
  )
}


#' Trade analyzer Server Functions
#' @noRd
mod_trade_analyzer_server <- function(
    id,
    selected_team,
    selected_season) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      teams <- get_teams()
      
      shiny::observe({
        shiny::req(
          selected_team()
        )
        
        partner_names <- teams$team_name[
          teams$team_name != selected_team()
        ]
        
        choices <- stats::setNames(
          partner_names,
          partner_names
        )
        
        shiny::updateSelectInput(
          session,
          "partner_team",
          choices = choices,
          selected = if (length(choices)) {
            choices[[1]]
          } else {
            NULL
          }
        )
      })
      
      money <- function(x) {
        x <- suppressWarnings(
          as.numeric(x)
        )
        
        if (
          !length(x) ||
          is.na(x[[1]]) ||
          !is.finite(x[[1]])
        ) {
          return("$0")
        }
        
        x <- x[[1]]
        
        if (abs(x) >= 1e6) {
          sprintf(
            "$%.1fM",
            x / 1e6
          )
        } else {
          sprintf(
            "$%.0fK",
            x / 1e3
          )
        }
      }
      
      trade_pool_a <- shiny::reactive({
        shiny::req(
          selected_team(),
          selected_season()
        )
        
        get_trade_player_pool(
          team_value = selected_team(),
          season = selected_season()
        )
      })
      
      trade_pool_b <- shiny::reactive({
        shiny::req(
          input$partner_team,
          selected_season()
        )
        
        get_trade_player_pool(
          team_value = input$partner_team,
          season = selected_season()
        )
      })
      
      output$team_a_name <- shiny::renderText({
        selected_team()
      })
      
      output$team_a_players <- shiny::renderUI({
        d <- trade_pool_a()
        
        choices <- stats::setNames(
          as.character(d$player_id),
          paste0(
            d$player_name,
            " — ",
            vapply(
              d$cap_hit,
              money,
              character(1)
            )
          )
        )
        
        shiny::checkboxGroupInput(
          session$ns("outgoing_players"),
          NULL,
          choices = choices
        )
      })
      
      output$team_b_players <- shiny::renderUI({
        d <- trade_pool_b()
        
        choices <- stats::setNames(
          as.character(d$player_id),
          paste0(
            d$player_name,
            " — ",
            vapply(
              d$cap_hit,
              money,
              character(1)
            )
          )
        )
        
        shiny::checkboxGroupInput(
          session$ns("incoming_players"),
          NULL,
          choices = choices
        )
      })
      
      selected_outgoing_rows <- shiny::reactive({
        d <- trade_pool_a()
        
        ids <- suppressWarnings(
          as.integer(
            input$outgoing_players %||%
              integer(0)
          )
        )
        
        d[
          d$player_id %in% ids,
          ,
          drop = FALSE
        ]
      })
      
      selected_incoming_rows <- shiny::reactive({
        d <- trade_pool_b()
        
        ids <- suppressWarnings(
          as.integer(
            input$incoming_players %||%
              integer(0)
          )
        )
        
        d[
          d$player_id %in% ids,
          ,
          drop = FALSE
        ]
      })
      
      outgoing <- shiny::reactive({
        trade_side_salary(
          selected_outgoing_rows()
        )
      })
      
      incoming <- shiny::reactive({
        trade_side_salary(
          selected_incoming_rows()
        )
      })
      
      trade_result <- shiny::reactive({
        shiny::req(
          selected_team(),
          input$partner_team,
          selected_season()
        )
        
        if (
          nrow(selected_outgoing_rows()) == 0 ||
          nrow(selected_incoming_rows()) == 0
        ) {
          return(NULL)
        }
        
        thresholds <- get_cap_thresholds(
          selected_season()
        )
        
        team_a_input <- build_trade_team_input(
          team_value = selected_team(),
          season = selected_season(),
          outgoing_player_ids =
            input$outgoing_players,
          incoming_players =
            selected_incoming_rows()
        )
        
        team_b_input <- build_trade_team_input(
          team_value = input$partner_team,
          season = selected_season(),
          outgoing_player_ids =
            input$incoming_players,
          incoming_players =
            selected_outgoing_rows()
        )
        
        evaluate_two_team_trade(
          team_a = team_a_input,
          team_b = team_b_input,
          thresholds = thresholds
        )
      })
      
      output$outgoing_salary <- shiny::renderText({
        money(
          outgoing()
        )
      })
      
      output$incoming_salary <- shiny::renderText({
        money(
          incoming()
        )
      })
      
      output$salary_difference <- shiny::renderText({
        money(
          incoming() - outgoing()
        )
      })
      
      output$salary_ratio <- shiny::renderText({
        if (outgoing() <= 0) {
          "—"
        } else {
          sprintf(
            "%.2fx",
            incoming() / outgoing()
          )
        }
      })
      
      output$framework_result <- shiny::renderText({
        result <- trade_result()
        
        if (is.null(result)) {
          return("Select players")
        }
        
        if (isTRUE(result$is_trade_screen_pass)) {
          if (isTRUE(result$requires_manual_review)) {
            "PASS WITH REVIEW"
          } else {
            "PASS"
          }
        } else {
          "FAIL"
        }
      })
      
      signal_row <- function(label, value) {
        shiny::div(
          class = "tbi-signal-row",
          shiny::span(label),
          shiny::strong(value)
        )
      }
      
      status_badge <- function(status) {
        status_class <- switch(
          status,
          "PASS" = "positive",
          "FAIL" = "negative",
          "PASS WITH REVIEW" = "warning",
          "neutral"
        )
        
        shiny::span(
          class = paste(
            "tbi-status-badge",
            status_class
          ),
          status
        )
      }
      
      team_cba_panel <- function(team_name, result) {
        restrictions <- result$restriction_flags %||%
          character(0)
        
        shiny::div(
          class = "tbi-panel tbi-framework-panel",
          
          shiny::div(
            class = "tbi-panel-kicker",
            paste(
              toupper(team_name),
              "CBA SCREEN"
            )
          ),
          
          shiny::div(
            class = "tbi-cba-status-row",
            shiny::h3(team_name),
            status_badge(
              if (isTRUE(result$is_screen_pass)) {
                if (
                  isTRUE(
                    result$requires_manual_review
                  )
                ) {
                  "PASS WITH REVIEW"
                } else {
                  "PASS"
                }
              } else {
                "FAIL"
              }
            )
          ),
          
          signal_row(
            "Pre-trade payroll status",
            result$pre_trade_status
          ),
          
          signal_row(
            "Post-trade payroll status",
            result$post_trade_status
          ),
          
          signal_row(
            "Outgoing salary",
            money(result$outgoing_salary)
          ),
          
          signal_row(
            "Incoming salary",
            money(result$incoming_salary)
          ),
          
          signal_row(
            "Maximum incoming salary",
            money(
              result$maximum_incoming_salary
            )
          ),
          
          signal_row(
            "Matching rule",
            result$matching_rule
          ),
          
          signal_row(
            "Salary matching",
            if (isTRUE(result$is_salary_match)) {
              "PASS"
            } else {
              "FAIL"
            }
          ),
          
          signal_row(
            "First-apron crossing",
            if (isTRUE(result$crosses_first_apron)) {
              "YES"
            } else {
              "NO"
            }
          ),
          
          signal_row(
            "Second-apron crossing",
            if (isTRUE(result$crosses_second_apron)) {
              "YES"
            } else {
              "NO"
            }
          ),
          
          signal_row(
            "Second-apron aggregation",
            if (
              isTRUE(
                result$second_apron_aggregation_violation
              )
            ) {
              "VIOLATION"
            } else {
              "CLEAR"
            }
          ),
          
          shiny::p(
            class = "tbi-cba-explanation",
            result$explanation
          ),
          
          if (length(restrictions)) {
            shiny::div(
              class = "tbi-cba-restrictions",
              
              shiny::strong(
                "Manual review flags"
              ),
              
              shiny::tags$ul(
                lapply(
                  restrictions,
                  shiny::tags$li
                )
              )
            )
          } else {
            NULL
          }
        )
      }
      
      output$cba_summary <- shiny::renderUI({
        result <- trade_result()
        
        if (is.null(result)) {
          return(
            shiny::div(
              class = "tbi-panel tbi-framework-panel",
              shiny::div(
                class = "tbi-panel-kicker",
                "CBA VALIDATION"
              ),
              shiny::h3(
                "Select at least one player from each team"
              ),
              shiny::p(
                paste(
                  "The CBA engine will evaluate salary matching,",
                  "team payroll status, apron crossings, and",
                  "second-apron aggregation restrictions."
                )
              )
            )
          )
        }
        
        shiny::tagList(
          bslib::layout_columns(
            col_widths = c(6, 6),
            
            team_cba_panel(
              result$team_a_name,
              result$team_a
            ),
            
            team_cba_panel(
              result$team_b_name,
              result$team_b
            )
          ),
          
          shiny::div(
            class = "tbi-panel tbi-framework-panel",
            
            shiny::div(
              class = "tbi-panel-kicker",
              "OVERALL CBA SCREEN"
            ),
            
            shiny::div(
              class = "tbi-cba-status-row",
              
              shiny::h3(
                if (
                  isTRUE(
                    result$is_trade_screen_pass
                  )
                ) {
                  "Transaction passes current screen"
                } else {
                  "Transaction fails current screen"
                }
              ),
              
              status_badge(
                if (
                  isTRUE(
                    result$is_trade_screen_pass
                  )
                ) {
                  if (
                    isTRUE(
                      result$requires_manual_review
                    )
                  ) {
                    "PASS WITH REVIEW"
                  } else {
                    "PASS"
                  }
                } else {
                  "FAIL"
                }
              )
            ),
            
            shiny::p(
              result$executive_summary
            ),
            
            shiny::tags$small(
              result$scope_note
            )
          )
        )
      })
      
      output$trade_notes <- shiny::renderUI({
        result <- trade_result()
        
        if (is.null(result)) {
          return(
            shiny::tagList(
              signal_row(
                "Outgoing players",
                length(
                  input$outgoing_players %||%
                    character(0)
                )
              ),
              signal_row(
                "Incoming players",
                length(
                  input$incoming_players %||%
                    character(0)
                )
              ),
              signal_row(
                "Net salary change",
                money(
                  incoming() - outgoing()
                )
              ),
              signal_row(
                "Validation status",
                "Awaiting complete player selection"
              )
            )
          )
        }
        
        shiny::tagList(
          signal_row(
            "Outgoing players",
            result$team_a$outgoing_player_count
          ),
          
          signal_row(
            "Incoming players",
            result$team_a$incoming_player_count
          ),
          
          signal_row(
            "Net salary change",
            money(
              result$team_a$salary_delta
            )
          ),
          
          signal_row(
            "Your organization rule",
            result$team_a$matching_rule
          ),
          
          signal_row(
            "Trade partner rule",
            result$team_b$matching_rule
          ),
          
          signal_row(
            "Manual review required",
            if (
              isTRUE(
                result$requires_manual_review
              )
            ) {
              "YES"
            } else {
              "NO"
            }
          )
        )
      })
    }
  )
}