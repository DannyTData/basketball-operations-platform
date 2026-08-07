#' Depth chart and player intelligence UI
#' @noRd
mod_depth_chart_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::div(
      class = "tbi-depth-layout",
      shiny::tags$section(
        class = "tbi-depth-board-panel",
        shiny::div(
          class = "tbi-section-heading",
          shiny::div(
            shiny::span(class = "tbi-section-eyebrow", "ROTATION"),
            shiny::h2("Depth Chart"),
            shiny::p("Select a player, then use Player Intelligence to move them among eligible positions.")
          ),
          shiny::uiOutput(ns("roster_count"))
        ),
        shiny::uiOutput(ns("depth_chart_ui"))
      ),
      shiny::tags$aside(
        class = "tbi-player-intelligence-panel",
        shiny::uiOutput(ns("player_profile_ui"))
      )
    )
  )
}

#' Depth chart and player intelligence server
#' @noRd
mod_depth_chart_server <- function(
    id,
    selected_team,
    selected_season,
    db_path = file.path("inst", "database", "tbi.sqlite")
) {
  shiny::moduleServer(id, function(input, output, session) {
    selected_player_id <- shiny::reactiveVal(NULL)
    refresh_key <- shiny::reactiveVal(0L)
    
    depth_chart_data <- shiny::reactive({
      refresh_key()
      shiny::req(selected_team(), selected_season())
      get_depth_chart_records(
        team_value = selected_team(),
        season = selected_season(),
        db_path = db_path
      )
    })
    
    shiny::observeEvent(depth_chart_data(), {
      chart <- depth_chart_data()
      
      if (nrow(chart) == 0) {
        selected_player_id(NULL)
        return()
      }
      
      current_id <- selected_player_id()
      if (is.null(current_id) || !current_id %in% chart$player_id) {
        starters <- which(chart$is_starter == 1L)
        default_row <- if (length(starters) > 0) starters[[1]] else 1L
        selected_player_id(chart$player_id[[default_row]])
      }
    }, ignoreInit = FALSE)
    
    shiny::observeEvent(input$selected_player_click, {
      click <- input$selected_player_click
      shiny::req(click, click$player_id)
      player_id <- suppressWarnings(as.integer(click$player_id))
      shiny::req(!is.na(player_id))
      selected_player_id(player_id)
    }, ignoreInit = TRUE)
    
    selected_player <- shiny::reactive({
      chart <- depth_chart_data()
      player_id <- selected_player_id()
      shiny::req(nrow(chart) > 0, !is.null(player_id), !is.na(player_id))
      
      player <- chart[as.integer(chart$player_id) == as.integer(player_id), , drop = FALSE]
      shiny::validate(shiny::need(nrow(player) > 0, "The selected player could not be found."))
      player[1, , drop = FALSE]
    })
    
    eligible_positions <- shiny::reactive({
      player <- selected_player()
      
      database_positions <- tryCatch(
        get_player_eligible_positions(
          player_id = player$player_id[[1]],
          primary_position = player$primary_position[[1]],
          db_path = db_path
        ),
        error = function(e) character(0)
      )
      
      displayed_positions <- parse_depth_chart_positions(
        c(
          player$primary_position[[1]],
          player$position[[1]]
        )
      )
      
      eligible <- unique(
        c(
          parse_depth_chart_positions(database_positions),
          displayed_positions
        )
      )
      
      eligible <- eligible[
        eligible %in% c("PG", "SG", "SF", "PF", "C")
      ]
      
      if (length(eligible) == 0L) {
        eligible <- parse_depth_chart_positions(
          player$position[[1]]
        )
      }
      
      if (length(eligible) == 0L) {
        eligible <- "PG"
      }
      
      eligible
    })
    
    shiny::observeEvent(selected_player(), {
      player <- selected_player()
      official <- eligible_positions()
      
      current_position <- as.character(
        player$position[[1]]
      )
      
      current_override <- identical(
        as.integer(
          player$is_position_override[[1]] %||%
            0L
        ),
        1L
      )
      
      shiny::updateCheckboxInput(
        session,
        "allow_position_override",
        value = current_override
      )
      
      shiny::updateTextAreaInput(
        session,
        "position_override_reason",
        value = depth_chart_text(
          player$position_override_reason[[1]],
          ""
        )
      )
      
      choices <- if (current_override) {
        c("PG", "SG", "SF", "PF", "C")
      } else {
        official
      }
      
      if (!current_position %in% choices) {
        choices <- unique(
          c(
            current_position,
            choices
          )
        )
      }
      
      shiny::updateSelectInput(
        session,
        "edit_position",
        choices = stats::setNames(
          choices,
          choices
        ),
        selected = current_position
      )
      
      shiny::updateNumericInput(
        session,
        "edit_depth_order",
        value = max(
          1L,
          as.integer(
            player$depth_order[[1]]
          )
        )
      )
      
      shiny::updateCheckboxInput(
        session,
        "edit_starter",
        value = identical(
          as.integer(
            player$is_starter[[1]]
          ),
          1L
        )
      )
    }, ignoreInit = FALSE)
    
    shiny::observeEvent(
      input$allow_position_override,
      {
        player <- selected_player()
        official <- eligible_positions()
        
        choices <- if (
          isTRUE(
            input$allow_position_override
          )
        ) {
          c("PG", "SG", "SF", "PF", "C")
        } else {
          official
        }
        
        selected <- input$edit_position %||%
          player$position[[1]]
        
        if (!selected %in% choices) {
          selected <- official[[1]]
        }
        
        shiny::updateSelectInput(
          session,
          "edit_position",
          choices = stats::setNames(
            choices,
            choices
          ),
          selected = selected
        )
      },
      ignoreInit = TRUE
    )
    
    shiny::observeEvent(input$save_depth_edit, {
      player <- selected_player()
      
      shiny::req(
        input$edit_position,
        input$edit_depth_order
      )
      
      official <- eligible_positions()
      
      is_override <-
        !input$edit_position %in% official
      
      if (
        is_override &&
        !isTRUE(
          input$allow_position_override
        )
      ) {
        shiny::showNotification(
          paste0(
            input$edit_position,
            " is not listed as an eligible position. ",
            "Enable Lineup Assignment Override to continue."
          ),
          type = "error",
          duration = 7
        )
        
        return()
      }
      
      reason <- trimws(
        input$position_override_reason %||%
          ""
      )
      
      if (
        is_override &&
        !nzchar(reason)
      ) {
        shiny::showNotification(
          "Enter a reason for the out-of-position lineup assignment.",
          type = "error",
          duration = 7
        )
        
        return()
      }
      
      tryCatch(
        {
          result <- save_depth_chart_override(
            player_id =
              player$player_id[[1]],
            team_value =
              selected_team(),
            season =
              selected_season(),
            position =
              input$edit_position,
            depth_order =
              input$edit_depth_order,
            is_starter =
              isTRUE(input$edit_starter),
            allow_position_override =
              isTRUE(
                input$allow_position_override
              ),
            position_override_reason =
              reason,
            db_path = db_path
          )
          
          refresh_key(
            refresh_key() + 1L
          )
          
          notification <- if (
            isTRUE(
              result$is_position_override
            )
          ) {
            paste0(
              "Lineup assignment saved: ",
              result$assigned_position,
              " position override."
            )
          } else {
            "Depth chart updated."
          }
          
          shiny::showNotification(
            notification,
            type = "message"
          )
        },
        error = function(e) {
          shiny::showNotification(
            conditionMessage(e),
            type = "error",
            duration = 7
          )
        }
      )
    }, ignoreInit = TRUE)
    
    shiny::observeEvent(input$reset_depth_edit, {
      player <- selected_player()
      reset_depth_chart_override(
        player_id = player$player_id[[1]],
        team_value = selected_team(),
        season = selected_season(),
        db_path = db_path
      )
      refresh_key(refresh_key() + 1L)
      shiny::showNotification("Player reset to the generated depth chart.", type = "message")
    }, ignoreInit = TRUE)
    
    output$roster_count <- shiny::renderUI({
      chart <- depth_chart_data()
      planning_count <- sum(grepl("qualifying|restricted", chart$roster_status, ignore.case = TRUE), na.rm = TRUE)
      shiny::div(
        class = "tbi-depth-count-wrap",
        shiny::span(class = "tbi-count-pill", paste(nrow(chart), "Players")),
        if (planning_count > 0) shiny::span(class = "tbi-count-pill tbi-count-pill-muted", paste(planning_count, "Planning"))
      )
    })
    
    output$depth_chart_ui <- shiny::renderUI({
      chart <- depth_chart_data()
      
      shiny::validate(
        shiny::need(
          nrow(chart) > 0,
          paste("No depth-chart data is available for", selected_team(), "in", selected_season())
        )
      )
      
      positions <- c("PG", "SG", "SF", "PF", "C")
      
      shiny::div(
        class = "tbi-depth-grid",
        lapply(positions, function(pos) {
          position_players <- chart[chart$position == pos, , drop = FALSE]
          
          shiny::tags$section(
            class = "tbi-depth-position",
            shiny::div(
              class = "tbi-depth-position-header",
              shiny::span(class = "tbi-position-chip", pos),
              shiny::span(class = "tbi-position-count", paste(nrow(position_players), "players"))
            ),
            shiny::div(
              class = "tbi-depth-position-body",
              if (nrow(position_players) == 0) {
                shiny::div(class = "tbi-depth-empty", "No player assigned")
              } else {
                lapply(seq_len(nrow(position_players)), function(i) {
                  player <- position_players[i, , drop = FALSE]
                  is_selected <- identical(as.integer(selected_player_id()), as.integer(player$player_id[[1]]))
                  status <- value_or(player$roster_status[[1]], "Active")
                  status_code <- if (grepl("qualifying", status, ignore.case = TRUE)) "QO" else
                    if (grepl("restricted", status, ignore.case = TRUE)) "RFA" else
                      if (isTRUE(as.integer(player$two_way_flag[[1]]) == 1L)) "2W" else ""
                  
                  shiny::tags$button(
                    type = "button",
                    class = paste(
                      "tbi-depth-player",
                      if (player$is_starter[[1]] == 1L) "tbi-depth-player-starter" else "tbi-depth-player-reserve",
                      if (is_selected) "is-selected" else "",
                      if (nzchar(status_code)) "tbi-depth-player-planning" else ""
                    ),
                    onclick = sprintf(
                      "Shiny.setInputValue('%s', {player_id: %d, clicked_at: Date.now()}, {priority: 'event'});",
                      session$ns("selected_player_click"),
                      as.integer(player$player_id[[1]])
                    ),
                    shiny::div(
                      class = "tbi-depth-player-topline",
                      shiny::span(
                        class = "tbi-depth-order",
                        if (player$is_starter[[1]] == 1L) "STARTER" else paste0("DEPTH ", player$depth_order[[1]])
                      ),
                      shiny::span(
                        if (nzchar(status_code)) shiny::span(class = "tbi-roster-status-mini", status_code),
                        class = "tbi-depth-position-label",
                        pos
                      )
                    ),
                    shiny::strong(class = "tbi-depth-player-name", player$player_name[[1]]),
                    shiny::span(
                      class = "tbi-depth-player-meta",
                      paste(
                        format_age(player$player_age[[1]]),
                        format_height(player$height_inches[[1]]),
                        format_weight(player$weight_lbs[[1]]),
                        sep = "  •  "
                      )
                    )
                  )
                })
              }
            )
          )
        })
      )
    })
    
    output$position_assignment_controls <- shiny::renderUI({
      player <- selected_player()
      official <- eligible_positions()
      
      override_enabled <- isTRUE(
        input$allow_position_override
      ) ||
        identical(
          as.integer(
            player$is_position_override[[1]] %||%
              0L
          ),
          1L
        )
      
      choices <- if (override_enabled) {
        c("PG", "SG", "SF", "PF", "C")
      } else {
        official
      }
      
      selected <- input$edit_position %||%
        player$position[[1]]
      
      if (!selected %in% choices) {
        selected <- choices[[1]]
      }
      
      shiny::tagList(
        shiny::selectInput(
          inputId =
            session$ns("edit_position"),
          label = if (override_enabled) {
            "Lineup assignment"
          } else {
            "Eligible position"
          },
          choices = stats::setNames(
            choices,
            choices
          ),
          selected = selected
        ),
        
        if (override_enabled) {
          shiny::textAreaInput(
            inputId =
              session$ns(
                "position_override_reason"
              ),
            label = "Assignment reason",
            value = depth_chart_text(
              player$position_override_reason[[1]],
              ""
            ),
            placeholder = paste(
              "Example:",
              "Small-ball starting lineup",
              "or matchup-specific deployment"
            ),
            rows = 3,
            resize = "vertical"
          )
        } else {
          NULL
        }
      )
    })
    
    output$player_profile_ui <- shiny::renderUI({
      player <- selected_player()
      eligible <- eligible_positions()
      roster_status <- value_or(player$roster_status[[1]], "Active")
      
      shiny::tagList(
        shiny::div(
          class = "tbi-player-profile-heading",
          shiny::span(class = "tbi-section-eyebrow", "PLAYER INTELLIGENCE"),
          shiny::h2(player$player_name[[1]]),
          shiny::div(
            class = "tbi-player-profile-position",
            paste(
              eligible,
              collapse = ", "
            )
          )
        ),
        shiny::div(
          class = "tbi-player-profile-status",
          shiny::span(
            class = if (player$is_starter[[1]] == 1L) "tbi-status-badge starter" else "tbi-status-badge reserve",
            if (player$is_starter[[1]] == 1L) "Starter" else "Reserve"
          ),
          shiny::span(
            class = "tbi-depth-slot",
            paste(
              player$position[[1]],
              "· Depth",
              player$depth_order[[1]]
            )
          ),
          if (
            identical(
              as.integer(
                player$is_position_override[[1]] %||%
                0L
              ),
              1L
            )
          ) {
            shiny::span(
              class = "tbi-status-badge planning",
              "Lineup Override"
            )
          },
          if (!identical(roster_status, "Active")) {
            shiny::span(
              class = "tbi-status-badge planning",
              roster_status
            )
          }
        ),
        shiny::div(
          class = "tbi-player-profile-grid",
          profile_metric("Age", format_age_value(player$player_age[[1]])),
          profile_metric("Height", format_height(player$height_inches[[1]])),
          profile_metric("Weight", format_weight(player$weight_lbs[[1]])),
          profile_metric("Salary", format_salary(player$salary[[1]]))
        ),
        shiny::div(
          class = "tbi-depth-editor",
          
          shiny::span(
            class = "tbi-contract-label",
            "EDIT LINEUP ASSIGNMENT"
          ),
          
          shiny::div(
            class = "tbi-lineup-position-summary",
            
            shiny::tags$small(
              paste(
                "Official eligibility:",
                paste(
                  eligible,
                  collapse = ", "
                )
              )
            ),
            
            if (
              identical(
                as.integer(
                  player$is_position_override[[1]] %||%
                  0L
                ),
                1L
              )
            ) {
              shiny::tags$small(
                class = "tbi-position-override-note",
                paste0(
                  "Current assignment: ",
                  player$position[[1]],
                  " — ",
                  depth_chart_text(
                    player$position_override_reason[[1]],
                    "Coaching decision"
                  )
                )
              )
            }
          ),
          
          shiny::checkboxInput(
            inputId =
              session$ns(
                "allow_position_override"
              ),
            label =
              "Allow lineup assignment outside official eligibility",
            value = identical(
              as.integer(
                player$is_position_override[[1]] %||%
                  0L
              ),
              1L
            )
          ),
          
          shiny::uiOutput(
            session$ns(
              "position_assignment_controls"
            )
          ),
          
          shiny::numericInput(
            inputId =
              session$ns(
                "edit_depth_order"
              ),
            label = "Depth order",
            value =
              player$depth_order[[1]],
            min = 1,
            max = 10,
            step = 1
          ),
          
          shiny::checkboxInput(
            inputId =
              session$ns(
                "edit_starter"
              ),
            label = "Mark as starter",
            value =
              player$is_starter[[1]] == 1L
          ),
          
          shiny::div(
            class =
              "tbi-depth-editor-actions",
            
            shiny::actionButton(
              inputId =
                session$ns(
                  "save_depth_edit"
                ),
              label = "Save assignment",
              class = "btn-primary"
            ),
            
            shiny::actionButton(
              inputId =
                session$ns(
                  "reset_depth_edit"
                ),
              label = "Reset",
              class = "btn-outline-light"
            )
          )
        ),
        shiny::div(
          class = "tbi-contract-summary",
          shiny::span(class = "tbi-contract-label", "CONTRACT"),
          shiny::strong(value_or(player$contract_type[[1]], "Not loaded")),
          shiny::span(paste("Season", selected_season()))
        )
      )
    })
    
    invisible(list(
      roster_data = depth_chart_data,
      selected_player_id = selected_player_id,
      selected_player = selected_player
    ))
  })
}

parse_depth_chart_positions <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(character(0))
  }
  
  values <- as.character(x)
  values <- values[!is.na(values)]
  values <- trimws(values)
  values <- values[nzchar(values)]
  
  if (length(values) == 0L) {
    return(character(0))
  }
  
  pieces <- unlist(
    strsplit(
      values,
      split = "[,;/|]+",
      perl = TRUE
    ),
    use.names = FALSE
  )
  
  pieces <- toupper(trimws(pieces))
  pieces <- gsub(
    "[^A-Z]",
    "",
    pieces
  )
  
  aliases <- c(
    POINTGUARD = "PG",
    SHOOTINGGUARD = "SG",
    SMALLFORWARD = "SF",
    POWERFORWARD = "PF",
    CENTER = "C"
  )
  
  pieces <- vapply(
    pieces,
    function(position) {
      if (position %in% names(aliases)) {
        aliases[[position]]
      } else {
        position
      }
    },
    character(1)
  )
  
  unique(
    pieces[
      pieces %in% c("PG", "SG", "SF", "PF", "C")
    ]
  )
}


depth_chart_text <- function(x, default = "") {
  if (
    is.null(x) ||
    !length(x) ||
    is.na(x[[1]])
  ) {
    return(default)
  }
  
  value <- trimws(
    as.character(
      x[[1]]
    )
  )
  
  if (!nzchar(value)) {
    default
  } else {
    value
  }
}


profile_metric <- function(label, value) {
  shiny::div(class = "tbi-profile-metric", shiny::span(label), shiny::strong(value))
}

format_age <- function(age) {
  if (length(age) == 0 || is.na(age)) return("Age —")
  paste("Age", as.integer(age))
}

format_age_value <- function(age) {
  if (length(age) == 0 || is.na(age)) return("—")
  as.character(as.integer(age))
}

format_height <- function(height_inches) {
  if (length(height_inches) == 0 || is.na(height_inches)) return("—")
  height_inches <- as.integer(round(height_inches))
  paste0(height_inches %/% 12, "'", height_inches %% 12, '"')
}

format_weight <- function(weight_lbs) {
  if (length(weight_lbs) == 0 || is.na(weight_lbs)) return("—")
  paste0(as.integer(round(weight_lbs)), " lbs")
}

format_salary <- function(salary) {
  salary <- suppressWarnings(as.numeric(salary))
  if (length(salary) == 0 || is.na(salary) || salary <= 0) return("—")
  if (salary >= 1e6) return(paste0("$", format(round(salary / 1e6, 1), nsmall = 1), "M"))
  paste0("$", format(round(salary / 1e3, 0), big.mark = ","), "K")
}

value_or <- function(x, fallback) {
  if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(trimws(as.character(x)))) fallback else x
}
