#' Player Management UI
#' @noRd
mod_player_manager_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "tbi-module-page tbi-player-manager-page",
    shiny::div(
      class = "tbi-page-header",
      shiny::div(
        shiny::div(class = "tbi-page-eyebrow", "BASKETBALL OPERATIONS DATABASE"),
        shiny::h2("Player Management"),
        shiny::p("Review the authoritative player profile and safely update roster biographical and positional data.")
      ),
      shiny::div(class = "tbi-page-icon", bsicons::bs_icon("person-gear"))
    ),

    bslib::layout_columns(
      col_widths = c(4, 8),
      shiny::div(
        class = "tbi-panel tbi-manager-selector-panel",
        shiny::div(class = "tbi-panel-kicker", "PLAYER DIRECTORY"),
        shiny::selectInput(ns("player_id"), "Player", choices = NULL),
        shiny::uiOutput(ns("directory_summary")),
        shiny::tags$small("Edits are written to the shared SQLite database and flow into roster, depth-chart, trade, and executive views.")
      ),
      shiny::div(
        class = "tbi-panel tbi-manager-profile-panel",
        shiny::uiOutput(ns("player_profile"))
      )
    ),

    shiny::div(
      class = "tbi-panel tbi-player-editor-panel",
      shiny::div(class = "tbi-panel-kicker", "PLAYER EDITOR"),
      shiny::h3("Authoritative player record"),
      bslib::layout_columns(
        col_widths = c(6, 3, 3),
        shiny::textInput(ns("player_name"), "Player name"),
        shiny::numericInput(ns("player_age"), "Age", value = NA, min = 18, max = 50),
        shiny::textInput(ns("birth_date"), "Birth date", placeholder = "YYYY-MM-DD")
      ),
      bslib::layout_columns(
        col_widths = c(3, 3, 3, 3),
        shiny::numericInput(ns("height_inches"), "Height (inches)", value = NA, min = 60, max = 96),
        shiny::numericInput(ns("weight_lbs"), "Weight (lbs)", value = NA, min = 130, max = 400),
        shiny::selectInput(ns("primary_position"), "Primary position", choices = player_manager_valid_positions()),
        shiny::checkboxGroupInput(ns("eligible_positions"), "Eligible positions", choices = player_manager_valid_positions(), inline = TRUE)
      ),
      bslib::layout_columns(
        col_widths = c(4, 2, 2, 2, 2),
        shiny::selectInput(ns("roster_status"), "Roster status", choices = c("Active", "Qualifying Offer", "Restricted Free Agent", "Two-Way", "Exhibit 10")),
        shiny::textInput(ns("jersey_number"), "Jersey number"),
        shiny::checkboxInput(ns("two_way_flag"), "Two-way player", FALSE),
        shiny::checkboxInput(ns("is_active"), "Active player", TRUE),
        shiny::div(class = "tbi-manager-save-wrap", shiny::actionButton(ns("save_player"), "Save player", class = "btn-primary"))
      ),
      shiny::uiOutput(ns("editor_status"))
    )
  )
}

#' Player Management server
#' @noRd
mod_player_manager_server <- function(id, selected_team, selected_season) {
  shiny::moduleServer(id, function(input, output, session) {
    refresh_key <- shiny::reactiveVal(0L)

    pool <- shiny::reactive({
      refresh_key()
      shiny::req(selected_team(), selected_season())
      get_player_manager_pool(selected_team(), selected_season())
    })

    shiny::observe({
      d <- pool()
      choices <- stats::setNames(as.character(d$player_id), paste0(d$player_name, " — ", d$roster_status))
      current <- input$player_id
      selected <- if (!is.null(current) && current %in% choices) current else if (length(choices)) choices[[1]] else NULL
      shiny::updateSelectInput(session, "player_id", choices = choices, selected = selected)
    })

    record <- shiny::reactive({
      shiny::req(input$player_id)
      d <- pool()
      row <- d[d$player_id == as.integer(input$player_id), , drop = FALSE]
      shiny::validate(shiny::need(nrow(row) > 0, "Select a player."))
      row[1, , drop = FALSE]
    })

    shiny::observeEvent(record(), {
      p <- record()
      eligible <- player_manager_clean_positions(unlist(strsplit(player_manager_text(p$eligible_positions[[1]], ""), ",")))
      if (!length(eligible)) eligible <- player_manager_clean_positions(p$primary_position[[1]])
      shiny::updateTextInput(session, "player_name", value = player_manager_text(p$player_name[[1]], ""))
      shiny::updateNumericInput(session, "player_age", value = p$player_age[[1]])
      shiny::updateTextInput(session, "birth_date", value = player_manager_text(p$birth_date[[1]], ""))
      shiny::updateNumericInput(session, "height_inches", value = p$height_inches[[1]])
      shiny::updateNumericInput(session, "weight_lbs", value = p$weight_lbs[[1]])
      shiny::updateSelectInput(session, "primary_position", selected = p$primary_position[[1]])
      shiny::updateCheckboxGroupInput(session, "eligible_positions", selected = eligible)
      shiny::updateSelectInput(session, "roster_status", selected = p$roster_status[[1]])
      shiny::updateTextInput(session, "jersey_number", value = player_manager_text(p$jersey_number[[1]], ""))
      shiny::updateCheckboxInput(session, "two_way_flag", value = identical(as.integer(p$two_way_flag[[1]]), 1L))
      shiny::updateCheckboxInput(session, "is_active", value = identical(as.integer(p$is_active[[1]]), 1L))
    }, ignoreInit = FALSE)

    output$directory_summary <- shiny::renderUI({
      d <- pool()
      shiny::tagList(
        shiny::div(class = "tbi-signal-row", shiny::span("Players"), shiny::strong(nrow(d))),
        shiny::div(class = "tbi-signal-row", shiny::span("Organization"), shiny::strong(selected_team())),
        shiny::div(class = "tbi-signal-row", shiny::span("Season"), shiny::strong(selected_season()))
      )
    })

    output$player_profile <- shiny::renderUI({
      p <- record()
      eligible <- player_manager_text(p$eligible_positions[[1]], player_manager_text(p$primary_position[[1]], "—"))
      shiny::div(
        class = "tbi-manager-profile",
        shiny::div(
          class = "tbi-manager-profile-heading",
          shiny::div(class = "tbi-manager-avatar", substr(p$player_name[[1]], 1, 1)),
          shiny::div(
            shiny::div(class = "tbi-panel-kicker", "PLAYER PROFILE V1"),
            shiny::h2(p$player_name[[1]]),
            shiny::p(paste(p$team_name[[1]], "·", p$roster_status[[1]]))
          )
        ),
        shiny::div(
          class = "tbi-manager-metric-grid",
          player_manager_metric("Jersey", player_manager_text(p$jersey_number[[1]], "—")),
          player_manager_metric("Age", p$player_age[[1]] %||% "—"),
          player_manager_metric("Height", player_manager_height(p$height_inches[[1]])),
          player_manager_metric("Weight", ifelse(is.na(p$weight_lbs[[1]]), "—", paste0(round(p$weight_lbs[[1]]), " lbs"))),
          player_manager_metric("Primary", player_manager_text(p$primary_position[[1]], "—")),
          player_manager_metric("Eligible", eligible),
          player_manager_metric("Salary", player_manager_money(p$current_salary[[1]])),
          player_manager_metric("Contract", player_manager_text(p$contract_type[[1]], "Not loaded"))
        ),
        shiny::div(class = "tbi-manager-profile-note", paste("Contract through:", player_manager_text(p$contract_end_season[[1]], "Not loaded")))
      )
    })

    output$editor_status <- shiny::renderUI(NULL)

    shiny::observeEvent(input$save_player, {
      shiny::req(input$player_id)
      tryCatch({
        save_player_manager_record(
          player_id = input$player_id,
          team_value = selected_team(),
          season = selected_season(),
          player_name = input$player_name,
          player_age = input$player_age,
          birth_date = input$birth_date,
          height_inches = input$height_inches,
          weight_lbs = input$weight_lbs,
          primary_position = input$primary_position,
          eligible_positions = input$eligible_positions,
          roster_status = input$roster_status,
          two_way_flag = isTRUE(input$two_way_flag),
          jersey_number = input$jersey_number,
          is_active = isTRUE(input$is_active)
        )
        refresh_key(refresh_key() + 1L)
        shiny::showNotification("Player record saved.", type = "message")
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error", duration = 8)
      })
    })
  })
}

player_manager_metric <- function(label, value) {
  shiny::div(
    class = "tbi-manager-metric",
    shiny::span(label),
    shiny::strong(if (is.null(value) || !length(value) || is.na(value[[1]]) || !nzchar(as.character(value[[1]]))) "—" else as.character(value[[1]]))
  )
}
