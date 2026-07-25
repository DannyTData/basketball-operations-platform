#' The application user interface
#'
#' @param request Internal parameter used by Shiny.
#'
#' @noRd
app_ui <- function(request) {
  
  # ----------------------------------------------------------
  # Static resources
  # ----------------------------------------------------------
  
  www_path <- app_sys("app/www")
  
  if (!nzchar(www_path) || !dir.exists(www_path)) {
    www_path <- file.path("inst", "app", "www")
  }
  
  shiny::addResourcePath(
    prefix = "tbi-assets",
    directoryPath = www_path
  )
  
  
  # ----------------------------------------------------------
  # Team list
  # ----------------------------------------------------------
  
  teams_path <- app_sys("app/data/teams.csv")
  
  if (!nzchar(teams_path) || !file.exists(teams_path)) {
    teams_path <- file.path("data", "teams.csv")
  }
  
  teams_master <- utils::read.csv(
    teams_path,
    stringsAsFactors = FALSE
  )
  
  team_choices <- sort(unique(teams_master$team_name))
  
  default_team <- if ("Boston Celtics" %in% team_choices) {
    "Boston Celtics"
  } else {
    team_choices[[1]]
  }
  
  
  # ----------------------------------------------------------
  # Application theme
  # ----------------------------------------------------------
  
  app_theme <- bslib::bs_theme(
    version = 5,
    bg = "#08111f",
    fg = "#f5f7fb",
    primary = "#4f8cff",
    secondary = "#9aa7bd",
    success = "#35d399",
    danger = "#ff667a",
    warning = "#f6c85f",
    base_font = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Space Grotesk")
  )
  
  
  # ----------------------------------------------------------
  # Page
  # ----------------------------------------------------------
  
  bslib::page_sidebar(
    title = shiny::div(
      class = "tbi-top-brand",
      
      shiny::span(
        class = "tbi-mark",
        "TBI"
      ),
      
      shiny::div(
        class = "tbi-top-brand-copy",
        
        shiny::div(
          class = "tbi-product-name",
          "Thompson's Basketball Intelligence"
        ),
        
        shiny::div(
          class = "tbi-product-subtitle",
          "Basketball Operations Platform"
        )
      )
    ),
    
    window_title = "Thompson's Basketball Intelligence",
    theme = app_theme,
    
    # Important: FALSE prevents the dashboard panel from being
    # clipped or collapsed inside the fillable page container.
    fillable = FALSE,
    
    sidebar = bslib::sidebar(
      width = 310,
      open = "desktop",
      class = "tbi-sidebar",
      
      shiny::div(
        class = "tbi-sidebar-inner",
        
        shiny::div(
          class = "tbi-brand-block",
          
          shiny::div(
            class = "tbi-logo-orbit",
            shiny::span("TBI")
          ),
          
          shiny::div(
            class = "tbi-brand-copy",
            
            shiny::strong(
              "THOMPSON'S"
            ),
            
            shiny::span(
              "BASKETBALL INTELLIGENCE"
            )
          )
        ),
        
        shiny::div(
          class = "tbi-eyebrow",
          "ORGANIZATION"
        ),
        
        shiny::selectInput(
          inputId = "selected_team",
          label = NULL,
          choices = team_choices,
          selected = default_team,
          width = "100%"
        ),
        
        shiny::div(
          class = "tbi-context-strip",
          
          shiny::span(
            class = "tbi-live-dot"
          ),
          
          shiny::span(
            "Decision-support workspace"
          )
        ),
        
        shiny::hr(),
        
        shiny::div(
          class = "tbi-sidebar-note",
          
          shiny::div(
            class = "tbi-eyebrow",
            "PLATFORM STATUS"
          ),
          
          shiny::div(
            class = "tbi-status-row",
            shiny::span("Data environment"),
            shiny::strong("Connected")
          ),
          
          shiny::div(
            class = "tbi-status-row",
            shiny::span("Build"),
            shiny::strong("v1.1")
          ),
          
          shiny::div(
            class = "tbi-status-row",
            shiny::span("Owner"),
            shiny::strong("Danny Thompson")
          )
        )
      )
    ),
    
    shiny::tags$head(
      shiny::tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "tbi-assets/tbi.css?v=1.1.5"
      ),
      
      shiny::tags$meta(
        name = "theme-color",
        content = "#08111f"
      )
    ),
    
    shiny::mainPanel(
      width = 12,
      
      shiny::div(
        class = "tbi-app-shell container-fluid",
        
        bslib::navset_pill_list(
          id = "primary_navigation",
          widths = c(2.5, 9.5),
          well = FALSE,
          
          bslib::nav_panel(
            title = shiny::tagList(
              bsicons::bs_icon("speedometer2"),
              shiny::span("Executive Dashboard")
            ),
            
            shiny::div(
              class = "tbi-page-content",
              mod_executive_dashboard_ui(
                "executive_dashboard"
              )
            )
          ),
          
          bslib::nav_panel(
            title = shiny::tagList(
              bsicons::bs_icon("building"),
              shiny::span("Team Overview")
            ),
            
            shiny::div(
              class = "tbi-page-content",
              mod_team_overview_ui(
                "team_overview"
              )
            )
          ),
          
          bslib::nav_panel(
            title = shiny::tagList(
              bsicons::bs_icon("people"),
              shiny::span("Roster Intelligence")
            ),
            
            shiny::div(
              class = "tbi-page-content",
              mod_roster_contracts_ui(
                "roster_contracts"
              )
            )
          ),
          
          bslib::nav_panel(
            title = shiny::tagList(
              bsicons::bs_icon("cash-stack"),
              shiny::span("Salary Cap Intelligence")
            ),
            
            shiny::div(
              class = "tbi-page-content",
              mod_salary_cap_ui(
                "salary_cap"
              )
            )
          ),
          
          bslib::nav_panel(
            title = shiny::tagList(
              bsicons::bs_icon("arrow-left-right"),
              shiny::span("Trade Intelligence")
            ),
            
            shiny::div(
              class = "tbi-page-content",
              mod_trade_analyzer_ui(
                "trade_analyzer"
              )
            )
          ),
          
          bslib::nav_panel(
            title = shiny::tagList(
              bsicons::bs_icon("graph-up-arrow"),
              shiny::span("Five-Year Outlook")
            ),
            
            shiny::div(
              class = "tbi-page-content",
              mod_five_year_outlook_ui(
                "five_year_outlook"
              )
            )
          ),
          
          bslib::nav_panel(
            title = shiny::tagList(
              bsicons::bs_icon("calendar3"),
              shiny::span("Draft Intelligence")
            ),
            
            shiny::div(
              class = "tbi-page-content",
              mod_draft_assets_ui(
                "draft_assets"
              )
            )
          ),
          
          bslib::nav_panel(
            title = shiny::tagList(
              bsicons::bs_icon("calculator"),
              shiny::span("Extension Simulator")
            ),
            
            shiny::div(
              class = "tbi-page-content",
              mod_extension_simulator_ui(
                "extension_simulator"
              )
            )
          )
        )
      )
    )
  )
}