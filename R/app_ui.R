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
  
  team_choices <- sort(
    unique(teams_master$team_name)
  )
  
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
  
  bslib::page_fillable(
    title = "Thompson's Basketball Intelligence",
    theme = app_theme,
    fillable_mobile = TRUE,
    
    shiny::tags$head(
      shiny::includeCSS(
        app_sys("app/www/tbi.css")
      ),
      
      shiny::tags$script(
        shiny::HTML(
          "
          Shiny.addCustomMessageHandler(
            'tbi-set-navigation',
            function(message) {

              document
                .querySelectorAll('.tbi-nav-link')
                .forEach(function(link) {
                  link.classList.remove('active');
                });

              var activeLink = document.getElementById(message.activeId);

              if (activeLink) {
                activeLink.classList.add('active');
              }

              var pageTitle = document.getElementById('tbi_page_title');

              if (pageTitle) {
                pageTitle.textContent = message.title;
              }

              window.scrollTo({
                top: 0,
                behavior: 'smooth'
              });
            }
          );
          "
        )
      ),
      
      shiny::tags$meta(
        name = "theme-color",
        content = "#08111f"
      )
    ),
    
    shiny::div(
      class = "tbi-product-shell",
      
      # ------------------------------------------------------
      # Permanent left navigation
      # ------------------------------------------------------
      
      shiny::tags$aside(
        class = "tbi-product-sidebar",
        
        shiny::div(
          class = "tbi-sidebar-header",
          
          shiny::div(
            class = "tbi-sidebar-logo",
            "TBI"
          ),
          
          shiny::div(
            class = "tbi-sidebar-brand",
            
            shiny::strong(
              "THOMPSON'S"
            ),
            
            shiny::span(
              "Basketball Intelligence"
            )
          )
        ),
        
        shiny::tags$nav(
          class = "tbi-primary-navigation",
          
          shiny::div(
            class = "tbi-nav-section-label",
            "OVERVIEW"
          ),
          
          shiny::actionLink(
            inputId = "nav_executive",
            label = shiny::tagList(
              bsicons::bs_icon("speedometer2"),
              shiny::span("Executive Dashboard")
            ),
            class = "tbi-nav-link active"
          ),
          
          shiny::actionLink(
            inputId = "nav_team",
            label = shiny::tagList(
              bsicons::bs_icon("building"),
              shiny::span("Team Overview")
            ),
            class = "tbi-nav-link"
          ),
          
          shiny::div(
            class = "tbi-nav-section-label",
            "ROSTER"
          ),
          
          shiny::actionLink(
            inputId = "nav_roster",
            label = shiny::tagList(
              bsicons::bs_icon("person-lines-fill"),
              shiny::span("Roster Intelligence")
            ),
            class = "tbi-nav-link"
          ),
          
          shiny::div(
            class = "tbi-nav-section-label",
            "FINANCE"
          ),
          
          shiny::actionLink(
            inputId = "nav_salary",
            label = shiny::tagList(
              bsicons::bs_icon("currency-dollar"),
              shiny::span("Salary Cap Intelligence")
            ),
            class = "tbi-nav-link"
          ),
          
          shiny::actionLink(
            inputId = "nav_extension",
            label = shiny::tagList(
              bsicons::bs_icon("calculator"),
              shiny::span("Extension Simulator")
            ),
            class = "tbi-nav-link"
          ),
          
          shiny::div(
            class = "tbi-nav-section-label",
            "TEAM BUILDING"
          ),
          
          shiny::actionLink(
            inputId = "nav_trade",
            label = shiny::tagList(
              bsicons::bs_icon("arrow-left-right"),
              shiny::span("Trade Intelligence")
            ),
            class = "tbi-nav-link"
          ),
          
          shiny::actionLink(
            inputId = "nav_draft",
            label = shiny::tagList(
              bsicons::bs_icon("calendar-event"),
              shiny::span("Draft Intelligence")
            ),
            class = "tbi-nav-link"
          ),
          
          shiny::actionLink(
            inputId = "nav_outlook",
            label = shiny::tagList(
              bsicons::bs_icon("graph-up-arrow"),
              shiny::span("Five-Year Outlook")
            ),
            class = "tbi-nav-link"
          )
        ),
        
        shiny::div(
          class = "tbi-sidebar-footer",
          
          shiny::div(
            class = "tbi-sidebar-team",
            
            shiny::div(
              class = "tbi-sidebar-footer-label",
              "CURRENT ORGANIZATION"
            ),
            
            shiny::strong(
              shiny::textOutput(
                outputId = "sidebar_team_name",
                inline = TRUE
              )
            )
          ),
          
          shiny::div(
            class = "tbi-user-summary",
            
            shiny::div(
              class = "tbi-user-avatar",
              "DT"
            ),
            
            shiny::div(
              class = "tbi-user-copy",
              
              shiny::strong(
                "Danny Thompson"
              ),
              
              shiny::span(
                "Basketball Operations"
              )
            )
          )
        )
      ),
      
      # ------------------------------------------------------
      # Right side of application
      # ------------------------------------------------------
      
      shiny::tags$section(
        class = "tbi-product-main",
        
        # ----------------------------------------------------
        # Top application header
        # ----------------------------------------------------
        
        shiny::tags$header(
          class = "tbi-product-header",
          
          shiny::div(
            class = "tbi-header-title-group",
            
            shiny::h1(
              id = "tbi_page_title",
              "Executive Dashboard"
            ),
            
            shiny::span(
              class = "tbi-header-subtitle",
              "Basketball Operations Decision Support"
            )
          ),
          
          shiny::div(
            class = "tbi-header-controls",
            
            shiny::div(
              class = "tbi-header-filter",
              
              shiny::span(
                class = "tbi-header-filter-label",
                "Organization"
              ),
              
              shiny::selectInput(
                inputId = "selected_team",
                label = NULL,
                choices = team_choices,
                selected = default_team,
                width = "220px"
              )
            ),
            
            shiny::div(
              class = "tbi-header-filter",
              
              shiny::span(
                class = "tbi-header-filter-label",
                "Season"
              ),
              
              shiny::selectInput(
                inputId = "selected_season",
                label = NULL,
                choices = c(
                  "2026-27"
                ),
                selected = "2026-27",
                width = "130px"
              )
            ),
            
            shiny::tags$button(
              type = "button",
              class = "tbi-header-icon-button",
              title = "Notifications",
              bsicons::bs_icon("bell")
            ),
            
            shiny::tags$button(
              type = "button",
              class = "tbi-header-icon-button",
              title = "Settings",
              bsicons::bs_icon("gear")
            )
          )
        ),
        
        # ----------------------------------------------------
        # Main data workspace
        # ----------------------------------------------------
        
        shiny::tags$main(
          class = "tbi-product-workspace",
          
          bslib::navset_hidden(
            id = "primary_navigation",
            selected = "executive",
            
            bslib::nav_panel(
              title = "Executive Dashboard",
              value = "executive",
              
              shiny::div(
                class = "tbi-page-content",
                
                mod_executive_dashboard_ui(
                  "executive_dashboard"
                )
              )
            ),
            
            bslib::nav_panel(
              title = "Team Overview",
              value = "team",
              
              shiny::div(
                class = "tbi-page-content",
                
                mod_team_overview_ui(
                  "team_overview"
                )
              )
            ),
            
            bslib::nav_panel(
              title = "Roster Intelligence",
              value = "roster",
              
              shiny::div(
                class = "tbi-page-content",
                
                mod_depth_chart_ui("depth_chart"),
                
                mod_roster_contracts_ui(
                  "roster_contracts"
                  
                )
              )
            ),
            
            bslib::nav_panel(
              title = "Salary Cap Intelligence",
              value = "salary",
              
              shiny::div(
                class = "tbi-page-content",
                
                mod_salary_cap_ui(
                  "salary_cap"
                )
              )
            ),
            
            bslib::nav_panel(
              title = "Trade Intelligence",
              value = "trade",
              
              shiny::div(
                class = "tbi-page-content",
                
                mod_trade_analyzer_ui(
                  "trade_analyzer"
                )
              )
            ),
            
            bslib::nav_panel(
              title = "Five-Year Outlook",
              value = "outlook",
              
              shiny::div(
                class = "tbi-page-content",
                
                mod_five_year_outlook_ui(
                  "five_year_outlook"
                )
              )
            ),
            
            bslib::nav_panel(
              title = "Draft Intelligence",
              value = "draft",
              
              shiny::div(
                class = "tbi-page-content",
                
                mod_draft_assets_ui(
                  "draft_assets"
                )
              )
            ),
            
            bslib::nav_panel(
              title = "Extension Simulator",
              value = "extension",
              
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
  )
}