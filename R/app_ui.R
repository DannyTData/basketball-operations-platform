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
  # Theme
  # ----------------------------------------------------------
  
  app_theme <- bslib::bs_theme(
    version = 5,
    bg = "#0b0f17",
    fg = "#eef2f7",
    primary = "#2563eb",
    secondary = "#94a3b8",
    success = "#22c55e",
    danger = "#ef4444",
    warning = "#f59e0b",
    base_font = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter")
  )
  
  # ----------------------------------------------------------
  # Navigation helper
  # ----------------------------------------------------------
  
  nav_item <- function(id, icon, label) {
    shiny::actionLink(
      inputId = id,
      label = shiny::tagList(
        shiny::span(
          class = "tbi-v2-nav-icon",
          bsicons::bs_icon(icon)
        ),
        shiny::span(
          class = "tbi-v2-nav-text",
          label
        )
      ),
      class = paste(
        "tbi-nav-link",
        if (identical(id, "nav_executive")) "active" else ""
      )
    )
  }
  
  nav_group <- function(label, ...) {
    shiny::tagList(
      shiny::div(
        class = "tbi-v2-nav-group-label",
        label
      ),
      ...
    )
  }
  
  # ----------------------------------------------------------
  # Page
  # ----------------------------------------------------------
  
  bslib::page_fillable(
    title = "TBI | Thompson's Basketball Intelligence",
    theme = app_theme,
    fillable_mobile = TRUE,
    
    shiny::tags$head(
      shiny::includeCSS(
        app_sys("app/www/tbi.css")
      ),
      
      shiny::includeCSS(
        app_sys("app/www/tbi_v2.css")
      ),

      shiny::includeCSS(
        app_sys("app/www/tbi_ux_foundation.css")
      ),

      shiny::tags$script(
        src = "tbi-assets/tbi_ux_foundation.js",
        defer = NA
      ),
      
      shiny::includeCSS(
        app_sys("app/www/tbi_phase14.css")
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

              var workspace = document.querySelector('.tbi-product-workspace');

              if (workspace) {
                workspace.scrollTo({
                  top: 0,
                  behavior: 'smooth'
                });
              }
            }
          );
          "
        )
      ),
      
      shiny::tags$meta(
        name = "theme-color",
        content = "#0b0f17"
      )
    ),
    
    shiny::div(
      class = "tbi-product-shell tbi-v2-shell tbi-phase14-shell",
      
      # ------------------------------------------------------
      # Sidebar
      # ------------------------------------------------------
      
      shiny::tags$aside(
        class = "tbi-product-sidebar tbi-v2-sidebar",
        
        shiny::div(
          class = "tbi-v2-brand",
          
          shiny::div(
            class = "tbi-v2-brand-mark",
            shiny::span("TBI")
          ),
          
          shiny::div(
            class = "tbi-v2-brand-copy",
            shiny::strong("THOMPSON'S"),
            shiny::span("Basketball Intelligence")
          )
        ),
        
        shiny::div(
          class = "tbi-v2-workspace-chip",
          shiny::span(class = "tbi-v2-live-dot"),
          shiny::span("LIVE WORKSPACE")
        ),
        
        shiny::tags$nav(
          class = "tbi-primary-navigation tbi-v2-navigation",
          
          nav_group(
            "EXECUTIVE",
            nav_item(
              "nav_executive",
              "grid-1x2-fill",
              "Command Center"
            ),
            nav_item(
              "nav_team",
              "building",
              "Team Overview"
            )
          ),
          
          nav_group(
            "ROSTER",
            nav_item(
              "nav_roster",
              "people",
              "Roster Intelligence"
            ),
            nav_item(
              "nav_depth",
              "diagram-3",
              "Depth Chart"
            ),
            nav_item(
              "nav_player_manager",
              "person-badge",
              "Player Management"
            )
          ),
          
          nav_group(
            "FINANCIAL",
            nav_item(
              "nav_salary",
              "cash-stack",
              "Cap Intelligence"
            ),
            nav_item(
              "nav_extension",
              "calculator",
              "Extension Simulator"
            )
          ),
          
          nav_group(
            "TRANSACTIONS",
            nav_item(
              "nav_trade",
              "arrow-left-right",
              "Trade Intelligence"
            ),
            nav_item(
              "nav_draft",
              "calendar3",
              "Draft Intelligence"
            )
          ),
          
          nav_group(
            "STRATEGY",
            nav_item(
              "nav_outlook",
              "graph-up-arrow",
              "Five-Year Outlook"
            )
          ),
          
          nav_group(
            "REFERENCE",
            nav_item(
              "nav_cba_glossary",
              "book",
              "CBA Info Hub"
            )
          )
        ),
        
        shiny::div(
          class = "tbi-sidebar-footer tbi-v2-sidebar-footer",
          
          shiny::div(
            class = "tbi-v2-org-block",
            shiny::span(
              class = "tbi-v2-org-label",
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
            class = "tbi-v2-user",
            shiny::div(
              class = "tbi-v2-avatar",
              "DT"
            ),
            shiny::div(
              class = "tbi-v2-user-copy",
              shiny::strong("Danny Thompson"),
              shiny::span("Basketball Operations")
            )
          )
        )
      ),
      
      # ------------------------------------------------------
      # Main application
      # ------------------------------------------------------
      
      shiny::tags$section(
        class = "tbi-product-main tbi-v2-main",
        
        # ----------------------------------------------------
        # Executive header
        # ----------------------------------------------------
        
        shiny::tags$header(
          class = "tbi-product-header tbi-v2-header",
          
          shiny::div(
            class = "tbi-v2-title-wrap",
            
            shiny::div(
              class = "tbi-v2-breadcrumb",
              shiny::span("BASKETBALL OPERATIONS"),
              shiny::span(class = "tbi-v2-breadcrumb-divider", "/"),
              shiny::span(class = "tbi-v2-breadcrumb-current", "DECISION SUPPORT")
            ),
            
            shiny::h1(
              id = "tbi_page_title",
              "Executive Dashboard"
            ),
            
            shiny::div(
              class = "tbi-v2-title-meta",
              shiny::span(
                class = "tbi-v2-meta-dot"
              ),
              shiny::span(
                "Front Office Workspace"
              )
            )
          ),
          
          shiny::div(
            class = "tbi-header-controls tbi-v2-header-controls",
            
            shiny::div(
              class = "tbi-v2-filter",
              shiny::span(
                class = "tbi-v2-filter-label",
                "ORGANIZATION"
              ),
              shiny::selectInput(
                inputId = "selected_team",
                label = NULL,
                choices = team_choices,
                selected = default_team,
                width = "230px"
              )
            ),
            
            shiny::div(
              class = "tbi-v2-filter",
              shiny::span(
                class = "tbi-v2-filter-label",
                "SEASON"
              ),
              shiny::selectInput(
                inputId = "selected_season",
                label = NULL,
                choices = "2026-27",
                selected = "2026-27",
                width = "128px"
              )
            ),
            
            shiny::div(
              class = "tbi-v2-header-status",
              shiny::span(class = "tbi-v2-live-dot"),
              shiny::div(
                shiny::span("SYSTEM"),
                shiny::strong("OPERATIONAL")
              )
            )
          )
        ),
        
        # ----------------------------------------------------
        # Workspace
        # ----------------------------------------------------
        
        shiny::tags$main(
          class = "tbi-product-workspace tbi-v2-workspace",
          
          shiny::tags$style(
            shiny::HTML(
              "\\n\\n/* ========================================================\\n   PHASE 15K-B4B — DEDICATED DEPTH CHART PAGE\\n   Remove non-actionable generic BIE presentation from the\\n   Depth workspace while preserving operational tools.\\n   ======================================================== */\\n.tbi-depth-page .depth-bie-toolbar,\\n.tbi-depth-page .depth-bie-explain {\\n  display: none !important;\\n}\\n\\n.tbi-depth-page .depth-v21-shell,\\n.tbi-depth-page .depth-v21-board,\\n.tbi-depth-page .depth-v23-scenario-shell {\\n  margin-top: 0;\\n}\\n"
            )
          ),
          
          bslib::navset_hidden(
            id = "primary_navigation",
            selected = "executive",
            
            bslib::nav_panel(
              title = "Executive Dashboard",
              value = "executive",
              shiny::div(
                class = "tbi-page-content tbi-v2-page-content",
                mod_executive_dashboard_ui(
                  "executive_dashboard"
                )
              )
            ),
            
            bslib::nav_panel(
              title = "Team Overview",
              value = "team",
              shiny::div(
                class = "tbi-page-content tbi-v2-page-content",
                mod_team_overview_ui(
                  "team_overview"
                )
              )
            ),
            
            bslib::nav_panel(
              title = "Roster Intelligence",
              value = "roster",
              shiny::div(
                class = "tbi-page-content tbi-v2-page-content",
                mod_roster_contracts_ui(
                  "roster_contracts"
                )
              )
            ),
            
            bslib::nav_panel(
              title = "Depth Chart",
              value = "depth",
              shiny::div(
                class = "tbi-page-content tbi-v2-page-content tbi-depth-page",
                mod_depth_chart_ui(
                  "depth_chart"
                )
              )
            ),
            
            bslib::nav_panel(
              title = "Player Management",
              value = "player_manager",
              shiny::div(
                class = "tbi-page-content tbi-v2-page-content",
                mod_player_manager_ui(
                  "player_manager"
                )
              )
            ),
            
            bslib::nav_panel(
              title = "Salary Cap Intelligence",
              value = "salary",
              shiny::div(
                class = "tbi-page-content tbi-v2-page-content",
                mod_salary_cap_ui(
                  "salary_cap"
                )
              )
            ),
            
            bslib::nav_panel(
              title = "Trade Intelligence",
              value = "trade",
              shiny::div(
                class = "tbi-page-content tbi-v2-page-content",
                mod_trade_analyzer_ui(
                  "trade_analyzer"
                )
              )
            ),
            
            bslib::nav_panel(
              title = "Five-Year Outlook",
              value = "outlook",
              shiny::div(
                class = "tbi-page-content tbi-v2-page-content",
                mod_five_year_outlook_ui(
                  "five_year_outlook"
                )
              )
            ),
            
            bslib::nav_panel(
              title = "Draft Intelligence",
              value = "draft",
              shiny::div(
                class = "tbi-page-content tbi-v2-page-content",
                mod_draft_assets_ui(
                  "draft_assets"
                )
              )
            ),
            
            bslib::nav_panel(
              title = "Extension Simulator",
              value = "extension",
              shiny::div(
                class = "tbi-page-content tbi-v2-page-content",
                mod_extension_simulator_ui(
                  "extension_simulator"
                )
              )
            ),
            
            bslib::nav_panel(
              title = "CBA Info Hub",
              value = "cba_glossary",
              shiny::div(
                class = "tbi-page-content tbi-v2-page-content",
                mod_cba_glossary_ui(
                  "cba_glossary"
                )
              )
            )
          )
        )
      )
    )
  )
}
