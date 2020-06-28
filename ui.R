
dashboardPage(
  dashboardHeader(title = "US COVID19 Dashboard",
    dropdownMenuOutput("errors")
  ),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-line"), selected = TRUE),
      menuItem("About", tabName = "about", icon = icon("question-circle"))
    )
  ),
  dashboardBody(
    add_busy_spinner(spin = "fading-circle", timeout = 1000, position = "full-page"),
    tabItems(
      tabItem("dashboard",
        sidebarLayout(
          sidebarPanel(
            width = 8,
            style = 'height: 90vh; overflow-y: auto;',
            fluidRow(
              fixedRow(
                column(1, actionButton("clear_markers", label = NULL, icon = icon("eraser"))),
                column(2, textInput("zipcode", label = NULL, placeholder = "zip code"))
              ),
              style = "padding-bottom: 10px; padding-left: 15px"
            ),
            leafletOutput("map", height = "600px"),
            sliderInput("active_days",
              label = "Active Days Assumption",
              min = 1,
              max = 30,
              value = 14,
              step = 1
            ),
            bsTooltip("map",
              title = paste("Click +/-, pull/pinch, or scroll wheel up/down, to zoom in/out.",
                            "Click county to select, click again to unselect.",
                            "You may select more than 1 county at a time.",
                            sep = "<br/>"),
              placement = "bottom"
            ),
            bsTooltip("active_days",
              title = "Click and drag endpoint or click on bar to increase/decrease",
              placement = "top"
            ),
            bsTooltip("clear_markers",
              title = "Click to remove all selected counties",
              placement = "bottom"
            ),
            bsTooltip("zipcode",
              title = "Enter a zip code to zoom map",
              placement = "bottom"
            )
          ),
          mainPanel(
            width = 4,
            style = 'height: 90vh; overflow-y: auto;',
            fluidRow(box(plotlyOutput("dash_plots", height = "600px"), width = 12)),
            fluidRow(box(uiOutput("stats_data"), width = 12)),
            fluidRow(box(uiOutput("co_selected"), width = 12))
          )
        )
      ),
      tabItem("about",
        includeMarkdown("www/about.md")
      )
    )
  ),
  skin = "black"
)
