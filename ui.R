
dashboardPage(
  tags$head(
    tags$meta(
      property="og:site_name",
      content="COVID19 Dashboard"
    ),
    tags$meta(
      property="og:title",
      content="COVID19 Dashboard"
    ),
    tags$meta(
      property="og:description",
      content="Review COVID19 case data and you're chances of exposure"
    ),
    tags$meta(
      property="og:image",
      content="https://raw.githubusercontent.com/ca3tech/covid/master/www/preview.jpg"
    ),
    tags$meta(
      property="og:image:secure_url",
      content="https://raw.githubusercontent.com/ca3tech/covid/master/www/preview.jpg"
    ),
    tags$meta(
      property="og:image:type",
      content="image/jpeg"
    ),
    tags$meta(
      property="og:image:width",
      content="295"
    ),
    tags$meta(
      property="og:image:height",
      content="230"
    ),
    tags$meta(
      property="og:image:alt",
      content="Dashboard Preview"
    ),
    tags$script(src="covid.js")
  ),
  header = dashboardHeader(title = "US COVID19 Dashboard",
    dropdownMenuOutput("errors")
  ),
  sidebar = dashboardSidebar(
    sidebarMenu(id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-line"), selected = TRUE),
      menuItem("About", tabName = "about", icon = icon("question-circle"))
    )
  ),
  body = dashboardBody(
    add_busy_spinner(spin = "fading-circle", timeout = 1000, position = "full-page"),
    tabItems(
      tabItem("dashboard",
        sidebarLayout(
          sidebarPanel(
            width = 8,
            style = 'height: 90vh; overflow-y: auto;',
            fluidRow(
              fixedRow(
                column(2, textInput("zipcode", label = NULL, placeholder = "zip code")),
                column(1, actionButton("clear_markers", label = NULL, icon = icon("eraser"))),
                column(1, actionButton("nav_prev", label = NULL, icon = icon("chevron-left"))),
                column(1, actionButton("nav_next", label = NULL, icon = icon("chevron-right")))
              ),
              style = "padding-bottom: 10px; padding-left: 15px"
            ),
            leafletOutput("map", height = "600px"),
            uiOutput("date_range_ui"),
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
            bsTooltip("date_range_ui",
              title = "Click to select the dates date range to review",
              placement = "top"
            ),
            bsTooltip("active_days",
                      title = "Click and drag endpoint or click on bar to increase/decrease",
                      placement = "top"
            ),
            bsTooltip("zipcode",
              title = "Enter a zip code to zoom map",
              placement = "bottom"
            ),
            bsTooltip("clear_markers",
                      title = "Click to remove all selected counties",
                      placement = "bottom"
            ),
            bsTooltip("nav_next",
                      title = "Click to navigate to next most active county",
                      placement = "bottom"
            ),
            bsTooltip("nav_prev",
                      title = "Click to navigate to previous most active county",
                      placement = "bottom"
            )
          ),
          mainPanel(
            width = 4,
            style = 'height: 90vh; overflow-y: auto;',
            box(uiOutput("stats_data"), width = 12),
            box(plotlyOutput("confirmed_cases_plot", height = "200px"), width = 12, style = "padding: 2px"),
            box(plotlyOutput("new_cases_plot", height = "200px"), width = 12, style = "padding: 2px"),
            box(plotlyOutput("confirmed_deaths_plot", height = "200px"), width = 12, style = "padding: 2px"),
            box(plotlyOutput("new_deaths_plot", height = "200px"), width = 12, style = "padding: 2px"),
            box(plotlyOutput("death_rate_plot", height = "200px"), width = 12, style = "padding: 2px"),
            box(plotlyOutput("exposure_prob_plot", height = "200px"), width = 12, style = "padding: 2px"),
            box(uiOutput("co_selected"), width = 12)
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
