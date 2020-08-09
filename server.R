plan(multiprocess)

function(input, output, session) {
  db <- db.new()
  
  rvals <- reactiveValues(
    map_county_geo = NULL,
    county_geo = NULL,
    county_summary_rank = NULL,
    date_summary = NULL,
    date_summary_updating = TRUE,
    stats = NULL,
    stats_updating = TRUE,
    selftrids = c(),
    navi = 0,
    nav_prev_vis = TRUE,
    nav_next_vis = TRUE,
    urlQueryParams = list(),
    notifications = list()
  )
  
  output$errors <- renderMenu({
    if(length(rvals$notifications) > 0) {
      dropdownMenu(type = "notifications", badgeStatus = "danger", .list = rvals$notifications)
    }
  })
  
  db.registerCountyGeo(db, function(county_geo) {
    if(is.null(rvals$map_county_geo)) {
      # I only do this if not already set as I can't redraw the map
      rvals$map_county_geo <<- county_geo
    }
    rvals$county_geo <<- county_geo
  })
  
  db.registerCountySummaryRankData(db, function(county_summary_rank) {
    if(is.null(rvals$county_summary_rank)) {
      rvals$county_summary_rank <<- county_summary_rank
    }
  })
  
  db.registerDateSummaryData(db, function(date_summary) {
    rvals$date_summary_updating <<- FALSE
    rvals$date_summary <<- date_summary
  })
  
  # Render map
  output$map <- renderLeaflet({
    getMap(rvals$map_county_geo)
  })
  
  output$map_ui <- renderUI({
    if(! is.null(rvals$map_county_geo)) {
      leafletOutput("map", height = "600px")
    } else {
      tags$div(
        style = "width: 40px; height: 40px; position: relative",
        spin_kit("fading-circle")
      )
    }
  })
  ##
  
  # Define entities for managing data update displays
  dataUpdatingUI <- function() {
    tags$div(
      style = "width: 40px; height: 40px; position: relative",
      spin_kit("fading-circle")
    )
  }
  
  observeEvent(
    {
      input$date_range
      input$active_days
      rvals$selftrids
    },
    {
      rvals$stats_updating <<- TRUE
      rvals$date_summary_updating <<- TRUE
    }
  )
  ##
  
  # Handle Stats Data Display
  db.registerStatsData(db, function(stats) {
    rvals$stats_updating <<- FALSE
    rvals$stats <<- stats
  })
  
  output$stats_data <- renderUI({
    if(! (is.null(rvals$stats) || rvals$stats_updating)) {
      titles <- c(
        "Active Case Estimate" = 'Sum of "New Cases"',
        "Death Rate" = 'Most recent "Confirmed Deaths" divided by "Confirmed Cases"',
        "Probability of Exposure" = "Likelihood of being exposed when encountering a person",
        "N50" = "Number of people above which more likely than not been exposed",
        "New Case Trend" = 'Trend of daily "New Cases"; positive (increasing), negative (decreasing)',
        "New Death Trend" = 'Trend of daily "New Deaths"; positive (increasing), negative (decreasing)'
      )
      sl <- statsToList(rvals$stats)
      tipify(
        tags$table(
          tagList(
            lapply(names(sl), function(key) {
              tags$tr(
                tags$td(tags$b(paste0(key,":")), style = "padding: 5px"),
                tags$td(sl[[key]],
                        title = ifelse(key %in% names(titles), titles[key], ""),
                        style = "padding: 5px")
              )
            })
          )
        ),
        title = "Summary given <b><i>Active Days Assumption</i></b> and selected counties",
        trigger = "hover",
        placement = "bottom"
      )
    } else {
      dataUpdatingUI()
    }
  })
  ##
  
  # Handle confirmed cases plot
  output$confirmed_cases_plot_ui <- renderUI({
    if(! (is.null(rvals$date_summary) || rvals$date_summary_updating)) {
      plotlyOutput("confirmed_cases_plot", height = "200px")
    } else {
      dataUpdatingUI()
    }
  })
  
  output$confirmed_cases_plot <- renderPlotly({
    confirmedCasesPlot(rvals$date_summary)
  })
  ##
  
  # Handle new cases plot
  output$new_cases_plot_ui <- renderUI({
    if(! (is.null(rvals$date_summary) || rvals$date_summary_updating)) {
      plotlyOutput("new_cases_plot", height = "200px")
    } else {
      dataUpdatingUI()
    }
  })
  
  output$new_cases_plot <- renderPlotly({
    newCasesPlot(rvals$date_summary)
  })
  ##
  
  # Handle confirmed deaths plot
  output$confirmed_deaths_plot_ui <- renderUI({
    if(! (is.null(rvals$date_summary) || rvals$date_summary_updating)) {
      plotlyOutput("confirmed_deaths_plot", height = "200px")
    } else {
      dataUpdatingUI()
    }
  })
  
  output$confirmed_deaths_plot <- renderPlotly({
    confirmedDeathsPlot(rvals$date_summary)
  })
  ##
  
  # Handle new deaths plot
  output$new_deaths_plot_ui <- renderUI({
    if(! (is.null(rvals$date_summary) || rvals$date_summary_updating)) {
      plotlyOutput("new_deaths_plot", height = "200px")
    } else {
      dataUpdatingUI()
    }
  })
  
  output$new_deaths_plot <- renderPlotly({
    newDeathsPlot(rvals$date_summary)
  })
  ##
  
  # Handle death rate plot
  output$death_rate_plot_ui <- renderUI({
    if(! (is.null(rvals$date_summary) || rvals$date_summary_updating)) {
      plotlyOutput("death_rate_plot", height = "200px")
    } else {
      dataUpdatingUI()
    }
  })
  
  output$death_rate_plot <- renderPlotly({
    deathRatePlot(rvals$date_summary)
  })
  ##
  
  # Handle exposure probability plot
  output$exposure_prob_plot_ui <- renderUI({
    if(! (is.null(rvals$stats) || rvals$stats_updating)) {
      plotlyOutput("exposure_prob_plot", height = "200px")
    } else {
      dataUpdatingUI()
    }
  })
  
  output$exposure_prob_plot <- renderPlotly({
    exposureProbPlot(rvals$stats)
  })
  ##
  
  # Handle display of selected counties
  output$co_selected <- renderUI({
    tbl <- tags$table(tags$thead(tags$tr(tags$th("Selected Counties", colspan=2))))
    if(! is.null(rvals$county_geo) && length(rvals$selftrids) > 0) {
      tbod <- tags$tbody()
      geo <- rvals$county_geo
      sel <- vapply(geo$features, function(f) f$id %in% rvals$selftrids, TRUE)
      conames <- sort(vapply(geo$features[sel], function(f) {
        paste(f$properties$county_name, f$properties$state_name)
      }, "string"))
      lapply(seq(from=1, to=length(conames), by=2), function(i) {
        trow <- tags$tr(tags$td(conames[i], style = "padding: 5px"))
        if(i < length(conames)) {
          trow <- tagAppendChild(trow, tags$td(conames[i+1]))
        }
        tbod <<- tagAppendChild(tbod, trow)
      })
      tbl <- tagAppendChild(tbl, tbod)
    }
    tbl
  })
  
  # Handle date range IO
  db.registerActiveData(db, function(active_data) {
    sdt <- min(active_data$date)
    edt <- max(active_data$date)
    updateDateRangeInput(
      session = session,
      inputId = "date_range",
      start = edt - 13,
      end = edt,
      min = sdt,
      max = edt
    )
  })
  
  observeEvent(input$date_range, {
    db.setMinDate(db, input$date_range[1])
    db.setMaxDate(db, input$date_range[2])
  })
  ##
  
  # Handle active days assumption changes
  observeEvent(input$active_days, {
    db.setActiveDays(db, input$active_days)
  })
  ##
  
  # Handle Date Range changes
  observeEvent(input$date_range, {
    db.setMinDate(db, input$date_range[1])
    db.setMaxDate(db, input$date_range[2])
  })
  ##
  
  # Handle zipcode selection
  observeEvent(input$zipcode, {
    if(! is.null(input$zipcode)) {
      if(nchar(input$zipcode) >= 5) {
        resetNavigation()
        zc <- sub("-.+$", "", input$zipcode)
        lldf <- getZipcodeLonLat(zc)
        if(nrow(lldf) > 0) {
          leafletProxy("map") %>%
            flyTo(lng = lldf$lon, lat = lldf$lat, zoom = 8)
        } else {
          rvals$notifications[[length(rvals$notifications)+1]] <<-
            notificationItem(paste("Invalid Zip Code", zc), status = "warning")
          updateTextInput(session, "zipcode", value = "")
        }
      }
    }
  })
  ##
  
  # Handle map selections
  db.registerCounties(db, function(counties) {
    rvals$selftrids <<- counties
  })
  
  resolveSelMarkers <- function(lid, selids) {
    if(lid %in% selids) {
      selids <- selids[selids != lid]
    } else {
      selids <- c(selids, lid)
    }
    selids
  }
  
  observeEvent(input$map_geojson_click, {
    lid <- as.character(input$map_geojson_click$featureId)
    selids <- resolveSelMarkers(lid, rvals$selftrids)
    db.setCounties(db, selids)
  })
  
  observeEvent(input$map_marker_click, {
    mlid <- input$map_marker_click$id
    lid <- sub("^marker_", "", mlid)
    selids <- resolveSelMarkers(lid, rvals$selftrids)
    db.setCounties(db, selids)
  })
  
  curmrkids <- reactiveVal(c())
  observeEvent(rvals$selftrids, {
    selids <- rvals$selftrids
    cmids <- curmrkids()
    if(length(cmids) > 0) {
      # remove any already selected so that we don't pass to
      # updateCountyMarkers which will unselect them
      selids <- selids[! selids %in% cmids]
      sel <- ! cmids %in% rvals$selftrids
      if(any(sel)) {
        # I need to add marker ids that were selected, but
        # no longer are as updateCountyMarkers will remove
        # any ids in the first parameter that are in the
        # second parameter
        selids <- c(selids, cmids[sel])
      }
    }
    updateCountyMarkers(selids, cmids,
                        county_geo = rvals$county_geo,
                        center = FALSE)
    curmrkids(rvals$selftrids)
  })
  ##
  
  # Handle markers
  clear_markers <- function(ftrids) {
    if(length(ftrids) > 0) {
      map <- leafletProxy("map")
      lapply(ftrids, function(fid) {
        mlid <- paste0("marker_",fid)
        map <- map %>% removeMarker(mlid)
      })
      curmrkids(c())
      db.setCounties(db, rvals$selftrids[! rvals$selftrids %in% ftrids])
    }
  }
  
  observeEvent(
    {
      input$clear_markers
      input$active_days
      input$date_range
    },
    {
      resetNavigation()
      clear_markers(rvals$selftrids)
    }
  )
  ##

  # Enable navigating by active case
  observeEvent(input$nav_next, {
    if(rvals$navi < nrow(rvals$county_summary_rank)) {
      rvals$navi <<- rvals$navi + 1
    }
  })
  
  observeEvent(input$nav_prev, {
    if(rvals$navi > 1) {
      rvals$navi <<- rvals$navi - 1
    }
  })
  
  observeEvent(rvals$navi, {
    if(rvals$navi > 0) {
      df <- rvals$county_summary_rank
      clear_markers(rvals$selftrids)
      db.setCounties(db,
         updateCountyMarker(df$county_fips[rvals$navi], rvals$selftrids,
                            county_geo = rvals$county_geo,
                            center = TRUE)
      )
    }
  })
  
  observeEvent(rvals$navi, {
    df <- rvals$county_summary_rank
    if((rvals$navi < 2 && rvals$nav_prev_vis)
       || (rvals$navi > 1 && ! rvals$nav_prev_vis)) {
      session$sendCustomMessage("toggleVisibility", "nav_prev")
      rvals$nav_prev_vis <<- ! rvals$nav_prev_vis
    } else if((rvals$navi >= nrow(df) && rvals$nav_next_vis)
              || (rvals$navi < nrow(df) && ! rvals$nav_next_vis)) {
      session$sendCustomMessage("toggleVisibility", "nav_next")
      rvals$nav_next_vis <<- ! rvals$nav_next_vis
    }
  })
  
  resetNavigation <- function() {
    if(rvals$navi > 0) {
      df <- rvals$county_summary_rank
      clear_markers(df$county_fips[rvals$navi])
      rvals$navi <<- 0
    }
  }
  ##
  
  # Add GET parameters for bookmarking
  observeEvent(rvals$urlQueryParams, {
    qstr <- paste(vapply(names(rvals$urlQueryParams), function(key) {
      paste(key, rvals$urlQueryParams[[key]], sep = "=")
    }, "string"), collapse = "&")
    updateQueryString(paste0("?",qstr), session = session)
  })
  
  observeEvent(input$active_days, {
    rvals$urlQueryParams[["active_days"]] <<- input$active_days
  })
  
  observeEvent(input$zipcode, {
    val <- input$zipcode
    if(val == "") {
      val <- NULL
    }
    rvals$urlQueryParams[["zipcode"]] <<- val
  })
  
  observe({
    val <- NULL
    if(length(rvals$selftrids) > 0) {
      val <- paste(rvals$selftrids, collapse = ",")
    }
    isolate({
      rvals$urlQueryParams[["feature_ids"]] <<- val
    })
  })
  
  # Handle GET parameters
  # These inputs are set in covid.js
  qfeature_ids_updated <- reactiveVal(FALSE)
  observe({
    if(! (qfeature_ids_updated() || is.null(input$qfeature_ids) || is.null(input$map_rendered))) {
      db.setCounties(db, as.character(unlist(strsplit(input$qfeature_ids, ",", fixed = TRUE))))
      qfeature_ids_updated(TRUE)
    }
  })
  
  observeEvent(input$qzipcode, {
    updateTextInput(session, "zipcode", value = input$qzipcode)
  })
  
  observeEvent(input$qactive_days, {
    updateSliderInput(session, "active_days", value = as.integer(input$qactive_days))
  })
}