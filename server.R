
function(input, output, session) {
  db <- NULL
  {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    db <- db.new(progress = progress)
  }
  
  rvals <- reactiveValues(
    selftrids = c(),
    refresh_counter = 0,
    notifications = list(),
    navi = 0,
    nav_prev_vis = TRUE,
    nav_next_vis = TRUE,
    urlQueryParams = list()
  )
  
  refresh <- function() {
    rvals$refresh_counter <<- rvals$refresh_counter + 1
  }
  
  observeEvent(input$active_days, {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    db.setActiveDaysEstimate(db, input$active_days, progress=progress)
    db.updateGeoFeatures(db, progress)
    # Reset the navigation index as the data will change
    rvals$navi <<- 0
    refresh()
  })
  
  output$map <- renderLeaflet({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    getMap(db, progress=progress)
  })
  
  output$date_range_ui <- renderUI({
    dateRangeInput("date_range",
      label = "Date Range",
      start = db.getMaxDate(db) - 13,
      end = db.getMaxDate(db),
      min = db.getMinDate(db),
      max = db.getMaxDate(db)
    )
  })
  
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
  
  getStatsList <- reactive({
    if(rvals$refresh_counter > 0) {
      statsToList(db.getStatsData(db))
    }
  })
  
  output$stats_data <- renderUI({
    if(rvals$refresh_counter > 0) {
      titles <- c(
        "Active Case Estimate" = 'Sum of "New Cases"',
        "Death Rate" = 'Most recent "Confirmed Deaths" divided by "Confirmed Cases"',
        "Probability of Exposure" = "Likelihood of being exposed when encountering a person",
        "N50" = "Number of people above which more likely than not been exposed",
        "New Case Trend" = 'Trend of daily "New Cases"; positive (increasing), negative (decreasing)',
        "New Death Trend" = 'Trend of daily "New Deaths"; positive (increasing), negative (decreasing)'
      )
      sl <- getStatsList()
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
    }
  })
  
  output$errors <- renderMenu({
    if(length(rvals$notifications) > 0) {
      dropdownMenu(type = "notifications", badgeStatus = "danger", .list = rvals$notifications)
    }
  })
  
  output$confirmed_cases_plot <- renderPlotly({
    if(rvals$refresh_counter > 0) {
      confirmedCasesPlot(db)
    }
  })
  
  output$new_cases_plot <- renderPlotly({
    if(rvals$refresh_counter > 0) {
      newCasesPlot(db)
    }
  })
  
  output$confirmed_deaths_plot <- renderPlotly({
    if(rvals$refresh_counter > 0) {
      confirmedDeathsPlot(db)
    }
  })
  
  output$new_deaths_plot <- renderPlotly({
    if(rvals$refresh_counter > 0) {
      newDeathsPlot(db)
    }
  })
  
  output$death_rate_plot <- renderPlotly({
    if(rvals$refresh_counter > 0) {
      deathRatePlot(db)
    }
  })
  
  output$exposure_prob_plot <- renderPlotly({
    if(rvals$refresh_counter > 0) {
      exposureProbPlot(db)
    }
  })
  
  output$co_selected <- renderUI({
    tbl <- tags$table(tags$thead(tags$tr(tags$th("Selected Counties", colspan=2))))
    if(length(rvals$selftrids) > 0) {
      tbod <- tags$tbody()
      geo <- db.getCountyGeo(db)
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
  
  # Handle Date Range changes
  observeEvent(input$date_range, {
    db.setMinDate(db, input$date_range[1])
    db.setMaxDate(db, input$date_range[2])
    refresh()
  })
  ##
  
  # Handle map selections
  observeEvent(input$map_geojson_click, {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    lid <- input$map_geojson_click$featureId
    rvals$selftrids <<- updateCountyMarker(db, lid, rvals$selftrids, FALSE, progress=progress)
  })
  
  observeEvent(input$map_marker_click, {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    mlid <- input$map_marker_click$id
    fid <- sub("^marker_", "", mlid)
    rvals$selftrids <<- updateCountyMarker(db, fid, rvals$selftrids, FALSE, progress=progress)
  })
  
  observeEvent(rvals$selftrids, {
    if(length(rvals$selftrids) == 0) {
      # Nothing selected so we need to clear that filter
      db.resetCounties(db)
    } else {
      db.setCounties(db, rvals$selftrids)
    }
    refresh()
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
      rvals$selftrids <<- rvals$selftrids[! rvals$selftrids %in% ftrids]
    }
  }
  
  observeEvent(input$clear_markers, {
    resetNavigation()
    clear_markers(rvals$selftrids)
  })
  
  # Enable navigating by active case
  observeEvent(input$nav_next, {
    if(rvals$navi < nrow(db.getCountySummaryRankData(db))) {
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
      df <- db.getCountySummaryRankData(db)
      clear_markers(rvals$selftrids)
      rvals$selftrids <<- updateCountyMarker(db, df$county_fips[rvals$navi], rvals$selftrids, TRUE)
    }
  })
  
  observeEvent(rvals$navi, {
    df <- db.getCountySummaryRankData(db)
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
      df <- db.getCountySummaryRankData(db)
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
  restoreMarkers <- function(ftrcsv) {
    fids <- as.character(unlist(strsplit(ftrcsv, ",", fixed = TRUE)))
    rvals$selftrids <<- updateCountyMarkers(db, fids, rvals$selftrids, FALSE, progress=progress)
  }
  
  observeEvent(input$qfeature_ids, {
    restoreMarkers(input$qfeature_ids)
  })
  
  observeEvent(input$qzipcode, {
    updateTextInput(session, "zipcode", value = input$qzipcode)
  })
  
  observeEvent(input$qactive_days, {
    updateSliderInput(session, "active_days", value = as.integer(input$qactive_days))
  })
}