
function(input, output, session) {
  rvals <- reactiveValues(
    covconfdf = NULL,
    dtfcovconfdf = NULL,
    covcosumdf = NULL,
    covpopdf = NULL,
    geojson = NULL,
    maprender = FALSE,
    selftrids = c(),
    notifications = list(),
    urlQueryParams = list(),
    srtcovconfdf = NULL,
    navi = 0,
    nav_prev_vis = TRUE,
    nav_next_vis = TRUE
  )
  
  covid_confirmed <- reactive({
    if(is.null(rvals$covconfdf)) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      df <- covid_confirmed_usa(progress)
      # We don't do anything with the state level data so remove it
      rvals$covconfdf <<- df[df$county_fips != 0,]
      isolate({
        progress$set(0, "Computing county summary statistics")
        rvals$covcosumdf <<- compute_county_rollup(rvals$covconfdf)
        rvals$maprender <<- TRUE
      })
    }
    rvals$covconfdf
  })
  
  covid_population <- reactive({
    if(is.null(rvals$covpopdf)) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      rvals$covpopdf <<- covid_county_population(progress)
    }
    rvals$covpopdf
  })
  
  county_geo <- reactive({
    if(is.null(rvals$geojson)) {
      rvals$geojson <<- filter_update_geo_features(county_fips_geo(), casedf = covid_confirmed())
    }
    rvals$geojson
  })
  
  observeEvent(rvals$dtfcovconfdf, {
    withProgress({
      rvals$covcosumdf <<- compute_county_rollup(rvals$dtfcovconfdf)
      setProgress(value = 1)
    }, message = "Calculating county summary")
  })
  
  observeEvent(input$active_days, {
    df <- covid_confirmed()
    mindate <- max(df$date) - input$active_days
    rvals$dtfcovconfdf <<- df[df$date >= mindate,]
  })
  
  # Enable navigating by active case
  observeEvent(rvals$covcosumdf, {
    rvals$srtcovconfdf <<- dplyr::arrange(rvals$covcosumdf, dplyr::desc(new_cases))
    rvals$srtcovconfdf$active_rank <- 1:nrow(rvals$srtcovconfdf)
    rvals$navi <<- 0
  })
  
  observeEvent(input$nav_next, {
    if(rvals$navi < nrow(rvals$srtcovconfdf)) {
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
      clear_markers(rvals$selftrids)
      updateCountyMarker(rvals$srtcovconfdf$county_fips[rvals$navi], TRUE)
    }
  })
  
  observeEvent(rvals$navi, {
    if((rvals$navi < 2 && rvals$nav_prev_vis)
       || (rvals$navi > 1 && ! rvals$nav_prev_vis)) {
      session$sendCustomMessage("toggleVisibility", "nav_prev")
      rvals$nav_prev_vis <<- ! rvals$nav_prev_vis
    } else if((rvals$navi >= nrow(rvals$srtcovconfdf) && rvals$nav_next_vis)
              || (rvals$navi < nrow(rvals$srtcovconfdf) && ! rvals$nav_next_vis)) {
      session$sendCustomMessage("toggleVisibility", "nav_next")
      rvals$nav_next_vis <<- ! rvals$nav_next_vis
    }
  })
  
  resetNavigation <- function() {
    if(rvals$navi > 0) {
      clear_markers(rvals$srtcovconfdf$county_fips[rvals$navi])
      rvals$navi <<- 0
    }
  }
  #####
  
  getCaseData <- reactive({
    df <- rvals$dtfcovconfdf
    if(! is.null(df)) {
      withProgress({
        sids <- sel_feature_ids()
        if(length(sids) > 0) {
          # message("Filtering case data for county_fips (",paste(sids, collapse = ","),")")
          setProgress(value = 0.25,
                      message = "Filtering case data for features")
          df <- df[df$county_fips %in% sids,]
          setProgress(value = 1)
        }
      }, message = "Retrieving case data")
    }
    df
  })
  
  getStatsData <- reactive({
    withProgress({
      # message("Building case stats data...")
      ccdf <- getCaseData()
      incProgress(0.5)
      cpdf <- covid_population()
      incProgress(0.25, message = "Computing case stats data")
      sdf <- NULL
      if(! (is.null(ccdf) || is.null(cpdf) || nrow(ccdf) == 0 || nrow(cpdf) == 0)) {
        # message("Computing case stats data...")
        sdf <- compute_stats(ccdf, cpdf)
      } else {
        if(is.null(ccdf)) {
          warning("Covid case data was null")
        }
        if(is.null(cpdf)) {
          warning("Covid population data was null")
        }
      }
      setProgress(value = 1)
      sdf
    }, message = "Building case stats data")
  })
  
  getStatsList <- reactive({
    stats_to_list(getStatsData())
  })
  
  output$stats_data <- renderUI({
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
  })
  
  output$errors <- renderMenu({
    if(length(rvals$notifications) > 0) {
      dropdownMenu(type = "notifications", badgeStatus = "danger", .list = rvals$notifications)
    }
  })
  
  output$confirmed_cases_plot <- renderPlotly({
    ccdf <- getCaseData()
    if(! is.null(ccdf) && nrow(ccdf) > 0) {
      confirmed_cases_plot(ccdf)
    }
  })
  
  output$new_cases_plot <- renderPlotly({
    ccdf <- getCaseData()
    if(! is.null(ccdf) && nrow(ccdf) > 0) {
      new_cases_plot(ccdf)
    }
  })
  
  output$confirmed_deaths_plot <- renderPlotly({
    ccdf <- getCaseData()
    if(! is.null(ccdf) && nrow(ccdf) > 0) {
      confirmed_deaths_plot(ccdf)
    }
  })
  
  output$new_deaths_plot <- renderPlotly({
    ccdf <- getCaseData()
    if(! is.null(ccdf) && nrow(ccdf) > 0) {
      new_deaths_plot(ccdf)
    }
  })
  
  output$death_rate_plot <- renderPlotly({
    ccdf <- getCaseData()
    if(! is.null(ccdf) && nrow(ccdf) > 0) {
      death_rate_plot(ccdf)
    }
  })
  
  output$exposure_prob_plot <- renderPlotly({
    ccdf <- getCaseData()
    if(! is.null(ccdf) && nrow(ccdf) > 0) {
      stdf <- getStatsData()
      if(! is.null(stdf)) {
        exposure_prob_plot(stdf)
      }
    }
  })
  
  output$map <- renderLeaflet({
    if(rvals$maprender) {
      isolate({
        p <- shiny::Progress$new()
        on.exit(p$close())
        m <- get_map(county_geo(), progress = p)
        p$set(1, "Rendering map")
        m
      })
    }
  })
  
  sel_feature_ids <- reactive({
    rvals$selftrids
  })
  
  getCountyMarkerId <- function(featureId) {
    paste0("marker_",featureId)
  }
  
  addCountyMarker <- function(featureId, geo, covcosumdf, center=FALSE) {
    mlid <- getCountyMarkerId(featureId)
    sel <- vapply(geo$features, function(f) f$id == featureId, TRUE)
    if(any(sel)) {
      features <- geo$features[sel]
      feature <- features[[1]]
      mpoint <- get_lon_lat_center(feature)
      fcovdf <- covcosumdf[covcosumdf$county_fips==featureId,]
      if(nrow(fcovdf) == 0) {
        message("ERROR: No county summary found for feature ",featureId)
      }
      lab <- HTML(paste(
        paste0("<b>County:</b>",fcovdf$county_name),
        paste0("<b>Confirmed:</b>",fcovdf$confirmed_cases),
        paste0("<b>Active:</b>",fcovdf$new_cases),
        ifelse("active_rank" %in% colnames(fcovdf), paste0("<b>Active Rank:</b>",fcovdf$active_rank), ""),
        sep = "</br>"
      ))
      m <- leafletProxy("map") %>%
        addMarkers(lng = mpoint[1], lat = mpoint[2], layerId = mlid, label = lab)
      if(center) {
        m %>% flyTo(lng = mpoint[1], mpoint[2], zoom = 8)
      }
    }
  }
  
  observeEvent(rvals$covcosumdf, {
    sfids <- rvals$selftrids
    if(length(sfids) > 0) {
      lapply(sfids, addCountyMarker, geo = county_geo(), covcosumdf = rvals$covcosumdf)
    }
  })
  
  updateCountyMarker <- function(ftrid, center=FALSE) {
    mlid <- getCountyMarkerId(ftrid)
    sfids <- rvals$selftrids
    if(ftrid %in% sfids) {
      sfids <- sfids[sfids != ftrid]
      leafletProxy("map") %>%
        removeMarker(mlid)
    } else {
      sfids <- c(sfids, ftrid)
      addCountyMarker(ftrid, geo = county_geo(), covcosumdf = rvals$srtcovconfdf, center = center)
    }
    rvals$selftrids <<- sfids
  }
  
  observeEvent(input$map_geojson_click, {
    lid <- input$map_geojson_click$featureId
    # message("Feature with feature id ",lid," clicked")
    updateCountyMarker(lid)
  })
  
  observeEvent(input$map_marker_click, {
    mlid <- input$map_marker_click$id
    fid <- sub("^marker_", "", mlid)
    leafletProxy("map") %>%
      removeMarker(mlid)
    rvals$selftrids <<- rvals$selftrids[rvals$selftrids != fid]
  })
  
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
  
  observeEvent(input$zipcode, {
    if(! is.null(input$zipcode)) {
      if(nchar(input$zipcode) >= 5) {
        resetNavigation()
        zc <- sub("-.+$", "", input$zipcode)
        lldf <- get_zipcode_lon_lat(zc)
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
  
  output$co_selected <- renderUI({
    fids <- sel_feature_ids()
    tbl <-tags$table(tags$thead(tags$tr(tags$th("Selected Counties", colspan=2))))
    if(length(fids) > 0) {
      tbod <- tags$tbody()
      geo <- county_geo()
      sel <- vapply(geo$features, function(f) f$id %in% fids, TRUE)
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
    lapply(as.character(unlist(strsplit(ftrcsv, ",", fixed = TRUE))), updateCountyMarker)
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