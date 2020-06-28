
function(input, output, session) {
  rvals <- reactiveValues(
    covconfdf = NULL,
    dtfcovconfdf = NULL,
    covcosumdf = NULL,
    covpopdf = NULL,
    geojson = NULL,
    maprender = FALSE,
    selftrids = c(),
    notifications = list()
  )
  
  covid_confirmed <- reactive({
    if(is.null(rvals$covconfdf)) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      rvals$covconfdf <<- covid_confirmed_usa(progress)
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
    # message("Filtering covid data for date >= ",mindate)
    rvals$dtfcovconfdf <<- df[df$date >= mindate,]
  })
  
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
      "Probability of Exposure" = "Likelihood of being exposed when encountering a person",
      "N50" = "Number of people above which more likely than not been exposed",
      "New Case Trend" = 'Trend of daily "New Cases"; positive (increasing), negative (decreasing)'
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
      placement = "top"
    )
  })
  
  output$errors <- renderMenu({
    if(length(rvals$notifications) > 0) {
      dropdownMenu(type = "notifications", badgeStatus = "danger", .list = rvals$notifications)
    }
  })
  
  output$dash_plots <- renderPlotly({
    ccdf <- getCaseData()
    if(! is.null(ccdf) && nrow(ccdf) > 0) {
      withProgress({
        setProgress(message = "Building confirmed cases plot")
        # message("Building confirmed cases plot for case data with dim (",paste(dim(ccdf), collapse=","),")")
        plots <- list(confirmed_cases_plot(ccdf))
        incProgress(0.2, message = "Building new cases plot")
        # message("Building new cases plot for case data with dim (",paste(dim(ccdf), collapse=","),")")
        plots[[length(plots)+1]] <- new_cases_plot(ccdf)
        incProgress(0.2, message = "Retrieving stats")
        stdf <- getStatsData()
        if(! is.null(stdf)) {
          incProgress(0.2, message = "Building exposure plot")
          # message("Building exposure plot for stats data with dim (",paste(dim(stdf), collapse=","),")")
          plots[[length(plots)+1]] <- exposure_prob_plot(stdf)
        }
        incProgress(0.2, message = "Building composite plot")
        subplot(plots, nrows = 3, margin = c(0, 0, 0.05, 0.08), titleX = TRUE, titleY = TRUE)
      }, message = "Building plots")
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
  
  addCountyMarker <- function(featureId, geo, covcosumdf) {
    mlid <- getCountyMarkerId(featureId)
    sel <- vapply(geo$features, function(f) f$id == featureId, TRUE)
    if(any(sel)) {
      features <- geo$features[sel]
      feature <- features[[1]]
      mpoint <- get_lon_lat_center(feature)
      fcovdf <- covcosumdf[covcosumdf$county_fips==featureId,]
      lab <- HTML(paste(
        paste0("<b>County:</b>",fcovdf$county_name),
        paste0("<b>Confirmed:</b>",fcovdf$confirmed_cases),
        paste0("<b>Active:</b>",fcovdf$new_cases),
        sep = "</br>"
      ))
      leafletProxy("map") %>%
        addMarkers(lng = mpoint[1], lat = mpoint[2], layerId = mlid, label = lab)
    }
  }
  
  observeEvent(rvals$covcosumdf, {
    sfids <- rvals$selftrids
    if(length(sfids) > 0) {
      lapply(sfids, addCountyMarker, geo = county_geo(), covcosumdf = rvals$covcosumdf)
    }
  })
  
  observeEvent(input$map_geojson_click, {
    lid <- input$map_geojson_click$featureId
    # message("Feature with feature id ",lid," clicked")
    mlid <- getCountyMarkerId(lid)
    sfids <- rvals$selftrids
    if(lid %in% sfids) {
      sfids <- sfids[sfids != lid]
      leafletProxy("map") %>%
        removeMarker(mlid)
    } else {
      sfids <- c(sfids, lid)
      addCountyMarker(lid, geo = county_geo(), covcosumdf = rvals$covcosumdf)
    }
    rvals$selftrids <<- sfids
  })
  
  observeEvent(input$map_marker_click, {
    mlid <- input$map_marker_click$id
    fid <- sub("^marker_", "", mlid)
    # message("Removing marker ",mlid)
    leafletProxy("map") %>%
      removeMarker(mlid)
    rvals$selftrids <<- rvals$selftrids[rvals$selftrids != fid]
  })
  
  observeEvent(input$clear_markers, {
    if(length(rvals$selftrids) > 0) {
      map <- leafletProxy("map")
      lapply(rvals$selftrids, function(fid) {
        mlid <- paste0("marker_",fid)
        map %>% removeMarker(mlid)
      })
      rvals$selftrids <<- c()
    }
  })
  
  observeEvent(input$zipcode, {
    if(! is.null(input$zipcode)) {
      if(nchar(input$zipcode) >= 5) {
        zc <- sub("-.+$", "", input$zipcode)
        lldf <- get_zipcode_lon_lat(zc)
        if(nrow(lldf) > 0) {
          leafletProxy("map") %>%
            setView(lng = lldf$lon, lat = lldf$lat, zoom = 8)
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
  
}