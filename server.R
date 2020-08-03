plan(multiprocess)

function(input, output, session) {
  db <- db.new()
  
  rvals <- reactiveValues(
    county_geo = NULL,
    min_date = NULL,
    max_date = NULL,
    selftrids = c(),
    navi = 0,
    nav_prev_vis = TRUE,
    nav_next_vis = TRUE,
    urlQueryParams = list()
  )
  
  db.registerCountyGeo(db, function(county_geo) {
    message("db.registerCountyGeo callback called")
    if(is.null(rvals$county_geo)) {
      rvals$county_geo <<- county_geo
    }
  })
  
  output$map <- renderLeaflet({
    getMap(rvals$county_geo)
  })
  
  db.registerActiveData(db, function(active_data) {
    message("db.registerActiveData callback called")
    rvals$min_date <<- min(active_data$date)
    rvals$max_date <<- max(active_data$date)
  })
  
  output$date_range_ui <- renderUI({
    dateRangeInput("date_range",
                   label = "Date Range",
                   start = rvals$max_date - 13,
                   end = rvals$max_date,
                   min = rvals$min_date,
                   max = rvals$max_date
    )
  })
  
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
}