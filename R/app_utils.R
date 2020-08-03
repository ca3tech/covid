
# getMap <- function(db, progress=NULL) {
#   .notify(progress, 0, "Building map")
#   m <- leaflet() %>%
#     setView(lng = -98.583, lat = 39.833, zoom = 3) %>%
#     addTiles() %>%
#     addGeoJSON(db.getCountyGeo(db))
#   .notify(progress, 0.75, "Rendering map")
#   m
# }
getMap <- function(county_geo) {
  if(! is.null(county_geo)) {
    leaflet() %>%
      setView(lng = -98.583, lat = 39.833, zoom = 3) %>%
      addTiles() %>%
      addGeoJSON(county_geo)
  }
}

# Manage map markers
updateCountyMarkers <- function(db, ftrids, sfids, center=FALSE, progress=NULL) {
  lapply(ftrids, function(ftrid) {
    sfids <<- updateCountyMarker(db, ftrid, sfids, center, progress)
  })
  sfids
}

updateCountyMarker <- function(db, ftrid, sfids, center=FALSE, progress=NULL) {
  mlid <- getCountyMarkerId(ftrid)
  if(ftrid %in% sfids) {
    sfids <- sfids[sfids != ftrid]
    db.excludeCounties(db, ftrid, progress=progress)
    leafletProxy("map") %>%
      removeMarker(mlid)
  } else {
    sfids <- c(sfids, ftrid)
    db.includeCounties(db, ftrid, progress=progress)
    addCountyMarker(ftrid, geo=db.getCountyGeo(db),
                    covcosumdf=db.getCountySummaryData(db),
                    center=center)
  }
  sfids
}

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
    # fcovdf <- covcosumdf[covcosumdf$county_fips==featureId,]
    # if(nrow(fcovdf) == 0) {
    #   message("ERROR: No county summary found for feature ",featureId)
    # }
    # lab <- HTML(paste(
    #   paste0("<b>County:</b>",fcovdf$county_name),
    #   paste0("<b>Confirmed:</b>",fcovdf$confirmed_cases),
    #   paste0("<b>Active:</b>",fcovdf$active_case_est),
    #   ifelse("active_rank" %in% colnames(fcovdf), paste0("<b>Active Rank:</b>",fcovdf$active_rank), ""),
    #   paste0("<b>Deaths:</b>",fcovdf$confirmed_deaths),
    #   sep = "</br>"
    # ))
    lab <- HTML(paste(
      paste0("<b>County:</b>",feature$properties["county_name"]),
      paste0("<b>Confirmed:</b>",feature$properties["confirmed_cases"]),
      paste0("<b>Active:</b>",feature$properties["active_case_est"]),
      ifelse("active_rank" %in% names(feature$properties), paste0("<b>Active Rank:</b>",feature$properties["active_rank"]), ""),
      paste0("<b>Deaths:</b>",feature$properties["confirmed_deaths"]),
      sep = "</br>"
    ))
    m <- leafletProxy("map") %>%
      addMarkers(lng = mpoint[1], lat = mpoint[2], layerId = mlid, label = lab)
    if(center) {
      m %>% flyTo(lng = mpoint[1], mpoint[2], zoom = 8)
    }
  }
}

# I found a feature that had a geometry$coordinates
# value that did not match the expected structure
# resulting in an application crash. This fixes that.
.fix_feature_coords <- function(feature) {
  if(length(feature$geometry$coordinates) > 1) {
    if(all(vapply(feature$geometry$coordinates, length, 1) > 1)) {
      feature$geometry$coordinates <- lapply(feature$geometry$coordinates, list)
      feature$geometry$type <- "MultiPolygon"
    }
  }
  feature
}

.pt_lst_to_df <- function(ptlist) {
  plyr::ldply(ptlist, function(pt) data.frame(lon = pt[1], lat = pt[2]))
}

.geo_coord_to_points <- function(coords) {
  if(length(coords) == 1) {
    .pt_lst_to_df(coords[[1]])
  } else {
    lapply(coords, .geo_coord_to_points)
  }
}

get_lon_lat_center <- function(feature) {
  feature <- .fix_feature_coords(feature)
  pts <- .geo_coord_to_points(feature$geometry$coordinates)
  if(is.data.frame(pts)) {
    as.numeric(geosphere::centroid(pts))
  } else {
    # There is more than one region
    # Find the region with the greatest area
    areas <- vapply(pts, geosphere::areaPolygon, 1.0)
    armax <- max(areas)
    imax <- which(vapply(areas, function(a) a == armax, TRUE))
    # Return the centroid of the largest region
    as.numeric(geosphere::centroid(pts[[imax[1]]]))
  }
}
##

# Plot generation

confirmedCasesPlot <- function(db) {
  plot_ly(db.getDateSummaryData(db), x = ~date, y = ~confirmed_cases, type = "scatter", mode = "lines", color = I("blue"), hoverinfo = "y") %>%
    layout(plot_bgcolor = "#888",
           xaxis = list(title = "Date", tickangle = -45),
           yaxis = list(title = "Confirmed Cases")
    ) %>%
    config(displayModeBar = FALSE)
}

newCasesPlot <- function(db) {
  dsumdf <- db.getDateSummaryData(db)
  fit <- stats::lm(new_cases ~ date, data = dsumdf)
  plot_ly(dsumdf, x = ~date, y = ~new_cases, type = "scatter", mode = "lines", color = I("blue"), hoverinfo = "y") %>%
    add_lines(x = ~date, y = fitted(fit), color = I("yellow")) %>%
    layout(plot_bgcolor = "#888", showlegend = FALSE,
           xaxis = list(title = "Date", tickangle = -45),
           yaxis = list(title = "New Cases")
    ) %>%
    config(displayModeBar = FALSE)
}

confirmedDeathsPlot <- function(db) {
  plot_ly(db.getDateSummaryData(db), x = ~date, y = ~confirmed_deaths, type = "scatter", mode = "lines", color = I("blue"), hoverinfo = "y") %>%
    layout(plot_bgcolor = "#888",
           xaxis = list(title = "Date", tickangle = -45),
           yaxis = list(title = "Confirmed Deaths")
    ) %>%
    config(displayModeBar = FALSE)
}

newDeathsPlot <- function(db) {
  dsumdf <- db.getDateSummaryData(db)
  fit <- stats::lm(new_deaths ~ date, data = dsumdf)
  plot_ly(dsumdf, x = ~date, y = ~new_deaths, type = "scatter", mode = "lines", color = I("blue"), hoverinfo = "y") %>%
    add_lines(x = ~date, y = fitted(fit), color = I("yellow")) %>%
    layout(plot_bgcolor = "#888", showlegend = FALSE,
           xaxis = list(title = "Date", tickangle = -45),
           yaxis = list(title = "New Deaths")
    ) %>%
    config(displayModeBar = FALSE)
}

deathRatePlot <- function(db) {
  dsumdf <- db.getDateSummaryData(db)
  dsumdf$death_rate <- 0
  sel <- dsumdf$confirmed_cases > 0
  dsumdf$death_rate[sel] <- round(dsumdf$confirmed_deaths[sel] / dsumdf$confirmed_cases[sel], 4)
  plot_ly(dsumdf, x = ~date, y = ~death_rate, type = "scatter", mode = "lines", color = I("blue"), hoverinfo = "y") %>%
    layout(plot_bgcolor = "#888",
           xaxis = list(title = "Date", tickangle = -45),
           yaxis = list(title = "Death Rate")
    ) %>%
    config(displayModeBar = FALSE)
}

computeExposureData <- function(statsdf) {
  pop <- statsdf$Population
  p_not_infected <- 1 - statsdf$Probability_of_Exposure
  n50 <- statsdf$N50
  lxmax <- as.integer(log(pop, 2))
  x <- 2 ^ (0:lxmax)
  lx <- log(x, 2)
  list(
    plot = data.frame(x = x, xlog = lx, y = 1 - (p_not_infected ^ x)),
    label = data.frame(x = log(n50, 2), y = 1 - (p_not_infected ^ n50), label = paste0("N50=",n50))
  )
}

exposureProbPlot <- function(db) {
  expdfs <- computeExposureData(db.getStatsData(db))
  pldf <- expdfs[["plot"]]
  xti <- seq.int(from = 3, to = nrow(pldf), by = 3)
  xb <- pldf$xlog[xti]
  xl <- pldf$x[xti]
  plot_ly(pldf, x = ~xlog, y = ~y, type = "scatter", mode = "lines", color = I("blue"), hoverinfo = "y") %>%
    add_text(x = ~x, y = ~y, text = ~label, data = expdfs[["label"]], color = I("yellow")) %>%
    layout(plot_bgcolor = "#888", showlegend = FALSE,
           xaxis = list(
             title = "Number of People",
             tickmode = "array",
             tickvals = xb,
             ticktext = xl,
             tickangle = -45
           ),
           yaxis = list(title = "Probability Exposed")
    ) %>%
    config(displayModeBar = FALSE)
}
##

# General utilities
statsToList <- function(statsdf) {
  sl <- as.list(statsdf)
  names(sl) <- gsub("_", " ", names(sl))
  sl
}

data(zipcode)
getZipcodeLonLat <- function(zip_codes) {
  dplyr::rename(zipcode[zipcode$zip %in% zip_codes, c("zip", "longitude", "latitude")],
                zipcode=zip, lon=longitude, lat=latitude)
}