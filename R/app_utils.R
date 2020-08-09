
getMap <- function(county_geo) {
  # The htmlwidgets::onRender was reverse engineered by looking
  # at the R leaflet GitHub code and the help for htmlwidgets.
  # It is there to facilitate adding markers to the map from
  # feature ids in the URL query parameters.
  if(! is.null(county_geo)) {
    leaflet() %>%
      setView(lng = -98.583, lat = 39.833, zoom = 3) %>%
      addTiles() %>%
      addGeoJSON(county_geo) %>%
      htmlwidgets::onRender("function(el, data, map) {
        Shiny.setInputValue('map_rendered', true);
      }")
  }
}

# Manage map markers
updateCountyMarkers <- function(ftrids, sfids, county_geo, center=FALSE, progress=NULL) {
  if(is.null(ftrids) || length(ftrids) == 0) {
    ftrids <- sfids
  }
  lapply(ftrids, function(ftrid) {
    sfids <<- updateCountyMarker(ftrid, sfids, county_geo, center, progress)
  })
  sfids
}

updateCountyMarker <- function(ftrid, sfids, county_geo, center=FALSE, progress=NULL) {
  mlid <- getCountyMarkerId(ftrid)
  if(ftrid %in% sfids) {
    sfids <- sfids[sfids != ftrid]
    leafletProxy("map") %>%
      removeMarker(mlid)
  } else {
    sfids <- c(sfids, ftrid)
    addCountyMarker(ftrid, geo=county_geo, center=center)
  }
  sfids
}

getCountyMarkerId <- function(featureId) {
  paste0("marker_",featureId)
}

addCountyMarker <- function(featureId, geo, center=FALSE) {
  mlid <- getCountyMarkerId(featureId)
  sel <- vapply(geo$features, function(f) f$id == featureId, TRUE)
  if(any(sel)) {
    features <- geo$features[sel]
    feature <- features[[1]]
    mpoint <- get_lon_lat_center(feature)
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

confirmedCasesPlot <- function(date_summary) {
  future({
    plot_ly(date_summary, x = ~date, y = ~confirmed_cases, type = "scatter", mode = "lines", color = I("blue"), hoverinfo = "y") %>%
      layout(plot_bgcolor = "#888",
             xaxis = list(title = "Date", tickangle = -45),
             yaxis = list(title = "Confirmed Cases")
      ) %>%
      config(displayModeBar = FALSE)
  })
}

newCasesPlot <- function(date_summary) {
  future({
    fit <- stats::lm(new_cases ~ date, data = date_summary)
    plot_ly(date_summary, x = ~date, y = ~new_cases, type = "scatter", mode = "lines", color = I("blue"), hoverinfo = "y") %>%
      add_lines(x = ~date, y = fitted(fit), color = I("yellow")) %>%
      layout(plot_bgcolor = "#888", showlegend = FALSE,
             xaxis = list(title = "Date", tickangle = -45),
             yaxis = list(title = "New Cases")
      ) %>%
      config(displayModeBar = FALSE)
  })
}

confirmedDeathsPlot <- function(date_summary) {
  future({
    plot_ly(date_summary, x = ~date, y = ~confirmed_deaths, type = "scatter", mode = "lines", color = I("blue"), hoverinfo = "y") %>%
      layout(plot_bgcolor = "#888",
             xaxis = list(title = "Date", tickangle = -45),
             yaxis = list(title = "Confirmed Deaths")
      ) %>%
      config(displayModeBar = FALSE)
  })
}

newDeathsPlot <- function(date_summary) {
  future({
    fit <- stats::lm(new_deaths ~ date, data = date_summary)
    plot_ly(date_summary, x = ~date, y = ~new_deaths, type = "scatter", mode = "lines", color = I("blue"), hoverinfo = "y") %>%
      add_lines(x = ~date, y = fitted(fit), color = I("yellow")) %>%
      layout(plot_bgcolor = "#888", showlegend = FALSE,
             xaxis = list(title = "Date", tickangle = -45),
             yaxis = list(title = "New Deaths")
      ) %>%
      config(displayModeBar = FALSE)
  })
}

deathRatePlot <- function(date_summary) {
  future({
    date_summary$death_rate <- 0
    sel <- date_summary$confirmed_cases > 0
    date_summary$death_rate[sel] <- round(date_summary$confirmed_deaths[sel] / date_summary$confirmed_cases[sel], 4)
    plot_ly(date_summary, x = ~date, y = ~death_rate, type = "scatter", mode = "lines", color = I("blue"), hoverinfo = "y") %>%
      layout(plot_bgcolor = "#888",
             xaxis = list(title = "Date", tickangle = -45),
             yaxis = list(title = "Death Rate")
      ) %>%
      config(displayModeBar = FALSE)
  })
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

exposureProbPlot <- function(stats) {
  future({
    expdfs <- computeExposureData(stats)
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
  })
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