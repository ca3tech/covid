
add_new_cases <- function(gdf) {
  df <- gdf %>%
    dplyr::arrange(date_num)
  states <- unique(df$state)
  cfips <- unique(df$county_fips)
  if(length(states) > 1 || length(cfips) > 1) {
    stop("add_new_cases got dataframe with non-unique state (",paste(states,collapse=","),") and/or county_fips (",paste(cfips,collapse=","),")")
  }
  df$new_cases <- as.numeric(NA)
  df$new_cases[2:nrow(df)] <- df$confirmed_cases[2:nrow(df)] - df$confirmed_cases[1:(nrow(df)-1)]
  df
}

update_state_cases <- function(gdf) {
  df <- dplyr::ungroup(gdf) %>%
    dplyr::mutate(datest=as.character(date, "%Y-%m-%d"))
  stdf <- df[df$county_fips == 0,]
  cntdf <- df[df$county_fips != 0, c("state", "county_name", "datest", "confirmed_cases", "new_cases")]
  cstdf <- dplyr::group_by(cntdf, state, datest) %>%
    dplyr::summarise(confirmed_cases_total=sum(confirmed_cases, na.rm = TRUE), new_cases_total=sum(new_cases, na.rm = TRUE))
  stdf <- merge(stdf, cstdf)
  stdf$confirmed_cases_rev <- stdf$confirmed_cases + stdf$confirmed_cases_total
  stdf$new_cases_rev <- stdf$new_cases + stdf$new_cases_total
  stcols <- colnames(stdf)
  stcols <- stcols[! grepl("cases(_total)*$", tolower(stcols), perl = TRUE)]
  df <- merge(df, stdf[, stcols], all.x = TRUE)
  sel <- ! is.na(df$confirmed_cases_rev)
  df$confirmed_cases[sel] <- df$confirmed_cases_rev[sel]
  sel <- ! is.na(df$new_cases_rev)
  df$new_cases[sel] <- df$new_cases_rev[sel]
  cnames <- colnames(df)
  cnames <- cnames[! grepl("(cases_rev|datest)", cnames, ignore.case = TRUE)]
  df[, cnames]
}

.notify <- function(progress, amount = 0.1, msg = NULL) {
  if(! is.null(progress)) {
    if(amount == 0.0) {
      progress$set(amount, message = msg)
    } else {
      progress$inc(amount, message = msg)
    }
  } else if(! is.null(msg)) {
    message(msg)
  }
}

covid_confirmed_usa <- function(progress=NULL) {
  i <- 1 / 12
  .notify(progress, 0.0, "Retrieving COVID19 confirmed cases data")
  df <- read.csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv")
  .notify(progress, i)
  df <- df %>%
    tidyr::gather(key="date", value="confirmed_cases", -countyFIPS, -County.Name, -State, -stateFIPS)
  .notify(progress, i)
  df <- df %>%
    dplyr::mutate(date=as.Date(date, "X%m.%d.%y"))
  .notify(progress, i)
  df <- df %>%
    dplyr::mutate(date_num=as.numeric(date))
  .notify(progress, i)
  df <- df %>%
    dplyr::rename(county_fips=countyFIPS, county_name=County.Name, state=State, state_fips=stateFIPS)
  .notify(progress, i)
  df <- df %>%
    dplyr::arrange(state, county_fips, date_num)
  .notify(progress, i)
  df <- df %>%
    dplyr::group_by(state, county_fips)
  .notify(progress, i)
  df <- df %>%
    dplyr::do(add_new_cases(.))
  .notify(progress, i)
  df <- df %>%
    dplyr::group_by(state)
  .notify(progress, i)
  df <- df %>%
    dplyr::do(update_state_cases(.))
  .notify(progress, i)
  df <- df %>%
    dplyr::arrange(state, county_fips, date_num)
  .notify(progress, i)
  df %>%
    dplyr::mutate(date_num=NULL)
}

covid_county_population <- function(progress=NULL) {
  i <- 1 / 2
  .notify(progress, 0.0, "Retrieving population data")
  df <- read.csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_county_population_usafacts.csv")
  .notify(progress, i)
  df <- df %>%
    dplyr::rename(county_fips=countyFIPS, county_name=County.Name, state=State)
  i <- 1 / 3
  .notify(progress, 0.0, "Calculating state populations")
  sdf <- dplyr::group_by(df[df$county_fips!=0,], state)
  .notify(progress, i)
  sdf <- sdf %>%
    dplyr::summarise(population=sum(population, na.rm = TRUE))
  .notify(progress, i)
  sdf <- sdf %>%
    dplyr::mutate(county_fips=0) %>%
    as.data.frame()
  .notify(progress, 0.0, "Updating state populations")
  df <- dplyr::left_join(df, sdf, by=c("county_fips", "state"))
  .notify(progress, i)
  df <- df %>%
    dplyr::mutate(population=ifelse(is.na(population.y), population.x, population.y))
  .notify(progress, i)
  df %>%
    dplyr::select(-population.x, -population.y)
}

calculate_active_case_est <- function(dtgdf, window=14) {
  dtgdf <- dtgdf[order(dtgdf$date),]
  vapply(1:nrow(dtgdf), function(i) {
    si <- max(1, i - window)
    sum(dtgdf$new_cases[si:i], na.rm = TRUE)
  }, 1)
}

add_active_case_est <- function(dtgdf, window=21) {
  dtgdf$active_case_est <- calculate_active_case_est(dtgdf, window)
  dtgdf
}

compute_date_rollup <- function(casedf) {
  df <- dplyr::mutate(casedf, date_num=as.numeric(date)) %>%
    dplyr::group_by(date_num)
  if("active_case_est" %in% colnames(casedf)) {
    dplyr::summarise(df, date=date[1],
                     confirmed_cases=sum(confirmed_cases, na.rm = TRUE),
                     new_cases=sum(new_cases, na.rm = TRUE),
                     active_case_est=sum(active_case_est, na.rm = TRUE))
  } else {
    dplyr::summarise(df, date=date[1],
                     confirmed_cases=sum(confirmed_cases, na.rm = TRUE),
                     new_cases=sum(new_cases, na.rm = TRUE))
  }
}

calc_prob_infection <- function(dsumdf, pop, sorted=FALSE) {
  # P(person infected and infect me)
  #   = P(infect me | person infected)
  #     * P(person infected)
  if(! sorted) {
    dsumdf <- dsumdf %>%
      dplyr::arrange(dplyr::desc(date))
  }
  # How many active cases at timepoint i
  Ai <- dsumdf$active_case_est[1]
  # Calculate the probability of getting infected given
  # that someone is infected
  # P(infect me | person infected)
  # Unfortunately I can't find an answer to this and
  # it cannot be answered given the data I have available
  # therefore I will assume a probability of 1
  p_infect_infected <- 1
  # Calculate the probability that any 1 person is infected
  # P(person infected)
  p_infected <- Ai / pop
  p_infect_infected * p_infected
}

stats_to_list <- function(statsdf) {
  sl <- as.list(statsdf)
  names(sl) <- gsub("_", " ", names(sl))
  sl
}

compute_stats <- function(casedf, popdf) {
  # Make sure population data only contains that for casedf
  cnames <- intersect(colnames(casedf), colnames(popdf))
  popdf <- merge(popdf, unique(casedf[, cnames]))
  # Calculate the total population
  # Note we assume that the case dataframe contains data
  # either for counties, or for states, and not for both
  pop <- sum(popdf$population, na.rm = TRUE)
  ldate <- max(casedf$date)
  ndays <- as.integer(ldate - min(casedf$date))
  acasedf <- dplyr::group_by(casedf, county_fips) %>%
    dplyr::do(add_active_case_est(., ndays))
  dsumdf <- compute_date_rollup(acasedf) %>%
    dplyr::arrange(date)
  # Calculate the trend (slope) of the new cases linear model
  nclm <- stats::lm(new_cases ~ date_num, dsumdf)
  nctrend <- coef(summary(nclm))[2]
  dsumdf <- dsumdf %>%
    dplyr::arrange(dplyr::desc(date))
  cases <- dsumdf$active_case_est[1]
  p_infected <- calc_prob_infection(dsumdf, pop, TRUE)
  # Calculate the probability that any 1 person is not infected
  p_not_infected <- 1 - p_infected
  # Calculate the number of people one would need to encounter
  # in order for the probability of being exposed is greater
  # than the probability of not being exposed
  n50 <- ceiling(log(0.5, p_not_infected))
  data.frame(
    Active_Case_Estimate = cases,
    Active_Case_Estimate_Days = ndays,
    Population = pop,
    Probability_of_Exposure = round(p_infected, digits=4),
    N50 = n50,
    New_Case_Trend = round(nctrend, digits=2)
  )
}

confirmed_cases_plot <- function(casedf) {
  csumdf <- compute_date_rollup(casedf)
  plot_ly(csumdf, x = ~date, y = ~confirmed_cases, type = "scatter", mode = "lines", color = I("blue"), hoverinfo = "y") %>%
    layout(plot_bgcolor = "#888",
           xaxis = list(title = "Date", tickangle = -45),
           yaxis = list(title = "Confirmed Cases")
    )
}

new_cases_plot <- function(casedf) {
  csumdf <- compute_date_rollup(casedf)
  fit <- stats::lm(new_cases ~ date, data = csumdf)
  plot_ly(csumdf, x = ~date, y = ~new_cases, type = "scatter", mode = "lines", color = I("blue"), hoverinfo = "y") %>%
    add_lines(x = ~date, y = fitted(fit), color = I("yellow")) %>%
    layout(plot_bgcolor = "#888", showlegend = FALSE,
           xaxis = list(title = "Date", tickangle = -45),
           yaxis = list(title = "New Cases")
    )
}

compute_exposure_data <- function(statsdf) {
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

exposure_prob_plot <- function(statsdf) {
  expdfs <- compute_exposure_data(statsdf)
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
    )
}

county_fips_geo <- function() {
  readRDS("data/geojson-counties-fips.Rds")
}

compute_county_rollup <- function(casedf) {
  dplyr::arrange(casedf, state, dplyr::desc(date)) %>%
    dplyr::group_by(state, state_fips, county_name, county_fips) %>%
    dplyr::summarise(confirmed_cases=confirmed_cases[1], new_cases=sum(new_cases, na.rm = TRUE))
}

filter_update_geo_features <- function(cfgeo, casedf=NULL, csumdf=NULL) {
  if(is.null(csumdf)) {
    if(is.null(casedf)) {
      casedf <- covid_confirmed_usa()
      casedf <- casedf[casedf$county_fips != 0,]
    }
    csumdf <- compute_county_rollup(casedf)
  }
  ncmax <- log(max(csumdf$new_cases), 2)
  sel <- vapply(cfgeo$features, function(feature) {
    as.numeric(feature$id) %in% csumdf$county_fips
  }, TRUE)
  cfgeo$features <- lapply(cfgeo$features[sel], function(feature) {
    feature$id <- as.integer(feature$id)
    df <- as.data.frame(csumdf[csumdf$county_fips == feature$id,])
    if(nrow(df) > 0) {
      lapply(colnames(df), function(key) {
        if(is.factor(df[, key])) {
          v <- as.character(df[, key])
        } else {
          v <- df[, key]
        }
        if(key == "state") {
          key <- "state_name"
        }
        feature$properties[key] <<- v
      })
      feature$properties$style <- feature_style(feature,
        fillOpacity = log(max(c(1, df$new_cases)), 2) / ncmax
      )
    }
    feature
  })
  cfgeo
}

feature_style <- function(feature, color=NULL, fillColor=NULL, weight=NULL, fillOpacity=NULL, fill=NULL) {
  def <- list(
    color = "#333",
    fillColor = "blue",
    weight = 1,
    fillOpacity = 0,
    fill = TRUE
  )
  hasstyle <- ! is.null(feature$properties$style)
  if(hasstyle) {
    style <- feature$properties$style
  } else {
    style <- list()
  }
  lapply(c("color", "fillColor", "weight", "fillOpacity", "fill"), function(k) {
    v <- get(k)
    if(is.null(v)) {
      if(! hasstyle) {
        style[k] <<- def[[k]]
      }
    } else {
      style[k] <<- v
    }
  })
  style
}

get_map <- function(cfgeo=NULL, casedf=NULL, csumdf=NULL, cfgeo.update=FALSE, progress=NULL) {
  if(is.null(cfgeo)) {
    .notify(progress, 0, "Retrieving county map data")
    cfgeo <- county_fips_geo()
    cfgeo.update <- TRUE
  }
  if(cfgeo.update) {
    .notify(progress, 0, "Updating county map data")
    cfgeo <- filter_update_geo_features(cfgeo, casedf, csumdf)
  }
  inc <- 1 / (length(cfgeo$features) + 1)
  .notify(progress, 0, "Building map")
  leaflet() %>%
    setView(lng = -98.583, lat = 39.833, zoom = 3) %>%
    addTiles() %>%
    addGeoJSON(cfgeo)
}

data(zipcode)
get_zipcode_lon_lat <- function(zip_codes) {
  dplyr::rename(zipcode[zipcode$zip %in% zip_codes, c("zip", "longitude", "latitude")],
                zipcode=zip, lon=longitude, lat=latitude)
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