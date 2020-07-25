
db.new <- function(excludeStateData=TRUE, progress=NULL) {
  casedf <- .getCaseData(progress)
  fcasedf <- casedf
  popdf <- .getPopulationData(progress)
  stfips <- .getStateFIPSMap(casedf, progress)
  cofips <- .getCountyFIPSMap(casedf, progress)
  cosumdf <- NULL  # gets set when calling setExcludeStateData below
  dtsumdf <- NULL  # gets set when calling setExcludeStateData below
  statsdf <- NULL  # gets set when calling setExcludeStateData below
  cogeo <- NULL
  atvdays <- NULL
  atvcache <- list()
  mindate <- min(casedf$date, na.rm = TRUE)
  maxdate <- max(casedf$date, na.rm = TRUE)
  xstate <- NULL
  states <- as.integer(c())
  counties <- as.integer(c())
  self <- list(
    setActiveDays = function(days) atvdays <<- days,
    getActiveDays = function() atvdays,
    setActiveData = function(df, days) {
      atvcache[[as.character(days)]] <<- df
      .updateFilteredCaseData(self)
    },
    getActiveData = function(days) atvcache[[as.character(days)]],
    getCaseData = function() casedf,
    setFilteredCaseData = function(df) fcasedf <<- df,
    getFilteredCaseData = function() fcasedf,
    getPopulationData = function() popdf,
    setCountyGeo = function(x) cogeo <<- x,
    getCountyGeo = function() cogeo,
    setCountySummaryData = function(df) cosumdf <<- df,
    getCountySummaryData = function() cosumdf,
    setCountySummaryRankData = function(df) cosumrkdf <<- df,
    getCountySummaryRankData = function() cosumrkdf,
    setDateSummaryData = function(df) dtsumdf <<- df,
    getDateSummaryData = function() dtsumdf,
    setStatsData = function(df) statsdf <<- df,
    getStatsData = function() statsdf,
    setMinDate = function(d, prog=NULL) {
      mindate <<- d
      .updateFilteredCaseData(self, progress=prog)
    },
    getMinDate = function() mindate,
    setMaxDate = function(d, prog=NULL) {
      maxdate <<- d
      .updateFilteredCaseData(self, progress=prog)
    },
    getMaxDate = function() maxdate,
    setExcludeStateData = function(tf, prog=NULL) {
      xstate <<- tf
      .updateFilteredCaseData(self, progress=prog)
    },
    getExcludeStateData = function() xstate,
    setStates = function(v, prog=NULL) {
      if(is.null(v)) {
        v <- as.integer(c())
      } else {
        v <- sort(v)
      }
      if((length(states) == 0 && length(v) > 0)
         || (length(states) > 0 && length(v) == 0)
         || any(states != v)) {
        states <<- v
        .updateFilteredCaseData(self, progress=prog)
      } else {
        states <<- v
      }
    },
    getStates = function() states,
    setCounties = function(v, prog=NULL) {
      if(is.null(v)) {
        v <- as.integer(c())
      } else {
        v <- sort(v)
      }
      if((length(counties) == 0 && length(v) > 0)
         || (length(counties) > 0 && length(v) == 0)
         || any(counties != v)) {
        counties <<- v
        .updateFilteredCaseData(self, progress=prog)
      } else {
        counties <<- v
      }
    },
    getCounties = function() counties,
    getStateFIPSMap = function() stfips,
    getCountyFIPSMap = function() cofips
  )
  class(self) <- "database"
  self$setExcludeStateData(excludeStateData, progress)
  self$setCountySummaryRankData(.getCountySummaryRankData(self$getCountySummaryData(), progress))
  self$setCountyGeo(.getCountyGeo(cosumrkdf, progress))
  self
}

db.getCaseData <- function(self) {
  self$getFilteredCaseData()
}

db.getPopulationData <- function(self) {
  self$getPopulationData()
}

db.getCountyGeo <- function(self) {
  self$getCountyGeo()
}

db.updateGeoFeatures <- function(self, progress=NULL) {
  .notify(progress, 0, "Updating feature data")
  geo <- self$getCountyGeo()
  inc <- 1 / (length(geo$features) + 1)
  .notify(progress, inc)
  csrdf <- self$getCountySummaryRankData()
  .notify(progress, inc)
  geo$features <- lapply(geo$features, function(feature) {
    df <- as.data.frame(csrdf[csrdf$county_fips == feature$id,])
    if(nrow(df) > 0) {
      feature <- .updateFeatureData(df, feature)
    }
    .notify(progress, inc)
    feature
  })
  self$setCountyGeo(geo)
}

db.getCountySummaryData <- function(self) {
  self$getCountySummaryData()
}

db.getCountySummaryRankData <- function(self) {
  self$getCountySummaryRankData()
}

db.getDateSummaryData <- function(self) {
  self$getDateSummaryData()
}

db.getStatsData <- function(self) {
  self$getStatsData()
}

db.setActiveDaysEstimate <- function(self, days, progress=NULL) {
  self$setActiveDays(days)
  if(is.null(self$getActiveData(days))) {
    self$setActiveData(.getActiveData(self$getCaseData(), days, progress), days)
  }
}

db.setMinDate <- function(self, d) {
  self$setMinDate(d)
}

db.getMinDate <- function(self) {
  self$getMinDate()
}

db.setMaxDate <- function(self, d) {
  self$setMaxDate(d)
}

db.getMaxDate <- function(self) {
  self$getMaxDate()
}

db.setStates <- function(self, states) {
  self$setStates(.getStateFIPS(self, states))
}

db.includeStates <- function(self, states) {
  self$setStates(unique(c(self$getStates(), .getStateFIPS(self, states))))
}

db.excludeStates <- function(self, states) {
  states <- .getStateFIPS(self, states)
  sf <- self$getStates()
  self$setStates(sf[! sf %in% states])
}

db.excludeStateData <- function(self, tf) {
  self$setExcludeStateData(tf)
}

db.setCounties <- function(self, counties, progress=NULL) {
  self$setCounties(.getCountyFIPS(self, counties), prog=progress)
}

db.resetCounties <- function(self, progress=NULL) {
  self$setCounties(c(), prog=progress)
}

db.includeCounties <- function(self, counties, progress=NULL) {
  self$setCounties(unique(c(self$getCounties(), .getCountyFIPS(self, counties))), prog=progress)
}

db.excludeCounties <- function(self, counties, progress=NULL) {
  counties <- .getCountyFIPS(self, counties)
  cf <- self$getCounties()
  self$setCounties(cf[! cf %in% counties], prog=progress)
}

.updateFilteredCaseData <- function(self, progress=NULL) {
  inc <- 1 / 7
  .notify(progress, 0, "Updating data for filters")
  df <- self$getCaseData()
  .notify(progress, inc)
  updateRank <- FALSE
  if(! is.null(self$getActiveDays())) {
    df <- self$getActiveData(self$getActiveDays())
    updateRank <- TRUE
  }
  .notify(progress, inc)
  if(self$getExcludeStateData()) {
    df <- df[df$county_fips != 0,]
    updateRank <- TRUE
  }
  .notify(progress, inc)
  if(updateRank) {
    # CountySummaryRankData needs to reflect changes to the active case estimate
    # but not county selections, etc.
    self$setCountySummaryRankData(.getCountySummaryRankData(.getCountySummaryData(df, progress), progress))
    .notify(progress, 0)
    .notify(progress, 4 * inc)
  }
  if(length(self$getStates()) > 0) {
    df <- df[df$state_fips %in% self$getStates(),]
  }
  .notify(progress, inc)
  if(length(self$getCounties()) > 0) {
    df <- df[df$county_fips %in% self$getCounties(),]
  }
  .notify(progress, inc)
  df <- df[df$date >= self$getMinDate() & df$date <= self$getMaxDate(),]
  .notify(progress, inc)
  self$setFilteredCaseData(df)
  .notify(progress, inc)
  self$setCountySummaryData(.getCountySummaryData(df, progress=progress))
  self$setDateSummaryData(.getDateSummaryData(df, progress=progress))
  self$setStatsData(.getStatsData(self, progress=progress))
}

.getStateFIPS <- function(self, states) {
  if(length(states) > 0) {
    if(is.character(states)) {
      if(all(grepl("^\\d+$", states))) {
        states <- as.integer(states)
      } else {
        mdf <- self$getStateFIPSMap()
        states <- mdf$state_fips[mdf$state %in% states]
      }
    }
  }
  states
}

.getCountyFIPS <- function(self, counties) {
  if(length(counties) > 0) {
    if(is.character(counties)) {
      if(all(grepl("^\\d+$", counties))) {
        counties <- as.integer(counties)
      } else {
        mdf <- self$getCountyFIPSMap()
        counties <- mdf$county_fips[mdf$county_name %in% counties]
      }
    }
  }
  counties
}

.getStateFIPSMap <- function(casedf, progress) {
  readRDS("data/state_fips_map.rds")
}

.getCountyFIPSMap <- function(casedf, progress) {
  readRDS("data/county_fips_map.rds")
}

.getActiveData <- function(casedf, days, progress=NULL) {
  .notify(progress, 0, "Calculating active case estimates")
  dplyr::group_by(casedf, state_fips, county_fips) %>%
    dplyr::do(.notify.df(., progress, 0.33)) %>%
    dplyr::do(.getCountyActiveData(., days)) %>%
    dplyr::do(.notify.df(., progress, 0.33)) %>%
    dplyr::ungroup()
}

.getCountyActiveData <- function(df, days) {
  df$active_case_est <- vapply(1:nrow(df), function(i) {
    mi <- max(i - days + 1, 1)
    max(sum(df$new_cases[mi:i], na.rm = TRUE), 0)
  }, 1)
  df
}

.addNewCases <- function(gdf) {
  df <- gdf %>%
    dplyr::arrange(date_num)
  states <- unique(df$state)
  cfips <- unique(df$county_fips)
  if(length(states) > 1 || length(cfips) > 1) {
    stop(".addNewCases got dataframe with non-unique state (",paste(states,collapse=","),") and/or county_fips (",paste(cfips,collapse=","),")")
  }
  df$new_cases <- as.numeric(NA)
  df$new_cases[2:nrow(df)] <- df$confirmed_cases[2:nrow(df)] - df$confirmed_cases[1:(nrow(df)-1)]
  df
}

.updateStateCases <- function(gdf) {
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

.notify.df  <- function(df, progress, amount = 0.1, msg = NULL) {
  .notify(progress, amount, msg)
  df
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

.getCachedData <- function(cachefile) {
  df <- NULL
  if(file.exists(cachefile)) {
    fmd <- file.info(cachefile, FALSE)
    if(Sys.Date() - as.Date(fmd$mtime)[1] == 0) {
      # We updated the cached data today so return it
      df <- readRDS(cachefile)
    }
  }
  df
}

.getCaseData <- function(progress=NULL) {
  cachefile <- "data/covid_case.rds"
  df <- .getCachedData(cachefile)
  if(is.null(df)) {
    i <- 1 / 14
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
      dplyr::do(.addNewCases(.))
    .notify(progress, i)
    df <- df %>%
      dplyr::group_by(state)
    .notify(progress, i)
    df <- df %>%
      dplyr::do(.updateStateCases(.))
    .notify(progress, i)
    
    cddf <- .getDeathData(progress) %>%
      dplyr::mutate(date_num=as.numeric(date))
    .notify(progress, i)
    df <- merge(df, cddf[, c("state_fips", "county_fips", "date_num", "confirmed_deaths", "new_deaths")], all.x=TRUE)
    .notify(progress, i)
    
    df <- df %>%
      dplyr::arrange(state, county_fips, date_num)
    .notify(progress, i)
    df <- df %>%
      dplyr::mutate(date_num=NULL)
    saveRDS(df, cachefile)
  }
  df
}

.getCountySummaryData <- function(casedf, progress=NULL) {
  .notify(progress, 0, "Computing county summary")
  df <- dplyr::arrange(casedf, state, dplyr::desc(date)) %>%
    dplyr::do(.notify.df(., progress, 0.33)) %>%
    dplyr::group_by(state, state_fips, county_name, county_fips)
  .notify(progress, 0.33)
  if("active_case_est" %in% colnames(casedf)) {
    df <- df %>%
      dplyr::summarise(confirmed_cases=confirmed_cases[1], active_case_est=active_case_est[1],
                       confirmed_deaths=confirmed_deaths[1], new_deaths=sum(new_deaths, na.rm = TRUE))
  } else {
    df <- df %>%
      dplyr::summarise(confirmed_cases=confirmed_cases[1], active_case_est=sum(new_cases, na.rm = TRUE),
                       confirmed_deaths=confirmed_deaths[1], new_deaths=sum(new_deaths, na.rm = TRUE))
  }
  .notify(progress, 0.34)
  df
}

.getCountySummaryRankData <- function(cosumdf, progress=NULL) {
  .notify(progress, 0, "Building county active case rank data")
  df <- dplyr::arrange(cosumdf, dplyr::desc(active_case_est))
  .notify(progress, 0.9)
  df$active_rank <- 1:nrow(df)
  .notify(progress, 1)
  df
}

.getDateSummaryData <- function(casedf, progress=NULL) {
  .notify(progress, 0, "Computing date summary")
  df <- dplyr::mutate(casedf, date_num=as.numeric(date)) %>%
    dplyr::do(.notify.df(., progress, 0.33)) %>%
    dplyr::group_by(date_num)
  .notify(progress, 0.33)
  if("active_case_est" %in% colnames(casedf)) {
    dplyr::summarise(df, date=date[1],
                     confirmed_cases=sum(confirmed_cases, na.rm = TRUE),
                     new_cases=sum(new_cases, na.rm = TRUE),
                     confirmed_deaths=sum(confirmed_deaths, na.rm = TRUE),
                     new_deaths=sum(new_deaths, na.rm = TRUE),
                     active_case_est=sum(active_case_est, na.rm = TRUE)) %>%
      dplyr::do(.notify.df(., progress, 0.34))
  } else {
    dplyr::summarise(df, date=date[1],
                     confirmed_cases=sum(confirmed_cases, na.rm = TRUE),
                     new_cases=sum(new_cases, na.rm = TRUE),
                     confirmed_deaths=sum(confirmed_deaths, na.rm = TRUE),
                     new_deaths=sum(new_deaths, na.rm = TRUE)) %>%
      dplyr::do(.notify.df(., progress, 0.34))
  }
}

.getPopulationData <- function(progress=NULL) {
  cachefile <- "data/county_population.rds"
  df <- .getCachedData(cachefile)
  if(is.null(df)) {
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
    df <- df %>%
      dplyr::select(-population.x, -population.y)
    saveRDS(df, cachefile)
  }
  df
}

.addNewDeaths <- function(gdf) {
  df <- gdf %>%
    dplyr::arrange(date_num)
  states <- unique(df$state)
  cfips <- unique(df$county_fips)
  if(length(states) > 1 || length(cfips) > 1) {
    stop(".addNewDeaths got dataframe with non-unique state (",paste(states,collapse=","),") and/or county_fips (",paste(cfips,collapse=","),")")
  }
  df$new_deaths <- as.numeric(NA)
  df$new_deaths[2:nrow(df)] <- df$confirmed_deaths[2:nrow(df)] - df$confirmed_deaths[1:(nrow(df)-1)]
  df
}

.updateStateDeaths <- function(gdf) {
  df <- dplyr::ungroup(gdf) %>%
    dplyr::mutate(datest=as.character(date, "%Y-%m-%d"))
  stdf <- df[df$county_fips == 0,]
  cntdf <- df[df$county_fips != 0, c("state", "county_name", "datest", "confirmed_deaths", "new_deaths")]
  cstdf <- dplyr::group_by(cntdf, state, datest) %>%
    dplyr::summarise(confirmed_deaths_total=sum(confirmed_deaths, na.rm = TRUE), new_deaths_total=sum(new_deaths, na.rm = TRUE))
  stdf <- merge(stdf, cstdf)
  stdf$confirmed_deaths_rev <- stdf$confirmed_deaths + stdf$confirmed_deaths_total
  stdf$new_deaths_rev <- stdf$new_deaths + stdf$new_deaths_total
  stcols <- colnames(stdf)
  stcols <- stcols[! grepl("deaths(_total)*$", tolower(stcols), perl = TRUE)]
  df <- merge(df, stdf[, stcols], all.x = TRUE)
  sel <- ! is.na(df$confirmed_deaths_rev)
  df$confirmed_deaths[sel] <- df$confirmed_deaths_rev[sel]
  sel <- ! is.na(df$new_deaths_rev)
  df$new_deaths[sel] <- df$new_deaths_rev[sel]
  cnames <- colnames(df)
  cnames <- cnames[! grepl("(deaths_rev|datest)", cnames, ignore.case = TRUE)]
  df[, cnames]
}

.getDeathData <- function(progress=NULL) {
  cachefile <- "data/covid_death.rds"
  df <- .getCachedData(cachefile)
  if(is.null(df)) {
    i <- 1 / 12
    .notify(progress, 0.0, "Retrieving COVID19 confirmed deaths data")
    df <- read.csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv")
    .notify(progress, i)
    df <- df %>%
      tidyr::gather(key="date", value="confirmed_deaths", -countyFIPS, -County.Name, -State, -stateFIPS)
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
      dplyr::do(.addNewDeaths(.))
    .notify(progress, i)
    df <- df %>%
      dplyr::group_by(state)
    .notify(progress, i)
    df <- df %>%
      dplyr::do(.updateStateDeaths(.))
    .notify(progress, i)
    df <- df %>%
      dplyr::arrange(state, county_fips, date_num)
    .notify(progress, i)
    df <- df %>%
      dplyr::mutate(date_num=NULL)
    saveRDS(df, cachefile)
  }
  df
}

.getCountyGeo <- function(csumdf, progress=NULL) {
  .notify(progress, 0, "Retrieving county geoJSON")
  geo <- readRDS("data/geojson-counties-fips.rds")
  .notify(progress, 0.05)
  ncmax <- 9 * log(max(csumdf$active_case_est), 2) / 8
  .notify(progress, 0.05)
  sel <- vapply(geo$features, function(feature) {
    as.numeric(feature$id) %in% csumdf$county_fips
  }, TRUE)
  .notify(progress, 0.05)
  inc <- 0.85 / length(geo$features)
  geo$features <- lapply(geo$features[sel], function(feature) {
    feature$id <- as.integer(feature$id)
    df <- as.data.frame(csumdf[csumdf$county_fips == feature$id,])
    if(nrow(df) > 0) {
      feature <- .updateFeatureData(df, feature)
      feature$properties$style <- .featureStyle(feature,
                                                fillOpacity = log(max(c(1, df$active_case_est)), 2) / ncmax
      )
    }
    .notify(progress, inc)
    feature
  })
  geo
}

.updateFeatureData <- function(df, feature) {
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
  feature
}

.featureStyle <- function(feature, color=NULL, fillColor=NULL, weight=NULL, fillOpacity=NULL, fill=NULL) {
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

.calculateProbInfection <- function(dsumdf, pop, sorted=FALSE) {
  # P(person infected and infect me)
  #   = P(infect me | person infected)
  #     * P(person infected)
  if(! sorted) {
    dsumdf <- dsumdf %>%
      dplyr::arrange(dplyr::desc(date))
  }
  # How many active cases at timepoint i
  if("active_case_est" %in% colnames(dsumdf)) {
    Ai <- dsumdf$active_case_est[1]
  } else {
    Ai <- sum(dsumdf$new_cases, na.rm = TRUE)
  }
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

.getStatsData <- function(db, progress=NULL) {
  dsumdf <- db.getDateSummaryData(db)
  popdf <- db.getPopulationData(db)
  if(length(db$getStates()) > 0) {
    smdf <- db$getStateFIPSMap()
    states <- smdf$state[smdf$state_fips %in% db$getStates()]
    popdf <- popdf[popdf$state %in% states,]
  }
  if(length(db$getCounties()) > 0) {
    popdf <- popdf[popdf$county_fips %in% db$getCounties(),]
  }
  # Calculate the total population
  pop <- sum(popdf$population, na.rm = TRUE)
  # Calculate the trend (slope) of the new cases linear model
  nclm <- stats::lm(new_cases ~ date_num, dsumdf)
  nctrend <- stats::coef(summary(nclm))[2]
  dsumdf <- dplyr::arrange(dsumdf, dplyr::desc(date))
  # Calculate the death rate
  # I sorted summary dataframe based on descending date so
  # the first row should contain the most recent data. Therefore,
  # I can calculate the death rate from the last row
  # confirmed death and cases
  dthrt <- dsumdf$confirmed_deaths[1] / dsumdf$confirmed_cases[1]
  # Calculate the trend (slope) of the new deaths linear model
  ndlm <- stats::lm(new_deaths ~ date_num, dsumdf)
  ndtrend <- coef(summary(ndlm))[2]
  # Calculate probability of exposure statistics
  p_infected <- .calculateProbInfection(dsumdf, pop, TRUE)
  # Calculate the probability that any 1 person is not infected
  p_not_infected <- 1 - p_infected
  # Calculate the number of people one would need to encounter
  # in order for the probability of being exposed is greater
  # than the probability of not being exposed
  n50 <- as.numeric(NA)
  if(round(p_not_infected, digits=4) < 1) {
    n50 <- ceiling(log(0.5, p_not_infected))
  }
  actdays <- db$getActiveDays()
  if(is.null(actdays)) {
    actdays <- nrow(dsumdf)
  }
  if("active_case_est" %in% colnames(dsumdf)) {
    actcases <- dsumdf$active_case_est[1]
  } else {
    actcases <- sum(dsumdf$new_cases[1:min(db$getActiveDays(), nrow(dsumdf))], na.rm = TRUE)
  }
  data.frame(
    Active_Case_Estimate = actcases,
    Active_Case_Estimate_Days = actdays,
    Death_Rate = round(dthrt, digits = 4),
    Population = pop,
    Probability_of_Exposure = round(p_infected, digits=4),
    N50 = n50,
    New_Case_Trend = round(nctrend, digits=2),
    New_Death_Trend = round(ndtrend, digits=2)
  )
}
