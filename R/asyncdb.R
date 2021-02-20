source("R/dbus.R")

db.new <- function() {
  self <- dbus.new()
  kcase <- "case"
  kpop <- "population"
  kstfips <- "state_fips"
  kcofips <- "county_fips"
  kactdays <- "active_days"
  kactdata <- "active_data"
  kdract <- "date_range_active"
  kmindate <- "min_date"
  kmaxdate <- "max_date"
  kcounties <- "counties"
  kcosum <- "county_summary"
  kcosumrk <- "county_summary_rank"
  kcocase <- "county_case"
  kdtsum <- "date_summary"
  kstats <- "stats"
  kcogeo <- "county_geo"
  actdays <- NULL
  counties <- NULL
  class(self) <- c("asyncdb", class(self))
  self$getActiveDays <- function() self$getValue(kactdays)
  self$setActiveDays <- function(days) {
    actdays <<- days
    dbus.set(self, kactdays, days)
  }
  self$getActiveData <- function() self$getValue(kactdata)
  self$setActiveData <- function(df) {
    if(! is.null(df)) {
      dbus.set(self, kactdata, df)
    }
  }
  self$registerActiveData <- function(obs) {
    dbus.register(self, kactdata, obs, async=FALSE)
  }
  self$getCounties <- function() self$getValue(kcounties)
  self$setCounties <- function(v) {
    counties <<- v
    dbus.set(self, kcounties, v)
  }
  self$registerCounties <- function(obs) {
    dbus.register(self, kcounties, obs, async=FALSE)
  }
  self$getCountyCaseData <- function() self$getValue(kcocase)
  self$setCountyCaseData <- function(df) dbus.set(self, kcocase, df)
  self$getCountyGeo <- function() self$getValue(kcogeo)
  self$setCountyGeo <- function(df) dbus.set(self, kcogeo, df)
  self$registerCountyGeo <- function(obs) {
    dbus.register(self, kcogeo, obs, async=FALSE)
  }
  self$getCountySummaryData <- function() self$getValue(kcosum)
  self$setCountySummaryData <- function(df) dbus.set(self, kcosum, df)
  self$getCountySummaryRankData <- function() self$getValue(kcosumrk)
  self$setCountySummaryRankData <- function(df) dbus.set(self, kcosumrk, df)
  self$registerCountySummaryRankData <- function(obs) {
    dbus.register(self, kcosumrk, obs, async=FALSE)
  }
  self$getDateRangeActiveData <- function() self$getValue(kdract)
  self$setDateRangeActiveData <- function(df) dbus.set(self, kdract, df)
  self$getDateSummaryData <- function() self$getValue(kdtsum)
  self$setDateSummaryData <- function(df) dbus.set(self, kdtsum, df)
  self$registerDateSummaryData <- function(obs) {
    dbus.register(self, kdtsum, obs, async=FALSE)
  }
  self$getMaxDate <- function() self$getValue(kmaxdate)
  self$setMaxDate <- function(d) dbus.set(self, kmaxdate, d)
  self$getMinDate <- function() self$getValue(kmindate)
  self$setMinDate <- function(d) dbus.set(self, kmindate, d)
  self$getStatsData <- function() self$getValue(kstats)
  self$setStatsData <- function(df) dbus.set(self, kstats, df)
  self$registerStatsData <- function(obs) {
    dbus.register(self, kstats, obs, async=FALSE)
  }
  dbus.register(self, c(kcase, kactdays), .getActiveData,
                onFulfilled = self$setActiveData,
                onRejected = function(e) {
                  stop("Active case estimate data retrieval failed: ", e)
                })
  dbus.register(self, c(kactdata, kmindate, kmaxdate), .getDateRangeActiveData,
                async = FALSE,
                onFulfilled = self$setDateRangeActiveData,
                onRejected = function(e) {
                  stop("Date range active data retrieval failed: ", e)
                })
  dbus.register(self, kdract, .getCountySummaryData,
                onFulfilled = self$setCountySummaryData,
                onRejected = function(e) {
                  stop("County summary data retrieval failed: ", e)
                })
  dbus.register(self, kcosum, .getCountySummaryRankData,
                onFulfilled = self$setCountySummaryRankData,
                onRejected = function(e) {
                  stop("County summary rank data retrieval failed: ", e)
                })
  dbus.register(self, c(kdract, kcounties), .getCountyCaseData,
                async = FALSE,
                onFulfilled = self$setCountyCaseData,
                onRejected = function(e) {
                  stop("County case data retrieval failed: ", e)
                })
  dbus.register(self, kcocase, .getDateSummaryData,
                onFulfilled = self$setDateSummaryData,
                onRejected = function(e) {
                  stop("Date summary data retrieval failed: ", e)
                })
  dbus.register(self, c(kdtsum, kpop),
                function(date_summary, population) {
                  .getStatsData(date_summary, population,
                                counties = counties,
                                active_days = actdays)
                },
                onFulfilled = self$setStatsData,
                onRejected = function(e) {
                  stop("Stats data retrieval failed: ", e)
                })
  dbus.register(self, kcosumrk, .getCountyGeo,
                onFulfilled = self$setCountyGeo,
                onRejected = function(e) {
                  stop("County geo retrieval failed: ", e)
                })
  future(.getStateFIPSMap()) %...>% {
    dbus.set(self, kstfips, .)
  } %...!% {
    stop("State FIPS map retrieval failed:", .)
  }
  future(.getCountyFIPSMap()) %...>% {
    dbus.set(self, kcofips, .)
  } %...!% {
    stop("County FIPS map retrieval failed:", .)
  }
  future(.getPopulationData()) %...>% {
    dbus.set(self, kpop, .)
  } %...!% {
    stop("Population data retrieval failed:", .)
  }
  future(.getCaseData()) %...>% {
    dbus.set(self, kcase, .)
  } %...!% {
    stop("Case data retrieval failed:", .)
  }
  self
}

db.getActiveDays <- function(self) {
  self$getActiveDays()
}

db.setActiveDays <- function(self, days) {
  self$setActiveDays(days)
}

db.getCounties <- function(self) {
  self$getCounties()
}

db.setCounties <- function(self, counties) {
  self$setCounties(counties)
}

db.registerCounties <- function(self, observer) {
  self$registerCounties(observer)
}

db.getMaxDate <- function(self) {
  self$getMaxDate()
}

db.setMaxDate <- function(self, dt) {
  self$setMaxDate(dt)
}

db.getMinDate <- function(self) {
  self$getMinDate()
}

db.setMinDate <- function(self, dt) {
  self$setMinDate(dt)
}

db.registerActiveData <- function(self, observer) {
  self$registerActiveData(observer)
}

db.registerCountyGeo <- function(self, observer) {
  self$registerCountyGeo(observer)
}

db.registerCountySummaryRankData <- function(self, observer) {
  self$registerCountySummaryRankData(observer)
}

db.registerDateSummaryData <- function(self, observer) {
  self$registerDateSummaryData(observer)
}

db.registerStatsData <- function(self, observer) {
  self$registerStatsData(observer)
}

.getCaseData <- function() {
  cachefile <- "data/covid_case.rds"
  df <- .getCachedData(cachefile)
  if(is.null(df)) {
    df <- read.csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv") %>%
      tidyr::gather(key="date", value="confirmed_cases", -countyFIPS, -County.Name, -State, -StateFIPS) %>%
      dplyr::mutate(date=as.Date(date, "X%Y.%m.%d")) %>%
      dplyr::mutate(date_num=as.numeric(date)) %>%
      dplyr::rename(county_fips=countyFIPS, county_name=County.Name, state=State, state_fips=StateFIPS) %>%
      dplyr::filter(county_fips != 0) %>%
      dplyr::arrange(state, county_fips, date_num) %>%
      dplyr::group_by(state, county_fips) %>%
      dplyr::do(.addNewCases(.))
    
    cddf <- .getDeathData() %>%
      dplyr::mutate(date_num=as.numeric(date))
    df <- merge(df, cddf[, c("state_fips", "county_fips", "date_num", "confirmed_deaths", "new_deaths")], all.x=TRUE) %>%
      dplyr::arrange(state, county_fips, date_num) %>%
      dplyr::mutate(date_num=NULL)
    saveRDS(df, cachefile)
  }
  df
}

.getDeathData <- function() {
  cachefile <- "data/covid_death.rds"
  df <- .getCachedData(cachefile)
  if(is.null(df)) {
    df <- read.csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv") %>%
      tidyr::gather(key="date", value="confirmed_deaths", -countyFIPS, -County.Name, -State, -StateFIPS) %>%
      dplyr::mutate(date=as.Date(date, "X%Y.%m.%d")) %>%
      dplyr::mutate(date_num=as.numeric(date)) %>%
      dplyr::rename(county_fips=countyFIPS, county_name=County.Name, state=State, state_fips=StateFIPS) %>%
      dplyr::filter(county_fips != 0) %>%
      dplyr::arrange(state, county_fips, date_num) %>%
      dplyr::group_by(state, county_fips) %>%
      dplyr::do(.addNewDeaths(.)) %>%
      dplyr::arrange(state, county_fips, date_num) %>%
      dplyr::mutate(date_num=NULL)
    saveRDS(df, cachefile)
  }
  df
}

.getPopulationData <- function() {
  cachefile <- "data/county_population.rds"
  df <- .getCachedData(cachefile)
  if(is.null(df)) {
    df <- read.csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_county_population_usafacts.csv") %>%
      dplyr::rename(county_fips=countyFIPS, county_name=County.Name, state=State)
    sdf <- dplyr::group_by(df[df$county_fips!=0,], state) %>%
      dplyr::summarise(population=sum(population, na.rm = TRUE)) %>%
      dplyr::mutate(county_fips=0)
    df <- dplyr::left_join(df, sdf, by=c("county_fips", "state")) %>%
      dplyr::mutate(population=ifelse(is.na(population.y), population.x, population.y)) %>%
      dplyr::select(-population.x, -population.y)
    saveRDS(df, cachefile)
  }
  df
}

.getStateFIPSMap <- function() {
  readRDS("data/state_fips_map.rds")
}

.getCountyFIPSMap <- function() {
  readRDS("data/county_fips_map.rds")
}

.getActiveData <- function(case, active_days) {
  if(! (is.null(case) || is.null(active_days))) {
    dplyr::group_by(case, state_fips, county_fips) %>%
      dplyr::do(.getCountyActiveData(., active_days)) %>%
      dplyr::ungroup()
  }
}

.getCountyActiveData <- function(df, days) {
  df$active_case_est <- vapply(1:nrow(df), function(i) {
    mi <- max(i - days + 1, 1)
    max(sum(df$new_cases[mi:i], na.rm = TRUE), 0)
  }, 1)
  df
}

.log <- function(msg) {
  write(msg, "async.log", append = T)
}

.getDateRangeActiveData <- function(active_data, min_date, max_date) {
  df <- NULL
  if(! is.null(active_data)) {
    df <- active_data
    if(! is.null(min_date)) {
      df <- df[df$date >= min_date,]
    }
    if(! is.null(max_date)) {
      df <- df[df$date <= max_date,]
    }
    # .log(paste0("(",paste(dim(active_data),collapse=","),")[",min_date,",",max_date,"] = (",paste(dim(df),collapse=","),"): .getDateRangeActiveData"))
  } else {
    # .log("active_data is null: .getDateRangeActiveData")
  }
  df
}

.getCountySummaryData <- function(date_range_active) {
  if(! is.null(date_range_active)) {
    dplyr::arrange(date_range_active, state, dplyr::desc(date)) %>%
      dplyr::group_by(state, state_fips, county_name, county_fips) %>%
      dplyr::summarise(confirmed_cases=confirmed_cases[1], active_case_est=active_case_est[1],
                       confirmed_deaths=confirmed_deaths[1], new_deaths=sum(new_deaths, na.rm = TRUE))
  }
}

.getCountySummaryRankData <- function(county_summary) {
  if(! is.null(county_summary)) {
    df <- dplyr::arrange(county_summary, dplyr::desc(active_case_est)) %>%
      as.data.frame()
    df$active_rank <- 1:nrow(df)
    df
  }
}

.getCountyCaseData <- function(date_range_active, counties) {
#  .log(paste0(is.null(date_range_active),": .getCountyCaseData"))
  df <- NULL
  if(! is.null(date_range_active)) {
    df <- date_range_active
    if(! is.null(counties) && length(counties) > 0) {
      # .log(paste0("filtering counties (",paste(counties,collapse=","),"): .getCountyCaseData"))
      df <- df[df$county_fips %in% counties,]
    }
    # .log(paste0("dim(df) = ",dim(df),": .getCountyCaseData"))
  }
  df
}

.getDateSummaryData <- function(county_case) {
  # .log(paste0(is.null(county_case),": .getDateSummaryData"))
  if(! is.null(county_case)) {
    # .log(paste0("dim(county_case) = ",dim(county_case),": .getDateSummaryData"))
    dplyr::mutate(county_case, date_num=as.numeric(date)) %>%
      dplyr::group_by(date_num) %>%
      dplyr::summarise(date=date[1],
                       confirmed_cases=sum(confirmed_cases, na.rm = TRUE),
                       new_cases=sum(new_cases, na.rm = TRUE),
                       confirmed_deaths=sum(confirmed_deaths, na.rm = TRUE),
                       new_deaths=sum(new_deaths, na.rm = TRUE),
                       active_case_est=sum(active_case_est, na.rm = TRUE))
  }
}

.getStatsData <- function(date_summary, population, counties, active_days) {
  if(! (is.null(date_summary) || is.null(population) || is.null(active_days))) {
    # .log(paste0("colnames(date_summary) = (",paste(colnames(date_summary),collapse=","),"): .getStatsData"))
    # .log(paste0("dim(date_summary) = (",paste(dim(date_summary),collapse=","),"): .getStatsData"))
    if(! is.null(counties) && length(counties) > 0) {
      # .log(paste0("Filtering population for counties (",paste(counties,collapse=","),"): .getStatsData"))
      population <- population[population$county_fips %in% counties,]
    }
    # Calculate the total population
    pop <- sum(population$population, na.rm = TRUE)
    # Calculate the trend (slope) of the new cases linear model
    # .log("Calculating new_case model: .getStatsData")
    nclm <- stats::lm(new_cases ~ date_num, date_summary)
    nctrend <- stats::coef(summary(nclm))[2]
    date_summary <- dplyr::arrange(date_summary, dplyr::desc(date))
    # Calculate the death rate
    # I sorted summary dataframe based on descending date so
    # the first row should contain the most recent data. Therefore,
    # I can calculate the death rate from the last row
    # confirmed death and cases
    dthrt <- date_summary$confirmed_deaths[1] / date_summary$confirmed_cases[1]
    # Calculate the trend (slope) of the new deaths linear model
    # .log("Calculating new_death model: .getStatsData")
    ndlm <- stats::lm(new_deaths ~ date_num, date_summary)
    # .log("Calculating new_death trend: .getStatsData")
    ndtrend <- coef(summary(ndlm))[2]
    # Calculate probability of exposure statistics
    p_infected <- .calculateProbInfection(date_summary, pop, TRUE)
    # Calculate the probability that any 1 person is not infected
    p_not_infected <- 1 - p_infected
    # Calculate the number of people one would need to encounter
    # in order for the probability of being exposed is greater
    # than the probability of not being exposed
    n50 <- as.numeric(NA)
    if(round(p_not_infected, digits=4) < 1) {
      n50 <- ceiling(log(0.5, p_not_infected))
    }
    data.frame(
      Active_Case_Estimate = date_summary$active_case_est[1],
      Active_Case_Estimate_Days = active_days,
      Death_Rate = round(dthrt, digits = 4),
      Population = pop,
      Probability_of_Exposure = round(p_infected, digits=4),
      N50 = n50,
      New_Case_Trend = round(nctrend, digits=2),
      New_Death_Trend = round(ndtrend, digits=2)
    )
  }
}

.getCountyGeo <- function(county_summary_rank) {
  if(! is.null(county_summary_rank)) {
    geo <- readRDS("data/geojson-counties-fips.Rds")
    ncmax <- 9 * log(max(county_summary_rank$active_case_est), 2) / 8
    sel <- vapply(geo$features, function(feature) {
      as.numeric(feature$id) %in% county_summary_rank$county_fips
    }, TRUE)
    geo$features <- lapply(geo$features[sel], function(feature) {
      feature$id <- as.integer(feature$id)
      df <- as.data.frame(county_summary_rank[county_summary_rank$county_fips == feature$id,])
      if(nrow(df) > 0) {
        feature <- .updateFeatureData(df, feature)
        feature$properties$style <- .featureStyle(feature,
                                                  fillOpacity = log(max(c(1, df$active_case_est)), 2) / ncmax)
      }
      feature
    })
    geo
  }
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