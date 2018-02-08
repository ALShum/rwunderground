#' Hourly weather data for specified date.
#'
#' @param location location set by set_location
#' @param date Date as YYYYMMDD format
#' @param use_metric Metric or imperial units
#' @param key weather underground API key
#' @param raw if TRUE return raw httr object
#' @param message if TRUE print out requested URL
#' @return tbl_df with date, temperature, dew point,
#'         humidity, wind speed, gust and direction,
#'         visibility, pressure, wind chill, heat index,
#'         precipitation, condition, fog, rain, snow,
#'         hail, thunder, tornado
#' @export
#' @examples
#' \dontrun{
#' history(set_location(territory = "Hawaii", city = "Honolulu"), "20130101")
#' history(set_location(airport_code = "SEA"), "20130101")
#' history(set_location(zip_code = "90210"), "20130131")
#' history(set_location(territory = "IR", city = "Tehran"), "20140131")
#' }
history <- function(location,
                    date = "20150101",
                    use_metric = FALSE,
                    key = get_api_key(),
                    raw = FALSE,
                    message = TRUE) {
  
  ###  make the R checker happy
  hour <- year <- mon <- mday <- hr <- mn <- tz <- NULL
  
  parsed_req <- wunderground_request(
    request_type = "history",
    location = location,
    date = date,
    key = key,
    message = message
  )
  if (raw) {
    return(parsed_req)
  }
  stop_for_error(parsed_req)

  if (!("history" %in% names(parsed_req))) {
    stop(paste0("Cannot parse history data for: ", location))
  }

  hist <- parsed_req$history

  if(length(hist$observations) == 0){
    stop(paste0("No observations were returned for ",
                location,
                " on ",
                date)
    )
  }
  
  suffix <- ifelse(use_metric, "m", "i")
  df <- lapply(hist$observations, function(x) {
    list(
      year = x$date$year,
      mon = x$date$mon,
      mday = x$date$mday, 
      hour = x$date$hour,
      min = x$date$min,
      tz = x$date$tzname,
      temp = measurement_exists(x[[paste0("temp",
                                          suffix)]]),
      dew_pt = measurement_exists(x[[paste0("dewpt",
                                            suffix)]]),
      hum = measurement_exists(x$hum),
      wind_spd = measurement_exists(x[[paste0("wspd",
                                              suffix)]]),
      wind_gust = measurement_exists(x[[paste0("wgust",
                                               suffix)]]),
      dir = measurement_exists(x$wdire,
                               class = "character"),
      vis = measurement_exists(x[[paste0("vis",
                                         suffix)]]),
      pressure = measurement_exists(x[[paste0("pressure",
                                              suffix)]]),
      wind_chill = measurement_exists(x[[paste0("windchill",
                                                suffix)]]),
      heat_index = measurement_exists(x[[paste0("heatindex",
                                                suffix)]]),
      precip = measurement_exists(x[[paste0("precip",
                                                 suffix)]]),
      precip_rate = measurement_exists(x[[paste0("precip_rate",
                                            suffix)]]),
      precip_total = measurement_exists(x[[paste0("precip_total",
                                                 suffix)]]),
      cond = measurement_exists(x$conds,
                                class = "character"),
      fog = measurement_exists(x$fog),
      rain = measurement_exists(x$rain),
      snow = measurement_exists(x$snow),
      hail = measurement_exists(x$hail),
      thunder = measurement_exists(x$thunder),
      tornado = measurement_exists(x$tornado)
    )
  })
  df <- encode_NA(dplyr::bind_rows(lapply(df, tibble::as_tibble)))
  testdate <- paste0(df$year,df$mon,df$mday)
  if (sum(testdate!=date)>0) {
    print(paste0("dropping ",sum(testdate!=date),
                   " returned observations not on ", date))
    df <- df[testdate==date,]
  }
  if (length(df) > 0) {
    df$date <- dst_POSIXct(y=df$year,m=df$mon,d=df$mday,
                           hr=df$hour,mn=df$min,sec="00",tz=df$tz)
    df <- df[,c("date",setdiff(names(df),"date"))]
    return(dplyr::select(df,-year,-mon,-mday,-hour,-min,-tz))
  } else {
    return(NULL)
  }
}

#' Summarized weather data for specified date.
#'
#' @param location location set by set_location
#' @param date Date as YYYYMMDD format
#' @param use_metric Metric or imperial units
#' @param key weather underground API key
#' @param raw if TRUE return raw httr object
#' @param message if TRUE print out requested URL
#' @return tbl_df of summarized weather
#' @export
#' @examples
#' \dontrun{
#' history_daily(set_location(territory = "Hawaii", city = "Honolulu"), "20130101")
#' history_daily(set_location(airport_code = "SEA"), "20130101")
#' history_daily(set_location(zip_code = "90210"), "20130131")
#' history_daily(set_location(territory = "IR", city = "Tehran"), "20140131")
#' }
history_daily <- function(location,
                          date = "20150101",
                          use_metric = FALSE,
                          key = get_api_key(),
                          raw = FALSE,
                          message = TRUE) {
  parsed_req <- wunderground_request(
    request_type = "history",
    location = location,
    date = date,
    key = key,
    message = message
  )
  if (raw) {
    return(parsed_req)
  }
  stop_for_error(parsed_req)

  if (!("history" %in% names(parsed_req))) {
    stop(paste0("Cannot parse history data for: ", location))
  }

  hist <- parsed_req$history

  suffix <- ifelse(use_metric, "m", "i")
  
  if (length(hist$dailysummary) == 0) {
    stop(paste0("Daily summary for ", location, " does not exist."))
  }
  
  ds <- hist$dailysummary[[1]]
  df <- data.frame(
    date = as.POSIXct(
      paste0(
        ds$date$year, "-", ds$date$mon, "-",
        ds$date$mday, " ", ds$date$hour, ":", ds$date$min
      ),
      tz = ds$date$tzname
    ),
    fog = measurement_exists(ds$fog),
    rain = measurement_exists(ds$rain),
    snow = measurement_exists(ds$snow),
    snow_fall = measurement_exists(ds[[paste0("snowfall", suffix)]]),
    mtd_snow = measurement_exists(ds[[paste0("monthtodatesnowfall", suffix)]]),
    since_jul_snow = measurement_exists(ds[[paste0("since1julsnowfall", suffix)]]),
    snow_depth = measurement_exists(ds[[paste0("snowdepth", suffix)]]),
    hail = measurement_exists(ds$hail),
    thunder = measurement_exists(ds$thunder),
    tornado = measurement_exists(ds$tornado),
    mean_temp = measurement_exists(ds[[paste0("meantemp", suffix)]]),
    mean_dewp = measurement_exists(ds[[paste0("meandewpt", suffix)]]),
    mean_pressure = measurement_exists(ds[[paste0("meanpressure", suffix)]]),
    mean_wind_spd = measurement_exists(ds[[paste0("meanwindspd", suffix)]]),
    mean_wind_dir = measurement_exists(ds$meanwdird),
    mean_visib = measurement_exists(ds[[paste0("meanvis", suffix)]]),
    humid = measurement_exists(ds$humidity),
    max_temp = measurement_exists(ds[[paste0("maxtemp", suffix)]]),
    min_temp = measurement_exists(ds[[paste0("mintemp", suffix)]]),
    max_humid = measurement_exists(ds$maxhumidity),
    min_humid = measurement_exists(ds$minhumidity),
    max_dew_pt = measurement_exists(ds[[paste0("maxdewpt", suffix)]]),
    min_dew_pt = measurement_exists(ds[[paste0("mindewpt", suffix)]]),
    max_pressure = measurement_exists(ds[[paste0("maxpressure", suffix)]]),
    min_pressure = measurement_exists(ds[[paste0("minpressure", suffix)]]),
    max_wind_spd = measurement_exists(ds[[paste0("maxwspd", suffix)]]),
    min_wind_spd = measurement_exists(ds[[paste0("minwspd", suffix)]]),
    max_vis = measurement_exists(ds[[paste0("maxvis", suffix)]]),
    min_vis = measurement_exists(ds[[paste0("minvis", suffix)]]),
    gdegree = measurement_exists(ds$gdegreedays),
    heating_days = measurement_exists(ds$heatingdegreedays),
    cooling_days = measurement_exists(ds$coolingdegreedays),
    precip = measurement_exists(ds[[paste0("precip", suffix)]]),
    precip_source = measurement_exists(ds$precipsource,
                                       class = "character"),
    heating_day_normal = measurement_exists(ds$heatingdegreedaysnormal),
    mtd_heating_degree = measurement_exists(ds$monthtodateheatingdegreedays),
    mtd_heating_degree_normal = measurement_exists(ds$monthtodateheatingdegreedaysnormal),
    since_sep_heating = measurement_exists(ds$since1sepheatingdegreedays),
    since_sep_heating_normal = measurement_exists(ds$since1sepheatingdegreedaysnormal),
    since_jul_heating = measurement_exists(ds$since1julheatingdegreedays),
    since_jul_heating_normal = measurement_exists(ds$since1julheatingdegreedaysnormal),
    cooling_degree_normal = measurement_exists(ds$coolingdegreedaysnormal),
    mtd_cooling_day = measurement_exists(ds$monthtodatecoolingdegreedays),
    mtd_cooling_day_normal = measurement_exists(ds$monthtodatecoolingdegreedaysnormal),
    since_sep_cooling = measurement_exists(ds$since1sepcoolingdegreedays),
    since_sep_cooling_normal = measurement_exists(ds$since1sepcoolingdegreedaysnormal),
    since_jan_cooling = measurement_exists(ds$since1jancoolingdegreedays),
    since_jan_cooling_normal = measurement_exists(ds$since1jancoolingdegreedaysnormal)
  )

  return(dplyr::tbl_df(df))
}

#' Hourly weather data for specified date range.
#'
#' @param location location set by set_location
#' @param date_start start date
#' @param date_end end date
#' @param limit Maximum number of API requests per minute,
#'              NULL to have no limits
#' @param no_api bypass API and use URL requests
#' @param use_metric Metric or imperial units
#' @param key weather underground API key
#' @param raw if TRUE return raw httr object
#' @param message if TRUE print out requested URL
#' @return tbl_df with date, temperature, dew point,
#'         humidity, wind speed, gust and direction,
#'         visibility, pressure, wind chill, heat index,
#'         precipitation, condition, fog, rain, snow,
#'         hail, thunder, tornado
#' @export
#' @examples
#' \dontrun{
#' history_range(set_location(territory = "Hawaii", city = "Honolulu"), "20130101", "20130105")
#' history_range(set_location(airport_code = "SEA"), "20130101", "20130105")
#' history_range(set_location(zip_code = "90210"), "20130131", "20130205")
#' history_range(set_location(territory = "IR", city = "Tehran"), "20140131", "20140202")
#' }
history_range <- function(location,
                          date_start = "20150101",
                          date_end = "20150105",
                          limit = 10,
                          no_api = FALSE, # get data from URL instead of API
                          use_metric = FALSE,
                          key = get_api_key(),
                          raw = FALSE,
                          message = TRUE) {
  if (no_api) {
    warning("no_api: this feature is not yet working")
  }

  date_start <- as.Date(date_start, "%Y%m%d")
  date_end <- as.Date(date_end, "%Y%m%d")
  date_range <- format(
    seq.Date(date_start, date_end, "day"),
    format = "%Y%m%d"
  )

  history_list <-
  lapply(date_range, function(x) {
    if (!is.null(limit) &&
      length(date_range) > limit &&
      which(x == date_range) > 0 &&
      which(x == date_range) %% limit == 0) {
      if (message) {
        print("Waiting to not exceed API rate limit.")
      }
      Sys.sleep(60)
    }
    try(
      history(
        location = location,
        date = x,
        use_metric = use_metric,
        raw = raw,
        key = key,
        message = message
      )
    )
  })
  
  history_list <- 
    history_list[which(sapply(history_list, class) != "try-error")]

  if (raw) {
    return(history_list)
  }

  dplyr::tbl_df(dplyr::bind_rows(history_list))
}
