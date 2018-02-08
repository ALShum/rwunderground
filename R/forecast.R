#' Forecast for the next 3 days.
#'
#' @param location location set by set_location
#' @param use_metric Metric or imperial units
#' @param key weather underground API key
#' @param raw if TRUE return raw httr object
#' @param message if TRUE print out requested URL
#' @return tbl_df with date (in posix format), high and low temp,
#'         conditions, precipitation, rain, snow,
#'         max and avg wind speed, max/min and avg humidity
#' @export
#' @examples
#' \dontrun{
#' forecast3day(set_location(territory = "Hawaii", city = "Honolulu"))
#' forecast3day(set_location(airport_code = "SEA"))
#' forecast3day(set_location(zip_code = "90210"))
#' forecast3day(set_location(territory = "IR", city = "Tehran"))
#' }
forecast3day <- function(location,
                         use_metric = FALSE,
                         key = get_api_key(),
                         raw = FALSE,
                         message = TRUE) {
  parsed_req <- wunderground_request(
    request_type = "forecast",
    location = location,
    key = key,
    message = message
  )
  if (raw) {
    return(parsed_req)
  }
  stop_for_error(parsed_req)

  if (!("forecast" %in% names(parsed_req))) {
    stop(paste0("Unable to parse forecast JSON for this location: ", location))
  }

  if (use_metric) {
    tempCol <- "celsius"
    amtCol <- "mm"
    amtCol2 <- "cm"
    spdCol <- "kph"
  } else {
    tempCol <- "fahrenheit"
    amtCol <- "in"
    amtCol2 <- "in"
    spdCol <- "mph"
  }

  fcast <- parsed_req$forecast$simpleforecast$forecastday
  df <- lapply(fcast, function(x) {
    data.frame(
      date = as.POSIXct(as.numeric(x$date$epoch), origin = "1970-01-01", tz = x$date$tz_long),
      temp_high = measurement_exists(as.numeric(x$high[[tempCol]])),
      temp_low = measurement_exists(as.numeric(x$low[[tempCol]])),
      cond = measurement_exists(x$conditions, class = "character"), ## multiple conditions unhandled
      p_precip = measurement_exists(x$pop),
      rain_allday = measurement_exists(x$qpf_allday[[amtCol]]),
      rain_day = measurement_exists(x$qpf_day[[amtCol]]),
      rain_night = measurement_exists(x$qpf_night[[amtCol]]),
      snow_allday = measurement_exists(x$snow_allday[[amtCol2]]),
      snow_day = measurement_exists(x$snow_day[[amtCol2]]),
      snow_night = measurement_exists(x$snow_night[[amtCol2]]),
      max_wind = measurement_exists(paste(x$maxwind[[spdCol]], x$maxwind$dir, sep = " "), class = "character"),
      ave_wind = measurement_exists(paste(x$avewind[[spdCol]], x$avewind$dir, sep = " "), class = "character"),
      max_humid = measurement_exists(x$maxhumidity),
      min_humid = measurement_exists(x$minhumidity),
      ave_humidity = measurement_exists(x$avehumidity),
      stringsAsFactors = FALSE
    )
  })
  
  df <- encode_NA(dplyr::bind_rows(lapply(df, tibble::as_tibble)))
  
  if (length(df) > 0) {
    return(df)
  } else {
    return(NULL)
  }
}

#' Forecast for the next 10 days.
#'
#' @param location location set by set_location
#' @param use_metric Metric or imperial units
#' @param key weather underground API key
#' @param raw if TRUE return raw httr object
#' @param message if TRUE print out requested URL
#' @return tbl_df with date (in posix format), high and low temp,
#'         conditions, precipitation, rain, snow,
#'         max and avg wind speed, max/min and avg humidity
#' @export
#' @examples
#' \dontrun{
#' forecast10day(set_location(territory = "Hawaii", city = "Honolulu"))
#' forecast10day(set_location(airport_code = "SEA"))
#' forecast10day(set_location(zip_code = "90210"))
#' forecast10day(set_location(territory = "IR", city = "Tehran"))
#' }
forecast10day <- function(location,
                          use_metric = FALSE,
                          key = get_api_key(),
                          raw = FALSE,
                          message = TRUE) {
  parsed_req <- wunderground_request(
    request_type = "forecast10day",
    location = location,
    key = key,
    message = message
  )
  if (raw) {
    return(parsed_req)
  }
  stop_for_error(parsed_req)

  if (!("forecast" %in% names(parsed_req))) {
    stop(paste0("Unable to parse forecast JSON for this location: ", location))
  }

  if (use_metric) {
    tempCol <- "celsius"
    amtCol <- "mm"
    amtCol2 <- "cm"
    spdCol <- "kph"
  } else {
    tempCol <- "fahrenheit"
    amtCol <- "in"
    amtCol2 <- "in"
    spdCol <- "mph"
  }

  fcast <- parsed_req$forecast$simpleforecast$forecastday
  df <- lapply(fcast, function(x) {
    data.frame(
      date = as.POSIXct(as.numeric(x$date$epoch), origin = "1970-01-01", tz = x$date$tz_long),
      temp_high = measurement_exists(as.numeric(x$high[[tempCol]])),
      temp_low = measurement_exists(as.numeric(x$low[[tempCol]])),
      cond = measurement_exists(x$conditions, class = "character"), ## multiple conditions unhandled
      p_precip = measurement_exists(x$pop),
      rain_allday = measurement_exists(x$qpf_allday[[amtCol]]),
      rain_day = measurement_exists(x$qpf_day[[amtCol]]),
      rain_night = measurement_exists(x$qpf_night[[amtCol]]),
      snow_allday = measurement_exists(x$snow_allday[[amtCol2]]),
      snow_day = measurement_exists(x$snow_day[[amtCol2]]),
      snow_night = measurement_exists(x$snow_night[[amtCol2]]),
      max_wind = measurement_exists(paste(x$maxwind[[spdCol]], x$maxwind$dir, sep = " "), class = "character"),
      ave_wind = measurement_exists(paste(x$avewind[[spdCol]], x$avewind$dir, sep = " "), class = "character"),
      max_humid = measurement_exists(x$maxhumidity),
      min_humid = measurement_exists(x$minhumidity),
      ave_humidity = measurement_exists(x$avehumidity),
      stringsAsFactors = FALSE
    )
  })

  df <- encode_NA(dplyr::bind_rows(lapply(df, tibble::as_tibble)))
  
  if (length(df) > 0) {
    return(df)
  } else {
    return(NULL)
  }
}
