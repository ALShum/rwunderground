#' Hourly forecast for the next 24 hours.
#' 
#' @param location location set by set_location
#' @param use_metric Metric or imperial units
#' @param key weather underground API key
#' @param raw if TRUE return raw httr object
#' @param message if TRUE print out requested URL
#' @return tbl_df with date, temperature, dew point,
#'         condition, wind speed and direction, UV index,
#'         humidity, windchill, heat index, real feel,
#'         rain, snow, pop, mslp
#' @export 
hourly = function(location,
                  use_metric = TRUE,
                  key = get_api_key(),
                  raw = FALSE,
                  message = TRUE) {

  parsed_req = wunderground_request(request_type = "hourly",
                                    location = location, 
                                    key = key,
                                    message = message)
  if(raw) {
    return(parsed_req)
  }
  stop_for_error(parsed_req)

  if(!("hourly_forecast" %in% names(parsed_req))) {
    stop(paste0("Cannot parse hourly forecast for: ", location))
  }

  hourly_forecast = parsed_req$hourly_forecast

  units = ifelse(use_metric, "metric", "english")    
  df = lapply(hourly_forecast, function(x) {
    data.frame(
         date = as.POSIXct(as.numeric(x$FCTTIME$epoch),
          origin = "1970-01-01", tz = strsplit(x$FCTTIME$pretty, split = " ")[[1]][3]),
         temp = as.numeric(x$temp[[units]]),
         dew_pt = as.numeric(x$dewpoint[[units]]),
         cond = x$condition,
         wind_spd = as.numeric(x$wspd[[units]]),
         wind_dir = x$wdir$dir,
         uvi = as.numeric(x$uvi),
         humidity = as.numeric(x$humidity),
         windchill = as.numeric(x$windchill[[units]]),
         heatindex = as.numeric(x$heatindex[[units]]),
         feelslike = as.numeric(x$feelslike[[units]]),
         rain = as.numeric(x$qpf[[units]]),
         snow = as.numeric(x$snow[[units]]),
         pop = as.numeric(x$pop),
         mslp = as.numeric(x$mslp[[units]]),
          stringsAsFactors = FALSE
         )
  })

  encode_NA(
    dplyr::bind_rows(df)
  )
}

#' Hourly forecast for the next 10 days.
#' 
#' @param location location set by set_location
#' @param use_metric Metric or imperial units
#' @param key weather underground API key
#' @param raw if TRUE return raw httr object
#' @param message if TRUE print out requested URL
#' @return tbl_df with date, temperature, dew point,
#'         condition, wind speed and direction, UV index,
#'         humidity, windchill, heat index, real feel,
#'         rain, snow, pop, mslp
#' @export 
hourly10day = function(location,
                  use_metric = TRUE,
                  key = get_api_key(),
                  raw = FALSE,
                  message = TRUE) {

  parsed_req = wunderground_request(request_type = "hourly10day",
                                    location = location, 
                                    key = key,
                                    message = message)
  if(raw) {
    return(parsed_req)
  }
  stop_for_error(parsed_req)
  
  if(!("hourly_forecast" %in% names(parsed_req))) {
    stop(paste0("Cannot parse hourly forecast for: ", location))
  }

  hourly_forecast = parsed_req$hourly_forecast

  units = ifelse(use_metric, "metric", "english")    
  df = lapply(hourly_forecast, function(x){
    data.frame(
         date = as.POSIXct(as.numeric(x$FCTTIME$epoch),
          origin = "1970-01-01", tz = strsplit(x$FCTTIME$pretty, split = " ")[[1]][3]),
         temp = as.numeric(x$temp[[units]]),
         dew_pt = as.numeric(x$dewpoint[[units]]),
         cond = x$condition,
         wind_spd = as.numeric(x$wspd[[units]]),
         wind_dir = x$wdir$dir,
         uvi = as.numeric(x$uvi),
         humidity = as.numeric(x$humidity),
         windchill = as.numeric(x$windchill[[units]]),
         heatindex = as.numeric(x$heatindex[[units]]),
         feelslike = as.numeric(x$feelslike[[units]]),
         rain = as.numeric(x$qpf[[units]]),
         snow = as.numeric(x$snow[[units]]),
         pop = as.numeric(x$pop),
         mslp = as.numeric(x$mslp[[units]]),
          stringsAsFactors = FALSE
         )
  })

  encode_NA(
    dplyr::bind_rows(df)    
  )
}