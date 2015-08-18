#' Hourly forecast for the next 24 hours.
#' 
#' @param location location set by set_location
#' @param date_fmt date format to return
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
                  date_fmt = "pretty",  ## TODO 
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

  hourly_forecast = parsed_req$hourly_forecast

  units = ifelse(use_metric, "metric", "english")    
  df = lapply(hourly_forecast, function(x) {
    list(date = x$FCTTIME$pretty,
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
         mslp = as.numeric(x$mslp[[units]])
         )
  })

  dplyr::bind_rows(lapply(df, data.frame, stringsAsFactors = FALSE))
}

#' Hourly forecast for the next 10 days.
#' 
#' @param location location set by set_location
#' @param date_fmt date format to return
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
                  date_fmt = "pretty",  ## TODO 
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
    
  hourly_forecast = parsed_req$hourly_forecast

  units = ifelse(use_metric, "metric", "english")    
  df = lapply(hourly_forecast, function(x){
    list(date = x$FCTTIME$pretty,
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
         mslp = as.numeric(x$mslp[[units]])
         )
  })

  dplyr::bind_rows(lapply(df, data.frame, stringsAsFactors = FALSE))
}