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

  return(data.frame(do.call(rbind, df)))
}

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

  return(data.frame(do.call(rbind, df)))
}