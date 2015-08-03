#next 3 days

forecast3day = function(location, 
                    date_fmt = "pretty",  ## TODO 
                    col_names = "pretty", ## TODO: (orig names)
                    use_metric = TRUE,
                    key = get_api_key(), 
                    raw = FALSE, 
                    message = TRUE) {
  
  parsed_req = wunderground_request(request_type = "forecast",
                                    location = location, 
                                    key = key,
                                    message = message)   
  if(raw) {
    return(parsed_req)
  }
  stop_for_error(parsed_req)

  if(!("forecast" %in% names(parsed_req))) {
    stop(paste0("Unable to parse forecast JSON for this location: ", location))
  }
  
  if(use_metric) {
    tempCol = "celsius"
    amtCol = "mm"
    amtCol2 = "cm"
    spdCol = "kph"
  } else {
    tempCol = "fahrenheit"
    amtCol = "in"
    amtCol2 = "in"
    spdCol = "mph"
  }
  
  fcast = parsed_req$forecast$simpleforecast$forecastday
  df = lapply(fcast, function(x) {
    list(date = x$date$pretty,
        temp_high = as.numeric(x$high[[tempCol]]),
        temp_low = as.numeric(x$low[[tempCol]]),
        cond = x$conditions,  ## TODO: multiple conditions?
        p_precip = x$pop,
        rain_allday = x$qpf_allday[[amtCol]],
        rain_day = x$qpf_day[[amtCol]],
        rain_night = x$qpf_night[[amtCol]],
        snow_allday = x$snow_allday[[amtCol2]],
        snow_day = x$snow_day[[amtCol2]],
        snow_night = x$snow_night[[amtCol2]],
        max_wind = paste(x$maxwind[[spdCol]], x$maxwind$dir, sep = " "),
        ave_wind = paste(x$avewind[[spdCol]], x$avewind$dir, sep = " "),
        max_humid = x$maxhumidity,
        min_humid = x$minhumidity,
        ave_humidity = x$avehumidity
    )
  })
  
  data.frame(do.call(rbind, df))
} 





forecast10day = function(location, 
                         date_fmt = "pretty",  ## TODO 
                         col_names = "pretty", ## TODO: (orig names)
                         use_metric = TRUE,
                         key = get_api_key(), 
                         raw = FALSE, 
                         message = TRUE) {

  parsed_req = wunderground_request(request_type = "forecast10day",
                                    location = location, 
                                    key = key,
                                    message = message) 
  if(raw) {
    return(parsed_req)
  }
  stop_for_error(parsed_req)

  if(!("forecast" %in% names(parsed_req))) {
    stop(paste0("Unable to parse forecast JSON for this location: ", location))
  }
  
  if(use_metric) {
    tempCol = "celsius"
    amtCol = "mm"
    amtCol2 = "cm"
    spdCol = "kph"
  } else {
    tempCol = "fahrenheit"
    amtCol = "in"
    amtCol2 = "in"
    spdCol = "mph"
  }
  
  fcast = parsed_req$forecast$simpleforecast$forecastday
  df = lapply(fcast, function(x) {
    list(date = x$date$pretty,
         temp_high = as.numeric(x$high[[tempCol]]),
         temp_low = as.numeric(x$low[[tempCol]]),
         cond = x$conditions,  ## TODO: multiple conditions?
         p_precip = x$pop,
         rain_allday = x$qpf_allday[[amtCol]],
         rain_day = x$qpf_day[[amtCol]],
         rain_night = x$qpf_night[[amtCol]],
         snow_allday = x$snow_allday[[amtCol2]],
         snow_day = x$snow_day[[amtCol2]],
         snow_night = x$snow_night[[amtCol2]],
         max_wind = paste(x$maxwind[[spdCol]], x$maxwind$dir, sep = " "),
         ave_wind = paste(x$avewind[[spdCol]], x$avewind$dir, sep = " "),
         max_humid = x$maxhumidity,
         min_humid = x$minhumidity,
         ave_humidity = x$avehumidity
    )
  })
  
  data.frame(do.call(rbind, df))
}