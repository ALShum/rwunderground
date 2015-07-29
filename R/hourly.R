hourly = function(location,
                  date_fmt = "pretty",  ## TODO 
                  use_metric = TRUE,
                  key = get_api_key(),
                  raw = FALSE,
                  message = TRUE) {

  URL = build_url(key = key, request_type = "hourly", location = location)
  req = httr::GET(URL)
  httr::stop_for_status(req)

  parsed_req = httr::content(req, type = "application/json")

  if(message) {
    print(paste0("Requesting: ", URL))
  }
  if(raw) {
    return(parsed_req)
  }
  stop_for_error(parsed_req)

  if(use_metric) {
    units = "metric"
  } else {
    units = "english"
  }
    
  hourly_forecast = parsed_req$hourly_forecast

  df = lapply(hourly_forecast, function(x){
    list(date = x$FCTTIME$pretty,
         temp = as.numeric(x$temp[[units]]),
         dew_pt = as.numeric(x$dewpoint[[units]]),
         cond = x$condition,
         wind_spd = x$wspd[[units]],
         wind_dir = x$wdir$dir,
         uvi = x$uvi,
         humidity = x$humidity,
         windchill = x$windchill[[units]],
         )
    
  })
}









hourly10day = function(location, key = get_api_key()) {
  URL = build_url(key = key, request_type = "hourly10day", location = location)
  req = httr::GET(URL)
  if(raw) {
    return(parsed_req)
  }

  parsed_req = httr::content(req, type = "application/json")

}
