forecast = function(location, 
                    date_fmt = "pretty",  ## TODO
                    metric,
                    key = get_api_key(), 
                    raw = FALSE, 
                    message = TRUE) {
  
  URL = build_url(key = key, request_type = "forecast", location = location)
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
  
  if(!("forecast" %in% names(parsed_req))) {
    stop(paste0("Unable to parse forecast JSON for this location: ", location))
  }
  fcast = parsed_req$forecast$simpleforecast$forecastday
  df = lapply(fcast, function(x){
    
  })
  
  return(df)
} 