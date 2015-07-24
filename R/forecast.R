forecast = function(location, 
                    date_fmt,
                    metric,
                    key = get_api_key(), 
                    raw = FALSE, 
                    message = TRUE) {
  URL = build_url(key = key, request_type = "forecast", location = location)
  req = httr::GET(URL)
  parsed_req = httr::content(req, type = "application/json")
  if(raw) {
    return(parsed_req)
  }
  
  stop_for_error(parsed_req)
  
}