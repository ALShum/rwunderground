almanac = function(location, 
                   use_metric = TRUE, 
                   key = get_api_key(), 
                   raw = FALSE, 
                   message = TRUE) {
  
  URL = build_url(key = key, request_type = "almanac", location = location)
  req = httr::GET(URL)
  parsed_req = httr::content(req, type = "application/json")
  if(raw) {
    return(parsed_req)
  }
  
  stop_for_error(parsed_req)
  
  #check for empty almanac
  if(length(parsed_req$almanac) == 0) {
    stop(paste0("No Almanac available for this location: ", location))
  }
  
  almanac = parsed_req$almanac
  
  #Celsius
  if(use_metric) {
    
  }
  #Fahrenheit
  else {
    
  }
  
  
}