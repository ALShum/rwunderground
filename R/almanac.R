almanac = function(location, 
                   use_metric = TRUE, 
                   key = get_api_key(), 
                   raw = FALSE, 
                   message = TRUE) {
  
  URL = build_url(key = key, request_type = "almanac", location = location)
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
  
  #null almanac
  if(is.null(parsed_req$almanac)) {
    stop(paste0("Unable to parse almanac information from JSON: ", location))
  }
  
  #check for empty almanac
  if(length(parsed_req$almanac) == 0) {
    stop(paste0("No almanac available for this location: ", location))
  }
  
  almanac = parsed_req$almanac
  
  #checks to make sure almanac is structured correctly
  if(!all(c("airport_code", "temp_high", "temp_low") %in% names(almanac))) {
    stop(paste0("Invalid structure for alamanac for this location: "), location)
  }
  
  #Celsius or F
  if(use_metric) {
    tempCol = "C"
  } else {
    tempCol = "F"
  }
  
  return(data.frame(location = location, 
                    airport = almanac$airport_code,
                    avg_high = almanac$temp_high$normal[[tempCol]],
                    rcd_high = almanac$temp_high$record[[tempCol]],
                    avg_low = almanac$temp_low$normal[[tempCol]],
                    rcd_low = almanac$temp_low$record[[tempCol]])
         )
}