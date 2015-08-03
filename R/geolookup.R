geolookup = function(location, 
                     key = get_api_key(), 
                     raw = FALSE, 
                     message = TRUE) {
  
  parsed_req = wunderground_request(request_type = "geolookup",
                                    location = location, 
                                    key = key,
                                    message = message)
  if(raw) {
    return(parsed_req)
  }
  stop_for_error(parsed_req)


}