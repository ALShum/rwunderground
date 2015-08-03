tide = function(location, 
                use_metric = TRUE, 
                key = get_api_key(), 
                raw = FALSE, 
                message = TRUE) {
  
  parsed_req = wunderground_request(request_type = "tide",
                                    location = location, 
                                    key = key,
                                    message = message)
  if(raw) {
    return(parsed_req)
  }
  stop_for_error(parsed_req)

  tide = parsed_req$tide
  
}

rawtide = function(location, 
                use_metric = TRUE, 
                key = get_api_key(), 
                raw = FALSE, 
                message = TRUE) {
  
  parsed_req = wunderground_request(request_type = "rawtide",
                                    location = location, 
                                    key = key,
                                    message = message)
  if(raw) {
    return(parsed_req)
  }
  stop_for_error(parsed_req)

  tide = parsed_req$tide

}