
planner = function(location, 
                   key = get_api_key(), 
                   raw = FALSE,
                   message = TRUE) {

  parsed_req = wunderground_request(request_type = "planner",
                                    location = "view",
                                    date = "", ##TODO 
                                    key = key,
                                    message = message)   
  if(raw) {
    return(parsed_req)
  }
  stop_for_error(parsed_req)

}