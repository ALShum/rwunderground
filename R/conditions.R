## TODO

conditions = function(location, key = get_api_key(), raw = FALSE) {
  URL = build_url(key = key, request_type = "conditions", location = location)
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
}