forecast = function(location, key = get_api_key()) {
  URL = build_url(key = key, request_type = "forecast", location = location)
  req = httr::GET(URL)
  parsed_req = httr::content(req, type = "application/json")
  stop_for_error(parsed_req)
  
  
}

geolookup = function(location, key = get_api_key()) {
  URL = build_url(key = key, request_type = "geolookup", location = location)
  req = httr::GET(URL)
  parsed_req = httr::content(req, type = "application/json")
  
}

hourly = function(location, key = get_api_key()) {
  URL = build_url(key = key, request_type = "hourly", location = location)
  req = httr::GET(URL)
  parsed_req = httr::content(req, type = "application/json")
  
}

