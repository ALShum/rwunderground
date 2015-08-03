current_hurricane = function(location, key = get_api_key(), raw = FALSE) {
  ##TODO
  URL = build_url(key = key, request_type = "currenthurricane", location = NULL)
  req = httr::GET(URL)
  parsed_req = httr::content(req, type = "application/json")
  if(raw) {
    return(parsed_req)
  }
}

planner = function(location, key = get_api_key()) {
  URL = build_url(key = key, request_type = "planner", location = location)
  ##TODO
  req = httr::GET(URL)
  if(raw) {
    return(parsed_req)
  }

  parsed_req = httr::content(req, type = "application/json")

}

satellite = function(location, key = get_api_key()) {
  URL = build_url(key = key, request_type = "satellite", location = location)
  req = httr::GET(URL)
  if(raw) {
    return(parsed_req)
  }

  parsed_req = httr::content(req, type = "application/json")

}

webcam = function(location, key = get_api_key()) {
  URL = build_url(key = key, request_type = "webcam", location = location)
  req = httr::GET(URL)
  if(raw) {
    return(parsed_req)
  }

  parsed_req = httr::content(req, type = "application/json")

}
