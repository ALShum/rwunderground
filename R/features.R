almanac = function(location, key = get_api_key(), raw = FALSE) {
  URL = build_url(key = key, request_type = "almanac", location = location)
  req = httr::GET(URL)
  parsed_req = httr::content(req, type = "application/json")
  if(raw) {
    return(parsed_req)
  }
}

astronomy = function(location, key = get_api_key(), raw = FALSE) {
  URL = build_url(key = key, request_type = "astronomy", location = location)
  req = httr::GET(URL)
  parsed_req = httr::content(req, type = "application/json")
  if(raw) {
    return(parsed_req)
  }
}

conditions = function(location, key = get_api_key(), raw = FALSE) {
  URL = build_url(key = key, request_type = "conditions", location = location)
  req = httr::GET(URL)
  parsed_req = httr::content(req, type = "application/json")
  if(raw) {
    return(parsed_req)
  }
}

current_hurricane = function(location, key = get_api_key(), raw = FALSE) {
  ##TODO
  URL = build_url(key = key, request_type = "currenthurricane", location = NULL)
  req = httr::GET(URL)
  parsed_req = httr::content(req, type = "application/json")
  if(raw) {
    return(parsed_req)
  }
}

forecast = function(location, key = get_api_key(), raw = FALSE) {
  URL = build_url(key = key, request_type = "forecast", location = location)
  req = httr::GET(URL)
  parsed_req = httr::content(req, type = "application/json")
  if(raw) {
    return(parsed_req)
  }
  
  stop_for_error(parsed_req)

}

forecast10day = function(location, key = get_api_key(), raw = FALSE) {
  URL = build_url(key = key, request_type = "forecast", location = location)
  req = httr::GET(URL)
  parsed_req = httr::content(req, type = "application/json")
  if(raw) {
    return(parsed_req)
  }
  
  stop_for_error(parsed_req)
  
}

geolookup = function(location, key = get_api_key()) {
  URL = build_url(key = key, request_type = "geolookup", location = location)
  req = httr::GET(URL)
  if(raw) {
    return(parsed_req)
  }
  
  parsed_req = httr::content(req, type = "application/json")
  
}

history = function(location, key = get_api_key()) {
  URL = build_url(key = key, request_type = "history", location = location)
  req = httr::GET(URL)
  if(raw) {
    return(parsed_req)
  }
  
  parsed_req = httr::content(req, type = "application/json")
  
}

hourly = function(location, key = get_api_key()) {
  URL = build_url(key = key, request_type = "hourly", location = location)
  req = httr::GET(URL)
  if(raw) {
    return(parsed_req)
  }
  
  parsed_req = httr::content(req, type = "application/json")
  
}

hourly10day = function(location, key = get_api_key()) {
  URL = build_url(key = key, request_type = "hourly10day", location = location)
  req = httr::GET(URL)
  if(raw) {
    return(parsed_req)
  }
  
  parsed_req = httr::content(req, type = "application/json")
  
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

rawtide = function(location, key = get_api_key()) {
  URL = build_url(key = key, request_type = "rawtide", location = location)
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

tide = function(location, key = get_api_key()) {
  URL = build_url(key = key, request_type = "tide", location = location)
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

yesterday = function(location, key = get_api_key()) {
  URL = build_url(key = key, request_type = "yesterday", location = location)
  req = httr::GET(URL)
  if(raw) {
    return(parsed_req)
  }
  
  parsed_req = httr::content(req, type = "application/json")
  
}