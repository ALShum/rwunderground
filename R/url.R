base_url = function() {
  return("http://api.wunderground.com/api")
}

build_url = function(key = get_api_key(), request_type, location) {
  URL = paste(base_url, key, request_type, "q", location)
  return(URL)
}