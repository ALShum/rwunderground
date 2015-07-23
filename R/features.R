forecast = function(location, key = get_api_key()) {
  URL = base_url(key)
  URL = paste(URL, "forecast", "q", location)
}