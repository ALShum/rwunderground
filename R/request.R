#'
#'
get_api_key = function() {
  env = Sys.getenv('WUNDERGROUNDID')
  if(!identical(env, "")) return(env)
  
  if(!interactive()) {
    stop("Please set env var WUNDERGROUNDID to your weather underground API key", call. = FALSE)
  }
  message("Please enter your weather undderground API key and press enter:")
  key = readline(": ")
  
  if(identical(key, "")) {
    stop("Invalid key!", call. = FALSE)
  }
  message("Updating WUNDERGROUNDID env var.")
  Sys.setenv(WUNDERGROUNDID = key)
  
  return(key)
}

#'
#'
set_api_key = function(key) {
  if(identical(key, "")) {
    stop("Invalid key!", call. = FALSE)
  }
  Sys.setenv(WUNDERGROUNDID = key)
  
  return(key)
}

#'
#'
has_api_key = function() {
  !identical(get_api_key, "")
}

base_url = function() {
  url = paste0("http://api.wunderground.com/api/", get_api_key())
  
  return(url)
}

#http://api.wunderground.com/history
#' add in better error handling
#link = paste0(base_url(), "/history_20140101/", "q/", "VABB.json")
get_request = function(link, date, location) {
  #http headers
  http_response = GET(link)
  req_content = content(http_response)
  
  #weatherunderground response headers
  header = req_content[[1]]
  req_data = req_content[[2]]
  
  #weather data headers
  date.loc = req_data[[1]]
  date.loc.utc = req_data[[2]]
  
  #actual data
  weather = req_data[[3]]
}

#' parses history request
#' 
parse_history = function(JSON_req, UTC=TRUE) {
  parse_entry = function(entry) {
    if(UTC) date = entry[[2]]
    else date = entry[[1]]
    entry = entry[-c(1:2)]
    
    df = data.frame(entry)
    df$date = date$pretty
    
    return(df)
  }
  
  
  lists = lapply(JSON_req, parse_entry)
  do.call(rbind, lists)
}