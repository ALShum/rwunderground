base_url = function() {
  return("http://api.wunderground.com/api")
}

build_url = function(key = get_api_key(), request_type, location) {
  location = paste0(location, ".json")
  URL = paste(base_url(), key, request_type, "q", location, sep = "/")
  return(URL)
}

stop_for_error = function(httr_parsed_req) {
  if(is.null(httr_parsed_req$response$error)) {
    return(invisible(TRUE))
  } else {
    type = httr_parsed_req$response$error$type
    description =  httr_parsed_req$response$error$description
    stop(paste0("Error from server:: ", type, " - ", description))
  }
}