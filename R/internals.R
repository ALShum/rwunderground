#####
# Internal pacakage functions for URL handling and data.frame formatting
#####

#' Base URL for wunderground API
#'
#' @return base wunderground URL
#' 
base_url = function() {
  return("http://api.wunderground.com/api")
}

#' Build wunderground request URL
#'
#' @param key wunderground API key 
#' @param request_type request type TODO::list all request_types
#' @param date Date, only applicable for history requests 
#' @param location location set by set_location 
#'
build_url = function(key = get_api_key(), 
                     request_type, 
                     date, 
                     location) {

  location = paste0(location, ".json")

  #check if request_type supports adding in a date
  if(!is.null(date) & !(request_type %in% c("history", "planner"))) {
    warning("Ignoring date as it is not used in this request.")
  } else if(!is.null(date) & (request_type %in% c("history", "planner"))) {
    request_type = paste(request_type, date, sep = "_")
  } 
    
  URL = paste(base_url(), key, request_type, "q", location, sep = "/")
  return(URL)
}

#' Detect and stop for any wunderground request errors 
#' 
#' @param httr_parsed_req httr request object 
#'
stop_for_error = function(httr_parsed_req) {
  if(is.null(httr_parsed_req$response)) {
    stop("Unknown error: Server failed to provide response status")
  }
  
  if(is.null(httr_parsed_req$response$error)) {
    return(invisible(TRUE))
  } else {
    type = httr_parsed_req$response$error$type
    description =  httr_parsed_req$response$error$description
    stop(paste0("Error from server:: ", type, " - ", description))
  }
}

#' wunderground api requests
#'
#' @param request_type Request type TODO::list all types 
#' @param location locations set of set_location
#' @param date Date, only applicable for history requests
#' @param key wunderground API key
#' @param message if TRUE print out requested 
#' @return httr request object 
#'
wunderground_request = function(request_type,
                                location, 
                                date = NULL,
                                key = get_api_key(),
                                message = TRUE) {
  URL = build_url(key = key, 
                  request_type = request_type, 
                  date = date,
                  location = location)
  if(request_type == "currenthurricane") URL = gsub("/q", "", URL)
  req = httr::GET(URL)
  httr::stop_for_status(req)
  
  parsed_req = httr::content(req, type = "application/json")
  
  if(message) {
    print(paste0("Requesting: ", URL))
  }
  
  parsed_req
}

#' Processes data.frames and replaces wunderground's -9999/-999 to NAs 
#'
#' @param df the data.frame to process
#' @return data.frame with correctly encoded NAs
#' 
encode_NA = function(df) {
  df[df == -9999] = NA 
  df[df == -999] = NA

  df
}