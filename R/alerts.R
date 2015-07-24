#' Weather Alerts for United States and Europe
#'
#' @param Location location object set by set_location
#' @param key weather underground API key
#' @param raw if TRUE return raw httr object
#' @param raw_JSON if TRUE return entire alert as JSON
#' @param message if TRUE print out requested URL
#' @return A string containing alert type, message, start time and expiration.
#' @export 
alerts = function(location, key = get_api_key(), raw = FALSE, raw_JSON = FALSE, message = TRUE) {
  URL = build_url(key = key, request_type = "alerts", location = location)
  req = httr::GET(URL)
  parsed_req = httr::content(req, type = "application/json")
  
  if(message) {
    print(paste0("Requesting: ", URL))
  }
  if(raw) {
    return(parsed_req)
  }
  stop_for_error(parsed_req)
  
  if(raw_JSON) {
    return(parsed_req$alerts)
  }
  
  #check for empty alert
  if(length(parsed_req$alerts) == 0) {
    return(paste0("No alerts found at this location: ", location))
  }
  
  zone = parsed_req$query_zone
  alerts = parsed_req$alerts[[1]]
  if(is.null(zone) | is.null(alerts)) {
    stop("Unable to parse zone or alert information from JSON")
  }
  
  alert_status = ""
  
  #Europe
  if(zone == "999") {
    if(is.null(alerts$wtype_meteoalarm_name) | 
       is.null(alerts$level_meteoalarm_description) | 
       is.null(alerts$date) |
       is.null(alerts$expires)) {
      warning("alerts:: unknown formatting for alerts JSON")
    }
    alert_status = paste0("Type: ", alerts$wtype_meteoalarm_name, "\n",
                          alerts$level_meteoalarm_description, "\n",
                          "Start: ", alerts$date, "\r\n",
                          "Expires: ", alerts$expires)
  } 
  
  #USA
  else {
    if(is.null(alerts$description) | 
       is.null(alerts$message) | 
       is.null(alerts$date) |
       is.null(alerts$expires)) {
      warning("alerts:: unknown formatting for alerts JSON")
    }
    alert_status = paste0("Type: ", alerts$description, "\n",
                          alerts$message, "\n",
                          "Start: ", alerts$date, "\n",
                          "Expires: ", alerts$expires)
  }
  
  return(alert_status)
}