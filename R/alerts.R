#' Weather Alerts for United States and Europe
#'
#' @param location location set by set_location
#' @param key weather underground API key
#' @param raw if TRUE return raw httr object
#' @param raw_JSON if TRUE return entire alert as JSON
#' @param message if TRUE print out requested URL
#' @return A string containing alert type, message, start time and expiration.
#' @export 
#' @examples
#' \dontrun{
#' alerts(set_location(territory = "Hawaii", city = "Honolulu"))
#' alerts(set_location(airport_code = "SEA"))
#' alerts(set_location(zip_code = "90210"))
#' alerts(set_location(territory = "IR", city = "Tehran"))
#' }
alerts = function(location, 
                  key = get_api_key(), 
                  raw = FALSE, 
                  raw_JSON = FALSE, 
                  message = TRUE) {
  
  parsed_req = wunderground_request(request_type = "alerts",
                                    location = location, 
                                    key = key,
                                    message = message)
  if(raw) {
    return(parsed_req)
  }
  stop_for_error(parsed_req)

  if(raw_JSON) {
    return(parsed_req$alerts)
  }
  
  #checks for empty alert
  if(length(parsed_req$alerts) == 0) {
    return(paste0("No alerts found at this location: ", location))
  }
  
  zone = parsed_req$query_zone
  alerts = parsed_req$alerts
  if(is.null(zone) | is.null(alerts)) {
    stop("Unable to parse zone or alert information from JSON")
  }
  
  alert_status = ""
  
  #Europe
  if(zone == "999") {
    alert_status = sapply(alerts, function(x) {
      if(is.null(x$wtype_meteoalarm_name) | 
         is.null(x$level_meteoalarm_description) | 
         is.null(x$date) |
         is.null(x$expires)) {
        warning("alerts:: unknown formatting for alerts JSON")
      }
      
      paste0("Type: ", alerts$wtype_meteoalarm_name, "\n",
             alerts$level_meteoalarm_description, "\n",
             "Start: ", alerts$date, "\r\n",
             "Expires: ", alerts$expires)
    })
  } 
  
  #USA
  else {
    alert_status = sapply(alerts, function(x) {
      if(is.null(x$description) | 
         is.null(x$message) | 
         is.null(x$date) |
         is.null(x$expires)) {
        warning("alerts:: unknown formatting for alerts JSON")
      }
      
      paste0("Type: ", x$description, "\n",
             x$message, "\n",
             "Start: ", x$date, "\n",
             "Expires: ", x$expires)
    })
  }
  
  return(alert_status)
}