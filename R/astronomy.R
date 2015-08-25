#' Moon phase, sunrise and sunset times for today.
#' 
#' @param location location set by set_location
#' @param key weather underground API key
#' @param raw if TRUE return raw httr object
#' @param message if TRUE print out requested URL
#' @return tbl_df with: location, moon phase, percent visible,
#'         moon rise and set times, sun rise and set times.
#' @export 
#' @examples
#' \dontrun{
#' astronomy(set_location(territory = "Hawaii", city = "Honolulu"))
#' astronomy(set_location(airport_code = "SEA"))
#' astronomy(set_location(zip_code = "90210"))
#' astronomy(set_location(territory = "IR", city = "Tehran"))
#' }
astronomy = function(location,
                     key = get_api_key(), 
                     raw = FALSE,
                     message = TRUE) {

  parsed_req = wunderground_request(request_type = "astronomy",
                                    location = location, 
                                    key = key,
                                    message = message)
  if(raw) {
    return(parsed_req)
  }
  stop_for_error(parsed_req)

  if(!all(c("moon_phase", "sun_phase") %in% names(parsed_req))) {
    stop(paste0("Unable to parse astronomy JSON for this location: ", location))
  } 
  
  moon = parsed_req$moon_phase
  sun = parsed_req$sun_phase
  return(dplyr::tbl_df(data.frame(location = location,
                    date = Sys.Date(),
                    moon_phase = moon$phaseofMoon,
                    pct_visible = moon$percentIlluminated,
                    moon_rise = paste(moon$sunrise$hour, moon$sunrise$minute, sep = ":"),
                    moon_set = paste(moon$sunset$hour, moon$sunset$minute, sep = ":"),
                    sun_rise = paste(sun$sunrise$hour, sun$sunrise$minute, sep = ":"),
                    sun_set = paste(sun$sunset$hour, sun$sunset$minute, sep = ":"),
                      stringsAsFactors=FALSE)))
}