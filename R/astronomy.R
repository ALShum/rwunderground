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
  return(data.frame(location = location,
                    moon_phase = moon$phaseofMoon,
                    pct_visible = moon$percentIlluminated,
                    moon_rise = paste(moon$sunrise$hour, moon$sunrise$minute, sep = ":"),
                    moon_set = paste(moon$sunset$hour, moon$sunset$minute, sep = ":"),
                    sun_rise = paste(sun$sunrise$hour, sun$sunrise$minute, sep = ":"),
                    sun_set = paste(sun$sunset$hour, sun$sunset$minute, sep = ":")))
}