#' Current conditions including current temperature, weather condition, 
#' humidity, wind, feels-like, temperature, barometric pressure, 
#' and visibility.
#'
#' @param location location set by set_location
#' @param use_metric Metric or imperial units
#' @param key weather underground API key
#' @param raw if TRUE return raw httr object
#' @param message if TRUE print out requested URL
#' @return tbl_df with conditions
#' @export 
#' @examples
#' \dontrun{
#' conditions(set_location(territory = "Hawaii", city = "Honolulu"))
#' conditions(set_location(airport_code = "SEA"))
#' conditions(set_location(zip_code = "90210"))
#' conditions(set_location(territory = "IR", city = "Tehran"))
#' }
conditions = function(location, 
                      use_metric = FALSE,
                      key = get_api_key(), 
                      raw = FALSE,
                      message = TRUE) {
  parsed_req = wunderground_request(request_type = "conditions",
                                    location = location, 
                                    key = key,
                                    message = message)
  if(raw) {
    return(parsed_req)
  }
  stop_for_error(parsed_req)

  if(!("current_observation" %in% names(parsed_req))) {
    stop(paste0("Unable to  parse conditions for this location: ", location))
  }

  cond = parsed_req$current_observation

  if(message) {
    print(paste0('Conditions for: ', cond$display_location$full))
    print(paste0('Observed at: ', cond$observation_location$full))
    print(paste0('Station id: ', cond$station_id))
    print(paste0('Time: ', cond$observation_time))
  }

  if(use_metric) {
    degree = "c"
    speed = "kph"
    pres = "mb"
    dist = "km"
    precip = "metric"
  } else {
    degree = "f"
    speed = "mph"
    pres = "in"
    dist = "mi"
    precip = "in"
  }

  df = data.frame(
    weather = cond$weather,
    temp = as.numeric(cond[[paste0("temp_", degree)]]),
    pct_humidity = as.numeric(gsub("%", "", cond$relative_humidity)),
    wind_spd = as.numeric(cond[[paste0("wind_", speed)]]),
    wind_spd_gust = as.numeric(cond[[paste0("wind_gust_", speed)]]),
    wind_dir = cond$wind_dir,
    pressure = as.numeric(cond[[paste0("pressure_", pres)]]),
    dew_pt = as.numeric(cond[[paste0("dewpoint_", degree)]]),
    heat_index = as.numeric(cond[[paste0("heat_index_", degree)]]),
    windchill = as.numeric(cond[[paste0("windchill_", degree)]]),
    feelslike = as.numeric(cond[[paste0("feelslike_", degree)]]),
    visibility = as.numeric(cond[[paste0("visibility_", dist)]]),
    UV = as.numeric(cond$UV),
    precip_1hr = as.numeric(cond[[paste0("precip_1hr_", precip)]]),
    precip_today = as.numeric(cond[[paste0("precip_today_", precip)]])
  )

  return(dplyr::tbl_df(df))
}