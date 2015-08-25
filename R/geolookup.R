#' Lists nearby weather stations for a given location
#'
#' @param location location set by set_location
#' @param use_metric Metric or imperial units
#' @param key weather underground API key
#' @param raw if TRUE return raw httr object
#' @param message if TRUE print out requested URL
#' @return tbl_df of nearby weather stations with:
#'         type, city, state, country, id, lat, lon and
#'         dist (in either mi or km)
#' @export
#' @examples
#' \dontrun{
#' geolookup(set_location(territory = "Hawaii", city = "Honolulu"))
#' geolookup(set_location(airport_code = "SEA"))
#' geolookup(set_location(zip_code = "90210"))
#' geolookup(set_location(territory = "IR", city = "Tehran"))
#' }
geolookup = function(location, 
                     use_metric = FALSE,
                     key = get_api_key(), 
                     raw = FALSE, 
                     message = TRUE) {
  
  parsed_req = wunderground_request(request_type = "geolookup",
                                    location = location, 
                                    key = key,
                                    message = message)
  if(raw) {
    return(parsed_req)
  }
  stop_for_error(parsed_req)

  if(!("location" %in% names(parsed_req))) {
    stop(paste0("Cannot parse geography information for: ", location))
  }

  loc = parsed_req$location
  if(message) {
    print(paste0(loc$country_iso3166, ", ", loc$state, " ", loc$city))
    print(paste0("tz: ", loc$tz_long))
    print(paste0("lat/long: ", loc$lat, "/", loc$lon))
  }

  ws = loc$nearby_weather_stations
  airport = ws$airport$station
  pws = ws$pws$station

  units = ifelse(use_metric, "km", "mi")
  airport_df = lapply(airport, function(x) {
    data.frame(
      type = "airport",
      city = x$city,
      state = x$state,
      country = x$country,
      id = x$icao,
      lat = as.numeric(x$lat),
      lon = as.numeric(x$lon),
        stringsAsFactors = FALSE
    )
  })
  airport_df = dplyr::bind_rows(airport_df)
  airport_df$dist = NA

  pws_df = lapply(pws, function(x) {
    data.frame(
      type = "pws",
      city = x$city,
      state = x$state,
      country = x$country,
      id = x$id,
      lat = as.numeric(x$lat),
      lon = as.numeric(x$lon),
      dist = x[[paste0("distance_", units)]],
        stringsAsFactors = FALSE
    )
  })
  pws_df = dplyr::bind_rows(pws_df)

  geo_df = dplyr::rbind_list(airport_df, pws_df)
  dplyr::filter(geo_df, !is.na(geo_df$lat) | !is.na(geo_df$lon))
}