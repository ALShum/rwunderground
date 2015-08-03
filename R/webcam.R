#' Returns locations of personal weather stations along with URLs for
#' their webcam images
#' 
#' @param location location set by set_location
#' @param key weather underground API key
#' @param raw if TRUE return raw httr object
#' @param message if TRUE print out requested URL
#' @return data.frame of weather stations including:
#' 		   handle, id, city, state, country, tz, lat,
#'		   lon, last updated, image URL and cam URL.
#' @export
webcam = function(location, 
                  key = get_api_key(), 
                  raw = FALSE,
                  message = TRUE) {

  parsed_req = wunderground_request(request_type = "webcams",
                                    location = location, 
                                    key = key,
                                	message = message)
  if(raw) {
    return(parsed_req)
  }
  stop_for_error(parsed_req)

  webcams = parsed_req$webcams
  df = lapply(webcams, function(x) {
  	list(
   	  handle = x$handle,
  	  station_id = x$assoc_station_id,
  	  city = x$city,
  	  state = x$state,
  	  country = x$country,
  	  tzname = x$tzname,
  	  lat = as.numeric(x$lat),
  	  lon = as.numeric(x$lon),
  	  updated = x$updated,
  	  cur_img = x$CURRENTIMAGEURL,
  	  cam_url = x$CAMURL
  	)
  })

  data.frame(do.call(rbind, df))
}

