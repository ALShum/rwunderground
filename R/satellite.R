#' Returns image URL for satellite imagery 
#' 
#' @param location location set by set_location
#' @param key weather underground API key
#' @param raw if TRUE return raw httr object
#' @param message if TRUE print out requested URL
#' @return URL to satellite imagery
#' @export
satellite = function(location, 
                     key = get_api_key(), 
                     raw = FALSE,
                     message = TRUE) {
  
  parsed_req = wunderground_request(request_type = "satellite",
                                    location = location, 
                                    key = key,
                                	message = message)
  if(raw) {
    return(parsed_req)
  }
  stop_for_error(parsed_req)

  ##TODO :: CHECK FOR STRUCTURE
  satellite = parsed_req$satellite
  return(c(satellite$image_url,
  		   satellite$image_url_ir4,
  		   satellite$image_url_vis))
}