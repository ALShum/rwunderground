#' Current hurricane - within the US only
#' 
#' @param key weather underground API key
#' @param raw if TRUE return raw httr object
#' @param message if TRUE print out requested URL
#' @return Hurricane info
#' @export
current_hurricane = function(key = get_api_key(), 
                             raw = FALSE,
                             message = TRUE) {

  parsed_req = wunderground_request(request_type = "currenthurricane",
                                    location = "view", 
                                    key = key,
                                    message = message)   
  if(raw) {
    return(parsed_req)
  }
  stop_for_error(parsed_req)


}
