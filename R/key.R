#' Returns the wunderground API key
#' 
#' @return API key
#' @export 
#' @examples
#' \dontrun{
#' get_api_key()
#' }
get_api_key = function() {
  env = Sys.getenv('WUNDERGROUNDID')
  if(!identical(env, "")) return(env)
  
  if(!interactive()) {
    stop("Please set env var WUNDERGROUNDID to your weather underground API key", call. = FALSE)
  }
  message("Please enter your weather underground API key and press enter:")
  key = readline(": ")
  
  if(identical(key, "")) {
    stop("Invalid key!", call. = FALSE)
  }
  message("Updating WUNDERGROUNDID env var.")
  Sys.setenv(WUNDERGROUNDID = key)
  
  return(key)
}

#' Sets the wunderground API key
#'
#' @param key wunderground API key
#' @return API key
#' @export 
#' @examples
#' \dontrun{
#' set_api_key("1a2b3c4d")
#' }
set_api_key = function(key) {
  if(identical(key, "")) {
    stop("Invalid API key!", call. = FALSE)
  }
  Sys.setenv(WUNDERGROUNDID = key)
  
  return(key)
}

#' Detects if wunderground API key is set
#'
#' @return TRUE if API key set, otherwise FALSE
#'
has_api_key = function() {
  !identical(get_api_key, "")
}