#' Current hurricane - within the US only.
#' Note: all times in eastern
#' @param key weather underground API key
#' @param use_metric Metric or imperial units
#' @param raw if TRUE return raw httr object
#' @param message if TRUE print out requested URL
#' @return Hurricane info
#' @export
#' @examples
#' \dontrun{
#' current_hurricane()
#' }
current_hurricane = function(key = get_api_key(), 
                             use_metric = FALSE,
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

  if(!("currenthurricane" %in% names(parsed_req))) {
    stop("Cannot parse hurricane information")
  }

  hurricane = parsed_req$currenthurricane

  if(use_metric) {
    spd = "Kph"
    pres = "mb"
  } else {
    spd = "Mph"
    pres = "inches"
  } #spd also available but not used: Kts

  df = lapply(hurricane, function(x) {
    data.frame(
      name = x$stormInfo$stormName_Nice,
      time = as.POSIXct(as.numeric(x$Current$Time$epoch), origin = "1970-01-01", tz = "EST"),
      lat = as.numeric(x$Current$lat),
      lon = as.numeric(x$Current$lon),
      saffirsimpsoncat = as.numeric(x$Current$SaffirSimpsonCategory),
      wind_spd = as.numeric(x$Current$WindSpeed[[spd]]),
      wind_gust = ifelse(is.null(x$Current$WindGust[[spd]]),
        NA,
        as.numeric(x$Current$WindGust[[spd]])
      ),
      fspeed = ifelse(is.null(x$Current$Fspeed[[spd]]),
        NA,
        as.numeric(x$Current$Fspeed[[spd]])
      ),
      movement = as.numeric(x$Current$Movement$Degrees),
      pressure = ifelse(is.null(x$Current$Pressure[[pres]]),
        NA,
        as.numeric(x$Current$Pressure[[pres]])
      ),
        stringsAsFactors = FALSE
    )
  })

  dplyr::bind_rows(df)
}
