#' Tidal information for a location within the USA.
#' Tidal information only available for US cities.  Units are in feet.
#'
#' @param location location set by set_location
#' @param key weather underground API key
#' @param raw if TRUE return raw httr object
#' @param message if TRUE print out requested URL
#' @return tbl_df with date, height and type
#' @export
#' @examples
#' \dontrun{
#' tide(set_location(territory = "Hawaii", city = "Honolulu"))
#' tide(set_location(territory = "Washington", city = "Seattle"))
#' tide(set_location(territory = "Louisiana", city = "New Orleans"))
#' }
tide = function(location, 
                key = get_api_key(), 
                raw = FALSE, 
                message = TRUE) {
  
  parsed_req = wunderground_request(request_type = "tide",
                                    location = location, 
                                    key = key,
                                    message = message)
  if(raw) {
    return(parsed_req)
  }
  stop_for_error(parsed_req)

  if(!("tide" %in% names(parsed_req))) {
    stop(paste0("Cannot parse tide information from JSON for: ", location))
  }

  tide = parsed_req$tide

  tide_info = tide$tideInfo[[1]]
  if(all(tide_info == "")) stop(paste0("Tide info not available for: ", location))
  if(length(tide$tideSummary) == 0) stop(paste0("Tide info not available for: ", location))
  if(message) {
    print(paste0(tide_info$tideSite, ": ", tide_info$lat, "/", tide_info$lon))  
  }

  ## summary stats unused (min/max tide for day)
  tide_summary_stats = tide$tideSummaryStats
  tide_summary = tide$tideSummary

  df = lapply(tide_summary, function(x) {
    data.frame(
      date = as.POSIXct(as.numeric(x$date$epoch), origin = "1970-01-01", tz = x$date$tzname),
      height = as.numeric(gsub("ft", "", x$data$height)),
      type = x$data$type,
        stringsAsFactors = FALSE
    )
  })

  tide_df = dplyr::bind_rows(df)
  dplyr::filter(tide_df, !is.na(tide_df$height))
}

#' Raw Tidal data with data every 5 minutes for US locations 
#' Tidal information only available for US cities.  Units are in feet.
#'
#' @param location location set by set_location
#' @param key weather underground API key
#' @param raw if TRUE return raw httr object
#' @param message if TRUE print out requested URL
#' @return tbl_df with time (epoch) and height
#' @export
#' @examples
#' \dontrun{
#' rawtide(set_location(territory = "Hawaii", city = "Honolulu"))
#' rawtide(set_location(territory = "Washington", city = "Seattle"))
#' rawtide(set_location(territory = "Louisiana", city = "New Orleans"))
#' }
rawtide = function(location, 
                   key = get_api_key(), 
                   raw = FALSE, 
                   message = TRUE) {
  
  parsed_req = wunderground_request(request_type = "rawtide",
                                    location = location, 
                                    key = key,
                                    message = message)
  if(raw) {
    return(parsed_req)
  }
  stop_for_error(parsed_req)

  if(!("rawtide" %in% names(parsed_req))) {
    stop(paste0("Cannot parse tide information from JSON for: ", location))
  }

  rawtide = parsed_req$rawtide

  tide_info = rawtide$tideInfo[[1]]
  if(all(tide_info == "")) stop(paste0("Tide info not available for: ", location))
  if(length(rawtide$rawTideObs) == 0) stop(paste0("Tide info not available for: ", location))
  if(message) {
    print(paste0(tide_info$tideSite, ": ", tide_info$lat, "/", tide_info$lon))  
  }

  ## summary stats unused (min/max tide for day)
  rawtide_summary_stats = rawtide$rawTideStats
  rawtide_summary = rawtide$rawTideObs

  tz = rawtide$tideInfo[[1]]$tzname
  df = lapply(rawtide_summary, function(x) {
    data.frame(
      date = as.POSIXct(x$epoch, origin = '1970-01-01', tz = tz),
      height = x$height,
        stringsAsFactors = FALSE
    )
  })

  dplyr::bind_rows(df)
}