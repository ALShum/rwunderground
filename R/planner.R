#' Weather summary based on historical information between the specified dates
#'
#' @param location location set by set_location
#' @param use_metric Metric or imperial units
#' @param start_date Start date as MMDD
#' @param end_date End date as MMDD
#' @param key weather underground API key
#' @param raw if TRUE return raw httr object
#' @param message if TRUE print out requested URL
#' @return tbl_df
#' @export
#' @examples
#' \dontrun{
#' planner(set_location(territory = "Hawaii", city = "Honolulu"), 
#'         start_date = "0101", end_date = "0131")
#' planner(set_location(territory = "Washington", city = "Seattle"), 
#'         start_date = "01201", end_date = "1231")
#' planner(set_location(territory = "Louisiana", city = "New Orleans"), 
#'         start_date = "0501", end_date = "0531")
#' }
planner = function(location, 
                   use_metric = FALSE,
                   start_date = "0501",
                   end_date = "0531",
                   key = get_api_key(), 
                   raw = FALSE,
                   message = TRUE) {

  parsed_req = wunderground_request(request_type = "planner",
                                    location = location,
                                    date = paste0(start_date, end_date), 
                                    key = key,
                                    message = message)   
  if(raw) {
    return(parsed_req)
  }
  stop_for_error(parsed_req)

  if(!("trip" %in% names(parsed_req))) {
    stop(paste0("Cannot parse historical information for: ", location))
  }  

  planner = parsed_req$trip

  if(message) {
    print(planner$title)
  }
  units_deg = ifelse(use_metric, "C", "F")
  units_len = ifelse(use_metric, "cm", "in")

  df = data.frame(
    airport = planner$airport_code,
    temp_high_min = as.numeric(planner$temp_high$min[[units_deg]]),
    temp_high_avg = as.numeric(planner$temp_high$avg[[units_deg]]),
    temp_high_max = as.numeric(planner$temp_high$max[[units_deg]]),
    temp_low_min = as.numeric(planner$temp_low$min[[units_deg]]),
    temp_low_avg = as.numeric(planner$temp_low$avg[[units_deg]]),
    temp_low_max = as.numeric(planner$temp_low$max[[units_deg]]),
    precip_min = as.numeric(planner$precip$min[[units_len]]),
    precip_avg = as.numeric(planner$precip$avg[[units_len]]),
    precip_max = as.numeric(planner$precip$max[[units_len]]),
    dewpt_high_min = as.numeric(planner$dewpoint_high$min[[units_deg]]),
    dewpt_high_avg = as.numeric(planner$dewpoint_high$avg[[units_deg]]),
    dewpt_high_max = as.numeric(planner$dewpoint_high$max[[units_deg]]),
    dewpt_low_min = as.numeric(planner$dewpoint_low$min[[units_deg]]),
    dewpt_low_avg = as.numeric(planner$dewpoint_low$avg[[units_deg]]),
    dewpt_low_max = as.numeric(planner$dewpoint_low$max[[units_deg]]),
    cloud = planner$cloud_cover,
      stringsAsFactors = FALSE
  )

  chance_of = planner$chance_of
  chance = data.frame(
    chance_humid = chance_of$chanceofhumidday$percentage,
    chance_temp_ovr60 = chance_of$tempoversixty$percentage,
    chance_part_cloudy = chance_of$chanceofpartlycloudyday$percentage,
    chance_wind_day = chance_of$chanceofwindyday$percentage,
    chance_sun_cloud_day = chance_of$chanceofsunnycloudyday$percentage,
    chance_precip = chance_of$chanceofprecip$percentage,
    chance_rain = chance_of$chanceofrainday$percentage,
    chance_swelter = chance_of$chanceofsultryday$percentage,
    chance_cloud = chance_of$chanceofcloudyday$percentage,
    chance_thunder = chance_of$chanceofthunderday$percentage,
    chance_temp_90 = chance_of$tempoverninety$percentage,
    chance_tornado = chance_of$chanceoftornadoday$percentage,
    chance_fog = chance_of$chanceoffogday$percentage,
    chance_snow_ground = chance_of$chanceofsnowonground$percentage,
    chance_temp_below_freeze = chance_of$tempbelowfreezing$percentage,
    chance_temp_ovr_freeze = chance_of$tempoverfreezing$percentage,
    chance_hail = chance_of$chanceofhailday$percentage,
    chance_of_snow = chance_of$chanceofsnowday$percentage,
      stringsAsFactors = FALSE
  )

  
  return(dplyr::tbl_df(data.frame(df, chance)))
}