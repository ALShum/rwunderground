#' 
#' 
#' 
planner = function(location, 
                   date_fmt = "pretty", ##TODO
                   use_metric = TRUE,
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

  ##TODO check for structure
  planner = parsed_req$trip

  if(message) {
    print(date$trip$title)
  }
  units_deg = ifelse(metric, "C", "F")
  units_len = ifelse(metric, "cm", "in")

  df = list(
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
    cond = planner$cond
  )

  chance_of = planner$chance_of
  chance = list(
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
    chance_of_snow = chance_of$chanceofsnowday$percentage
  )
}