
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
}