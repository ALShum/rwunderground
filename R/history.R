history = function(location, 
                   date = "20150101",
                   daily_summary = TRUE,
                   date_fmt = "pretty",  ## TODO 
                   col_names = "pretty", ## TODO: (orig names)
                   use_metric = TRUE,
                   key = get_api_key(), 
                   raw = FALSE, 
                   message = TRUE) {

  URL = build_url(key = key, request_type = "history", date = date, location = location)
  req = httr::GET(URL)
  httr::stop_for_status(req)
  
  parsed_req = httr::content(req, type = "application/json")
  
  if(message) {
    print(paste0("Requesting: ", URL))
  }
  if(raw) {
    return(parsed_req)
  }
  stop_for_error(parsed_req)
  
  ## TODO:: Check for errors
  hist = parsed_req$history

  if(use_metric) {
  	suffix = "m"
  } else {
  	suffix = "i"
  }

  df = lapply(hist$observations, function(x) {
  	list(date = x$date$pretty, ## TODO:: date 
  		 temp = as.numeric(x[[paste0("temp", suffix)]]),
  		 dew_pt = as.numeric(x[[paste0("dewpt", suffix)]]),
  		 hum = as.numeric(x$hum),
  		 wind_spd = as.numeric(x[[paste0("wspd", suffix)]]),
  		 wind_gust = as.numeric(x[[paste0("wgust", suffix)]]),
  		 dir = x$wdire,
  		 vis = as.numeric(x[[paste0("vis", suffix)]]),
  		 pressure = as.numeric(x[[paste0("pressure", suffix)]]),
  		 wind_chill = as.numeric(x[[paste0("windchill", suffix)]]),
  		 heat_index = as.numeric(x[[paste0("heatindex", suffix)]]),
  		 precip = as.numeric(x[[paste0("precip", suffix)]]),
  		 cond = x$conds,
  		 fog = as.numeric(x$fog),
  		 rain = as.numeric(x$rain),
  		 snow = as.numeric(x$snow),
  		 hail = as.numeric(x$hail),
  		 thunder = as.numeric(x$thunder),
  		 tornado = as.numeric(x$tornado)
  		)
  })

  return(data.frame(do.call(rbind, df)))
}

history_daily = function(location, 
                   date = "20150101",
                   daily_summary = TRUE,
                   date_fmt = "pretty",  ## TODO 
                   col_names = "pretty", ## TODO: (orig names)
                   use_metric = TRUE,
                   key = get_api_key(), 
                   raw = FALSE, 
                   message = TRUE) {
  
  URL = build_url(key = key, request_type = "history", date = date, location = location)
  req = httr::GET(URL)
  httr::stop_for_status(req)
  
  parsed_req = httr::content(req, type = "application/json")
  
  if(message) {
    print(paste0("Requesting: ", URL))
  }
  if(raw) {
    return(parsed_req)
  }
  stop_for_error(parsed_req)
  
  ## TODO:: Check for errors
  hist = parsed_req$history

  if(use_metric) {
  	suffix = "m"
  } else {
  	suffix = "i"
  }

  df = lapply(hist$dailysummary[[1]], function(x) {
  	list(date = x$date$pretty, ## TODO:: date 
  		 fog = as.numeric(x$fog),
  		 rain = as.numeric(x$rain),
  		 snow = as.numeric(x$snow),
  		 snow_fall = as.numeric(x[[paste0("snowfall", suffix)]]),
  		 mtd_snow = as.numeric(x[[paste0("monthtodatesnowfall", suffix)]]),
  		 since_jul_snow = as.numeric(x[[paste0("since1julsnowfall", suffix)]]),
  		 snow_depth = as.numeric(x[[paste0("snowdepth", suffix)]]),
  		 hail = as.numeric(x$hail),
  		 thunder = as.numeric(x$thunder),
  		 tornado = as.numeric(x$tornado),
  		 mean_temp = as.numeric(x[[paste0("meantemp", suffix)]]),
  		 mean_dewp = as.numeric(x[[paste0("meandewpt", suffix)]]),
  		 mean_pressure = as.numeric(x[[paste0("meanpressure", suffix)]]),
  		 mean_wind_spd = as.numeric(x[[paste0("meanwindspd", suffix)]]),
  		 mean_wind_dir = as.numeric(x$meanwdird),
  		 mean_visib = as.numeric(x[[paste0("meanvis", suffix)]]),
  		 humid = as.numeric(x$humidity),
  		 max_temp = as.numeric(x[[paste0("maxtemp", suffix)]]),
  		 min_temp = as.numeric(x[[paste0("mintemp", suffix)]]),
  		 max_humid = as.numeric(x$maxhumidity),
  		 min_humid = as.numeric(x$minhumidity),
  		 max_dew_pt = as.numeric(x[[paste0("maxdewpt", suffix)]]),
  		 min_dew_pt = as.numeric(x[[paste0("mindewpt", suffix)]],
  		 max_pressure = as.numeric(x[[paste0("maxpressure", suffix)]]),
  		 min_pressure = as.numeric(x[[paste0("minpressure", suffix)]]),
  		 max_wind_spd = as.numeric(x[[paste0("maxwspd", suffix)]]),
  		 min_wind_spd = as.numeric(x[[paste0("minwspd", suffix)]]),
  		 max_vis = as.numeric(x[[paste0("maxvis", suffix)]]),
  		 min_vis = as.numeric(x[[paste0("minvis", suffix)]]),
  		 gdegree = as.numeric(x$gdegreedays),
  		 heating_days = as.numeric(x$heatingdegreedays),
  		 cooling_days = as.numeric(x$coolingdegreedays),
  		 precip = as.numeric(x[[paste0("precip", suffix)]]),
  		 precip_source = x$precipsource,
  		 heating_day_normal = as.numeric(x$heatingdegreedaysnormal),
  		 mtd_heating_degree = as.numeric(x$monthtodateheatingdegreedays),
  		 mtd_heating_degree_normal = as.numeric(x$monthtodateheatingdegreedaysnormal),
  		 since_sep_heating = as.numeric(x$since1sepheatingdegreedays), 
  		 since_sep_heating_normal = as.numeric(x$since1sepheatingdegreedaysnormal),
  		 since_jul_heating = as.numeric(x$since1julheatingdegreedays),
  		 since_jul_heating_normal = as.numeric(x$since1julheatingdegreedaysnormal),
  		 cooling_degree_normal = as.numeric(x$coolingdegreedaysnormal),
  		 mtd_cooling_day,
  		 mtd_cooling_day_normal,
  		 since_sep_cooling,
  		 since_sep_cooling_normal,
  		 since_jan_cooling,
  		 since_jan_cooling_normal
  		)
  })

  return(data.frame(do.call(rbind, df)))
}