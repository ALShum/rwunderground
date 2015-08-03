yesterday = function(location, 
                   	 use_metric = TRUE, 
                     key = get_api_key(), 
                     raw = FALSE, 
                     message = TRUE,
                     summary = FALSE) {

  parsed_req = wunderground_request(request_type = "yesterday",
                                    location = location, 
                                    key = key,
                                    message = message)
  if(raw) {
    return(parsed_req)
  }
  stop_for_error(parsed_req)

  hist = parsed_req$history
  suffix = ifelse(use_metric, "m", "i")
  if(summary) {
    ds = hist$dailysummary[[1]]
    df = list(date = ds$date$pretty, ## TODO:: date 
       fog = as.numeric(ds$fog),
       rain = as.numeric(ds$rain),
       snow = as.numeric(ds$snow),
       snow_fall = as.numeric(x[[paste0("snowfall", suffix)]]),
       mtd_snow = as.numeric(x[[paste0("monthtodatesnowfall", suffix)]]),
       since_jul_snow = as.numeric(x[[paste0("since1julsnowfall", suffix)]]),
       snow_depth = as.numeric(x[[paste0("snowdepth", suffix)]]),
       hail = as.numeric(ds$hail),
       thunder = as.numeric(ds$thunder),
       tornado = as.numeric(ds$tornado),
       mean_temp = as.numeric(x[[paste0("meantemp", suffix)]]),
       mean_dewp = as.numeric(x[[paste0("meandewpt", suffix)]]),
       mean_pressure = as.numeric(x[[paste0("meanpressure", suffix)]]),
       mean_wind_spd = as.numeric(x[[paste0("meanwindspd", suffix)]]),
       mean_wind_dir = as.numeric(ds$meanwdird),
       mean_visib = as.numeric(x[[paste0("meanvis", suffix)]]),
       humid = as.numeric(ds$humidity),
       max_temp = as.numeric(x[[paste0("maxtemp", suffix)]]),
       min_temp = as.numeric(x[[paste0("mintemp", suffix)]]),
       max_humid = as.numeric(ds$maxhumidity),
       min_humid = as.numeric(ds$minhumidity),
       max_dew_pt = as.numeric(x[[paste0("maxdewpt", suffix)]]),
       min_dew_pt = as.numeric(x[[paste0("mindewpt", suffix)]]),
       max_pressure = as.numeric(x[[paste0("maxpressure", suffix)]]),
       min_pressure = as.numeric(x[[paste0("minpressure", suffix)]]),
       max_wind_spd = as.numeric(x[[paste0("maxwspd", suffix)]]),
       min_wind_spd = as.numeric(x[[paste0("minwspd", suffix)]]),
       max_vis = as.numeric(x[[paste0("maxvis", suffix)]]),
       min_vis = as.numeric(x[[paste0("minvis", suffix)]]),
       gdegree = as.numeric(ds$gdegreedays),
       heating_days = as.numeric(ds$heatingdegreedays),
       cooling_days = as.numeric(ds$coolingdegreedays),
       precip = as.numeric(x[[paste0("precip", suffix)]]),
       precip_source = ds$precipsource,
       heating_day_normal = as.numeric(ds$heatingdegreedaysnormal),
       mtd_heating_degree = as.numeric(ds$monthtodateheatingdegreedays),
       mtd_heating_degree_normal = as.numeric(ds$monthtodateheatingdegreedaysnormal),
       since_sep_heating = as.numeric(ds$since1sepheatingdegreedays), 
       since_sep_heating_normal = as.numeric(ds$since1sepheatingdegreedaysnormal),
       since_jul_heating = as.numeric(ds$since1julheatingdegreedays),
       since_jul_heating_normal = as.numeric(ds$since1julheatingdegreedaysnormal),
       cooling_degree_normal = as.numeric(ds$coolingdegreedaysnormal),
       mtd_cooling_day = as.numeric(ds$monthtodatecoolingdegreedays),
       mtd_cooling_day_normal = as.numeric(ds$monthtodatecoolingdegreedaysnormal),
       since_sep_cooling = as.numeric(ds$since1sepcoolingdegreedays),
       since_sep_cooling_normal = as.numeric(ds$since1sepcoolingdegreedaysnormal),
       since_jan_cooling = as.numeric(ds$since1jancoolingdegreedays),
       since_jan_cooling_normal = as.numeric(ds$since1jancoolingdegreedaysnormal)
    )

    return(as.data.frame(df))
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