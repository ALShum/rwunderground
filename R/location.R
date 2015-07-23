#https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat
list_airports = function() {
  airport_data = read.csv(system.file("data/airports.dat", package = "rwunderground"), header=F)
  airport_data = airport_data[,c(2:6, 12)]
  names(airport_data) = c("airport_name", "city", "country", "IATA", "ICAO", "region")
  airport_data[airport_data$ICAO == "\\N", ]$ICAO = NA
  airport_data[airport_data$IATA == "", ]$IATA = NA
  airport_data = airport_data %>% filter(!is.na(IATA) | !is.na(ICAO))
  
  return(airport_data)
}

list_states = function() {
  return(data.frame(abbr = state.abb, name = state.name))
}

list_countries = function() {
  country_data = countrycode::countrycode_data[, c("country.name", "iso2c", "region")]
  
  return(country_data)
}

lookup_airport = function(location, region = NULL) {
  airports = list_airports()
  if(!is.null(region)) {
    found_region = grep(region, airports$region, ignore.case=TRUE)
    airports = airports[found_region, ]
  }
  
  found = unique(
          c(grep(location, airports$airport_name, ignore.case=TRUE),
            grep(location, airports$city, ignore.case=TRUE),
            grep(location, airports$country, ignore.case=TRUE))
          )
  
  return(airports[found, ])
}

lookup_country_code = function(name, region) {
  countries = list_countries()
  if(!is.null(region)) {
    found_region = grep(region, countries$region, ignore.case=TRUE)
    countries = countries[found_region, ]
  }
  
  found = grep(location, countries[countries$country.name, ], ignore.case=TRUE)
  
  return(countries[found, ])
}

lookup_pws = function() {
  
}




set_location = function(zip_code = NULL,  
                        territory = NULL, city = NULL,
                        airport_code = NULL,
                        PWS_id = NULL,
                        autoip = NULL) {
  params = as.list(environment())
  if(all(is.null(params))) {
    stop("set_location: Specify a location")
  }
  if(xor(!is.null(country), !is.null(non_us_city))) {
    stop("set_location: Specify both territory and city")
  }
  
  if(!is.null(territory) & !is.null(city)) {
    
    return(paste0(territory, city, sep = "/"))
  }
  
}

is_valid_territory = function(name) {
  if(name %in% list_states()$abbr) return(TRUE)
  if(name %in% list_states()$name) return(TRUE)
  if(name %in% list_countries()) return(TRUE)
  
  return(FALSE)
}

