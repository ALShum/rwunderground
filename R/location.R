#https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat
list_airports = function() {
  airport_data = read.csv(system.file("data/airports.dat", package = "rwunderground"), header=F, stringsAsFactor = FALSE)
  airport_data = airport_data[,c(2:6, 12)]
  names(airport_data) = c("airport_name", "city", "country", "IATA", "ICAO", "region")
  airport_data[airport_data$ICAO == "\\N", ]$ICAO = NA
  airport_data[airport_data$IATA == "", ]$IATA = NA
  airport_data = dplyr::filter(airport_data, !is.na(IATA) | !is.na(ICAO))
  
  return(airport_data)
}

list_states = function() {
  return(data.frame(abbr = state.abb, name = state.name))
}

list_countries = function() {
  country_data = countrycode::countrycode_data[, c("country.name", "iso2c", "region")]
  country_data = dplyr::filter(country_data, !is.na(region))
  
  return(country_data)
}

list_zipcodes = function() {
  
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

lookup_country_code = function(name, region = NULL) {
  countries = list_countries()
  if(!is.null(region)) {
    found_region = grep(region, countries$region, ignore.case=TRUE)
    countries = countries[found_region, ]
  }
  
  found = grep(name, countries$country.name, ignore.case=TRUE)
  
  return(countries[found, ])
}

lookup_pws = function(name, region = NULL) {
  
}

is_valid_territory = function(name) {
  name = tolower(name)
  states = list_states()
  countries = list_countries()
  
  if(name %in% tolower(states$abbr)) return(TRUE)
  if(name %in% tolower(states$name)) return(TRUE)
  if(name %in% tolower(states$country.name)) return(TRUE)
  if(name %in% tolower(states$iso2c)) return(TRUE)
  if(name %in% tolower(countries$country.name)) return(TRUE)
  if(name %in% tolower(countries$iso2c)) return(TRUE)
  
  return(FALSE)
}

is_valid_airport = function(name) {
  name = tolower(name)
  airports = list_airports()
  
  if(name %in% tolower(airports$IATA)) return(TRUE)
  if(name %in% tolower(airports$ICAO)) return(TRUE)
  
  return(FALSE)
}

is_valid_zip_code = function(code) {
  zip = list_zipcodes()
  
  return(FALSE) 
}

#' Specifies location of request
#' 
#' This is a wrapper function that will validate and format location strings
#' for requesting data from weather underground.
#' 
#' @param zip_code 
#' @param territory 
#' @param city 
#' @param airport_code
#' @param PWS_id
#' @param lat_long
#' @param autoip
#' @return formatted and validated location string
#' @export
set_location = function(zip_code = NULL,  
                        territory = NULL, city = NULL,
                        airport_code = NULL,
                        PWS_id = NULL,
                        lat_long = NULL,
                        autoip = NULL) {
  params = as.list(environment())
  if(xor(!is.null(territory), !is.null(city))) {
    warning("set_location: Specify both state/country and city")
  }
  
  if(!is.null(territory) & !is.null(city)) {
    if(!is_valid_territory(territory)) warning("set_location: Invalid state/country")
    return(paste(territory, city, sep = "/"))
  }
  else if(!is.null(zip_code)) {
    if(!is_valid_zip_code(zip_code)) warning("set_location: Invalid zipcode")
    return(as.character(zip_code))
  }
  else if(!is.null(airport_code)) {
    if(!is_valid_airport(airport_code)) warning("set_location: Invalid airport code")
    return(airport_code)
  }
  else if(!is.null(PWS_id)) {
    return(paste0("pws:", PWS_id))
  }
  else if(!is.null(lat_long)) {
    return(lat_long)
  } 
  else if(!is.null(autoip)) {
    return(paste0("autoip.json?geo_ip=", autoip))
  }
  else {
    ##TODO
    return("autoip")
  }
}



