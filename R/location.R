#' Returns a data.frame of valid airport codes (ICAO and IATA).
#'
#' This dataset is from the openflights.org airport database.  It can be
#' found at \url{http://openflights.org/data.html#airport}.  This data
#' is provided under the open database license -- more information can
#' be found here: \url{http://opendatacommons.org/licenses/odbl/1.0/}.
#' 
#' @return data.frame of airport codes with country and city
#' @export
#' @examples
#' \dontrun{
#' list_airports()
#' }
list_airports = function() {
  airport_data = utils::read.csv(system.file("extdata/airport_data.csv", package = "rwunderground"), header=T, stringsAsFactor = FALSE)
  return(airport_data)
}

#' Returns a data.frame of valid states with abbreviations and regions
#'
#' @return data.frame of states with abbreviation and region
#' @export
#' @examples
#' \dontrun{
#' list_states()
#' }
list_states = function() {
  return(data.frame(abbr = datasets::state.abb, 
                    name = datasets::state.name, 
                    region = datasets::state.region))
}

#' Returns a data.frame of valid countries with iso abbreviations and region
#'
#' @return data.frame of valid country names with iso codes
#' @export
#' @examples
#' \dontrun{
#' list_countries()
#' }
list_countries = function() {
  country_data = countrycode::countrycode_data[, c("country.name", "iso2c", "region")]
  country_data = dplyr::filter(country_data, !is.na(country_data$region))
  
  return(country_data)
}

#' Lookup airport code (IATA and ICAO code).
#' weatherunderground API might not recognize the IATA/ICAO code for smaller airports.
#' 
#' @param location location string 
#' @param region region string
#' @return data.frame of matching airport name and IATA/ICAO codes
#' @export
#' @examples
#' \dontrun{
#' lookup_airport("Honolulu")
#' lookup_airport("Pyongyang")
#' lookup_airport("Portland", region = "Los_Angeles")
#' }
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

#' Lookup ISO country code
#' weatherunderground API doesn't recognize iso codes uniformly for every country.name
#' 
#' @param name Name of country 
#' @param region Geographic region 
#' @return data.frame of country codes
#' @export 
#' @examples
#' \dontrun{
#' lookup_country_code("Korea")
#' lookup_country_code("Guinea", region = "Africa")
#' }
lookup_country_code = function(name, region = NULL) {
  countries = list_countries()
  if(!is.null(region)) {
    found_region = grep(region, countries$region, ignore.case=TRUE)
    countries = countries[found_region, ]
  }
  
  found = grep(name, countries$country.name, ignore.case=TRUE)
  
  return(countries[found, ])
}

#' Checks if country/state is a valid one
#' 
#' @param name Name of state or country 
#' @return TRUE if valid state or country otherwise FALSE 
#'
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

#' Checks if airport code is valid 
#' 
#' @param name Airport code either IATA or ICAO
#' @return TRUE if valid otherwise FALSE
#'
is_valid_airport = function(name) {
  name = tolower(name)
  airports = list_airports()
  
  if(name %in% tolower(airports$IATA)) return(TRUE)
  if(name %in% tolower(airports$ICAO)) return(TRUE)
  
  return(FALSE)
}

#' Specifies location of request
#' 
#' This is a wrapper function that will validate and format location strings
#' for requesting data from weather underground.
#' 
#' @param zip_code zip code
#' @param territory state if in US, otherwise country
#' @param city city name 
#' @param airport_code IATA/ICAO airport code 
#' @param PWS_id personal weather station ID 
#' @param lat_long latitude and longitude
#' @param autoip location based on IP
#' @return formatted and validated location string
#' @export
#' @examples
#' set_location(zip_code = "90210")
#' set_location(territory = "Hawaii", city = "Honolulu")
#' set_location(territory = "Kenya", city = "Mombasa")
#' set_location(airport_code = "SEA")
#' set_location(PWS_id = "KMNCHASK10")
#' set_location(autoip = "172.227.205.140")
#' set_location()
#' 
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
    territory = gsub(" ", "_", territory)
    city = gsub(" ", "_", city)
    return(paste(territory, city, sep = "/"))
  }
  else if(!is.null(zip_code)) {
    return(paste0(zip_code))
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
    return("autoip")
  }
}