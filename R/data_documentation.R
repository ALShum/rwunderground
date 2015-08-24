#' Airport codes 
#' 
#' Name, city, country, IATA, ICAO and region of airports
#'
#' This data is a reformatted version of the Airport database from 
#' OpenFlights.org.  This dataset is under the open database license.
#' More information can be found at 
#' \url{http://opendatacommons.org/licenses/odbl/1.0/}.
#'
#' @format A data.frame with 7383 rows and 6 variables:
#' \describe{
#'	 \item{airport_name}{Name of the airport}
#' 	 \item{city}{City}
#'	 \item{country}{Country}
#' 	 \item{IATA}{IATA airport code}
#'	 \item{ICAO}{ICAO airport code}
#'	 \item{region}{Country region}
#' }
#' @source \url{http://openflights.org/data.html}
"airport_data"