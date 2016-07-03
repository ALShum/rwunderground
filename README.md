[![Build Status](https://travis-ci.org/ALShum/rwunderground.svg)](https://travis-ci.org/ALShum/rwunderground)

# Weather Underground R API

This is an R interface to weather underground's [API](http://www.wunderground.com/weather/api).  
In order to use this library please [register](http://www.wunderground.com/weather/api/d/login.html) for an API key.
The free-tier should be sufficient if you aren't calling the API more than a 500 times per day.  Please note that the free tier also limits requests to 10 per minute.  If you are grabbing weather for a large date range using `history_range` then by default `limit = 10` will limit the calls to a maximum of 10 per minute.  This package
has functions that follow the [online api](http://www.wunderground.com/weather/api/d/docs).

## Install and Setup

This package is officially on CRAN; install using `install.packages("rwunderground")`.

To install the latest version please use `devtools`.  If you don't have devtools install using `install.packages("devtools")`.  Afterwards install `rwunderground` using devtools: `devtools::install_github("ALShum/rwunderground")`.

Once you have your API key as indicated above you can set the key in R using: `rwunderground::set_api_key("YOUR KEY")`.  You only have to do this once per R session.  Alternatively you can save the key in your local .Renviron file by adding the line `WUNDERGROUNDID = 'your key here'`.

## Locations
For any of the API functions you must first specify a location -- the first argument of all the API functions is a location.  Locations can be specified by the airport code, zip code, personal weather station ID or simply by specifying state and city (if in US) or country and city (if outside US).  The `set_location` function will validate locations and format things correctly or you can use a (correctly formatted) string.

### Locations by country/state/city
Setting the location to Honolulu, HI:
`set_location(territory = "Hawaii", city = "Honolulu")`.  

Setting the location to Mombasa, Kenya: 
`set_location(territory = "Kenya", city = "Mombasa")`.

Setting the location to San Diego, California:
`set_location(territory = "California", city = "San Diego")`

As alluded above, `set_location` will attempt to validate and make sure the locations are formatted correctly.  If you have trust in your own geography and spelling skills then you can simple replace the `set_location` function with a string formatted as `"territory/city"` such as: `"Hawaii/Honolulu"` or `"Kenya/Mombasa"`.

### Locations by airport
Locations can be specified by airport codes.

Setting the location to Seattle/Tacoma airport:
`set_location(airport_code = "SEA")`

If you don't know the airport code you can look them up using `lookup_airport`:
`lookup_airport("seattle")`.  This will lookup IATA and ICAO airport codes.

### Locations by zip code
`set_location(zip_code = "96813")`

### Locations by lat/long
`set_location(lat_long = "50,-100")`
Note that coordinates should be comma separated.

### Other
If no argument is provided to set_location then by default the nearest weather station will be used.  You can also specify location based on lat/lon or personal weather station ID.

## Package Functionality Summary
Note: by default units are in imperial (temperature is F, windspeed in MPH etc.) -- sorry rest of the world!  To use metric, you can set `use_metric = TRUE` for many of the functions.

### Main Functions
* `history`, `history_daily`, `history_range`: weather history functions
* `forecast3day`, `forecast10day`: daily summary forecasts
* `hourly`, `hourly10day`: hourly forecasts
* `planner`: Historical weather summary for date range

### Additional API Functions
* `alerts`: Weather alerts
* `almanac`: historical weather records for current date
* `astronomy`: sunrise/sunset and moonrise/moonset
* `conditions`: current weather conditions
* `geolookup`: weather station lookup
* `current_hurricane`: current hurricane information
* `satellite`: satellite image URLs
* `tide`, `rawtide`: tide forecasts
* `webcam`: live webcam image URLS
* `yesterday`: historical weather information for yesterday

### Supporting Functions
* `lookup_airport`: Look up airport codes if you plan on looking up weather data using airport locations.

## History
After a location is set, weather history is available using `history`.

To request the weather for Honolulu, HI on January 31, 2015:

`history(set_location(territory = "Hawaii", city = "Honolulu"), date = 20150131)`

Note that dates must be in YYYYMMDD.

## Forecast
Forecast weather data is available using `forecast3day` and `forecast10day`.  Hourly forecasts are also available using `hourly` and  `hourly10day`.  The forecast functions do a daily summary forecast and the hourly functions do hourly forecasts.

To get the 10 day forecast and 10 day hourly forecast for Honolulu, Hawaii:

`forecast10day(set_location(territory = "Hawaii", city = "Honolulu"))`
`hourly10day(set_location(territory = "Hawaii", city = "Honolulu"))`

## Planner
Note that dates for this must be in MMDD form:
`planner(set_location(territory = "IR", city = "Tehran"), start_date = "0101", end_date = "0131")`

## Tide information
Tide high/low forecasts are available using `tide` and hourly tide forecasts available using `rawtide`.

To get the high/low tide information for Honolulu, Hawaii:

`tide(set_location(territory = "Hawaii", city = "Honolulu"))`
`rawtide(set_location(territory = "Hawaii", city = "Honolulu"))`

## Weather Alerts
Weather alerts are available as plain text.

Weather Alerts for Honolulu, Hawaii:
`alerts(set_location(territory = "Hawaii", city = "Honolulu"))`
