# Weather Underground R API

This is an R interface to weather underground's [API](http://www.wunderground.com/weather/api).  
In order to use this library please [register](http://www.wunderground.com/weather/api/d/login.html) for an API key.
The free-tier should be sufficient if you aren't calling the API more than a 500 times per day.  This package
has functions that follow the [online api](http://www.wunderground.com/weather/api/d/docs).

## Install and Setup

To install please use `devtools`.  If you don't have devtools install using `install.packages("devtools")`.  Afterwards install `rwunderground` using devtools: `devtools::install_github("ALShum/rwunderground")`.

Once you have your API key as indicated above you can set the key in R using: `rwunderground::set_api_key("YOUR KEY")`.  You only have to do this once as the key should save in your local .Renviron file.

## Locations
Locations can be specified by the airport code, zip code, personal weather station ID or simply by specifying
state and city (if in US) or country and city (if outside US).  You can use the `set_location` function which will validate locations.

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

### Other
If no argument is provided to set_location then by default the nearest weather station will be used.  You can also specify location based on lat/lon or personal weather station ID.

## Package Functionality Summary

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
* `hurricane`: current hurricane information
* `satellite`: satellite image URLs
* `tide`, `rawtide`: tide forecasts
* `webcam`: live webcam image URLS
* `yesterday`: historical weather information for yesterday

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

## Tide information
Tide high/low forecasts are available using `tide` and hourly tide forecasts available using `rawtide`.

To get the high/low tide information for Honolulu, Hawaii:

`tide(set_location(territory = "Hawaii", city = "Honolulu"))`
`rawtide(set_location(territory = "Hawaii", city = "Honolulu"))`
