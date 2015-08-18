# Weather Underground R API

This is an R interface to weather underground's [API](http://www.wunderground.com/weather/api).  
In order to use this library please [register](http://www.wunderground.com/weather/api/d/login.html) for an API key.
The free-tier should be sufficient if you aren't calling the API more than a thousand times per day.  This package
has functions that follow the [online api](http://www.wunderground.com/weather/api/d/docs).

## Install

To install please use `devtools`.  If you don't have devtools install using `install.packages("devtools")`.  Once devtools is installed to install `rwunderground` using devtools: `devtools::install_github("ALShum/rwunderground")`.

## Setup

Once you have your API key as indicated above you can set the key in R using: `rwunderground::set_api_key("YOUR KEY")`.

## Locations
Locations can be specified by the airport code, zip code, personal weather station ID or simply by specifying
state and city (if in US) or country and city (if outside US).  

### Locations by country/state/city
Setting the location to Honolulu, HI:
`set_location(territory = "Hawaii", city = "Honolulu")`.  

Setting the location to Mombasa, Kenya: 
`set_location(territory = "Kenya", city = "Mombasa")`.

### Locations by airport


### Locations by zip code

