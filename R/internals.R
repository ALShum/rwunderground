#####
# Internal pacakage functions for URL handling and data.frame formatting
#####

#' Base URL for wunderground API
#'
#' @return base wunderground URL
#'
base_url <- function() {
  return("http://api.wunderground.com/api")
}

#' Build wunderground request URL
#'
#' @param key wunderground API key
#' @param request_type request type TODO::list all request_types
#' @param date Date, only applicable for history requests
#' @param location location set by set_location
#'
build_url <- function(key = get_api_key(),
                      request_type,
                      date,
                      location) {
  location <- paste0(location, ".json")

  # check if request_type supports adding in a date
  if (!is.null(date) & !(request_type %in% c("history", "planner"))) {
    warning("Ignoring date as it is not used in this request.")
  } else if (!is.null(date) & (request_type %in% c("history", "planner"))) {
    request_type <- paste(request_type, date, sep = "_")
  }

  URL <- paste(base_url(), key, request_type, "q", location, sep = "/")
  return(URL)
}

#' Detect and stop for any wunderground request errors
#'
#' @param httr_parsed_req httr request object
#'
stop_for_error <- function(httr_parsed_req) {
  if (is.null(httr_parsed_req$response)) {
    stop("Unknown error: Server failed to provide response status")
  }

  if (is.null(httr_parsed_req$response$error)) {
    return(invisible(TRUE))
  } else {
    type <- httr_parsed_req$response$error$type
    description <- httr_parsed_req$response$error$description
    stop(paste0("Error from server:: ", type, " - ", description))
  }
}

#' wunderground api requests
#'
#' @param request_type Request type TODO::list all types
#' @param location locations set of set_location
#' @param date Date, only applicable for history requests
#' @param key wunderground API key
#' @param message if TRUE print out requested
#' @return httr request object
#'
wunderground_request <- function(request_type,
                                 location,
                                 date = NULL,
                                 key = get_api_key(),
                                 message = TRUE) {
  URL <- build_url(
    key = key,
    request_type = request_type,
    date = date,
    location = location
  )
  if (request_type == "currenthurricane") URL <- gsub("/q", "", URL)
  req <- httr::GET(URL)
  httr::stop_for_status(req)

  parsed_req <- httr::content(req, type = "application/json")

  if (message) {
    print(paste0("Requesting: ", URL))
  }

  parsed_req
}

#' Processes data.frames and replaces wunderground's -9999/-999 to NAs
#'
#' @param df the data.frame to process
#' @return data.frame with correctly encoded NAs
#'
encode_NA <- function(df) {
  df[df == -9999] <- NA
  df[df == -999] <- NA
  df[df == -99] <- NA

  df
}

#' as.numeric with special handling for length 0 (NULL) objects
#'
#' @param x the object to cast as numeric
#' @return value of type double
#'
as.numeric.nonempty <- function(x) {
  ifelse(length(x)>0, as.numeric(x), NA_real_)
}

#' return object, or NA for length 0 (NULL) objects
#'
#' @param x the object to cast as numeric
#' @return value of type double
#'
nonempty <- function(x) {
  ifelse(length(x)>0, x, NA)
}                                   

#' Check if a variable exists for a PWS. If not set the value to -9999
#' 
#' @param x the value to check
#' @param class a character given the desired class for the variable 
measurement_exists <- function(x, class = "numeric") {
  val <- ifelse(is.null(x),
                -9999,
                x)
  do.call(paste0("as.", class),
          list(val))
}

#' Return POSIXct time from 7 variables.
#'
#'    In locations with a Daylight Saving/Standard
#'    time change that occurs twice annually, the year has one 23 hour day 
#'    and one 25 hour day, if by day we mean "an ordered set of all instants
#'    in time which are assigned the same date".  In the US/Los_Angeles 
#'    timezone, there is one day in the spring where are no valid times 
#'    between the moment before 02:00:00 and 03:00:00.  Similarly, there
#'    is one day in the fall where there are two instants described by all
#'    times between 01:00:00 and 01:59:59, first as a set of PDT times, then
#'    as a set of PST times. \code{as.POSIXct()} doesn't handle this case well.
#'    Times inside this region are assigned to DST until the sequence of
#'    clock times has a time which is the same or earlier than its predecessor,
#'    and all subsequent ambiguous times are assigned to Standard Time.
#' 
#' @param y vector of years
#' @param m vector of months
#' @param d vector of days
#' @param hr vector of hours
#' @param mn vector of minutes
#' @param sec vector of seconds
#' @param tz vector of timezones
#' @return POSIXct time assuming vectors sorted by true chronological order,
#'    at least for the hour that "occurs twice", once with Daylight Time, 
#'    then again with Standard Time.  If there are no nonmonotonicities in 
#'    the times, all times in this hour will be assumed to be Daylight Time.
#'     
dst_POSIXct <- function(y,m,d,hr,mn,sec,tz) {
  
  if (length(unique(y))>1) 
    stop("all obs in call to dst_POSIXct need same year")
  if (length(unique(m))>1) 
    stop("all obs in call to dst_POSIXct need same month")
  if (length(unique(d))>1) 
    stop("all obs in call to dst_POSIXct need same day")
  if (length(unique(tz))>1) 
    stop("all obs in call to dst_POSIXct need same tz")
 
  if (!is_fall_back_day(y[1],m[1],d[1],tz[1])) {
    return(as.POSIXct(
      paste0(y, "-", m, "-", d, " ", hr, ":", mn, ":", sec),
      tz = tz[1]) 
    )
  }  else {
    hhmm.repeat.start <- 
      dst_repeat_starttime(y=y[1],m=m[1],d=d[1],tz=tz[1])["start"]
    hhmm.repeat.stop  <- 
      dst_repeat_starttime(y=y[1],m=m[1],d=d[1],tz=tz[1])["stop"]
    times.lt <- 
      as.POSIXlt(paste0(y,"-",m,"-",d," ",hr,":",mn,":",sec),tz=tz[1])
    times.num <- 100*as.numeric(hr)+as.numeric(mn)
    times.in.window <- (times.num >= hhmm.repeat.start) &
                       (times.num <  hhmm.repeat.stop)
    
    nonmono.out <- 
      sum( (times.num <= c(-Inf,times.num[-length(times.num)])) & 
                          !times.in.window )
    nonmono.out.tiesok <- 
      sum( (times.num < c(-Inf,times.num[-length(times.num)])) &
                          !times.in.window )
    if (nonmono.out.tiesok>0) 
      warning(paste0(nonmono.out.tiesok,
                     " strict time decreases outside interval on ",
                     y[1],"-",m[1],"-",d[1]))
    
    nonmono <- (times.num <= c(-Inf,times.num[-length(times.num)])) &
                      times.in.window
    nonmono.tiesok <- (times.num < c(-Inf,times.num[-length(times.num)])) &
                      times.in.window
    if (sum(nonmono) == 0) 
      warning(paste0("no nonmonotonicities inside interval, all set to DST ",
                     y[1],"-",m[1],"-",d[1]))
    if (sum(nonmono.tiesok) > 1) 
      warning(paste0("multiple nonmonotonicities (excluding ties)",
                     " inside interval on ",y[1],"-",m[1],"-",d[1]))
    if ((sum(nonmono.tiesok) == 1) & (sum(nonmono) > 1) & 
        (nonmono.out.tiesok == 0)) {
      # ignore ties if that gives exactly one nonmono and it is in the interval
      times.lt[(times.in.window)&(cumsum(nonmono.tiesok)==0)]$isdst <- TRUE
      times.lt[(times.in.window)&(cumsum(nonmono.tiesok)>0)]$isdst <- FALSE
    } else {
      times.lt[(times.in.window)&(cumsum(nonmono)==0)]$isdst <- TRUE
      times.lt[(times.in.window)&(cumsum(nonmono)>0)]$isdst <- FALSE
    }
   
    return(as.POSIXct(times.lt))
  }
}
#' Check if a date is a "fall back" transition from DST.
#' 
#' @param y the year
#' @param m the month
#' @param d the day
#' @param tz the timezone
#' @return logical
#' 

is_fall_back_day <- function(y,m,d,tz) {
  if (length(y)>1) stop("error in call to is_fall_back_day - year")
  if (length(m)>1) stop("error in call to is_fall_back_day - month")
  if (length(d)>1) stop("error in call to is_fall_back_day - day")
  if (length(tz)>1) stop("error in call to is_fall_back_day - tz")
  curday <- as.POSIXct(paste0(y,"-",m,"-",d," 00:00"),tz=tz)
  nextday <- as.POSIXct(paste0(y,"-",m,"-",d," 00:00"),tz=tz) +
    lubridate::days(x=1)
  dstday <- lubridate::dst(curday)
  dstnext <- lubridate::dst(nextday)
  return( dstday & !dstnext )  #transition from dst sometime during day
}

#' Find the text to POSIXct ambiguous interval.
#'
#'    Assumes that DST transitions happen on hour boundaries, which is true
#'    almost everywhere, and that the wall clock shifts back and repeats 
#'    exactly 1 hour, again true almost everywhere. This code relies on R 
#'    and the OS to properly manage DST in all timezones.
#' 
#' @param y the year
#' @param m the month
#' @param d the day
#' @param tz the timezone
#' @return list of two integers betweeen 0000 and 2359, hhmm format. 
#'   the first integer is the beginning of the interval of clock times which
#'   correspond to 2 separate instants of time, the second is the end of that
#'   interval.  The left endpoint is ambiguous, the right endpoint is not
#'   since it maps only to Standard Time.
#' 
dst_repeat_starttime <- function(y,m,d,tz){
  if (length(y)>1) stop("error in call to dst_repeat_starttime - year")
  if (length(m)>1) stop("error in call to dst_repeat_starttime - month")
  if (length(d)>1) stop("error in call to dst_repeat_starttime - day")
  if (length(tz)>1) stop("error in call to dst_repeat_starttime - tz")
  if (!is_fall_back_day(y,m,d,tz)) 
        stop(paste0("dst_starttime - no transition on ",y,"-",m,"-",d))  
     hhmm.repeat.start <- 
      100*(sum(lubridate::dst(
        as.POSIXct(paste0(y,"-",m,"-",d," ",seq(0,23),":00:00"),tz=tz))) - 1)
     hhmm.repeat.stop <- hhmm.repeat.start + 100
  
  return(list(start=hhmm.repeat.start,stop=hhmm.repeat.stop))
}
