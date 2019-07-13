#' Pull the clownfish table from the leyte database
#'
#' This function pulls in the entire clownfish table which can be filtered for a more specific query.
#'
#' @return A tibble/data frame
#' @export
#'
#' @examples
#' fish <- get_fish() %>%
#' filter(sample_id == "APCL18_201")
get_fish <- function(){
  if(!exists("leyte"))
    stop("Error: db connection called 'leyte' does not exist, see Michelle for help")
  fish <- leyte %>%
    dplyr::tbl("clownfish") %>%
    dplyr::collect()

  return(fish)
}

#' Pull the anemone table from the leyte database
#'
#' This function pulls in the entire anemone table which can be filtered for a more specific query.
#'
#' @return A tibble/data frame
#' @export
#'
#' @examples
#' anem <- get_anem() %>%
#' filter(anem_obs == 12)
get_anem <- function(){
  if(!exists("leyte"))
    stop("Error: db connection called 'leyte' does not exist, see Michelle for help")
  anem <- leyte %>%
    dplyr::tbl("anemones") %>%
    dplyr::collect()

  return(anem)
}

#' Pull the diveinfo table from the leyte database
#' This function pulls the entire diveinfo table which can be filtered for a more specific query
#'
#' @return A tibble/data frame
#' @export
#'
#' @examples
#' dive <- get_dive()
#'
get_dive <- function(){
  if(!exists("leyte"))
    stop("Error: db connection called 'leyte' does not exist, see Michelle for help")
  dive <- leyte %>%
    dplyr::tbl("diveinfo") %>%
    dplyr::collect()

  return(dive)
}


#' Pull a large joined table that includes fish, anemone, and diveinfo
#' This function pulls in clownfish data and joins all related anemone and diveinfo into a table in R.
#'
#' @return A tibble/dataframe
#' @export
#'
#' @examples
#' fish_of_interest <- c("APCL18_201", "APCL17_353", "APCL15_013")
#' fish_info <- fish_anem_dive() %>%
#' filter(sample_id %in% fish_of_interest)
fish_anem_dive <- function(){
  fish <- get_fish()
  anem <- get_anem() %>%
    dplyr::filter(anem_table_id %in% fish$anem_table_id)
  dive <- get_dive() %>%
    dplyr::filter(dive_table_id %in% fish$dive_table_id)

  fish <- dplyr::left_join(fish, anem, by = "anem_table_id") %>%
    dplyr::left_join(dive, by = "dive_table_id")

  return(fish)

}

#' Pull all of the dives from a specific date range
#'
#' @param begin_date A character date
#' @param end_date A character date
#'
#' @return A table of dives
#' @export
#'
#' @examples
#' dives <- date_range_dive("2017-01-01", "2017-12-30")
date_range_dive <- function(begin_date, end_date){
  dive <- get_dive() %>%
    dplyr::filter(date > begin_date & date < end_date)

  return(dive)
}

#' Return the lat lon for a set of sample_ids
#'
#' @param sample_ids
#'
#' @return A tibble/data frame that contains the columns sample_id, lat, and lon
#' @export
#'
#' @examples
#'location <- sample_latlon(c("APCL15_201", "APCL17_201"))
sample_latlon <- function(sample_ids){
  # find the anem_table_id for the sample
  fish <- fish_anem_dive() %>%
    dplyr::mutate(fish_obs_time = ifelse(is.na(fish_obs_time), anem_obs_time, fish_obs_time)) %>%
    dplyr::select(sample_id, fish_obs_time, date, gps) %>%
    dplyr::filter(sample_id %in% sample_ids) %>%
    # identify time zone as Asia
    dplyr::mutate(fish_obs_time = lubridate::force_tz(lubridate::ymd_hms(str_c(date, fish_obs_time, sep = " ")), tzone = "Asia/Manila"),
           # convert to UTC
           fish_obs_time = lubridate::with_tz(fish_obs_time, tzone = "UTC"),
           gpx_date = lubridate::date(fish_obs_time),
           gpx_hour = lubridate::hour(fish_obs_time),
           minute = lubridate::minute(fish_obs_time))

  if(!exists("leyte"))
    stop("Error: db connection called 'leyte' does not exist, see Michelle for help")

  gpx <- leyte %>%
    dplyr::tbl("GPX") %>%
    dplyr::select(lat, lon, time, unit) %>%
    dplyr::collect() %>%
    dplyr::separate(time, into = c("gpx_date", "gps_time"), sep = " ") %>%
    dplyr::mutate(gpx_date = lubridate::date(gpx_date)) %>%
    dplyr::filter(gpx_date %in% fish$gpx_date) %>%
    dplyr::separate(gps_time, into = c("gpx_hour", "minute", "second"), sep = ":") %>%
    dplyr::filter(as.numeric(gpx_hour) %in% fish$gpx_hour & as.numeric(minute) %in% fish$minute) %>%
    dplyr::mutate(gpx_hour = as.numeric(gpx_hour),
           minute = as.numeric(minute))

  # find matches for times to assign lat long - there are more than one set of seconds (sec.y) that match
  fish <- dplyr::left_join(fish, gpx, by = c("gps" = "unit",  "gpx_date","gpx_hour", "minute")) %>%
    dplyr::mutate(lat = as.numeric(lat),
           lon = as.numeric(lon)) # need to make decimal 5 digits - why? because that is all the gps can hold

  # calculate a mean lat lon for each anem observation
  coord <- fish %>%
    dplyr::group_by(sample_id) %>% # id should be referring to one row of the data
    dplyr::summarise(lat = mean(lat, na.rm = TRUE),
              lon = mean(lon, na.rm = T))

  return(coord)

}


#' Pull in the location of anemones
#'
#' @param anem_ids
#'
#' @return A tibble/data frame of locations for anem_ids
#' @export
#'
#' @examples
#' location <- anem_latlon(c(2554, 0815))
anem_latlon <- function(anem_ids){
  # find the anem_table_id for the sample
  anem <- anem_dive() %>%
    dplyr::select(anem_id, anem_obs, anem_obs_time, date, gps) %>%
    dplyr::filter(anem_id %in% anem_ids) %>%
    # identify time zone as Asia
    dplyr::mutate(anem_obs_time = lubridate::force_tz(lubridate::ymd_hms(str_c(date, anem_obs_time, sep = " ")), tzone = "Asia/Manila"),
           # convert to UTC
           anem_obs_time = lubridate::with_tz(anem_obs_time, tzone = "UTC"),
           gpx_date = lubridate::date(anem_obs_time),
           gpx_hour = lubridate::hour(anem_obs_time),
           minute = lubridate::minute(anem_obs_time))


  if(!exists("leyte"))
    stop("Error: db connection called 'leyte' does not exist, see Michelle for help")
  gpx <- leyte %>%
    dplyr::tbl("GPX") %>%
    dplyr::select(lat, lon, time, unit) %>%
    dplyr::collect() %>%
    dplyr::separate(time, into = c("gpx_date", "gps_time"), sep = " ") %>%
    dplyr::mutate(gpx_date = lubridate::date(gpx_date)) %>%
    dplyr::filter(gpx_date %in% anem$gpx_date) %>%
    dplyr::separate(gps_time, into = c("gpx_hour", "minute", "second"), sep = ":") %>%
    dplyr::filter(as.numeric(gpx_hour) %in% anem$gpx_hour & as.numeric(minute) %in% anem$minute) %>%
    dplyr::mutate(gpx_hour = as.numeric(gpx_hour),
           minute = as.numeric(minute))

  # find matches for times to assign lat long - there are more than one set of seconds (sec.y) that match
  anem <- dplyr::left_join(anem, gpx, by = c("gps" = "unit",  "gpx_date","gpx_hour", "minute")) %>%
    dplyr::mutate(lat = as.numeric(lat),
           lon = as.numeric(lon)) # need to make decimal 5 digits - why? because that is all the gps can hold

  # calculate a mean lat lon for each anem observation
  coord <- anem %>%
    dplyr::group_by(anem_id, anem_obs) %>%
    dplyr::summarise(lat = mean(lat, na.rm = TRUE),
              lon = mean(lon, na.rm = T))

  return(coord)

}
