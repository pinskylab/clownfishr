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
    filter(anem_table_id %in% fish$anem_table_id)
  dive <- get_dive() %>%
    filter(dive_table_id %in% fish$dive_table_id)

  fish <- left_join(fish, anem, by = "anem_table_id") %>%
    left_join(dive, by = "dive_table_id")

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
    filter(date > begin_date & date < end_date)

  return(dive)
}

