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


