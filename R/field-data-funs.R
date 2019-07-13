get_fish <- function(){
  if(!exists("leyte"))
    stop("Error: db connection called 'leyte' does not exist, see Michelle for help")
  fish <- leyte %>%
    dplyr::tbl("clownfish") %>%
    dplyr::collect()

  return(fish)
}

