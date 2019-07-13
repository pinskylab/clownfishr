#' Maps the well locations of samples from the database
#'
#' @param table_name
#' @param ... A column identifier
#'
#' @return A table and a platemap
#' @export
#'
#' @examples
#'
#' extractions <- plate_from_db(extractions, extraction_id)
plate_from_db <- function(table_name, ...){

  # split the well out into row and column
  table_name <- table_name %>%
    dplyr::mutate(row = stringr::str_sub(well, 1,1),
                  col = as.numeric(stringr::str_sub(well, 2,3)))

  # select columns for plate
  table_name <- table_name %>%
    select(row, col, ...) %>% #keep row & col, identifier
    arrange(row, col)

    # make map
  platemap <<- as.matrix(reshape2::acast(table_name,table_name[,1] ~ table_name[,2]))
  return(table_name)
}
