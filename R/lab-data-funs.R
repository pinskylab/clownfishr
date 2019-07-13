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
  platemap <- table_name %>%
    dplyr::mutate(row = stringr::str_sub(well, 1,1),
                  col = as.numeric(stringr::str_sub(well, 2,3)))

  # select columns for plate
  platemap <- platemap %>%
    dplyr::select(row, col, ...) %>% #keep row & col, identifier
    dplyr::arrange(row, col)

    # make map and return object platemap as well as the table
  platemap <- as.matrix(reshape2::acast(platemap,platemap[,1] ~ platemap[,2]))
  return(platemap)
}
