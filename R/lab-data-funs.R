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

make_plate_with_neg_ctrl <- function(list_of_ids, id_type){
  # make a dataframe of the list_of_ids
  ids <- tibble::tibble(list_of_ids)

  # how many rows are in the table (how many samples)?
  y <- nrow(ids)

  # how many plates would these make, 94 samples plus 2 blanks per plate
  (nplates <- floor(y/94)) # extra parenthesis are to print

  # define wells
  well <- 1:(96*nplates)

  # insert the negative controls and set up the plate
  plate <- tibble() # blank data frame to build upon
  for (i in 1:nplates){
    c <- 96*i-95 # well 1 on a plate
    d <- 96*i-85 # well 11
    e <- 96*i-84 # well 12 = negative control well
    f <- 96*i-83 # well 13
    g <- 96*i-36 # well60
    h <- 96*i-35 # well 61 negative control well
    j <- 96*i-34 #  well 62
    k <- 96*i-2  #  well 94
    l <- 96*i - 37 #  well 59
    m <- 96*i # well 96
    str1 <- as.data.frame(cbind(well[c:d], ids[c:d,])) # 1:11
    names(str1) <- c("well", "id_type")
    str2 <- as.data.frame(cbind(well[e], "XXXX")) # because the first blank is in the 12th position
    names(str2) <- c("well", "id_type")
    str3 <- as.data.frame(cbind(well[f:g], ids[e:l,])) #13:60 in plate, 12:59 in list
    names(str3) <- c("well", "id_type")
    str4 <- as.data.frame(cbind(well[h], "XXXX")) # because the 2nd blank is in the 61st position
    names(str4) <- c("well", "id_type")
    str5 <- as.data.frame(cbind(well[j:k], ids[g:k,]))# 62:96 in plate, 60:94 in list
    names(str5) <- c("well", "id_type")

    # and stick all of the rows together
    temp <- data.frame(rbind(str1, str2, str3, str4, str5))
    temp$row <- rep(LETTERS[1:8], 12)
    temp$col <- unlist(lapply(1:12, rep, 8))
    temp$plate <- paste("plate", i, sep = "")
    plate <- rbind(plate, temp)

  }

  # put the samples in order of id (with negative controls inserted)
  plate <- arrange(plate, plate, col, row)

  return(plate)
}

