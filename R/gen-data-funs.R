#' Read Genepop
#'read_genepop takes a genepop file (.gen or .genepop) and returns a data frame
#'
#' @param filename the name and location of the genepop
#'
#' @return A dataframe of the input genepop
#' @export
#'
#' @examples
#' url <- "https://github.com/pinskylab/genomics/raw/master/data/seq33-03_norecap.gen"
#' genedf <-read_genepop(url)
read_genepop <-  function(filename){
  # get all of the data
  if(readr::read_lines(filename, n_max = 3)[[3]] != "pop"){
    dat <-readr::read_delim(filename, skip = 2, col_types = readr::cols(.default = readr::col_character()), col_names = FALSE, delim = " ")
  }else{
    dat <-readr::read_delim(filename, skip = 3, col_types = readr::cols(.default = readr::col_character()), col_names = FALSE, delim = " ")
  }


  # get the header info
  info <-readr::read_lines(filename, n_max = 2)

  # define the loci names
  loci <- purrr::flatten(stringr::str_split(info[2], ","))

  new_names <- c("sample", loci)
  # rename the dat column
  names(dat) <- new_names

  # if there is a comma in the sample column, remove it
  dat$sample <- stringr::str_replace(dat$sample, ",", "")

  return(dat)

}

