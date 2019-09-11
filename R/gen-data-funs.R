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


#' lig_from_samp
#' Get any ligation_ids that exist for this sample_id, this will also include the extraction and digest_ids.
#'
#' @param sample_ids A set of sample_ids
#'
#' @return A table of ligation_ids for sample_ids
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr collect
#' @importFrom dplyr tbl
#' @importFrom dplyr left_join
#' @export
#'
#' @examples
#' samples <- c("APCL17_267", "APCL18_118", "APCL16_543")
#' lig_ids <- lig_from_samp(samples)
#'
lig_from_samp <- function(sample_ids){

  lab <- read_db("Laboratory")

  extr <- lab %>%
   tbl("extraction") %>%
   filter(sample_id %in% !!samples) %>%
   select(sample_id, extraction_id) %>%
   collect()

  dig <- lab %>%
  tbl("digest") %>%
  filter(extraction_id %in% !!extr$extraction_id) %>%
  select(extraction_id, digest_id) %>%
  collect()

  lig <- lab %>%
  tbl("ligation") %>%
  filter(digest_id %in% !!dig$digest_id) %>%
  select(ligation_id, digest_id) %>%
  collect()

  lig <-left_join(extr, dig, by = "extraction_id") %>%
   left_join(lig, by = "digest_id")

  return(lig)
}








