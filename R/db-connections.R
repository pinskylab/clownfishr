#' Access the Pinsky Lab database
#'
#' @param db_name name of a database
#'
#' @return database connection
#' @export
#'
#' @examples
read_db <- function(db_name){
  con <- DBI::dbConnect(RMySQL::MySQL(),
                        dbname = db_name,
                        host = "amphiprion.deenr.rutgers.edu",
                        username = rstudioapi::askForPassword("Username"),
                        password = rstudioapi::askForPassword("Password"))
  return(con)
}
