#' Search for R Functions by Descriptions
#'
#' @param goal Goal Descriptions
#' @examples
#' find_R_code("set directory")
#' @import utils
#' @import stringr
#' @import dplyr
#' @export
find_R_code <- function(goal){
  code <- dplyr::case_when(
  # 1.1 directory #####
   # find_R_code("directory")
   stringr::str_detect(goal, "directory")  ~ " [check and set working directory in R]
   getwd()  # get working directory
   setwd()  # set working directory",

  # 1.2 concatenate #####
   # find_R_code("concatenate")
   stringr::str_detect(goal, "concatenate")  ~ " [Concatenate Strings (multiple usage) in R]
   # usage 1: concatenate strings
   paste(1, ' ', 'a')

   # usage 2: map a leading string to a vector
   paste0('a_', 1:9)

   # usage 3: combine a string vector
   paste(c('a', 'b', 'c'), collapse = ' ')",

  # 2.1 read csv #####
   # find_R_code("read csv")
   stringr::str_detect(goal, "csv")  ~ " [Read csv in R]
   read.csv()",

    TRUE ~ "not found"
  )
  if(code != "not found"){
    cat(code)}else{
      search_ <- readline("not found... search stackoverflow (s) / google (g), or no (n) more searching? Answer:")
      if(search_ == "s"){
        utils::browseURL(paste0("https://stackoverflow.com/search?q=", stringr::str_replace_all(goal, " ", "+"), "+in+R"))}
      if(search_ == "g"){
        utils::browseURL(paste0("https://www.google.com/search?source=hp&q=", stringr::str_replace_all(goal, " ", "+"), "+in+R"))}
    }
}
