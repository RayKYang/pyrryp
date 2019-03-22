#' Search for Python Functions by Descriptions
#'
#' @param goal Goal Descriptions
#' @examples
#' find_Py_code("set directory")
#' @import utils
#' @import stringr
#' @import dplyr
#' @export
find_Py_code <- function(goal){
  code <- dplyr::case_when(
  # 1.1 directory #####
   # 1.1 directory
   # find_Py_code("directory")
   stringr::str_detect(goal, "directory")  ~ " [Check and Set Working Directory in Python]
   import os
   os.getcwd()  # get working directory
   os.chdir()  # set working directory",

  # 1.2 concatenate #####
   # find_Py_code("concatenate")
   stringr::str_detect(goal, "concatenate")  ~ " [Concatenates Each Element of An Iterable in Python]
   ' '.join(['a', 'b', 'c'])",

  # 2.1 read csv #####
   # find_Py_code("read csv")
   stringr::str_detect(goal, "csv")  ~ " [Read csv in Python]
   import pandas as pd
   pd.read_csv()",

    TRUE ~ "not found"
  )
  if(code != "not found"){
    cat(code)}else{
      search_ <- readline("not found... search stackoverflow (s) / google (g), or no (n) more searching? Answer:")
      if(search_ == "s"){
        utils::browseURL(paste0("https://stackoverflow.com/search?q=", stringr::str_replace_all(goal, " ", "+"), "+in+Python"))}
      if(search_ == "g"){
        utils::browseURL(paste0("https://www.google.com/search?source=hp&q=", stringr::str_replace_all(goal, " ", "+"), "+in+Python"))}
    }
}
