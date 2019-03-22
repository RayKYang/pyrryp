#' Translate R Functions into Python Functions
#'
#' @param R_code R Code String
#' @examples
#' rpy("getwd")
#' @import utils
#' @import stringr
#' @import dplyr
#' @export
rpy <- function(R_code){
  code <- dplyr::case_when(
  # 1.1 directory #####
   # rpy("getwd")
   R_code == "getwd" ~ " [Check Working Directory - Python Code]
   import os
   os.getcwd()",
   # rpy("setwd")
   R_code == "setwd" ~ " [Set Working Directory - Python Code]
   import os
   os.chdir()",

  # 1.2 paste #####
  # rpy("paste")
  R_code == "paste" ~ " [Concatenate Strings (multiple usage) - Python Code]
   # usage 1: concatenate strings, equivalent to { paste(1, ' ', 'a') }
   str(1) + ' ' + 'a'

   # usage 2: map a leading string to a vector, equivalent to { paste0('a_', 1:9) }
   ['a_' + str(i) for i in range(1, 10)]

   # usage 3: combine a string vector, equivalent to { paste(c('a', 'b', 'c'), collapse = ' ') }
   ' '.join(['a', 'b', 'c'])",

  # 2.1 read csv #####
   # rpy("read.csv")
   R_code == "read.csv" ~ " [read csv file - Python code]
   import pandas as pd
   pd.read_csv()",

    TRUE ~ "not found"
  )
  if(code != "not found"){
    cat(code)}else{
      search_ <- readline("not found... search stackoverflow (s) / google (g), or no (n) more searching? Answer:")
      if(search_ == "s"){
        utils::browseURL(paste0("https://stackoverflow.com/search?q=", "R+", stringr::str_replace_all(R_code, " ", "+"),
                                "+equivalent+in+Python"))}else{
      if(search_ == "g"){
        utils::browseURL(paste0("https://www.google.com/search?source=hp&q=", "R+", stringr::str_replace_all(R_code, " ", "+"),
                                "+equivalent+in+Python"))}
                                }
    }
}
