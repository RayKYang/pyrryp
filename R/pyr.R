#' Translate Python Functions into R Functions
#'
#' @param Py_code Python Code String
#' @examples
#' pyr("os.chdir")
#' @import utils
#' @import stringr
#' @import dplyr
#' @export
pyr <- function(Py_code){
  code <- dplyr::case_when(
  # 1.1 directory #####
   # pyr("os.chdir")
   Py_code == "os.getcwd" ~ " [Check Working Directory - R Code]
   getwd()",
   # pyr("os.chdir")
   Py_code == "os.chdir" ~ " [Set Working Directory - R Code]
   setwd()",

  # 1.2 .join() method #####
  # pyr(".join")
  Py_code == ".join" ~ " [concatenates each element of an iterable - R code]
   paste(c('a', 'b', 'c'), collapse = ' ') # equivalent to { ' '.join(['a', 'b', 'c']) }",

  # 2.1 read csv #####
   # pyr("pandas.read_csv")
   Py_code == "pandas.read_csv" ~ " [read csv file - R code]
   read.csv()",

    TRUE ~ "not found"
  )
  if(code != "not found"){
    cat(code)}else{
      search_ <- readline("not found... search stackoverflow (s) / google (g), or no (n) more searching? Answer:")
      if(search_ == "s"){
        utils::browseURL(paste0("https://stackoverflow.com/search?q=", "Python+", stringr::str_replace_all(Py_code, " ", "+"),
                                "+equivalent+in+R"))}else{
      if(search_ == "g"){
        utils::browseURL(paste0("https://www.google.com/search?source=hp&q=", "Python+", stringr::str_replace_all(Py_code, " ", "+"),
                                "+equivalent+in+R"))}
                                }
    }
}
