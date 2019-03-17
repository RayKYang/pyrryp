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
    # 1.1 directory
    # pyr("os.chdir")
    Py_code == "os.getcwd" ~ " [check working directory - R code]
   getwd()",
    # pyr("os.chdir")
    Py_code == "os.chdir" ~ " [set working directory - R code]
   setwd()",

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
