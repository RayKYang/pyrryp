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
   stringr::str_detect(goal, "directory")  ~ " [check and set working directory]
   import os
   os.getcwd()  # get working directory
   os.chdir()  # set working directory",

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
