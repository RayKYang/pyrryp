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
    # 1.1 directory
    # rpy("getwd")
    R_code == "getwd" ~ " [check working directory - Python Code]
    import os
    os.getcwd()",
    # rpy("setwd")
    R_code == "setwd" ~ " [set working directory - Python Code]
    import os
    os.chdir()",

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
