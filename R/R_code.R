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
    goal == "directory"  ~ " [check and set working directory]
   getwd()  # get working directory
   setwd()  # set working directory",

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
