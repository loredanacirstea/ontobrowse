#' Ontobrowse helps you browse and maintain your ontology
#'
#' This function allows you browse.
#' @param onto Does your ontology exists? Defaults to TRUE.
#' @keywords ontology
#' @export
#' @examples
#' ontobrowse()
#' 
ontobrowse <- function(onto=""){
  if(onto==""){
    list( message = "Write a package!" )
  }
  else {
    list( message = paste(onto, ", write a package!") )
  }
}