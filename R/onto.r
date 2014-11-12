#' Ontobrowse helps you browse and maintain your ontology
#'
#' This function allows you browse.
#' @param onto Does your ontology exists? Defaults to TRUE.
#' @keywords ontology
#' @export
#' @examples
#' ontobrowse()
#' 
ontobrowse <- function(onto=TRUE){
  if(onto==TRUE){
    print("Write a package!")
  }
  else {
    print("Lore, write a package!")
  }
}