library(RCurl)
x <- getURL("https://raw.githubusercontent.com/ctzurcanu/smp/master/data/term.csv")
terms <- read.csv(text = x)
y <- getURL("https://raw.githubusercontent.com/ctzurcanu/smp/master/data/term_relation.csv")
rels <- read.csv(text = y)
ancestry <- function(terms, rels, term, lang, origin, returnIds = TRUE) {
  if(class(term) == "numeric" || class(term) == "integer") {
    termId = term;
  }
  else {
    termId = terms[terms$term == term, "term_id"];
    termId <- termId[!is.na(termId)][1];
  }
  path = termId;
  if(termId == origin) {
    return();
  }
  id = termId;
  while((id != origin) == TRUE) {
    parent = rels[rels$term1 == id & rels$relation == 1,"term2"];
    path = c(path, parent);
    id = parent;
  }
  if(returnIds == TRUE) {
    return(path);
  }
  else {
    termsNames = c();
    for(id in path) {
      termsNames = c(termsNames, terms[terms$term_id == id & terms$lang == lang, "term"]);
    }
    return(termsNames);
  }
}
children <- function(terms, rels, term, lang, returnIds = TRUE) {
  if(class(term) == "numeric" || class(term) == "integer") {
    termId = term;
  }
  else {
    termId = terms[terms$term == term, "term_id"];
    termId <- termId[!is.na(termId)][1];
  }
  kidsIds = rels[rels$term2 == termId & rels$relation == 1,"term1"];
  if(returnIds == TRUE) {
    result <- kidsIds;
  }
  else {
    kids = c();
    for(id in kidsIds) {
      termN = terms[terms$term_id == id & terms$lang == lang, "term"];
      termN <- termN[!is.na(termN)][1];
      kids = c(kids, termN);
    }
    result <- kidsdebug()
  }
  return(result)
}
siblings <- function(terms, rels, term, lang, returnIds = TRUE) {
  if(returnIds == TRUE) { sibs <- children(terms, rels, rels[rels$term1 == term, "term2"]);}
  else { sibs <- children(terms, rels, rels[rels$term1 == term, "term2"], lang, returnIds = FALSE);}
  return(sibs);
}


#' Ontobrowse helps you browse your ontology
#'
#' This function allows you browse an ontology by id.
#' @param term Give a term_id. Defaults to 9000 (Terminologia Morphologica).
#' @param lang Give a display language (la=Latin, en=English, ro=Romanian). Defaults to "la" (Latin).
#' @param origin Give a term_id for the origin. Defaults to 9000 (Terminologia Morphologica).
#' @keywords ontology
#' @export
#' @examples
#' ontobrowse()
#' 
ontobrowse <- function(term=9000, lang="la", origin = 9000){
  term = as.integer(term)
  origin = as.integer(origin)
  text = paste("Ancestry: ", "\n")
  list <- list()
  list[["id"]] <- term
  list[["ancestry"]] <- list()
  list[["children"]] <- list()
  list[["siblings"]] <- list()
  parents <- ancestry(terms, rels, term, lang, origin)
  if(length(parents) == 0){
    text = paste(text, "No hierarchical path available in your loaded data frame.", "\n")
  }
  else {
    for(i in length(parents):2) {
      name <- as.character(terms[terms$term_id == parents[i] & terms$lang == lang, "term"])
      text = paste(text, name, "(id: ", parents[i], ")", " -> ")
      temp <- c()
      temp["id"] <- parents[i]
      temp["name"] <- name
      list[["ancestry"]][[i-1]] <- temp
    }
    text = paste(text, as.character(terms[terms$term_id == parents[1] & terms$lang == lang, "term"]), "(id: ", parents[1], ")", "\n")
    list[["name"]] <- as.character(terms[terms$term_id == parents[1] & terms$lang == lang, "term"])
  }
  text = paste(text, "Children:", "\n");
  kids <- children(terms, rels, term, lang)
  if(length(kids) == 0) { text = paste(text, "No children.", "\n") }
  else if(length(kids) > 1) {
    for(i in 1:(length(kids)-1)){
      name <- as.character(terms[terms$term_id == kids[i] & terms$lang == lang, "term"])
      text = paste(text, name, "(id:", kids[i], "); ")
      temp <- c()
      temp["id"] <- kids[i]
      temp["name"] <- name
      list[["children"]][[i]] <- temp
    }
  }
  text = paste(text, as.character(terms[terms$term_id == kids[length(kids)] & terms$lang == lang, "term"]), "(id:", kids[length(kids)], ")", "\n")
  
  text = paste(text, "Siblings: ", "\n")
  sibs <- siblings(terms, rels, term, lang)
  if(length(sibs) == 1 || length(sibs) == 0) { text = paste(text, "No siblings.", "\n") }
  else {
    for(i in 1:(length(sibs)-1)){
      name <- as.character(terms[terms$term_id == sibs[i] & terms$lang == lang, "term"])
      text = paste(text, name, "(id:", sibs[i], "); ")
      temp <- c()
      temp["id"] <- sibs[i]
      temp["name"] <- name
      list[["siblings"]][[i]] <- temp
    }
    name <- as.character(terms[terms$term_id == sibs[length(sibs)] & terms$lang == lang, "term"])
    text = paste(text, name, "(id:", sibs[length(sibs)], ")", "\n")
    temp <- c()
    temp["id"] <- sibs[length(sibs)]
    temp["name"] <- name
    list[["siblings"]][[length(sibs)]] <- temp
  }
  #print(text)
  #list( message = text )
  list(message = list)
}