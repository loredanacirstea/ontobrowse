library(RCurl)
x <- getURL("https://raw.githubusercontent.com/ctzurcanu/smp/master/data/term.csv")
terms <- read.csv(text = x)
y <- getURL("https://raw.githubusercontent.com/ctzurcanu/smp/master/data/term_relation.csv")
rels <- read.csv(text = y)
z <- getURL("https://raw.githubusercontent.com/ctzurcanu/smp/master/data/ontologies.csv")
ontoList <- read.csv(text = z)
s <- getURL("https://raw.githubusercontent.com/ctzurcanu/smp/master/data/jos_sliced_api.csv")
smp <- read.csv(text = s)

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
    parent = rels[rels$term1 == id & rels$relation == 1,"term2"][1];
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

#' Ontologies helps you browse your ontologies
#'
#' This function allows you to see what ontologies are available.
#' no param
#' ontologies()
#' 
ontologies <- function(){
  langs <- levels(ontoList$lang)
  data <- list()
  for(lang in langs){
    data[[lang]] <- list()
    ids <- as.character(ontoList[ontoList$lang == lang, "subject_id"])
    for(id in ids){
      data[[lang]][id] <- as.character(ontoList[ontoList$lang == lang & ontoList$subject_id == id, "description"])
    }
  }
  data
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
  list <- list()
  list[["id"]] <- term
  list[["name"]] <- as.character(terms[terms$term_id == term & terms$lang == lang, "term"])[1]
  list[["smp"]] <- list()
  api<- list()
  api[["x_med"]] <- as.character(unique(smp[smp$term_id == term & smp$x_med != 0, "x_med"]))
  api[["x_min"]] <- as.character(unique(smp[smp$term_id == term & smp$x_min != 0, "x_min"]))
  api[["x_max"]] <- as.character(unique(smp[smp$term_id == term & smp$x_max != 0, "x_max"]))
  api[["color"]] <- as.character(unique(smp[smp$term_id == term, "color"]))
  list[["smp"]] <- api
  #list[["ancestry"]] <- list()
  list[["children"]] <- list()
  #list[["siblings"]] <- list()
#   parents <- ancestry(terms, rels, term, lang, origin)
#   if(length(parents) > 0){
#     for(i in length(parents):2) {
#       name <- as.character(terms[terms$term_id == parents[i] & terms$lang == lang, "term"])
#       temp <- c()
#       temp["id"] <- parents[i]
#       temp["name"] <- name
#       list[["ancestry"]][[i-1]] <- temp
#     }
#   }
  kids <- children(terms, rels, term, lang)
  if(length(kids) > 0) {
    for(i in 1:(length(kids))){
      name <- as.character(terms[terms$term_id == kids[i] & terms$lang == lang, "term"])
      temp <- c()
      temp["id"] <- kids[i]
      temp["name"] <- name
      temp["haschildren"] <- (length(children(terms, rels, kids[i], lang))>0)
      list[["children"]][[i]] <- temp
    }
  }
#   sibs <- siblings(terms, rels, term, lang)
#   if(length(sibs) > 1) {
#     for(i in 1:(length(sibs))){
#       name <- as.character(terms[terms$term_id == sibs[i] & terms$lang == lang, "term"])
#       temp <- c()
#       temp["id"] <- sibs[i]
#       temp["name"] <- name
#       list[["siblings"]][[i]] <- temp
#     }
#   }
  list(message = list)
}