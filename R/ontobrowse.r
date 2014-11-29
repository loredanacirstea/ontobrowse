#terms
#"https://raw.githubusercontent.com/ctzurcanu/smp/master/data/term.csv"
#rels
#"https://raw.githubusercontent.com/ctzurcanu/smp/master/data/term_relation.csv"
#ontoList
#"https://raw.githubusercontent.com/ctzurcanu/smp/master/data/ontologies.csv"
#smp
#"https://raw.githubusercontent.com/ctzurcanu/smp/master/data/jos_sliced_api.csv"
#library(uuid)

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
  path = term
  if(term == origin) {
    return();
  }
  id = term;
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
      termsNames = c(termsNames, as.character(terms[terms$term_id == id & terms$lang == lang, "term"]));
    }
    return(termsNames);
  }
}
children <- function(terms, rels, term, lang, returnIds = TRUE) {
  termId = term
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

#' Init returns the .csv file from the provided url as an R data frame.
#'
#' This function returns the .csv file from the provided url as an R data frame.
#' @param url Provide an url (string) for a .csv file. 
#' init()
#' 
init <- function(url){
  library(RCurl)
  x <- getURL(url)
  table <- read.csv(text = x)
  table
}


#' Path shows the ancestry path from the root of the ontology to the given term ID. Does not support multiple inheritance now.
#'
#' This function outputs a list: key = ID, value = terms in the language provided. Order: term ->...-> ontology root(origin)
#' @param terms Data frame of terms.
#' @param rels Data frame of relations between terms.
#' @param term Give a term ID. 
#' @param lang Give a language. Default: "la"
#' @param origin Give an origin. Default: 9000
#' @param returnIds Return IDs or terms in the provided language. Default: FALSE
#' path()
#' 
path <- function(term, lang="la", origin=9000){
  ids <- ancestry(terms, rels, term, lang, origin)
  result <- list()
  for(id in ids){
    result[[as.character(id)]] <- as.character(terms[terms$term_id == id & terms$lang == lang, "term"])
  }
  result
}

#' Translations gives all the translations found in the ontologies, for a term ID
#'
#' This function outputs a list with key = language (ex."la","en" etc.) and value = translation
#' @param terms Data frame of terms.
#' @param term Give a term ID
#' translations()
#' 
translations <- function(term){
    transl <- terms[terms$term_id == term,]
    translations <- list()
    for(row in row.names(transl)){
      translations[[as.character(transl[row,"lang"])]] <- as.character(transl[row,"term"])
    }
    translations
}

#' Sapiens Mapping API allows accessing anatomy elements from the 2D atlas by id
#'
#' This function outputs the SMP color used for the element and the minimum, maximum and intermediary values for the slices in which the element is present.
#' @param smp Sapiens Mapping API data frame from https://raw.githubusercontent.com/ctzurcanu/smp/master/data/jos_sliced_api.csv
#' @param term Give a term ID
#' smp_api()
#' 
smp_api <- function(term){
  api<- list()
  api[["x_med"]] <- as.character(unique(smp[smp$term_id == term & smp$x_med != 0, "x_med"]))
  api[["x_min"]] <- as.character(unique(smp[smp$term_id == term & smp$x_min != 0, "x_min"]))
  api[["x_max"]] <- as.character(unique(smp[smp$term_id == term & smp$x_max != 0, "x_max"]))
  api[["color"]] <- as.character(unique(smp[smp$term_id == term, "color"]))
  api
}

#' Ontologies helps you browse your ontologies
#'
#' This function allows you to see what ontologies are available.
#' @param ontoList Data frame of ontologies details. See https://raw.githubusercontent.com/ctzurcanu/smp/master/data/ontologies.csv
#' ontologies()
#' 
ontologies <- function(){
  langs <- levels(ontoList$lang)
  data <- list()
  for(lang in langs){
    data[[lang]] <- list()
    ids <- as.character(ontoList[ontoList$lang == lang, "subject_id"])
    for(id in ids){
      data[[lang]][[id]] <- list()
      temp <- list()
      attr <- names(ontoList)
      for(att in attr){
        temp[[att]] <- as.character(ontoList[ontoList$lang == lang & ontoList$subject_id == id, att])
      }
      data[[lang]][[id]] <- temp
    }
  }
  data
}

#' Ontobrowse helps you browse your ontology by term.
#'
#' This function allows you browse an ontology by id. Returns a list with keys: id, name, children (for that term).
#' @param terms Data frame of terms.
#' @param rels Data frame of relations between terms.
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
  #list[["ancestry"]] <- list()
  list[["haschildren"]] <- 0
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
  list[["haschildren"]] <- length(kids)
  if(length(kids) > 0) {
    for(i in 1:(length(kids))){
      name <- as.character(terms[terms$term_id == kids[i] & terms$lang == lang, "term"])
      temp <- list()
      temp[["id"]] <- kids[i]
      temp[["name"]] <- name
      temp[["haschildren"]] <- length(children(terms, rels, kids[i], lang))
      temp[["children"]] <- (length(children(terms, rels, kids[i], lang))>0)
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
  list
}

#' Tree helps you browse your ontology from the id provided
#'
#' This function allows you browse an ontology from the origin id provided. Returns a multilevel list with keys: id, name, children (recursive, with id, name, children for the subchildren).
#' @param terms Data frame of terms.
#' @param rels Data frame of relations between terms.
#' @param term Give a term_id. Defaults to 9000 (Terminologia Morphologica).
#' @param lang Give a display language (la=Latin, en=English, ro=Romanian). Defaults to "la" (Latin).
#' @param origin Give a term_id for the origin. Defaults to 9000 (Terminologia Morphologica).
#' @keywords ontology
#' @export
#' @examples
#' tree()
#' 
tree <- function(term=9000, lang="la", origin = 9000, unlist=TRUE){
  list <- ontobrowse(term, lang, origin)
  if(list[["haschildren"]] > 0){
    list <- tree_recursive(term, lang, origin, list)
  }
  if(unlist == TRUE){
    result = unlist(list, use.names= FALSE)
  }
  else{
    result = list
  }
  result
}

tree_recursive <- function(term, lang, origin, list){
  for(kid in 1: length(list[["children"]])){
    if(list[["children"]][[kid]][["children"]] == TRUE){
        list[["children"]][[kid]] <- ontobrowse(list[["children"]][[kid]][["id"]], lang, origin)
        list[["children"]][[kid]] <- tree_recursive(list[["children"]][[kid]][["id"]], lang, origin, list[["children"]][[kid]])      
    }
  }
  list
}

uuid_df <- function(terms){
  arr = c()
  #rows <- seq_len(length(row.names(terms)))
  #temp <- cbind(terms, rows)
  dup <- duplicated(terms[,"term_id"])
  indexes <- sort.int(dup, index.return=TRUE)$ix
  unique <- indexes[which(dup[indexes] == FALSE)]
  for(un in unique){
    arr[un] <- UUIDgenerate()
  }
  if(any(indexes[which(dup[indexes] == TRUE)]) == TRUE){
    rest <- indexes[which(dup[indexes] == TRUE)]
    arr[rest] <- "0"
  }
  terms <- cbind(terms,uuid=arr)
  rows <- row.names(terms[terms$uuid == "0",])
  for(row in rows){
    terms[row,"uuid"]<- as.character(terms[terms$term_id == as.character(terms[row,"term_id"]) & terms$uuid != 0, "uuid"][1])
  }
      
#    rest <- sort.int(dup[dup==TRUE], index.return=TRUE)$ix 
#     for(row in rest){
#       id = as.character(temp[temp$rows == row,"term_id"])
#       row_first = min(as.integer(temp[temp$term_id == id,"rows"]))
#       arr[row] = arr[row_first]
#     }
  terms
}