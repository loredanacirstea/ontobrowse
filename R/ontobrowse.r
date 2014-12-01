#terms
#"https://raw.githubusercontent.com/ctzurcanu/smp/master/data/term.csv"
#rels
#"https://raw.githubusercontent.com/ctzurcanu/smp/master/data/term_relation.csv"
#onto_list
#"https://raw.githubusercontent.com/ctzurcanu/smp/master/data/ontologies.csv"
#smp
#"https://raw.githubusercontent.com/ctzurcanu/smp/master/data/jos_sliced_api.csv"
#library(uuid)

library(RCurl)
x <- getURL("https://raw.githubusercontent.com/ctzurcanu/smp/master/data/term.csv")
terms <- read.csv(text = x)
y <- getURL("https://raw.githubusercontent.com/ctzurcanu/smp/master/data/term_relation.csv")
rels <- read.csv(text = y)
s <- getURL("https://raw.githubusercontent.com/ctzurcanu/smp/master/data/jos_sliced_api.csv")
smp <- read.csv(text = s)
subject_apps <- readRDS("data/subject_apps.rds")
onto_list <- readRDS("data/onto_list.rds")
app_list <- readRDS("data/app_list.rds")

ancestry <- function(terms, rels, term, lang, origin, returnIds = TRUE) {
  path = term
  if(term == origin) {
    return();
  }
  id = term;
  while((id != origin) == TRUE) {
    parent = as.character(rels[rels$uuid1 == id & rels$relation == 1,"uuid2"][1])
    path = c(path, parent);
    id = parent;
  }
  if(returnIds == TRUE) {
    return(path);
  }
  else {
    termsNames = c();
    for(id in path) {
      termsNames = c(termsNames, as.character(terms[terms$uuid == id & terms$lang == lang, "term"]));
    }
    return(termsNames);
  }
}
children <- function(terms, rels, term, lang, returnIds = TRUE) {
  termId = term
  kidsIds = as.character(rels[rels$uuid2 == termId & rels$relation == 1,"uuid1"])
  if(returnIds == TRUE) {
    result <- kidsIds;
  }
  else {
    kids = c();
    for(id in kidsIds) {
      termN = as.character(terms[terms$uuid == id & terms$lang == lang, "term"])
      termN <- termN[!is.na(termN)][1];
      kids = c(kids, termN);
    }
    result <- kids
  }
  return(result)
}
siblings <- function(terms, rels, term, lang, returnIds = TRUE) {
  if(returnIds == TRUE) { sibs <- children(terms, rels, rels[rels$uuid1 == term, "uuid2"]);}
  else { sibs <- children(terms, rels, as.character(rels[rels$uuid1 == term, "uuid2"]), lang, returnIds = FALSE);}
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
#' @param origin Give an origin.
#' @param returnIds Return IDs or terms in the provided language. Default: FALSE
#' path()
#' 
path <- function(term, lang="la", origin="be81554a-7759-11e4-adb6-57ce06b062da"){
  ids <- ancestry(terms, rels, term, lang, origin)
  result <- list()
  for(id in ids){
    result[[as.character(id)]] <- as.character(terms[terms$uuid == id & terms$lang == lang, "term"])
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
    transl <- terms[terms$uuid == term,]
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

uuid_strip <- function(uuid){
  uuid <- gsub("-","",uuid, fixed=TRUE)
  uuid
}
uuid_dash <- function(uuid){
  uuid <- gsub("-","",uuid, fixed=TRUE)
  uuidn <- paste(c(substr(uuid, 1, 8), "-", substr(uuid, 9, 12), "-", 
                   substr(uuid, 13, 16), "-", substr(uuid, 17, 20), "-",
                   substr(uuid, 21, 32)), collapse="")
  uuidn
}
load_apps <- function(uuid, lang, origin){
  list <- list()
  path <- ancestry(terms, rels, uuid, lang, origin)
  if(length(row.names(subject_apps[subject_apps$uuid %in% path & 
                     (grepl(lang, subject_apps$langs,fixed=TRUE) |  subject_apps$langs == "*"),])) > 0){
    apps <- subject_apps[subject_apps$subject %in% path & 
                           (grepl(lang, subject_apps$langs,fixed=TRUE) |  subject_apps$langs == "*"),]
    for(row in row.names(apps)){
      name <- as.character(apps[row,"name"])
      if(length(app_list[[name]]) == 0){
        load_apps_list(subject_apps)
      }
      data <- app_list[[name]]
      url <- as.character(apps[row,"root_url"])
      param_ind <- gregexpr("(<)([^>]+)(>)", url)
      start_ind <- param_ind[[1]]
      length_ind <- attr(param_ind[[1]],"match.length")
      params <- c();
      ini <- 1
      urln <- ""
      for(i in 1:length(start_ind)){
        param <- substr(url,start_ind[i]+1,start_ind[i]+length_ind[i]-2)
        urln <- paste(c(urln,substr(url,ini, start_ind[i]-1)), collapse="")
        if(param == "uuid"){ 
          #url <- sub(paste(c("<",param,">"),collapse=""), uuid, url, fixed=TRUE)
          urln <- paste(c(urln, uuid, collapse=""))
        }
        else if(param == "lang"){
          #url <- sub(paste(c("<",param,">"),collapse=""), lang, url, fixed=TRUE)
          urln <- paste(c(urln, lang, collapse=""))
        }
        else{
          params[param] <- as.character(data[data$uuid == uuid, param])
          #url <- sub(paste(c("<",param,">"),collapse=""), params[param], url, fixed=TRUE)
          if(!params[param] %in% c("NULL","NA")){
            urln <- paste(c(urln, params[param], collapse=""))
          }
        }
        ini = start_ind[i]+length_ind[i]
      }
      urln <- paste(c(urln,substr(url,ini,  nchar(url))), collapse="")
      id <- as.character(apps[row,"id"])
      list[[id]] <- list()
      for(n in names(apps)){
        list[[id]][[n]] <- as.character(apps[row,n])
      }
      list[[id]][["root_url"]]<- urln
    }
  }
  list
}
load_app_list <- function(subject_apps){
  for(row in row.names(subject_apps)){
    temp <- getURL(as.character(subject_apps[row,"csv_url"]))
    app_list[[as.character(subject_apps[row,"id"])]] <- read.csv(text = temp)
  }
  saveRDS(app_list,"data/app_list.rds")
}
add_app <- function(name, icon, subject, type_value, type_level, langs, uuid_column, root_url, csv_url, origin=""){
  if(type_value == "uuid"){
    uuid <- subject
  }
  else {
    uuid <- get_uuid(subject, value_type)
  }
  id <- as.integer(subject_apps[length(row.names(subject_apps)),"id"])+1
  row <- data.frame(id, name, icon, uuid, subject, type_value, type_level, paste(langs, collapse=","), subject_column, root_url, csv_url)
  names(row) <- names(subject_apps)
  subject_apps <- rbind(subject_apps, row)
  saveRDS(subject_apps, file="data/subject_apps.rds")
  if(type_level == "tree"){
    subject_list <- tree(uuid, langs[1], unlist=FALSE)
    subject_list <- unlist(subject_list)
    uuids <- subject_list[grep("id",names(subject_list), fixed=TRUE)]
    subjects <- subject_list[grep("name",names(subject_list), fixed=TRUE)]
  }
  else if(type_level == "children"){
    uuids <- children(terms, rels, uuid)
    subjects <- children(terms, rels, uuid, langs[1], returnIds = FALSE)
  } else if(type_level == "path"){
    uuids <- ancestry(terms, rels, uuid, langs[1], origin)
    subjects <- ancestry(terms, rels, uuid, langs[1], origin, returnIds = FALSE)
  }
  ids <- seq_len(length(subject))
  app_id <- rep_len(id, length(subject))
  table <- data.frame(id=ids, uuid=uuids, subject=subjects, type_value=type_value, app_id)
  table
}
#tb<-add_app("smt", "icon","be7331b8-7759-11e4-adb6-57ce06b062da", "uuid", "tree")

delete_app <- function(app_id){
  subject_apps<-subject_apps[!subject_apps$id == app_id,]
  saveRDS(subject_apps, file="data/subject_apps.rds")
}

modify_app <- function(app_id, name="", icon="", subject="", type_value="", type_level="", langs="", uuid_column="", root_url="", csv_url=""){
  
}

#' Ontologies helps you browse your ontologies
#'
#' This function allows you to see what ontologies are available.
#' @param onto_list Data frame of ontologies.
#' ontologies()
#' 
ontologies <- function(){
  langs <- levels(onto_list$lang)
  data <- list()
  for(lang in langs){
    data[[lang]] <- list()
    ids <- as.character(onto_list[onto_list$lang == lang, "uuid"])
    for(id in ids){
      data[[lang]][[id]] <- list()
      temp <- list()
      attr <- names(onto_list)
      for(att in attr){
        temp[[att]] <- as.character(onto_list[onto_list$lang == lang & onto_list$uuid == id, att])
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
#' @param term Give a uuid.
#' @param lang Give a display language (la=Latin, en=English, ro=Romanian). Defaults to "la" (Latin).
#' @param origin Give a uuid for the origin.
#' @keywords ontology
#' @export
#' @examples
#' ontobrowse()
#' 
ontobrowse <- function(term="be81554a-7759-11e4-adb6-57ce06b062da", lang="la", origin = ""){
  list <- list()
  list[["id"]] <- term
  list[["name"]] <- as.character(terms[terms$uuid == term & terms$lang == lang, "term"])[1]
  #list[["ancestry"]] <- list()
  list[["haschildren"]] <- 0
  list[["children"]] <- list()
  #list[["siblings"]] <- list()
#   parents <- ancestry(terms, rels, term, lang, origin)
#   if(length(parents) > 0){
#     for(i in length(parents):2) {
#       name <- as.character(terms[terms$uuid == parents[i] & terms$lang == lang, "term"])
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
      name <- as.character(terms[terms$uuid == kids[i] & terms$lang == lang, "term"])
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
#       name <- as.character(terms[terms$uuid == sibs[i] & terms$lang == lang, "term"])
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
#' @param term Give a uuid.
#' @param lang Give a display language (la=Latin, en=English, ro=Romanian). Defaults to "la" (Latin).
#' @param origin Give a uuid for the origin.
#' @keywords ontology
#' @export
#' @examples
#' tree()
#' 
tree <- function(term="be81554a-7759-11e4-adb6-57ce06b062da", lang="la", origin = "", unlist=TRUE){
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

# uuid_df <- function(terms){
#   arr = c()
#   dup <- duplicated(terms[,"uuid"])
#   indexes <- sort.int(dup, index.return=TRUE)$ix
#   unique <- indexes[which(dup[indexes] == FALSE)]
#   for(un in unique){
#     arr[un] <- UUIDgenerate()
#   }
#   if(any(indexes[which(dup[indexes] == TRUE)]) == TRUE){
#     rest <- indexes[which(dup[indexes] == TRUE)]
#     arr[rest] <- "0"
#   }
#   terms <- cbind(terms,uuid=arr)
#   rows <- row.names(terms[terms$uuid == "0",])
#   for(row in rows){
#     terms[row,"uuid"]<- as.character(terms[terms$uuid == as.character(terms[row,"uuid"]) & terms$uuid != 0, "uuid"][1])
#   }
#   terms
# }

#' List of ontologies accessed by the RSubject package
#'
#' @name onto_list
#' @docType data
#' @author Loredana Cirstea \email{loredana.cirstea@gmail.com}
#' @keywords ontologies
NULL

#' Prices of 50,000 round cut diamonds.
#'
#' A dataset containing the prices and other attributes of almost 54,000
#' diamonds.
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{price}{price, in US dollars}
#'   \item{carat}{weight of the diamond, in carats}
#'   ...
#' }
#' @source \url{http://www.diamondse.info/}
#' @references \url{data_blah.com}
"diamonds"