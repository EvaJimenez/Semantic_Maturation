######################################################
## 1 # CHILDES DATA FOR WORD CO-OCCURRENCES MATRIX ###
######################################################

## 1 ##  Functions we need ####

# Load the necessary package
library("tm")
library("XML")
library("methods")
library("stringr")
library("rlist")
library("quanteda")


unZipFile <- function(fileName) {
  
  # check that file name ends with .zip
  fnl <- nchar(fileName)
  el = 4
  if((fnl > el) & (substr(fileName, fnl - el + 1, fnl ) == '.zip')) {
    unzip(zipfile=fileName)
    return( substr(fileName, 1, fnl - 4 ) )
  }
  else {
    return(paste(fileName,'ERROR', sep='_'))
  }
}


## Get a list of XML file locations given researchers name
listXMLFilesForResearcher <- function(researcher) {
  return <- unlist(
    list.files(path = paste('./',researcher, sep=""), pattern='.xml', full.names = TRUE, recursive = TRUE)
  )
}


## Get XML Node set documents from list of xml locations
doXMLDoc <- function(xmlLocation) {
  
  getWords( 
    unlist(list.clean( 
      getUtterances( 
        xmlParse(xmlLocation) 
      ),
      fun = function(x) length(x) == 0L,
      recursive = TRUE
    ))
  )
}


getDocs <- function(vector_of_researchers) {
  unlist(sapply(vector_of_researchers, function(researcher) 
    unlist(
      list.files(path = paste('./',researcher, sep=""), pattern='.xml', full.names = TRUE, recursive = TRUE)
    )
  )
  )
  # return a list of docs
}


## Global needed
ns <- c(ns="http://www.talkbank.org/ns/talkbank")


getWords  <- function(utterance) {
  # return a list of words in the utterance
  sapply(
    xpathSApply(utterance, ".//ns:w//ns:stem/text()", xmlValue, namespaces = ns, noMatchOkay=TRUE),
    function(x) tolower(x)
  )
}


getUtterances <- function(parsed_doc) {
  # return a list of matching utterance nodesets
  getNodeSet(parsed_doc, "//ns:u[@who='MOT']", namespaces = ns)
}


processDocument <- function(DocLocation) {
  # returns a list of vector of words
  parsedXML <- xmlParse(DocLocation)
  utterances <- list.clean(getUtterances(parsedXML), fun = function(x) length(x) == 0L, recursive = TRUE)
  
  finalcharvar <- unlist(list.clean(lapply(utterances, getWords), fun = function(x) length(x) == 0L, recursive = TRUE))
  
  # would like to return a character string
  # with the last 5 words as 'ZZZZZxx'
  finalcharvar2 <- paste(finalcharvar, collapse = " ")
  ending <- paste(rep('ZZZZZxx',5), collapse = " ")
  
  paste(finalcharvar2, ending, collapse = " ")
}


## Get a list of XML file locations given researchers name
listXMLFilesForResearcher <- function(researcher) {
  return <- unlist(
    list.files(path = paste('./',researcher, sep=""), pattern='.xml', full.names = TRUE, recursive = TRUE)
  )
}


## Take in a character string and de-dupe the words in it
dedupe <- function(ctrstring) {
  unlist(strsplit(ctrstring, " ")) 
}




## 2 ##  Get data ready from CHILDES ####

## Files location ##
setwd('XXX') ######  your data location

## Unzip all the zip files into a folder for each researcher and return reserchers name
fileNames <- Sys.glob("*.zip")
researchers <- sapply(fileNames, function(x) unZipFile(x))

#write(researchers, file = "researchers.text") ## Refer to this text file to find the names of the data files used for this project
xmlFilesToVisit <- unlist(sapply(researchers, listXMLFilesForResearcher))
list_of_charstrs_of_docs <- lapply(xmlFilesToVisit, processDocument) # This takes a while...
length(list_of_charstrs_of_docs) # 6235

vector_of_charstrs_of_docs <- unlist(list_of_charstrs_of_docs, recursive = TRUE, use.names = FALSE)
length(vector_of_charstrs_of_docs)

allWordsUnique <- sort(unique(unlist(lapply(vector_of_charstrs_of_docs, dedupe))))
allWordsUnique <- allWordsUnique[-1]
length(allWordsUnique)  ## 19401




## 3 ##  Create matrix of co-occurrences ####
library(quanteda)
CHILDES_matrix_all <- convert((fcm(vector_of_charstrs_of_docs, context = "window", window =10, 
                                 ordered=FALSE, span_sentence = TRUE, count="frequency")),to = "data.frame")
rownames(CHILDES_matrix_all) <- as.character(CHILDES_matrix_all$document)
CHILDES_matrix_all$document <- NULL

## 4 ## Save data ####
setwd('XXX')

save(CHILDES_matrix_all, file = "CHILDES_matrix_all.RData")
save(vector_of_charstrs_of_docs, file = "vector_of_charstrs_of_docs.RData")
save(allWordsUnique, file = "allWordsUnique.RData")
