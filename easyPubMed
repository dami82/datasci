library(XML)
library(easyPubMed)

myQuery <- "p53 AND Chicago[Affiliation]" 
myIdList <- getPubmedIds(myQuery)

myRetstart <- 0
myRetmax <- 200

myStarts <- seq(myRetstart, as.integer(as.character(myIdList$Count)), by= myRetmax)

myResult <- do.call(rbind, lapply(myStarts, (function(i){
  
  recordsXml <- fetchPubmedData(pubmedIdList = myIdList, retstart = i, retmax = myRetmax)
  
  #each article is included within a <PubmedArticle> tag
  # fun = saveXML returns a list of character strings instead of InternalNode-class objects
  recordList <- xpathApply(recordsXml, "//PubmedArticle", saveXML)
  
  
  tmpDF <- t(as.data.frame(lapply(1:length(recordList), (function(x){
   
    titlePosition <- regexpr("(<ArticleTitle).+(\\/ArticleTitle>)", recordList[[x]])
    tmpTitle <- substr(recordList[[x]], titlePosition, 
                       titlePosition + attributes(titlePosition)$match.length)
    tmpTitle <- gsub("<ArticleTitle>|</ArticleTitle>|([[:space:]]$)", "", tmpTitle)
    #tmpTitle
    
    pubmedIdPosition <- regexpr("(<PMID).+(\\/PMID>)", recordList[[x]])
    tmpPMID <- substr(recordList[[x]], pubmedIdPosition, 
                      pubmedIdPosition + attributes(pubmedIdPosition)$match.length)
    tmpPMID <- gsub("<PMID|<\\/PMID>|[[:space:]]", "", tmpPMID)
    tmpPMID <- gsub("^.*>", "", tmpPMID)
    #tmpPMID
    
    tmpAuthors <- strsplit(recordList[[x]], "<AuthorList")[[1]][[2]]
    tmpFirstAuthor <- strsplit(tmpAuthors, "<Author")[[1]][[2]]
    lastNamePos <- regexpr("(<LastName).*(\\/LastName>)",tmpFirstAuthor)
    lastName <- substr(tmpFirstAuthor, lastNamePos, 
                       lastNamePos + attributes(lastNamePos)$match.length)
    lastName <- gsub("<LastName|<\\/LastName>|([[:space:]]$)", "", lastName)
    lastName <- gsub("^.*>", "", lastName)
    #lastName
    
    firstNamePos <- regexpr("(<ForeName).*(\\/ForeName>)",tmpFirstAuthor)
    firstName <- substr(tmpFirstAuthor, firstNamePos, 
                        firstNamePos + attributes(firstNamePos)$match.length)
    firstName <- gsub("<ForeName|<\\/ForeName>|([[:space:]]$)", "", firstName)
    firstName <- gsub("^.*>", "", firstName)
    #firstName
    
    tmpName <- paste(firstName, lastName, sep = " ")
    
    #return
    c(tmpPMID, tmpName, tmpTitle)
  }))))
  
  rownames(tmpDF) <- NULL
  tmpDF
  
})))

colnames(myResult) <- c("PMID", "Author", "Title")
head(myResult)
write.csv(myResult, "p53_papers_Chicago.csv")

