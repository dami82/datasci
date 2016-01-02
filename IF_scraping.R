##
##  2015 - 12 -31
##  Damiano Fantini
##  Web Scraping: extract IF data from the web (http://www.citefactor.org/)
##

library(httr)
setwd("~/")

baseAddr <- "http://www.citefactor.org/journal-impact-factor-list-2014_"
extenAddr <- ".html"
sitePages <- c("0-A","B","C","D","E","F","G","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z")

jTab <- matrix(c(1,1),nrow=1, ncol=2)
colnames(jTab) <- c("jName", "IF")
jTab <- as.data.frame(jTab)


for (page in sitePages) {

  queryAddr<- paste (baseAddr,page,extenAddr,sep="")
  sourceHTML<-GET(queryAddr)
  sourceHTML<- toString(sourceHTML)  
  
  tabStart <- regexpr("<CAPTION>Impact Factor 2014</CAPTION>", sourceHTML, fixed = TRUE)
  tabEnd <- regexpr("</TABLE>", sourceHTML, fixed = TRUE)
  tabHTML <- substr(sourceHTML, tabStart+38, tabEnd-1)
  
  ## now, let's extract each row (TR block) of our table
  tabChuncks <- unlist(strsplit(tabHTML, "</TR>", fixed=TRUE))
  
  for (chunck in tabChuncks) {
    
    chunck <- gsub("<b>", "", chunck, fixed=TRUE)
    chunck <- gsub("</b>", "", chunck, fixed=TRUE)
    chunck <- gsub("\n", "", chunck, fixed=TRUE)
  
    tmp_entries <- unlist(strsplit(chunck, "</TD>", fixed = TRUE))
    if (chunck == "" | regexpr("(INDEX)$", tmp_entries[1]) > 0 | regexpr("<TD DIR", tmp_entries[1]) < 0) {
      next()
    }
  
    jTitle <- gsub("<TD DIR=LTR ALIGN=LEFT>","", tmp_entries[2], fixed=TRUE)
    jTitle <- toupper(jTitle)
    jIF <- gsub("<TD DIR=LTR ALIGN=LEFT>","", tmp_entries[4], fixed=TRUE)

    jIF <- gsub("-", "NA", jIF, fixed = TRUE)
    jTab <- rbind(jTab, c(jTitle, jIF))
  }
}

write.csv(jTab, "jTabIF.csv")
