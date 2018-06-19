#' A script to extract data from the Fitzwilliam news pages of the website.
#' This is partly inspired by Ben Marwick's day of archaeology distant reading

setwd("~/Documents/research/fitzwilliam")
library(RCurl)
library(XML)

n <- 30 # determined by inspecting the first page
# pre-allocate list to fill
links <- vector("list", length = n)
links[[1]] <-  unname(xpathSApply(htmlParse(getURI("http://www.fitzmuseum.cam.ac.uk/news")),"//h3/a/@href"))
for(i in 1:n){
  print(i)
   news <- htmlParse(getURI(paste0("http://www.fitzmuseum.cam.ac.uk/news?page=", i+1,"/")))
  links[[i+1]] <- unname(xpathSApply(news,"//h3/a/@href"))
}


# make one big list of URLs
linksall <- unlist(links)
#print(linksall)
# make a data.frame to store the full text in 
# and get date and author of full text also
newstext <- data.frame(
                      
                       title = vector(length = length(linksall)),
                       date = vector(length = length(linksall)),
                       text =  vector(length = length(linksall)),
                       slug = vector(length = length(linksall)),
                       image = vector(length = length(linksall)),
                       altText = vector(length = length(linksall)),
                       video = vector(length = length(linksall))
)

for(i in 1:length(linksall)){
  
  # track progress

  print(paste0("URL: ", "http://www.fitzmuseum.cam.ac.uk", linksall[[i]]))
  
  # get URL
  news <- htmlParse(getURI(paste0("http://www.fitzmuseum.cam.ac.uk",linksall[[i]])))
  
  result <- try(
    newstext[i,1] <- xpathSApply(news, "//h1[@class='campl-sub-title']", xmlValue)
  ); 
  if(class(result) == "try-error") next;
  
  result <- try(
    newstext[i,2] <- gsub("Posted: ", "", xpathSApply(news, "//div[@class='submitted']", xmlValue))
    ); 
 
  result <- try(
    newstext[i,3] <- xpathSApply(news, "//div[contains(@class, 'field-type-text-with-summary')]", xmlValue)
  );  
  
  print(xpathSApply(news, "//div[contains(@class, 'field-type-text-with-summary')]", xmlValue))
  
  result <- try(
    newstext[i,4] <- substring(linksall[[i]], 7)
  );  
  
  path <- xpathSApply(news, "//div[1]/div/div/a/img[contains(@class, 'campl-scale-with-grid')]/@src")[1]

  alt <- xpathSApply(news, "//div[1]/div/div/a/img[contains(@class, 'campl-scale-with-grid')]/@alt")[1]
  print(paste0("List:",length(alt)))
  
  video <- xpathSApply(news, "//div[contains(@class, 'file-video')]", xmlValue)
  print(paste0("Video: ", video))
 
  result <- try(
   if(length(path) >= 1) {
   newstext[i,5] <- path
   } else {
     newstext[i,5] <- NA
   }
 ); 

 result <- try(
   if(length(alt) >= 1) {
     newstext[i,6] <- alt
   } else {
     newstext[i,6] <- NA
   }
 ); 
  
 result <- try(
   if(length(video) >= 1) {
    newstext[i,7] <- video 
   } else {
     newstext[i,7] <- NA
   }
 ); 
 
}

newstext[newstext == FALSE] <- NA
newstext$date <- gsub(",","",trimws( newstext$date))

# Fix date string
library(lubridate)
dateNew <- newstext$date
dates <- dmy(dateNew)
# Add published as YYYY-mm-dd
newstext$published <- dates
# Drop old date column
newstext <- newstext[ , -which(names(newstext) %in% c("date"))]

# Add urls to data frame
newstext$url <- paste0("http://www.fitzmuseum.cam.ac.uk", linksall)

# Write a csv file
write.csv(newstext, file='fitz.csv',row.names=FALSE, na="")