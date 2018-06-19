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
                       video = vector(length = length(linksall)),
                       videoID = vector(length = length(linksall)),
                       videoTitle = vector(length = length(linksall)),
                       htmlText = vector(length = length(linksall))
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
  
  htmlText <-xpathSApply(news, "//div[contains(@class, 'field-type-text-with-summary')]/div/node()"  )
  raw <- paste(capture.output(htmlText[[1]], file=NULL), collapse="\n")
  result <- try(
    newstext[i,10] <- raw
  );  
  print(xpathSApply(news, "//div[contains(@class, 'field-type-text-with-summary')]/div/div/node()"))
  
  result <- try(
    newstext[i,4] <- substring(linksall[[i]], 7)
  );  
  
  path <- xpathSApply(news, "//div[1]/div/div/a/img[contains(@class, 'campl-scale-with-grid')]/@src")[1]

  alt <- xpathSApply(news, "//div[1]/div/div/a/img[contains(@class, 'campl-scale-with-grid')]/@alt")[1]
  print(paste0("List:",length(alt)))

  video <- xpathSApply(news, "//div[contains(@class, 'file-video')]", xmlValue)
  videoID <- xpathSApply(news, "//iframe[contains(@class, 'media-youtube-player')]/@src")
  videoTitle <- xpathSApply(news, "//iframe[contains(@class, 'media-youtube-player')]/@title")
  
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
    newstext[i,7] <- trimws(video) 
   } else {
     newstext[i,7] <- NA
   }
 ); 
 
  result <- try(
    if(length(videoID) >= 1) {
      newstext[i,8] <- videoID 
    } else {
      newstext[i,8] <- NA
    }
  ); 
  
  result <- try(
    if(length(videoTitle) >= 1) {
      newstext[i,9] <- videoTitle 
    } else {
      newstext[i,9] <- NA
    }
  ); 
}

newstext[newstext == FALSE] <- NA
newstext$date <- gsub(",","",trimws( newstext$date))
newstext$htmlText <- gsub("<div class=\"field-item even\">","", newstext$htmlText)
newstext$htmlText <- gsub("</div>","", newstext$htmlText)
newstext$htmlText <- gsub("<p> </p>","", newstext$htmlText)
newstext$htmlText <- gsub("<p> </p>","", newstext$htmlText)
newstext$htmlText <- gsub("class=\"rtejustify\"","", newstext$htmlText) 
newstext$videoID <- gsub("\\?wmode=opaque&controls=", "", newstext$videoID)
newstext$videoID <- gsub("https://www.youtube.com/embed/", "", newstext$videoID)

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

# Download the images and log 404s 
if (!file.exists("images")){
  dir.create("images")
}

if (!file.exists("logs")){
  dir.create("logs")
}

logfile <- "imageDownloads.log"
log_con <- file(logfile)

download <- function(data){
    url <- data['image'][[1]]
    name <- strsplit(basename(url), "\\?")
    name <- URLdecode(unlist(name)[1])
    exist <- url.exists(url) 
    if(exist == TRUE){
      download.file(URLencode(url), destfile = paste("images",name, sep = '/'))
    } else {
      print("That file is a 404")
      message <- paste0(url,"|","404 \n")
      cat(message, file ="logs/imageDownloads.log", append = TRUE)
    }
  
}
apply(newstext, 1, download)