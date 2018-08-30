setwd("/Users/danielpett/Documents/GitHub/winsorAndNewtonArchive")

library(jsonlite)
library(RCurl)
base <- 'http://webapps.fitzmuseum.cam.ac.uk/wndev/assets/'

data <- read.csv('imageList.csv')

log_con <- file("test.log")

download <- function(data){
  folder = data[1]
  file <- data[2]
  image <- paste0(base,folder,'/', substr(file,1,nchar(file)-3) ,'.jpg')
  print(image)
  if (!file.exists(paste0('public/assets/',folder))){
    dir.create(paste0('public/assets/',folder))
  }
  
  
  exist <- url.exists(image) 
  if(exist == TRUE){
    download.file(URLencode(image), destfile = paste('public/assets',folder,basename(image), sep = '/'))
    print("File downloaded")
    print(paste(folder,basename(image), sep = '/'))
  } else {
    print("That file is a 404")
    message <- paste0(file,"|",image,"|","404 \n")
    cat(message, file = "test.log", append = TRUE)
  }
  
}
apply(data, 1, download)





