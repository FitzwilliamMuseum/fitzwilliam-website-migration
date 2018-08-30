setwd("/Users/danielpett/githubProjects/windsorAndNewton")

library(jsonlite)
library(RCurl)
base <- 'http://webapps.fitzmuseum.cam.ac.uk/wndev'

data <- read.csv('imageList.csv')

log_con <- file("test.log")

download <- function(data){
    folder = data[1]
    file <- data[2]
    image <- paste(base,'assets',folder,file,'.jpg', sep='/')
    if (!file.exists(paste0('public/assets/',folder))){
      dir.create(paste0('public/assets/',folder))
    }
    
    
    exist <- url.exists(image) 
    
    if(exist == TRUE){
      download.file(URLencode(URL), destfile = paste(object,basename(URL), sep = '/'))
    } else {
      print("That file is a 404")
      message <- paste0(file,"|",image,"|","404 \n")
      cat(message, file = "test.log", append = TRUE)
    }
}
apply(data, 1, download)





