require("RCurl")
require("jsonlite")

## --------------------------------------------------------------------------------------------------
## Get employer data from Glassdoor
## Glassdoor limits us to 100 pages per day, but this API provides much more data than web scraping
## --------------------------------------------------------------------------------------------------
myurl <- paste0("http://api.glassdoor.com/api/api.htm?v=1&format=json&t.p=31640&t.k=j3G7m4V1Dbg&",
                "action=employers&city=new%20%york&state=ny&userip=69.136.97.180&format=json&action=employers&pn=1")
raw_data <- fromJSON(myurl, flatten=TRUE)
data <- as.data.frame(raw_data$response[5])
data2 <- as.data.frame(matrix(ncol=32,nrow=1, byrow=TRUE))
page_count <- raw_data$response$totalNumberOfPages
if (page_count > 1) { 
        i <- 2
} else {
        cat("\nError: no data retrieved!")
}
while (i <= page_count) {
        myurl <- paste0("http://api.glassdoor.com/api/api.htm?v=1&format=json&t.p=31640&t.k=j3G7m4V1Dbg&",
                        "action=employers&city=new%20%york&state=ny&userip=69.136.97.180&format=json&action=employers&pn=",i)
        raw_data <- fromJSON(myurl, flatten=TRUE)
        page <- as.data.frame(raw_data$response[5])
        if (ncol(page) == 35) {  
                data  <- rbind(data, page) 
        } else {                              ## some entries are missing the ceo image              
                data2 <- as.data.frame(raw_data$response[5])
        }
        i <- i + 10
}

## Remove ceo image and combine data
data$employers.ceo.image.src <- NULL
data$employers.ceo.image.height <- NULL
data$employers.ceo.image.width <- NULL
data <- rbind(data, data2[-1,])

## Write to a csv file
write.csv(df, "gd_data.csv", row.names=TRUE)

