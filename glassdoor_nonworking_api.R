#require("RCurl")
require("rjson")

## -------------------------------------------------
## Get employer data from Glassdoor
## Issue:  the Glassdoor API only returns 10 rows.  
## See get_companies.R instead 
## -------------------------------------------------
raw_data <- fromJSON("http://api.glassdoor.com/api/api.htm?v=1&format=json&t.p=31640&t.k=j3G7m4V1Dbg&action=employers&city=new%20%york&state=ny&userip=69.136.97.180&format=json&action=employers&pn=1", flatten=TRUE)
df <- as.data.frame(raw_data$response[5])
page_count <- raw_data$response$totalNumberOfPages
for (i in 2:10) {
        myurl <- paste0("http://api.glassdoor.com/api/api.htm?v=1&format=json&t.p=31640&t.k=j3G7m4V1Dbg&action=employers&",
                        "city=new%20%york&state=ny&&userip=69.136.97.180&format=json&action=employers&pn=",i)
        raw_data <- fromJSON(myurl, flatten=TRUE)
        page <- as.data.frame(raw_data$response[5])
        df <- rbind(df, page)
}

## Remove irrelevant columns
df$employers.ceo.image.src <- NULL
df$employers.ceo.image.height <- NULL
df$employers.ceo.image.width <- NULL

## Write to a csv file
write.csv(df, "gd_data.csv", row.names=TRUE)

