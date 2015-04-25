require("RCurl")
require("jsonlite")

## --------------------------------------------------------------------------------------------------
## Use this to continue an extract.  
## Assumes page_count was already set by running glassdoor_100companies_api.R
## --------------------------------------------------------------------------------------------------
i <- nrow(data)/10 + 1  ## start from this page 
while (i <= page_count) {
        myurl <- paste0("http://api.glassdoor.com/api/api.htm?v=1&format=json&t.p=31640&t.k=j3G7m4V1Dbg&",
                        "action=employers&city=new%20%york&state=ny&userip=69.136.97.180&format=json&action=employers&pn=",i)
        raw_data <- fromJSON(myurl, flatten=TRUE)
        page <- as.data.frame(raw_data$response[5])
        
        page$employers.featuredReview.headline <- NULL
        page$employers.featuredReview.pros <- NULL
        page$employers.featuredReview.cons <- NULL
        page$employers.ceo.image.src <- NULL
        page$employers.ceo.image.height <- NULL
        page$employers.ceo.image.width <- NULL    

        page[is.na(page)] <- " "
        data <- rbind(data, page)
        i <- i + 1  
}

## Write to a csv file
fn <- paste0("gd_data_", paste0(substr(Sys.time(),1,10), ".", 
                                substr(Sys.time(),12,13), 
                                substr(Sys.time(),15,16), 
                                substr(Sys.time(),18,19),
             ".csv"))
write.csv(data, file=fn, row.names=TRUE)

