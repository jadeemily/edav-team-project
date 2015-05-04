require("RCurl")
require("jsonlite")

# Glassdoor ratings data
start <- nrow(gdata)/10 + 1  ## start from this page
end <- start + 9
finished <- FALSE
while (finished == FALSE) {
        for (i in start:end) {
                myurl <- paste0("http://api.glassdoor.com/api/api.htm?v=1&format=json&t.p=31640&t.k=j3G7m4V1Dbg&",
                        "action=employers&city=new%20%york&state=ny&userip=69.136.97.180&format=json&action=employers&pn=",i)
                raw_data <- fromJSON(myurl, flatten=TRUE)
                page_count <- raw_data$response$totalNumberOfPages
                page <- as.data.frame(raw_data$response[5])

                page$employers.featuredReview.headline <- NULL
                page$employers.featuredReview.pros <- NULL
                page$employers.featuredReview.cons <- NULL
                page$employers.ceo.image.src <- NULL
                page$employers.ceo.image.height <- NULL
                page$employers.ceo.image.width <- NULL

                page[is.na(page)] <- " "
                #page <- page[,c(1:24)]
                gdata <- rbind(gdata, page)
                #i <- i + 1
        }
        start <- nrow(gdata)/10 + 1  ## start from this page
        end <- start + 9
        cat("start=", start, " end=", end, " pages=", page_count)
        if (end > min(page_count*10, 10000)) {
                finished <- TRUE
                fn <- paste0("gd_data_", paste0(substr(Sys.time(),1,10), ".",
                                        substr(Sys.time(),12,13),
                                        substr(Sys.time(),15,16),
                                        substr(Sys.time(),18,19),
                                        ".csv"))
                write.csv(gdata, file=fn, row.names=TRUE)
        }
}


