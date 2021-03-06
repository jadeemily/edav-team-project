require("RCurl")
require("jsonlite")

# Glassdoor ratings data
#start <- 1
start <- nrow(gdata)/10 + 1  ## start from this page
end <- start + 9
finished <- FALSE
while (finished == FALSE) {
        cat("start=", start, " end=", end)
        for (i in start:end) {
                myurl <- paste0("http://api.glassdoor.com/api/api.htm?v=1&format=json&t.p=31640&t.k=j3G7m4V1Dbg&",
                        "action=employers&city=new%20%york&state=ny&userip=69.136.97.180&format=json&action=employers&pn=",i)
                raw_data   <- fromJSON(myurl, flatten=TRUE)
                response   <- raw_data$response
                page_count <- response$totalNumberOfPages
                page       <- as.data.frame(response$employers)
                page$featuredReview.headline <- NULL
                page$featuredReview.pros <- NULL
                page$featuredReview.cons <- NULL
                page$ceo.name <- NULL
                page$ceo.title <- NULL
                page$ceo.numberOfRatings <- NULL
                #page$ceo.pctApprove <- NULL
                #page$ceo.pctDisapprove <- NULL
                page$ceo.image.src <- NULL
                page$ceo.image.height <- NULL
                page$ceo.image.width <- NULL

                page[is.na(page)] <- " "
                if (i > 1) {
                        gdata <- rbind(gdata, page)
                } else {
                        gdata <- page
                }
        }
        Sys.sleep(10)
        start <- as.integer(nrow(gdata)/10) + 1  ## start from this page
        Sys.sleep(10)
        end <- start + 9
        #cat("start=", start, " end=", end, " pages=", page_count)
        if (end > min(page_count, 1000)) {
                finished <- TRUE
                fn <- paste0("gd_data_", paste0(substr(Sys.time(),1,10), ".",
                                        substr(Sys.time(),12,13),
                                        substr(Sys.time(),15,16),
                                        substr(Sys.time(),18,19),
                                        ".csv"))
                write.csv(gdata, file=fn, row.names=TRUE)
        }
}


