
for (i in 861:1973) {
        myurl <- paste0("https://api.import.io/store/data/ba547b57-49f4-432a-a57e-61cea9cf8c7d/_query?",
                "input/webpage/url=http%3A%2F%2Fwww.glassdoor.com%2FReviews%2Fnew-york-city-reviews-SRCH_IL.0%2C13_IM615_IP",
                i,".htm&_user=94bbcc50-59b1-4168-9603-de5452fdedc6&_apikey=965MxL5U2UA5P18W4iNt7flPxtn%2FMr5RF%2",
                "BeyT6K3YbylHhH8tGDEnfWggAXViyTkAN94n2PexcJ1EUv45tWMTA%3D%3D")
        result <- try(raw_data <- fromJSON(myurl, flatten=TRUE), silent=TRUE) 
        
        if (class(result) != "try-error") {
                headquarters    <- raw_data$results$headquarters
                company_url     <- raw_data$results$company_url
                num_reviews     <- raw_data$results$num_reviews
                overall_rating  <- raw_data$results$overall_rating
                review_summary  <- raw_data$results$review_summary
                company_name    <- raw_data$results$`name/_text`
                link_to_reviews <- raw_data$results$num_reviews_link

                page <- as.data.frame(company_name, stringsAsFactors=FALSE)
                page <- cbind(page, overall_rating)
                page <- cbind(page, num_reviews)
                page <- cbind(page, company_url)
                page <- cbind(page, headquarters)
                page <- cbind(page, review_summary)
                page <- cbind(page, link_to_reviews)
                names(page)[1] <- "company_name"

                if (i > 1) {
                        df <- rbind(df, page)
                } else {
                        df <- page
                }
        }
}
## Write to a csv file
#write.csv(df, "gd_data.csv", row.names=TRUE)
