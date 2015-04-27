prepIndustryPlot <- function(top_rated, ratings, rating_type) {

        pdata <- suppressWarnings( inner_join(ratings, top_rated, by="industry", copy=TRUE) )
        pdata <- melt( pdata, "industry", na.rm=TRUE )
        cdata <- dcast( pdata, industry ~ variable, mean )
        cdata$number_of_reviews <- NULL
        cdata$overall_rating            <- round( cdata$overall_rating, 1 )
        cdata$culture_and_values        <- round( cdata$culture_and_values, 1 )
        cdata$compensation_and_benefits <- round( cdata$compensation_and_benefits, 1 )
        cdata$career_opportunities      <- round( cdata$career_opportunities, 1 )
        cdata$work_life_balance         <- round( cdata$work_life_balance, 1 )

        if (rating_type == "overall") {
                cdata <- arrange( cdata, desc(overall_rating) )
        } else if (rating_type == "culture_and_values") {
                cdata <- arrange( cdata, desc(culture_and_values) )
        } else if (rating_type == "compensation_and_benefits") {
                cdata <- arrange( cdata, desc(compensation_and_benefits) )
        } else if (rating_type == "career_opportunities") {
                cdata <- arrange ( cdata, desc(career_opportunities) )
        } else if (rating_type == "work_life_balance") {
                cdata <- arrange ( cdata, desc(work_life_balance) )
        } else if (rating_type == 'hiring') {
                cdata <- arrange( cdata, desc(overall_rating) )
        }

        pdata <- melt( cdata, "industry" )
        names(pdata) <- c("industry", "category", "rating")
        pdata <- arrange( pdata, industry, desc(rating) )
        pdata <- transform( pdata, category=ordered(category) )
        pdata$industry <- factor( pdata$industry, levels=unlist(top_rated), ordered=TRUE )

        pdata <- transform( pdata, category2 = factor(paste(industry, rating)) )
        pdata <- transform( pdata, category2 = reorder(category2, rank(rating), ordered=TRUE) )
        return(pdata)
}
