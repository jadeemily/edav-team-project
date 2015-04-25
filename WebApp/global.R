library(RMySQL)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(data.table)

# Connect to the database -> Credentials:
# Host : vichitra.cs.columbia.edu
# User : STATW4701
# PWD : V1sual1zati0n
# DB Name : glassdoor
#mydb = dbConnect(MySQL(), user='STATW4701', password='V1sual1zati0n', dbname='glassdoor', host='vichitra.cs.columbia.edu')
#rs = dbSendQuery(mydb, "select * from CompanyData")  
#companies = fetch(rs, n=-1)

#rs = dbSendQuery(mydb, "select * from IndeedData")  
#jobs = fetch(rs, n=-1)

rating_data <- data[,c(6,7,9,11,13,14,15)]
names(rating_data) <- c("industry", "number_of_reviews", "overall_rating", "culture_and_values", 
                        "compensation_and_benefits", "career_opportunities", "work_life_balance")
rating_data$culture_and_values <- as.numeric(rating_data$culture_and_values)
rating_data$compensation_and_benefits <- as.numeric(rating_data$compensation_and_benefits)
rating_data$career_opportunities <- as.numeric(rating_data$career_opportunities)
rating_data$work_life_balance <- as.numeric(rating_data$work_life_balance)


## User selects:  Overall Rating
selection <- "overall_rating"
library(dplyr)
by_industry <- group_by( rating_data, industry )
top <- summarize( by_industry, sum(number_of_reviews), mean(overall_rating) )
names(top) <- c("industry", "number_of_reviews", "rating")
top <- filter(top, number_of_reviews > 1000)
top <- arrange( top, desc(rating) )
top <- c(top[1:25,1])

pdata <- suppressWarnings(inner_join(rating_data, top, by="industry", copy=TRUE)) 
pdata <- melt(pdata, "industry", na.rm=TRUE)
cdata <- cast(pdata, industry ~ variable, mean)
cdata$number_of_reviews <- NULL
cdata$overall_rating <- round(cdata$overall_rating, 1)
cdata$culture_and_values <- round(cdata$culture_and_values, 1)
cdata$compensation_and_benefits <- round(cdata$compensation_and_benefits, 1)
cdata$career_opportunities <- round(cdata$career_opportunities, 1)
cdata$work_life_balance <- round(cdata$work_life_balance, 1)
cdata <- arrange(cdata, desc(overall_rating))

pdata <- melt(cdata, "industry")
names(pdata) <- c("Industry", "Category", "Rating")
pdata <- arrange(pdata, Industry, desc(Rating))
pdata <- transform(pdata, Category=ordered(Category))
pdata$Industry <- factor(pdata$Industry, levels=unlist(top), ordered=TRUE)

pdata <- transform(pdata, category2 = factor(paste(Industry, Rating)))
pdata <- transform(pdata, category2 = reorder(category2, rank(Rating), ordered=TRUE))

ggplot(pdata, aes(x=category2, y=Rating, fill=Category)) +
        geom_bar(stat="identity", position=position_dodge(), aes(order=Rating)) +
        #geom_text(data=pdata, aes(x=Industry, y=Rating, ymax=Rating, group=Category, label=Rating, vjust=1.8), 
        geom_text(data=pdata, aes(x=category2, y=Rating, ymax=Rating, group=Category, label=Rating, vjust=1.8), 
                  position=position_dodge(width=0.9), size=4) +
        ggtitle("Most Highly Rated Industries Overall") +
        ylim(0, 5) +
        theme_bw() +
        scale_colour_hue() +
        facet_wrap(~ Industry, scale='free_x') + 
        theme(axis.title.y = element_blank()) +
        theme(axis.title.x = element_blank()) +
        theme(axis.text.x = element_blank()) +
        theme(axis.ticks = element_blank()) 
        #theme(strip.background = element_blank(), strip.text.x = element_blank())
        #theme(axis.text.y=element_text(angle=0,hjust=1,vjust=.5,colour='gray50'))
ggsave(file="overall_rating.svg")
