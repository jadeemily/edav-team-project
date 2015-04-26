#install.packages("RMySQL")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("ggthemes")
#install.packages("reshape2")
#install.packages("data.table")

require(dplyr)
require(ggplot2)
require(ggthemes)
require(reshape2)
require(RMySQL)
#require(BH)

mydb = dbConnect(MySQL(), user='STATW4701', password='V1sual1zati0n', dbname='glassdoor', host='vichitra.cs.columbia.edu')
rs = dbSendQuery(mydb, "select * from CompanyRatings")
data = fetch(rs, n=-1)

#data <- read.table("Data/company_data_table", stringsAsFactors=FALSE, header=TRUE)
#rating_data <- data[,c(6,7,9,11,13,14,15)]
rating_data <- data[,c(7,8,10,12,14,15,16)]
names(rating_data) <- c("industry", "number_of_reviews", "overall_rating", "culture_and_values",
                        "compensation_and_benefits", "career_opportunities", "work_life_balance")
rating_data$culture_and_values <- as.numeric(rating_data$culture_and_values)
rating_data$compensation_and_benefits <- as.numeric(rating_data$compensation_and_benefits)
rating_data$career_opportunities <- as.numeric(rating_data$career_opportunities)
rating_data$work_life_balance <- as.numeric(rating_data$work_life_balance)

## Overall Rating
library(dplyr)
by_industry <- group_by( rating_data, industry )
top <- summarize( by_industry, sum(number_of_reviews), mean(overall_rating) )
names(top) <- c("industry", "number_of_reviews", "rating")
top <- filter(top, number_of_reviews > 1000)
top <- arrange( top, desc(rating) )
top <- c(top[1:25,1])

pdata <- suppressWarnings(inner_join(rating_data, top, by="industry", copy=TRUE))
pdata <- melt(pdata, "industry", na.rm=TRUE)
cdata <- dcast(pdata, industry ~ variable, mean)
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

overall_rating_plot <- ggplot(pdata, aes(x=category2, y=Rating, fill=Category)) +
        geom_bar(stat="identity", position=position_dodge(), aes(order=Rating)) +
        geom_text(data=pdata, aes(x=category2, y=Rating, ymax=Rating, group=Category, label=Rating, vjust=1.8),
                  position=position_dodge(width=0.9), size=4) +
        ylim(0, 5) +
        theme_fivethirtyeight() +
        scale_colour_hue() +
        facet_wrap(~ Industry, scale='free_x') +
        theme(legend.title=element_blank()) +
        theme(axis.title.y = element_blank()) +
        theme(axis.title.x = element_blank()) +
        theme(axis.text.x = element_blank())
#ggsave(overall_rating_plot, file="overall_rating.svg")
