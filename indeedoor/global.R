##---------------------------
## Generate industry plots
##---------------------------
require(dplyr)
require(ggplot2)
require(ggthemes)
require(reshape2)
require(RMySQL)
library(tm)
library(wordcloud)
library(memoise)

source("functions.R")

## Read glassdoor data
mydb <- dbConnect(MySQL(), user='STATW4701', password='V1sual1zati0n', dbname='glassdoor', host='vichitra.cs.columbia.edu')
rs <- dbSendQuery(mydb, "select * from CompanyRatings")
data <- fetch(rs, n=-1)
gd_data <- data
gd_data$match_company_name <- toupper(gd_data$employers.name)
names(gd_data)[7] <- "industry"
names(gd_data)[8] <- "number_of_reviews"
names(gd_data)[10] <- "overall_rating"

## Read indeed data
rs <- dbSendQuery(mydb, "select * from IndeedAnalyticsByTopCities")
all_indeed_data <- fetch(rs, n=-1)
names(all_indeed_data)[8] <- "job_title"
all_indeed_data$match_company_name <- toupper(all_indeed_data$company)
nyc_indeed_data <- filter(all_indeed_data, state %in% c('NY', 'NJ', 'CT'))

## Inner join of glassdoor ratings with NY area data science jobs for one of the industry plots
matches_only <- inner_join(gd_data, nyc_indeed_data, by="match_company_name")
dsjobs_by_company <- data.frame(matches_only[,c(3, 7, 8, 10, 12, 14, 15, 16, 39)])
names(dsjobs_by_company) <- c("company_name", "industry", "number_of_reviews", "overall_rating",
                              "culture_and_values", "compensation_and_benefits", "career_opportunities",
                              "work_life_balance", "job_title")
dsjobs_by_company$culture_and_values        <- as.numeric(dsjobs_by_company$culture_and_values)
dsjobs_by_company$compensation_and_benefits <- as.numeric(dsjobs_by_company$compensation_and_benefits)
dsjobs_by_company$career_opportunities      <- as.numeric(dsjobs_by_company$career_opportunities)
dsjobs_by_company$work_life_balance         <- as.numeric(dsjobs_by_company$work_life_balance)

## Prepare industry rating data for plotting
rating_data  <- gd_data[,c(7,8,10,12,14,15,16)]
names(rating_data) <- c("industry", "number_of_reviews", "overall_rating", "culture_and_values",
                        "compensation_and_benefits", "career_opportunities", "work_life_balance")
rating_data$culture_and_values        <- as.numeric(rating_data$culture_and_values)
rating_data$compensation_and_benefits <- as.numeric(rating_data$compensation_and_benefits)
rating_data$career_opportunities      <- as.numeric(rating_data$career_opportunities)
rating_data$work_life_balance         <- as.numeric(rating_data$work_life_balance)

## Compute top 25 industries by each rating category
by_industry <- group_by( rating_data, industry )
top <- summarize( by_industry,
                  sum(number_of_reviews),
                  mean(overall_rating),
                  mean(culture_and_values),
                  mean(compensation_and_benefits),
                  mean(career_opportunities),
                  mean(work_life_balance) )

by_industry <- group_by( dsjobs_by_company, industry )
top_hiring <- summarize( by_industry,
                         sum(number_of_reviews),
                         mean(overall_rating),
                         mean(culture_and_values),
                         mean(compensation_and_benefits),
                         mean(career_opportunities),
                         mean(work_life_balance) )

names(top) <- c("industry", "number_of_reviews", "overall_rating", "culture_and_values",
                "compensation_and_benefits", "career_opportunities", "work_life_balance")
names(top_hiring) <- c("industry", "number_of_reviews", "overall_rating", "culture_and_values",
                "compensation_and_benefits", "career_opportunities", "work_life_balance")

top <- filter( top, number_of_reviews >= 500 )
top_hiring <- filter( top_hiring, number_of_reviews >= 10)

ratings_culture      <- filter( top, culture_and_values > 0 )
ratings_compensation <- filter( top, compensation_and_benefits > 0 )
ratings_career       <- filter( top, career_opportunities > 0 )
ratings_worklife     <- filter( top, work_life_balance > 0 )

top_overall      <- arrange( top, desc(overall_rating) )
top_culture      <- arrange( top, desc(culture_and_values) )
top_compensation <- arrange( top, desc(compensation_and_benefits) )
top_career       <- arrange( top, desc(career_opportunities) )
top_worklife     <- arrange( top, desc(work_life_balance) )
top_hiring       <- arrange( top_hiring, desc(overall_rating) )

top_overall      <- c( top_overall[1:25,1] )
top_culture      <- c( top_culture[1:25,1] )
top_compensation <- c( top_compensation[1:25,1] )
top_career       <- c( top_career[1:25,1] )
top_worklife     <- c( top_worklife[1:25,1] )
top_hiring       <- c( top_hiring[1:25,1] )

pdata <- prepIndustryPlot(top_overall, rating_data, "overall")
overall_rating_plot <- ggplot(pdata, aes(x=category2, y=rating, fill=category)) +
        geom_bar(stat="identity", position=position_dodge(), aes(order=rating)) +
        geom_text(data=pdata, aes(x=category2, y=rating, ymax=rating, group=category, label=rating, vjust=1.8),
                  position=position_dodge(width=0.9), size=4) +
        ylim(0, 5) +
        theme_fivethirtyeight() +
        scale_colour_hue() +
        facet_wrap(~ industry, scale='free_x') +
        theme(legend.title=element_blank()) +
        theme(axis.title.y = element_blank()) +
        theme(axis.title.x = element_blank()) +
        theme(axis.text.x = element_blank())

pdata <- prepIndustryPlot(top_culture, rating_data, "culture_and_values")
culture_rating_plot <- ggplot(pdata, aes(x=category2, y=rating, fill=category)) +
        geom_bar(stat="identity", position=position_dodge(), aes(order=rating)) +
        geom_text(data=pdata, aes(x=category2, y=rating, ymax=rating, group=category, label=rating, vjust=1.8),
                  position=position_dodge(width=0.9), size=4) +
        ylim(0, 5) +
        theme_fivethirtyeight() +
        scale_colour_hue() +
        facet_wrap(~ industry, scale='free_x') +
        theme(legend.title=element_blank()) +
        theme(axis.title.y = element_blank()) +
        theme(axis.title.x = element_blank()) +
        theme(axis.text.x = element_blank())

pdata <- prepIndustryPlot(top_compensation, rating_data, "compensation_and_benefits")
compensation_rating_plot <- ggplot(pdata, aes(x=category2, y=rating, fill=category)) +
        geom_bar(stat="identity", position=position_dodge(), aes(order=rating)) +
        geom_text(data=pdata, aes(x=category2, y=rating, ymax=rating, group=category, label=rating, vjust=1.8),
                  position=position_dodge(width=0.9), size=4) +
        ylim(0, 5) +
        theme_fivethirtyeight() +
        scale_colour_hue() +
        facet_wrap(~ industry, scale='free_x') +
        theme(legend.title=element_blank()) +
        theme(axis.title.y = element_blank()) +
        theme(axis.title.x = element_blank()) +
        theme(axis.text.x = element_blank())

pdata <- prepIndustryPlot(top_career, rating_data, "career_opportunities")
career_opportunities_plot <- ggplot(pdata, aes(x=category2, y=rating, fill=category)) +
        geom_bar(stat="identity", position=position_dodge(), aes(order=rating)) +
        geom_text(data=pdata, aes(x=category2, y=rating, ymax=rating, group=category, label=rating, vjust=1.8),
                  position=position_dodge(width=0.9), size=4) +
        ylim(0, 5) +
        theme_fivethirtyeight() +
        scale_colour_hue() +
        facet_wrap(~ industry, scale='free_x') +
        theme(legend.title=element_blank()) +
        theme(axis.title.y = element_blank()) +
        theme(axis.title.x = element_blank()) +
        theme(axis.text.x = element_blank())

pdata <- prepIndustryPlot(top_worklife, rating_data, "work_life_balance")
worklife_plot <- ggplot(pdata, aes(x=category2, y=rating, fill=category)) +
        geom_bar(stat="identity", position=position_dodge(), aes(order=rating)) +
        geom_text(data=pdata, aes(x=category2, y=rating, ymax=rating, group=category, label=rating, vjust=1.8),
                  position=position_dodge(width=0.9), size=4) +
        ylim(0, 5) +
        theme_fivethirtyeight() +
        scale_colour_hue() +
        facet_wrap(~ industry, scale='free_x') +
        theme(legend.title=element_blank()) +
        theme(axis.title.y = element_blank()) +
        theme(axis.title.x = element_blank()) +
        theme(axis.text.x = element_blank())

ds_ratings <- dsjobs_by_company[,2:8]
pdata <- prepIndustryPlot( top_hiring, ds_ratings, "hiring")
hiring_plot <- ggplot(pdata, aes(x=category2, y=rating, fill=category)) +
        geom_bar(stat="identity", position=position_dodge(), aes(order=rating)) +
        geom_text(data=pdata, aes(x=category2, y=rating, ymax=rating, group=category, label=rating, vjust=1.8),
                  position=position_dodge(width=0.9), size=4) +
        ylim(0, 5) +
        theme_fivethirtyeight() +
        scale_colour_hue() +
        facet_wrap(~ industry, scale='free_x') +
        theme(legend.title=element_blank()) +
        theme(axis.title.y = element_blank()) +
        theme(axis.title.x = element_blank()) +
        theme(axis.text.x = element_blank())

##------------------------------
## Prepare map data and tooltips
## -----------------------------
map_df <- left_join(all_indeed_data, gd_data[,c(31,7,8,10)], by="match_company_name")

##--------------------------------------------------------------
## Prepare skills word cloud and friend recommendation analysis
##--------------------------------------------------------------
cities <<- list("Atlanta"  = "indeed_Atlanta_datascientist_or.csv", "NYC" =  "indeed_NYC_datascientist_or.csv",
                "Austin" = "indeed_Austin_datascientist_or.csv", "San Francisco" = 	"indeed_SF_datascientist_or.csv",
                "Chicago" = "indeed_Chicago_datascientist_or.csv",	"San Jose" = "indeed_Sanjose_datascientist_or.csv",
                "Washington DC" = "indeed_DC_datascientist_or.csv",	"Seattle" = 	"indeed_Seattle_datascientist_or.csv",
                "Los Angeles" = "indeed_LA_datascientist_or.csv")


dbcities <<- list("Atlanta"  = "Atlanta", "NYC" =  "New York",
                  "Austin" = "Austin", "San Francisco" =   "San Francisco",
                  "Chicago" = "Chicago",	"San Jose" = "San Jose",
                  "Washington DC" = "Washington",	"Seattle" = 	"Seattle",
                  "Los Angeles" = "Los Angeles")

ratingsVariables1 <<-list("Overall Rating" = "overallRating", "Work Life Balance" = "workLifeBalanceRating",
                          "Culture and Values" = "cultureAndValuesRating", "Senior Leadership" = "seniorLeadershipRating",
                          "Compensation and Benefits" = "compensationAndBenefitsRating", "Career Opportunities" = "careerOppotunitiesRating",
                          "CEO Approval" = "pctApprove")


ratingsVariables <<-list("Overall Rating" = 1, "Work Life Balance" = 2,
                         "Culture and Values" = 3, "Senior Leadership" = 4,
                         "Compensation and Benefits" = 5, "Career Opportunities" = 6,
                         "CEO Approval" = 7)

