require(devtools)
require(dplyr)
require(data.table)
require(ggplot2)
require(ggthemes)
require(reshape2)
require(RMySQL)
require(tm)
require(wordcloud)
require(memoise)
require(RCurl)
require(curl)
require(jsonlite)
require(shiny)
require(httr)

source("functions.R")

## Read glassdoor data
mydb <- dbConnect(MySQL(), user='STATW4701', password='V1sual1zati0n', dbname='glassdoor', host='vichitra.cs.columbia.edu')
rs <- dbSendQuery(mydb, "select * from CompanyRatings")
master_gd_data <- fetch(rs, n=-1)
gd_data <- master_gd_data
gd_data$overall_rating <- paste0(sprintf(gd_data$overallRating, fmt='%#.1f'), "-", gd_data$ratingDescription)
gd_data$squareLogo <- paste0('<img src="', gd_data$squareLogo, '" alt="', gd_data$name, '" height="42" width="42">')
gd_data$match_company_name <- toupper(gd_data$name)
names(gd_data)[7] <- "industry"
names(gd_data)[8] <- "number_of_reviews"

## Read indeed data
rs <- dbSendQuery(mydb, "select * from IndeedAnalyticsByTopCities")
master_indeed_data <- fetch(rs, n=-1)
all_indeed_data <- master_indeed_data
names(all_indeed_data)[8] <- "job_title"
all_indeed_data$match_company_name <- toupper(all_indeed_data$company)
nyc_indeed_data <- filter(all_indeed_data, state %in% c('NY', 'NJ', 'CT'))

## Get current jobs directly from Indeed API
alljobs <- getJobs()

## This would include the company logo in the data table.  Leaving off for now.
#jobdt <- data.frame(alljobs$squareLogo, alljobs$company, alljobs$job_link, alljobs$posted_at, alljobs$location, alljobs$posting_date,
#                    alljobs$industry, alljobs$number_of_reviews, alljobs$overall_rating)
#colnames(jobdt) <- c('', 'Company', 'Job title', 'How recent', 'Location', 'Posting date',
#                       'Glassdoor industry', 'Glassdoor number of reviews', 'Glassdoor overall company rating')

jobdt <- data.frame(alljobs$company, alljobs$job_link, alljobs$posted_at, alljobs$location, alljobs$posting_date,
                    alljobs$industry, alljobs$number_of_reviews, alljobs$overall_rating)
colnames(jobdt) <- c('Company', 'Job title', 'How recent', 'Location', 'Posting date',
                     'Glassdoor industry', 'Glassdoor number of reviews', 'Glassdoor overall company rating')

jobmap <- data.frame(alljobs$lat, alljobs$long, alljobs$city, alljobs$job_link, alljobs$company, alljobs$posted_at,
                     alljobs$industry, alljobs$number_of_reviews, alljobs$overall_rating)
colnames(jobmap) <- c('lat', 'long', 'city', 'job_title', 'company', 'posted_at', 'industry', 'number_of_reviews', 'overall_rating')

##---------------------------
## Generate industry plots
##---------------------------
## Inner join of glassdoor ratings with NY area data science jobs for one of the plots
matches_only <- inner_join(gd_data, alljobs, by="match_company_name")
matches_only <- filter(matches_only, industry.x != '')
#dsjobs_by_company <- data.frame(matches_only[,c(3, 7, 8, 10, 12, 14, 16, 15, 39)])
ds_companies <- data.frame(matches_only[,c(3,7,8,10,12,14,16,15)])
ds_companies <- unique(ds_companies)

names(ds_companies) <- c("company_name",
                         "industry",
                         "number_of_reviews",
                         "overall_rating",
                         "culture_and_values",
                         "compensation_and_benefits",
                         "work_life_balance",
                         "career_opportunities")

ds_companies$overall_rating            <- as.numeric(ds_companies$overall_rating)
ds_companies$culture_and_values        <- as.numeric(ds_companies$culture_and_values)
ds_companies$compensation_and_benefits <- as.numeric(ds_companies$compensation_and_benefits)
ds_companies$work_life_balance         <- as.numeric(ds_companies$work_life_balance)
ds_companies$career_opportunities      <- as.numeric(ds_companies$career_opportunities)

## Prepare industry rating data for plotting
rating_data  <- gd_data[,c(7,8,10,12,14,16,15)]
names(rating_data) <- c("industry",
                        "number_of_reviews",
                        "overall_rating",
                        "culture_and_values",
                        "compensation_and_benefits",
                        "work_life_balance",
                        "career_opportunities")
rating_data$overall_rating            <- as.numeric(rating_data$overall_rating)
rating_data$culture_and_values        <- as.numeric(rating_data$culture_and_values)
rating_data$compensation_and_benefits <- as.numeric(rating_data$compensation_and_benefits)
rating_data$work_life_balance         <- as.numeric(rating_data$work_life_balance)
rating_data$career_opportunities      <- as.numeric(rating_data$career_opportunities)

## Compute top industries for each rating category
by_industry <- group_by( rating_data, industry )
top <- summarize( by_industry,
                  sum(number_of_reviews),
                  mean(overall_rating),
                  mean(culture_and_values),
                  mean(compensation_and_benefits),
                  mean(work_life_balance),
                  mean(career_opportunities),
                  sum(mean(overall_rating), mean(culture_and_values), mean(compensation_and_benefits),
                      mean(work_life_balance), mean(career_opportunities)) )

by_industry <- group_by( ds_companies, industry )
top_hiring <- summarize( by_industry,
                         sum(number_of_reviews),
                         mean(overall_rating),
                         mean(culture_and_values),
                         mean(compensation_and_benefits),
                         mean(work_life_balance),
                         mean(career_opportunities),
                         sum(mean(overall_rating), mean(culture_and_values), mean(compensation_and_benefits),
                             mean(work_life_balance), mean(career_opportunities)) )

names(top) <- c("industry", "number_of_reviews", "overall_rating", "culture_and_values",
                "compensation_and_benefits", "work_life_balance", "career_opportunities", "tiebreaker")
names(top_hiring) <- c("industry", "number_of_reviews", "overall_rating", "culture_and_values",
                "compensation_and_benefits", "work_life_balance", "career_opportunities", "tiebreaker")

top <- filter( top, number_of_reviews >= 500 )
top_hiring <- filter( top_hiring, number_of_reviews >= 100)

ratings_culture      <- filter( top, culture_and_values > 0 )
ratings_compensation <- filter( top, compensation_and_benefits > 0 )
ratings_worklife     <- filter( top, work_life_balance > 0 )
ratings_career       <- filter( top, career_opportunities > 0 )

top_overall      <- arrange( top, desc(round(overall_rating, 1)), desc(tiebreaker) )
top_culture      <- arrange( top, desc(round(culture_and_values, 1)), desc(tiebreaker) )
top_compensation <- arrange( top, desc(round(compensation_and_benefits, 1)), desc(tiebreaker) )
top_worklife     <- arrange( top, desc(round(work_life_balance, 1)), desc(tiebreaker) )
top_career       <- arrange( top, desc(round(career_opportunities, 1)), desc(tiebreaker) )
top_hiring       <- arrange( top_hiring, desc(round(overall_rating, 1)), desc(tiebreaker) )

top_overall      <- c( top_overall[1:25,1] )
top_culture      <- c( top_culture[1:25,1] )
top_compensation <- c( top_compensation[1:25,1] )
top_worklife     <- c( top_worklife[1:25,1] )
top_career       <- c( top_career[1:25,1] )

n <- min(25, nrow(top_hiring) - (nrow(top_hiring) %% 5))  ## keep it to a multiple of 5
top_hiring <- c( top_hiring[1:n, 1] )

pdata <- prepIndustryPlot(top_overall, rating_data, "overall")
overall_rating_plot <<- ggplot(pdata, aes(x=category2, y=rating, fill=category)) +
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
culture_rating_plot <<- ggplot(pdata, aes(x=category2, y=rating, fill=category)) +
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
compensation_rating_plot <<- ggplot(pdata, aes(x=category2, y=rating, fill=category)) +
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
career_opportunities_plot <<- ggplot(pdata, aes(x=category2, y=rating, fill=category)) +
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
worklife_plot <<- ggplot(pdata, aes(x=category2, y=rating, fill=category)) +
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

ds_ratings <- ds_companies[,2:8]
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

##--------------------------------------------------------------
## Prepare skills word cloud and friend recommendation analysis
##--------------------------------------------------------------
dbcities <<- list("Atlanta"  = "Atlanta", "Austin" = "Austin",
                  "Boston" = "Boston", "Chicago" = "Chicago", "Houston" = "Houston",
                  "New York" =  "New York", "San Francisco" = "San Francisco",
                  "San Jose" = "San Jose", "Seattle" = "Seattle",
                  "Los Angeles" = "Los Angeles", "Washington DC" = "Washington")

ratingsVariables1 <<-list("overallRating" = "Overall Rating",  "workLifeBalanceRating" = "Work Life Balance",
                          "cultureAndValuesRating"= "Culture and Values" , "seniorLeadershipRating" = "Senior Leadership",
                          "compensationAndBenefitsRating" = "Compensation and Benefits",  "careerOpportunitiesRating" = "Career Opportunities" ,
                          "pctApprove" = "CEO Approval" )


reverseratingsVariables1 <<-list("Overall Rating" = "overallRating", "Work Life Balance" = "workLifeBalanceRating",
                          "Culture and Values" = "cultureAndValuesRating", "Senior Leadership" = "seniorLeadershipRating",
                          "Compensation and Benefits" = "compensationAndBenefitsRating", "Career Opportunities" = "careerOpportunitiesRating",
                          "CEO Approval" = "pctApprove")

ratingsVariables2 <<-list("workLifeBalanceRating" = "Work Life Balance",
                          "cultureAndValuesRating"= "Culture and Values" , "seniorLeadershipRating" = "Senior Leadership",
                          "compensationAndBenefitsRating" = "Compensation and Benefits",  "careerOpportunitiesRating" = "Career Opportunities" ,
                          "pctApprove" = "CEO Approval" )

reverseratingsVariables2 <<-list("Work Life Balance" = "workLifeBalanceRating",
                                 "Culture and Values" = "cultureAndValuesRating", "Senior Leadership" = "seniorLeadershipRating",
                                 "Compensation and Benefits" = "compensationAndBenefitsRating", "Career Opportunities" = "careerOpportunitiesRating",
                                 "CEO Approval" = "pctApprove")

analysisvariables <<-list("Overall Rating" = 1, "Recommend to Friends Rating" = 2)

##--------------------------------------
## Prepare resume help
##--------------------------------------
resume_words <<- read.csv("./Data/WordFrequenciesGroupedShiny.csv", stringsAsFactors=FALSE, header=TRUE)
