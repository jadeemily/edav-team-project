require(memoise)
require(dplyr)

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

getConnection <- function(group) {
        if (!exists('.connection', where=.GlobalEnv)) {
                .connection <<- dbConnect(MySQL(), user='STATW4701', password='V1sual1zati0n', dbname='glassdoor', host='vichitra.cs.columbia.edu')
        } else if (class(try(dbGetQuery(.connection, "SELECT 1"))) == "try-error") {
                dbDisconnect(.connection)
                .connection <<- dbConnect(MySQL(), user='STATW4701', password='V1sual1zati0n', dbname='glassdoor', host='vichitra.cs.columbia.edu')
        }

        return(.connection)
}

getTermMatrix <- memoise(function(dbCity) {
        ##con <- getConnection()  ## this was giving errors and corrupting the connection; use existing connection in global mydb
        rs = dbSendQuery(mydb, paste0("select * from IndeedAnalyticsByTopCities where city = '",dbCity,"'"))
        jobData = fetch(rs, n=-1)
        #dbDisconnect(con)

        # Read in data as dataFrame
        # reading only the text snippets
        x<-data.frame(v1=jobData$snippet)
        x<-subset(x,x$v1 != "")
        docs<- Corpus(DataframeSource(x))

        # now for some cleanups
        docs <- tm_map(docs, content_transformer(tolower))
        docs <- tm_map(docs, removeNumbers)
        docs <- tm_map(docs, removePunctuation)
        docs <- tm_map(docs, removeWords, stopwords("english"))
        docs <- tm_map(docs, removeWords, c("experience", "will", "data", "analytics", "skills", "analytic", "analysis", "big", "team", "scientist", "scientists", "engineers", "work"))
        docs <- tm_map(docs, stripWhitespace)
        #  docs <- tm_map(docs, stemDocument)

        # create the Document Term Matrix and compute the frequencies
        dtm <- DocumentTermMatrix(docs)
        m <- as.matrix(dtm)
        sort(colSums(m), decreasing = TRUE)
        #freqs <- colSums(as.matrix(dtm))

})

getVariables <-(function(choice)
{
  if(choice == 1)
  {
    reverseratingsVariables2
  }
  else
  {
    reverseratingsVariables1
  }
})

getRegressionAnalysis <- memoise(function(variable1, variable2, k, clustertype){
        #con <- getConnection()
        #rs = dbSendQuery(mydb, "select * from CompanyRatings")
        #data = fetch(rs, n=-1)
        data <- master_gd_data  ## already have this in a data frame
        #print(variable1)
        #print(variable2)
        data <- subset(data, data$employers.industry != "")
        industries <-unique(data$employers.industry)
        largedata<-subset(data, data$employers.numberOfRatings > 40)
        clusteringdata<-array()
        nameofindustry<-vector()
        mostimportant<-array()
        clusters<-data.frame()
        j <- 0
        for (i in industries)
        {
                fdata <- subset(largedata, data$employers.industry == i)
                if (nrow(fdata) > 40)
                {
                        #print (paste0("Industry is ", i, " and the number of companies is ", nrow(fdata)))
                        id <- fdata$employers.id
                        name <- fdata$employers.name
                        website <- fdata$employers.website
                        industry <- fdata$employers.industry

                        overallRating <- fdata$employers.overallRating
                        ratingDescription <- fdata$employers.ratingDescription
                        cultureAndValuesRating <- fdata$employers.cultureAndValuesRating
                        seniorLeadershipRating <- fdata$employers.seniorLeadershipRating
                        compensationAndBenefitsRating <- fdata$employers.compensationAndBenefitsRating
                        careerOpportunitiesRating <- fdata$employers.careerOpportunitiesRating
                        workLifeBalanceRating <- fdata$employers.workLifeBalanceRating
                        recommendToFriendRating <- fdata$employers.recommendToFriendRating
                        pctApprove <- fdata$employers.ceo.pctApprove

                        employer <- data.frame(id, name, website, industry, overallRating, ratingDescription, cultureAndValuesRating, seniorLeadershipRating, compensationAndBenefitsRating, careerOpportunitiesRating, workLifeBalanceRating, recommendToFriendRating, pctApprove)

                        if(clustertype==2)
                        {
                      #  fit <- lm(fdata$employers.recommendToFriendRating ~ fdata$employers.overallRating + fdata$employers.workLifeBalanceRating + fdata$employers.cultureAndValuesRating + fdata$employers.seniorLeadershipRating + fdata$employers.compensationAndBenefitsRating + fdata$employers.careerOpportunitiesRating + fdata$employers.ceo.pctApprove -1 , data=fdata)
                        fit <- lm(recommendToFriendRating ~ overallRating + workLifeBalanceRating + cultureAndValuesRating + seniorLeadershipRating + compensationAndBenefitsRating + careerOpportunitiesRating + pctApprove -1 , data=fdata)
                        }
                        else
                        {
                      #    fit <- lm(fdata$employers.overallRating ~ fdata$employers.workLifeBalanceRating + fdata$employers.cultureAndValuesRating + fdata$employers.seniorLeadershipRating + fdata$employers.compensationAndBenefitsRating + fdata$employers.careerOpportunitiesRating + fdata$employers.ceo.pctApprove -1 , data=fdata)
                          fit <- lm(overallRating ~ workLifeBalanceRating + cultureAndValuesRating + seniorLeadershipRating + compensationAndBenefitsRating + careerOpportunitiesRating + pctApprove -1 , data=fdata)
                        }
                        fit2 <- fit
                        fit2$coefficients <- fit2$coefficients/max(fit2$coefficients)

                        if(nrow(clusteringdata))
                        {
                                clusteringdata <- rbind(clusteringdata, fit2$coefficients)
                                nameofindustry <- rbind(nameofindustry, i)
                                mostimportant <- rbind(mostimportant, names(fit2$coefficients[order(fit2$coefficients, decreasing = TRUE)[1:2]]))
                        }
                        else
                        {
                                clusteringdata <- fit2$coefficients
                                nameofindustry <- i
                                mostimportant <- names(fit2$coefficients[order(fit2$coefficients, decreasing = TRUE)[1:2]])
                        }

                }
        }
        clusteringdata<-na.omit(clusteringdata)
        mostimportant<-na.omit(mostimportant)
        clusters<-kmeans(clusteringdata,k, nstart=10)

        print("Clusters are printed here:")
        print(sort(clusters$cluster))

        x<-data.frame()
        (x<-data.frame(v1 = clusteringdata[,variable1], v2 = clusteringdata[,variable2], v3 = clusters$cluster, v4 = nameofindustry))
        x<-x[1:nrow(clusteringdata),]
        clustertable<-array(,k*nrow(clusteringdata))
        dim(clustertable)<-c(nrow(clusteringdata),k)
        j <- 1.0

        for(i in 1:k)
        {
                #  clustertable[,1] <- clusters$cluster[1:nrow(clusteringdata)]
                indices <- which(clusters$cluster[1:nrow(clusteringdata)] == i)
                if(clustertype == 2)
                {
                clustertable[1:length(indices),i] <- paste0(nameofindustry[indices], ":                  ", ratingsVariables1[mostimportant[indices,1]],
                                                            " and ", ratingsVariables1[mostimportant[indices,2]])
                }
                else
                {clustertable[1:length(indices),i] <- paste0(nameofindustry[indices], ":                  ", ratingsVariables2[mostimportant[indices,1]],
                                                             " and ", ratingsVariables2[mostimportant[indices,2]])}
                if (j < length(indices)){
                        j <- length(indices)
                }
        }
        list(plotdata=x, name=clustertable[1:j,])
})


all_values <- function(x) {
  if(is.null(x)) return(NULL)
  paste0(names(x), ": ", format(x), collapse = "<br />")
}

getJobs <- function(start='', jq='data+scientist', l='10199', r=50) {
        ## Indeed returns a maximum of 25 jobs per call
        finished <- FALSE
        while (finished == FALSE) {
                myurl <- constructIndeedURL(start, jq, l, r)
                ijobs <- suppressWarnings(fromJSON(myurl, flatten=TRUE))
                end   <- ijobs$end
                num_jobs <- ijobs$totalResults
                i <- ijobs$results
                job_title <- i$jobtitle
                job_link <- createJobLink(job_title, i$url)
                company <- i$company
                city <- i$city
                location <- i$formattedLocation
                posted_by <- i$source
                posting_date <- i$date
                #snippet <- i$snippet
                url <- i$url
                #onmousedown <- i$onmousedown
                lat <- i$latitude
                long <- i$longitude
                #jobkey <- i$jobkey
                #sponsored <- i$sponsored
                expired <- i$expired
                #indeedApply <- i$indeedApply
                posted_at <- i$formattedRelativeTime
                page <- data.frame(job_title, job_link, posted_at, company, location,
                           posting_date, expired, lat, long, stringsAsFactors=FALSE)
                if (end > 25) {
                        alljobs  <- rbind(alljobs, page)
                } else {
                        alljobs  <- page

                }
                #cat('num_jobs=',num_jobs, '; end=', end, '  ')
                if (end >= min(num_jobs-25, 1000)) {
                        finished <- TRUE
                } else {
                        start <- end
                }
        }
        #alljobs <- filter(alljobs, expired == FALSE)
        alljobs$expired <- NULL

        # We only want jobs with 'data' in the job title;  Indeed search results are too broad
        alljobs <- alljobs[c(grep("data", alljobs$job_title, ignore.case=TRUE)),]

        alljobs$match_company_name <- toupper(alljobs$company)

        # Perform lookup and substitution for certain company names that we know do not match between Indeed and Glassdoor
        company_lookup <- read.csv("name_lookup.txt", stringsAsFactors=FALSE, header=TRUE)
        x    <- match(alljobs$match_company_name, company_lookup$Indeed)
        indx <- alljobs$match_company_name %in% company_lookup$Indeed
        alljobs$match_company_name[indx] <- sub("(^[[:space:]]+|[[:space:]]+$)", "", company_lookup$Glassdoor[x[indx]])

        alljobs <- left_join(alljobs, unique(gd_data[,c(31,7,8,10)]), by="match_company_name")

        # If company data is still missing, try pulling it directly from Glassdoor
        #missing <- which(is.na(alljobs$industry))
        #for (m in missing) {
        #        cq <- gsub(" ", "+", alljobs$company[m])
        #        cdata <- getGlassdoorData(cq)
        #        if (length(cdata) > 0) {
        #                alljobs[m, 9] <- cdata$industry
        #                alljobs[m, 10] <- cdata$numberOfRatings
        #                alljobs[m, 11] <- cdata$overallRating
        #        }
        #}
        #alljobs$match_company_name <- NULL

        #colnames(alljobs) <- c('Job title', 'How recent', 'Company', 'Location', 'Posting date',
        #                       'Glassdoor industry', 'Glassdoor number of reviews', 'Glassdoor overall company rating',
        #                       'lat', 'long')
        return(alljobs)
}

constructIndeedURL <- function(start, jq, l, r) {
        paste0("http://api.indeed.com/ads/apisearch?publisher=8835055801144634",
                "&q=", jq,
                "&l=", l,
                "&sort=date",
                "&radius=", r,
                "&st=&jt=",
                "&start=", start,
                "&limit=25",
                "&fromage=14",
                "&filter=1",
                "&latlong=1",
                "&co=us&chnl=&userip=1.2.3.4&useragent=Mozilla/%2F4.0%28Firefox%29&v=2&format=json")
}

getGlassdoorData <- function(cq) {
        myurl <- paste0("http://api.glassdoor.com/api/api.htm?v=1&format=json&t.p=31640&t.k=j3G7m4V1Dbg",
                        "&action=employers&city=&state=&userip=69.136.97.180",
                        "&q=", cq,
                        "&format=json&action=employers")
        raw_data <- fromJSON(myurl, flatten=TRUE)
        if (raw_data$success) {
                company <- as.data.frame(raw_data$response$employers)[1,]
                if (ncol(company) > 0) {
                        #cat(q)
                        cdata <- company[c(2,6,7,9)]
                        return(cdata)
                }
        }
}

createJobLink <- function(labeltext, urltext) {
        paste0('<a href="', urltext, '">', labeltext, '</a>')
}
