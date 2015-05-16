require(memoise)
require(dplyr)
require(lubridate)

prepIndustryPlot <- function(top_rated, ratings, rating_type) {
        pdata <- suppressWarnings( inner_join(ratings, top_rated, by="industry", copy=TRUE) )
        pdata <- melt( pdata, "industry", na.rm=TRUE )
        cdata <- dcast( pdata, industry ~ variable, mean )
        cdata$number_of_reviews <- NULL
        cdata$overall_rating            <- round( cdata$overall_rating, 1 )
        cdata$culture_and_values        <- round( cdata$culture_and_values, 1 )
        cdata$compensation_and_benefits <- round( cdata$compensation_and_benefits, 1 )
        cdata$work_life_balance         <- round( cdata$work_life_balance, 1 )
        cdata$career_opportunities      <- round( cdata$career_opportunities, 1 )

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
        #rs = dbSendQuery(mydb, paste0("select * from IndeedAnalyticsByTopCities where city = '",
        #                              dbCity,"' and `job.title` like '%Data%'"))
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
        docs <- tm_map(docs, removeWords, c("experience", "will", "data", "analytics", "skills", "analytic", "analysis", "big",
                                            "team", "scientist", "scientists", "engineers", "work", "etc", "including", "background",
                                            "currently", "like", "least", "must", "information", "leading", "solution", "interested",
                                            "seeking", "needs", "similar", "best", "roles", "year", "york", "common", "define", "youll",
                                            "youre", "using", "help", "years", "part", "area", "can", "also", "enable", "important", "make",
                                            "one", "plus", "two", "alongside", "better", "choice", "content", "conjunction", "desire",
                                            "either", "making", "meet", "months", "needed", "pull", "new", "use", "andor", "etc", "please",
                                            "role", "apply", "ensure", "job", "huge", "level", "maximize", "plus", "review", "subject", "trying",
                                            "year", "action", "adm", "addition", "additional", "assemble", "assignment", "associates", "blend",
                                            "based", "center", "choice", "capturing", "close", "complete", "consumed", "day", "display", "either",
                                            "full", "highest", "honor", "includes", "incorporate", "individual", "lots", "look", "minimum",
                                            "modifying", "mission", "now", "ongoing", "past", "propose", "requires", "sites", "summary", "via",
                                            "provide", "whether", "laboratory", "strong", "solutions", "department", "well", "results",
                                            "position", "project", "amp", "works", "successful", "clinical", "providing", "qualifications",
                                            "performs", "within", "employee", "relevant", "since", "california", "looking", "large",
                                            "san", "select", "appropriate", "responsible", "teams", "worked", "working", "scientific",
                                            "pharmacology", "toxicology", "company", "closely", "science", "equivalent", "according",
                                            "understanding", "current", "required", "license", "class", "questions", "answers",
                                            "challenging"))
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
        data <- subset(data, data$industry != "")
        industries <-unique(data$industry)
        largedata<-subset(data, data$numberOfRatings > 40)
        clusteringdata<-array()
        nameofindustry<-vector()
        mostimportant<-array()
        clusters<-data.frame()
        j <- 0
        for (i in industries)
        {
                fdata <- subset(largedata, data$industry == i)
                if (nrow(fdata) > 40)
                {
                        #print (paste0("Industry is ", i, " and the number of companies is ", nrow(fdata)))
                        id <- fdata$id
                        name <- fdata$name
                        website <- fdata$website
                        industry <- fdata$industry

                        overallRating <- fdata$overallRating
                        ratingDescription <- fdata$ratingDescription
                        cultureAndValuesRating <- fdata$cultureAndValuesRating
                        seniorLeadershipRating <- fdata$seniorLeadershipRating
                        compensationAndBenefitsRating <- fdata$compensationAndBenefitsRating
                        careerOpportunitiesRating <- fdata$careerOpportunitiesRating
                        workLifeBalanceRating <- fdata$workLifeBalanceRating
                        recommendToFriendRating <- fdata$recommendToFriendRating
                        pctApprove <- fdata$ceo.pctApprove

                        employer <- data.frame(id, name, website, industry, overallRating, ratingDescription, cultureAndValuesRating, seniorLeadershipRating, compensationAndBenefitsRating, careerOpportunitiesRating, workLifeBalanceRating, recommendToFriendRating, pctApprove)

                        if(clustertype==2)
                        {
                        fit <- lm(recommendToFriendRating ~ overallRating + workLifeBalanceRating + cultureAndValuesRating + seniorLeadershipRating + compensationAndBenefitsRating + careerOpportunitiesRating + pctApprove -1 , data=fdata)
                        }
                        else
                        {
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
        clusteringdata <- na.omit(clusteringdata)
        mostimportant <- na.omit(mostimportant)
        clusters <- kmeans(clusteringdata, k, nstart=10)

        xcluster<-data.frame()
        xcluster<-data.frame(row.names = NULL, v4 = nameofindustry, v1 = clusteringdata[,variable1], v2 = clusteringdata[,variable2], Clusters = paste0("Cluster ",clusters$cluster))
        xcluster<-xcluster[1:nrow(clusteringdata),]
        clustertable<-array(,k*nrow(clusteringdata))
        dim(clustertable)<-c(nrow(clusteringdata),k)
        j <- 1.0

        for(i in 1:k)
        {
                #  clustertable[,1] <- clusters$cluster[1:nrow(clusteringdata)]
                indices <- which(clusters$cluster[1:nrow(clusteringdata)] == i)
                if(clustertype == 2)
                {
                clustertable[1:length(indices),i] <- paste0(nameofindustry[indices],
                                                            ":                  ",
                                                            ratingsVariables1[mostimportant[indices,1]],
                                                            " and ", ratingsVariables1[mostimportant[indices,2]])
                }
                else
                {clustertable[1:length(indices),i] <- paste0(nameofindustry[indices],
                                                             ":                  ",
                                                             ratingsVariables2[mostimportant[indices,1]],
                                                             " and ", ratingsVariables2[mostimportant[indices,2]])}
                if (j < length(indices)){
                        j <- length(indices)
                }
        }
        list(plotdata=xcluster, name=clustertable[1:j,])
})

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
                job_link <- createJobLink(job_title, i$url, i$onmousedown)
                company <- i$company
                city <- i$city
                location <- i$formattedLocation
                #posted_by <- i$source
                posting_date <- dmy_hms(i$date)
                #snippet <- i$snippet
                lat <- i$latitude
                long <- i$longitude
                #jobkey <- i$jobkey
                sponsored <- i$sponsored
                #expired <- i$expired              ## already FALSE for all jobs returned
                #indeedApply <- i$indeedApply
                posted_at <- i$formattedRelativeTime
                page <- data.frame(job_title, job_link, posted_at, company, location, city,
                           posting_date, lat, long, sponsored, stringsAsFactors=FALSE)
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

        # Indeed search results are too broad
        djobs <- alljobs[c(grep("data", alljobs$job_title, ignore.case=TRUE)),]
        ajobs <- alljobs[c(grep("analytics", alljobs$job_title, ignore.case=TRUE)),]
        alljobs <- rbind(djobs, ajobs)
        alljobs <- unique(alljobs)
        alljobs <- arrange(alljobs, desc(posting_date))
        #rm(list=c(djobs, ajobs))


        # Perform lookup and substitution for certain company names that we know do not match between Indeed and Glassdoor
        alljobs$match_company_name <- toupper(alljobs$company)
        company_lookup <- read.csv("name_lookup.txt", stringsAsFactors=FALSE, header=TRUE)
        x    <- match(alljobs$match_company_name, company_lookup$Indeed)
        indx <- alljobs$match_company_name %in% company_lookup$Indeed
        alljobs$match_company_name[indx] <- sub("(^[[:space:]]+|[[:space:]]+$)", "", company_lookup$Glassdoor[x[indx]])

        alljobs <- left_join(alljobs, unique(gd_data[,c(29,7,8,9,28)]), by="match_company_name")

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
                "&fromage=30",
                "&filter=1",
                "&latlong=1",
                "&co=us&chnl=&userip=1.2.3.4&useragent=Mozilla/%2F4.0%28Firefox%29&v=2&format=json")
}

createJobLink <- function(labeltext, urltext, omd) {
        paste0('<a onmousedown="', omd, '" href="', urltext, '" target="_blank">', labeltext, '</a>')
}

getResumeWords <- function(input_group) {
        rw <- filter(resume_words, Group == input_group)
        rw <- arrange(rw, desc(Frequency), Word)
        rw$Group <- NULL
        rw <- rw[1:min(20, nrow(rw)),]
        return(rw)
}

