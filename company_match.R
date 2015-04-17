
gdata <- read.csv("./Data/gd_all_companies.csv", stringsAsFactors=FALSE, header=TRUE)
gdata$match_company_name <- toupper(gdata$company_name)

idata <- read.csv("./Data/datascientist_jobresults.csv", stringsAsFactors=FALSE, header=TRUE)
idata$match_company_name <- toupper(idata$company)

require("dplyr")
matches_only <- inner_join(idata, gdata, by="match_company_name")
all_data <- left_join(idata, gdata, by="match_company_name", copy=TRUE)
