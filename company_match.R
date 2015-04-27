
gdata <- read.csv("./Data/gd_all_companies.csv", stringsAsFactors=FALSE, header=TRUE)
gdata$match_company_name <- toupper(gdata$company_name)

idata <- read.csv("./Data/datascientist_jobresults.csv", stringsAsFactors=FALSE, header=TRUE)
idata$match_company_name <- toupper(idata$company)

require("dplyr")
matches_only <- inner_join(indeed_data, gd_data, by="match_company_name")
all_data <- left_join(gd_data, indeed_data, by="match_company_name", copy=TRUE)
