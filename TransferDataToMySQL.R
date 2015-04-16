library(RMySQL)

# Connect to the database -> Credentials:
# Host : vichitra.cs.columbia.edu
# User : STATW4701
# PWD : V1sual1zati0n
# DB Name : glassdoor
mydb = dbConnect(MySQL(), user='STATW4701', password='V1sual1zati0n', dbname='glassdoor', host='vichitra.cs.columbia.edu')

#Query database for table
dbListTables(mydb)

#Query table for Fields
dbListFields(mydb, 'CompanyData')

#Set working directory
setwd("~/Google Drive/VizProject/")

# Glassdoor data
if (file.exists("./Data/gd_data1-2.csv"))
{ 
  dbSendQuery(mydb, 'drop table if exists CompanyData')
  
  dbWriteTable(mydb, name='CompanyData', value='./Data/gd_data1-2.csv')
  
  rs = dbSendQuery(mydb, "select * from CompanyData")  
  data = fetch(rs, n=-1)
}


#Indeed data 
if (file.exists("./Data/indeed_data.csv"))
{ 
  dbSendQuery(mydb, 'drop table if exists IndeedData')
  
  #For some reason reading csv directly into table was failing so I haad to read it into a dataset first
  idata = read.csv(file="./Data/indeed_data.csv", header=TRUE, sep=",")  # read csv file
  dbWriteTable(mydb, name='IndeedData', value=idata)
  
  rs = dbSendQuery(mydb, "select * from IndeedData")  
  data = fetch(rs, n=-1)
}

# End of Code