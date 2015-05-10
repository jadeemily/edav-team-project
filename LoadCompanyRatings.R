library(RMySQL)

# Connect to the database -> Credentials:
# Host : vichitra.cs.columbia.edu
# User : STATW4701
# PWD : V1sual1zati0n
# DB Name : glassdoor
mydb = dbConnect(MySQL(), user='STATW4701', password='V1sual1zati0n', dbname='glassdoor', host='vichitra.cs.columbia.edu')

# Glassdoor ratings data
dbSendQuery(mydb, 'drop table if exists CompanyRatings')

dbWriteTable(mydb, name='CompanyRatings', value='gd_data_2015-05-10.013803.csv')

rs = dbSendQuery(mydb, "select count(*) from CompanyRatings")
db_data = fetch(rs, n=-1)
}
