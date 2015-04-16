# Python 2.7:
import urllib2
from lxml import etree
#import numpy as np
import pandas as pd
import sqlite3
import os
import csv


wd = os.getcwd() # Edit this for current path
os.chdir(wd)


######
# Define XML Parameters
publisher_id = '4751269202013823'
v = '2'
format = 'json'
callback = ''
q = 'analytics' # QUERY
location = '10024'
sort = ''
radius = '50'
st = 'jobsite'
jt = ''
start = 0
limit = '1800' # NOT EMPTY
fromage = '60'
highlight = '0'
filter = '1'
latlong = '1'
co = 'us'
chnl = ''
userip = '72.229.59.86'
useragent = 'Mozilla/%2F4.0%28Firefox%29'



xml_string = 'http://api.indeed.com/ads/apisearch?publisher=' + publisher_id + '&q=' + q +'&l=' + location +\
             '&sort=' + sort + '&radius=' + radius + '&st=' + st + '&jt=' + jt + '&start=' + str(start) +\
             '&limit=' + limit + '&fromage=' + fromage + '&filter=' + filter + '&latlong=' + latlong +\
             '&co=' + co + '&chnl=' + chnl + '&userip=' + userip + '&useragent=' + useragent + '&v=' + v

# Python 2.7:
job_xml = urllib2.urlopen(xml_string).read()
job_tree = etree.HTML(job_xml)

[num_results] = job_tree.xpath('//totalresults/text()')
num_results = min(int(num_results), int(limit))

city_list = []
state_list = []
snippet_list = []
lat_list = []
long_list = []
date_list = []
jobtitle_list = []
company_list = []


for p in range(0, num_results, 25):
    print('Retrieving records from page '+ str(p/25 + 1) +' out of '+ str(len(range(0,num_results,25))) +' pages.')
    start = p

    xml_string = 'http://api.indeed.com/ads/apisearch?publisher=' + publisher_id + '&q=' + q +'&l=' + location +\
             '&sort=' + sort + '&radius=' + radius + '&st=' + st + '&jt=' + jt + '&start=' + str(start) +\
             '&limit=' + limit + '&fromage=' + fromage + '&filter=' + filter + '&latlong=' + latlong +\
             '&co=' + co + '&chnl=' + chnl + '&userip=' + userip + '&useragent=' + useragent + '&v=' + v

    # Python 2.7:
    job_xml = urllib2.urlopen(xml_string).read()


    job_tree = etree.HTML(job_xml)

    r_index = min(25,num_results - p)

    for r in range(r_index):
        try:
            result_tag = job_tree.xpath('//result')[r]
        except:
            city_list.append('')
            state_list.append('')
            snippet_list.append('')
            lat_list.append('')
            long_list.append('')
            city_list.append('')
            jobtitle_list.append('')
            company_list.append('')

        try:
            city_list.append(result_tag.find('city').text)
        except:
            city_list.append('')

        try:
            state_list.append(result_tag.find('state').text)
        except:
            state_list.append('')

        try:
            snippet_list.append(str(result_tag.find('snippet').text))
        except:
            snippet_list.append('')

        try:
            lat_list.append(result_tag.find('latitude').text)
        except:
            lat_list.append('')

        try:
            long_list.append(result_tag.find('longitude').text)
        except:
            long_list.append('')

        try:
            date_list.append(result_tag.find('date').text)
        except:
            date_list.append('')

        try:
            jobtitle_list.append(str(result_tag.find('jobtitle').text))
        except:
            jobtitle_list.append('')

        try:
            company_list.append(result_tag.find('company').text)
        except:
            company_list.append('')


job_frame = pd.DataFrame({'city': city_list, 'state': state_list, 'snippet': snippet_list,
                          'latitude': lat_list, 'longitude': long_list, 'date': date_list,
                          'job title': jobtitle_list, 'company':company_list})


job_frame.to_csv("analytics.csv", index=False, columns=('city','state','snippet', 'latitude', 'longitude','date', 'job title', 'company'))
