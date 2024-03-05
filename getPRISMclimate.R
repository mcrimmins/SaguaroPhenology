#####
# code to download and save daily PRISM climate data from single location
# MAC 03/04/24

# required libraries
library(RCurl)
library(jsonlite)
library(dplyr)


##### download data for location
# set location 
lat=32.071 
lon=-110.640 

#download daily PRISM -----
endDate<-"2023-12-31"
jsonQuery=paste0('{"loc":"',lon,',',lat,'","sdate":"1981-01-01","edate":"',endDate,'","grid":"21",
                                    "meta":"ll,elev","elems":[{"name":"pcpn","units":"mm"},{"name":"mint","units":"degreeC"},{"name":"maxt","units":"degreeC"}]}')

outDaily<-postForm("http://data.rcc-acis.org/GridData",.opts = list(postfields = jsonQuery, 
                                                                    httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
# json to datframe
outDaily<-fromJSON(outDaily)
dataDaily<-data.frame(outDaily$data)
#####

# character to numeric conversion
data <- dataDaily[,2:4] %>% mutate_if(is.character, as.numeric)
# bind back together
dataDaily<-cbind.data.frame(as.Date(dataDaily$X1,"%Y-%m-%d"),data)
# set colnames
colnames(dataDaily)<-c("date","precip","minT","maxT")
# calculate daily average temperature
dataDaily$avgT<-(dataDaily$maxT+dataDaily$minT)/2

# add in date elements
dataDaily$month<-as.numeric(format(dataDaily$date, "%m"))
dataDaily$year<-as.numeric(format(dataDaily$date, "%Y"))
dataDaily$doy<-as.numeric(format(dataDaily$date, "%j"))

save(dataDaily,lat,lon, file = "PRISM_dailyData.RData")
######
