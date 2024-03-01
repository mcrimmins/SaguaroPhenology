
library(RCurl)
library(jsonlite)
library(dplyr)


#####
# adjusted year function
adj_yr <- function(dates, start_month) {
  # Convert dates into POSIXlt
  dates.posix = as.POSIXlt(dates)
  # Year offset
  offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
  # Water year
  adj.year = dates.posix$year + 1900 + offset
  # Return the water year
  adj.year
}

# adjusted doy function
adj_doy = function(x, start.month = 10L){
  start.yr = lubridate::year(x) - ( lubridate::month(x) < start.month)
  start.date = lubridate::make_date(start.yr, start.month, 1L)
  as.integer(x - start.date + 1L)
}
######

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

# create non-standard date like water year doy/year
startMo<-10
dataDaily$adjYear<-adj_yr(dataDaily$date, startMo)
dataDaily$adjDOY<-adj_doy(dataDaily$date, startMo)

# calc day avgT with base temp for gdd
baseT<-50
dataDaily$avgT_base <- unlist(lapply(dataDaily$avgT, function(x) ifelse(x>=baseT, x-baseT, 0)))

# cumulative precip totals
cumPrecip <- dataDaily %>% 
  group_by(adjYear, adjDOY) %>% # still doesn't quite work adjYear kicks off before adjDOY
  summarise(value = sum(precip, na.rm = T)) %>%
  mutate(csum = cumsum(value))
dataDaily$cumPrecip<-cumPrecip$csum

# cumulative doy climate - precip and temp
cumClim <- dataDaily %>% 
  group_by(adjYear, adjDOY) %>% # still doesn't quite work adjYear kicks off before adjDOY
  summarise(precip = sum(precip, na.rm = T),
            temp = sum(avgT_base, na.rm = T)) %>%
  mutate(psum = cumsum(precip),
         tsum = cumsum(temp))
dataDaily$cumPrecip<-cumClim$psum
dataDaily$gdd<-cumClim$tsum

##### analyze data ----

library(ggplot2)

# daily climatology
dailyClimo<-dataDaily %>% group_by(adjDOY) %>%
  summarise(meanT=mean(avgT),
            meanP=mean(precip))

ggplot(data = dailyClimo,
       mapping = aes(x = adjDOY, y = meanT, group = 1)) + 
  geom_bar(mapping = aes(y = meanP*5), stat = "identity") + 
  geom_line() + 
  scale_y_continuous(
    "temperature, C", 
    sec.axis = sec_axis(~ . / 5, name = "precipitation, mm")
  ) +
  theme_bw()


