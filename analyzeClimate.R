
library(RCurl)
library(jsonlite)
library(dplyr)

#####
# water year function
wtr_yr <- function(dates, start_month) {
  # Convert dates into POSIXlt
  dates.posix = as.POSIXlt(dates)
  # Year offset
  offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
  # Water year
  adj.year = dates.posix$year + 1900 + offset
  # Return the water year
  adj.year
}
######


lat=32.071 
lon=-110.640 
#download daily PRISM -----
#endDate<-"2020-12-31"

endDate<-"2023-12-31"
jsonQuery=paste0('{"loc":"',lon,',',lat,'","sdate":"1981-01-01","edate":"',endDate,'","grid":"21",
                                    "meta":"ll,elev","elems":[{"name":"pcpn"},{"name":"mint"},{"name":"maxt"}]}')

outDaily<-postForm("http://data.rcc-acis.org/GridData",.opts = list(postfields = jsonQuery, 
                                                                    httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))

outDaily<-fromJSON(outDaily)

dataDaily<-data.frame(outDaily$data)

data <- dataDaily[,2:4] %>% mutate_if(is.character, as.numeric)

dataDaily<-cbind.data.frame(as.Date(dataDaily$X1,"%Y-%m-%d"),data)

colnames(dataDaily)<-c("date","precip","minT","maxT")

dataDaily$avgT<-(dataDaily$maxT+dataDaily$minT)/2

# add in month,year,doy
dataDaily$month<-as.numeric(format(dataDaily$date, "%m"))
dataDaily$year<-as.numeric(format(dataDaily$date, "%Y"))
dataDaily$doy<-as.numeric(format(dataDaily$date, "%j"))

#temp$year<-wtr_yr(dataDaily$date, 10)


# daily climatology
dailyClimo<-dataDaily %>% group_by(doy) %>%
  summarise(meanT=mean(avgT),
            meanP=mean(precip))

# cumulative totals using dplyr
cumPrecip <- dataDaily %>% 
  group_by(year, doy) %>% # still doesn't quite work adjYear kicks off before adjDOY
  summarise(value = sum(precip, na.rm = T)) %>%
  mutate(csum = cumsum(value))
dataDaily$cumPrecip<-cumPrecip$csum

# calc GDDs
baseT<-50
dataDaily$avgT_base <- unlist(lapply(dataDaily$avgT, function(x) ifelse(x>=baseT, x-baseT, 0)))

#tempDataWide$tmean[tempDataWide$tmean < baseT] <- 0
cumGDD <- dataDaily %>% 
  dplyr::group_by(year, doy) %>% # still doesn't quite work adjYear kicks off before adjDOY
  dplyr::summarise(value = sum(avgT_base)) %>%
  dplyr::mutate(csum = cumsum(value))
dataDaily$GDD<-cumGDD$csum


