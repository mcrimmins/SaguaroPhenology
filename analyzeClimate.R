#####
# code to download daily PRISM climate data from single location
# and process into cumulative precip and gdd
# MAC 03/01/24

# required libraries
library(RCurl)
library(jsonlite)
library(dplyr)
#library(lubridate)
library(tidyr)
library(ggplot2)

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

save(dataDaily,lat,lon, file = "PRISM_dailyData.RData")
######



######### ANALYZE DATA -------
##### load saved data
load("PRISM_dailyData.RData")

# create non-standard date like water year doy/year
startMo<-7
dataDaily$adjYear<-adj_yr(dataDaily$date, startMo)
dataDaily$adjDOY<-adj_doy(dataDaily$date, startMo)

# calc day avgT with base temp for gdd
baseT<-10
dataDaily$avgT_base <- unlist(lapply(dataDaily$avgT, function(x) ifelse(x>=baseT, x-baseT, 0)))

# cumulative doy climate - precip and temp
cumClim <- dataDaily %>% 
  group_by(adjYear, adjDOY) %>% # still doesn't quite work adjYear kicks off before adjDOY
  summarise(precip = sum(precip, na.rm = T),
            temp = sum(avgT_base, na.rm = T)) %>%
  mutate(psum = cumsum(precip),
         tsum = cumsum(temp))
dataDaily$cumPrecip<-cumClim$psum
dataDaily$gdd<-cumClim$tsum
#####

##### analyze data ----

#####
# daily climatology
dailyClimo<-dataDaily %>% group_by(adjDOY) %>%
  summarise(meanT=mean(avgT),
            meanP=mean(precip))

# add in dummy date
dailyClimo$date<-seq.Date(as.Date(paste0("2019-",startMo,"-01")),by="day",length.out = nrow(dailyClimo))

# plot climograph
ggplot(data = dailyClimo,
       mapping = aes(x = date, y = meanT, group = 1)) + 
  geom_bar(mapping = aes(y = meanP*5), stat = "identity") + 
  geom_line() + 
  scale_y_continuous(
    "temperature, C", 
    sec.axis = sec_axis(~ . / 5, name = "precipitation, mm")
  ) +
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 month",
               date_labels = "%b")+
  theme_bw()+
  ggtitle(paste0("Daily Average Temperature and Precipitation (1981-2023, ",lat,", ",lon,")"))
#####


##### plot cumulative precip
# reformat dataframe
temp<- dataDaily[,c("adjDOY","adjYear","cumPrecip")] %>%  pivot_wider(names_from = "adjYear",values_from = "cumPrecip")
temp <- temp[order(temp$adjDOY),]
temp$date<-seq.Date(as.Date(paste0("2019-",startMo,"-01")),by="day",length.out = nrow(temp))

temp<-temp %>% pivot_longer(cols=c(2:(ncol(temp)-1)),
                    names_to='adjYear',
                    values_to='precip')
temp$adjYear<-as.numeric(temp$adjYear)

# make plot
trimYear<-2003
ggplot(subset(temp,adjYear>trimYear), aes(date,precip, color=as.factor(adjYear)))+
  geom_line()+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 month",
               date_labels = "%b")+
  theme_bw()+
  ggtitle(paste0("Cumulative Daily Precipitation (mm) [",trimYear+1,"-2023, ",lat,", ",lon,"]"))
#####

##### plot gdd
# reformat dataframe
temp<- dataDaily[,c("adjDOY","adjYear","gdd")] %>%  pivot_wider(names_from = "adjYear",values_from = "gdd")
temp <- temp[order(temp$adjDOY),]
temp$date<-seq.Date(as.Date(paste0("2019-",startMo,"-01")),by="day",length.out = nrow(temp))

temp<-temp %>% pivot_longer(cols=c(2:(ncol(temp)-1)),
                            names_to='adjYear',
                            values_to='gdd')
temp$adjYear<-as.numeric(temp$adjYear)

# plot gdd 
ggplot(subset(temp,adjYear>2003), aes(date,gdd, color=as.factor(adjYear)))+
  geom_line()+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 month",
               date_labels = "%b")+
  theme_bw()+
  ggtitle(paste0("Growing degree days (C, baseT 10C) [",trimYear+1,"-2023, ",lat,", ",lon,"]"))
#####

##### plot daily data
# make combined plot
ggplot(data = dataDaily) + 
  geom_bar(mapping = aes(x = date,y = precip/1), stat = "identity", color="forestgreen") + 
  geom_line(aes(x = date, y = minT, group = 1), color="blue") +
  geom_line(aes(x = date, y = maxT, group = 1), color="red") + 
  scale_y_continuous(
    "temperature, C", 
    sec.axis = sec_axis(~ . *1, name = "precipitation, mm")
  ) +
  scale_x_date(date_breaks = "2 year", date_minor_breaks = "1 year",
               date_labels = "%Y")+
  theme_bw()+
  ggtitle(paste0("Daily Average Temperature and Precipitation (1981-2023, ",lat,", ",lon,")"))
#####
#separate plots
library(cowplot)
p1<-ggplot(data = subset(dataDaily,year>2003)) + 
  geom_line(aes(x = date, y = minT, group = 1), color="blue") +
  geom_line(aes(x = date, y = maxT, group = 1), color="red") + 
  scale_y_continuous(
    "temperature, C") +
  scale_x_date(date_breaks = "2 year", date_minor_breaks = "1 year",
               date_labels = "%Y")+
  theme_bw()+
  ggtitle(paste0("Daily Average Temperature and Precipitation (1981-2023, ",lat,", ",lon,")"))
p2<-ggplot(data = subset(dataDaily,year>2003)) + 
  geom_bar(mapping = aes(x = date,y = precip/1), stat = "identity", color="forestgreen") + 
  scale_y_continuous(
    "precipitation, mm", 
  ) +
  scale_x_date(date_breaks = "2 year", date_minor_breaks = "1 year",
               date_labels = "%Y")+
  theme_bw()
plot_grid(p1,p2,labels="",ncol = 1)
#####

#####
# annual precip and temps
temp<- dataDaily %>% group_by(adjYear) %>%
            summarise(precip=sum(precip, na.rm = TRUE),
                      meanT=mean(avgT, na.rm = TRUE))

p1<-ggplot(subset(temp, adjYear>2003 & adjYear<2024), aes(adjYear,precip))+
  geom_bar(stat="identity")+
  geom_hline(yintercept = mean(temp$precip))+
  theme_bw()+
  ggtitle(paste0("Annual Average Temperature and Precipitation (2004-2023, ",lat,", ",lon,")"))

p2<-ggplot(subset(temp, adjYear>2003 & adjYear<2024), aes(adjYear,meanT))+
  geom_bar(stat="identity")+
  geom_hline(yintercept = mean(temp$meanT))+
  coord_cartesian(ylim=c(15,22.5))+
  theme_bw()

plot_grid(p1,p2,labels="",ncol = 1)

#####


#####
# precip anomalies
temp<- dataDaily[,c("adjDOY","adjYear","cumPrecip")] %>%  pivot_wider(names_from = "adjYear",values_from = "cumPrecip")
temp <- temp[order(temp$adjDOY),]

temp<-temp %>% 
  rowwise() %>% 
  mutate(med = median(c_across(2:ncol(temp)), na.rm = TRUE))
temp <- temp %>% mutate(across(2:(ncol(temp)-1), ~ .x - med))

temp$date<-seq.Date(as.Date(paste0("2019-",startMo,"-01")),by="day",length.out = nrow(temp))

temp<-temp %>% pivot_longer(cols=c(2:(ncol(temp)-2)),
                            names_to='adjYear',
                            values_to='precip')
temp$adjYear<-as.numeric(temp$adjYear)

# make plot
trimYear<-2003
p<-ggplot(subset(temp,adjYear>trimYear), aes(date,precip, color=as.factor(adjYear)))+
  geom_line()+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 month",
               date_labels = "%b")+
  theme_bw()+
  geom_hline(yintercept = 0)+
  ggtitle(paste0("Cumulative Daily Precipitation Anomaly (mm) [",trimYear+1,"-2023, ",lat,", ",lon,"]"))
plotly::ggplotly(p)
#####

#####
# temp anomalies
temp<- dataDaily[,c("adjDOY","adjYear","gdd")] %>%  pivot_wider(names_from = "adjYear",values_from = "gdd")
temp <- temp[order(temp$adjDOY),]

temp<-temp %>% 
  rowwise() %>% 
  mutate(med = median(c_across(2:ncol(temp)), na.rm = TRUE))
temp <- temp %>% mutate(across(2:(ncol(temp)-1), ~ .x - med))

temp$date<-seq.Date(as.Date(paste0("2019-",startMo,"-01")),by="day",length.out = nrow(temp))

temp<-temp %>% pivot_longer(cols=c(2:(ncol(temp)-2)),
                            names_to='adjYear',
                            values_to='gdd')
temp$adjYear<-as.numeric(temp$adjYear)

# make plot
trimYear<-2003
p<-ggplot(subset(temp,adjYear>trimYear), aes(date,gdd, color=as.factor(adjYear)))+
  geom_line()+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 month",
               date_labels = "%b")+
  theme_bw()+
  geom_hline(yintercept = 0)+
  ggtitle(paste0("GDD anomaly (mm) [",trimYear+1,"-2023, ",lat,", ",lon,"]"))
plotly::ggplotly(p)
#####




