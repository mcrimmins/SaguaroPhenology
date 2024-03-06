# analyze Peachy saguaro phenoloy data with PRISM climate time series
# MAC 03/04/24
# need PRISM climate data file generated with get getPRISMclimate.R

library(dplyr)
#library(lubridate)
library(tidyr)
library(ggplot2)

# load special functions
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

# load datafile from getPRISMclimate.R
load("PRISM_dailyData.RData")

##### calculate cumulative metrics from specified start month
# set start month
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
rm(cumClim)
#####

##### load and process bloom counts dataset
# read in data
blooms <- readxl::read_excel("full_bloom_count_dataset.xlsx", col_types = c("date", "numeric"))
# add in date fields for sorting/pivoting
blooms$month<-as.numeric(format(blooms$Date,"%m"))
blooms$day<-as.numeric(format(blooms$Date,"%d"))
blooms$year<-as.numeric(format(blooms$Date,"%Y"))
blooms$doy<-as.numeric(format(blooms$Date,"%j"))
# add in dummy date
blooms$dummyDate<-as.Date(paste0("2020-",blooms$month,"-",blooms$day))

# get avg/max blooms/year
bloomsYear<-blooms %>% group_by(year) %>%
  summarise(avgBlooms = mean(Count_blooms, na.rm = TRUE),
            maxBlooms = max(Count_blooms, na.rm = TRUE))

# find start/end/max dates by year
firstBloomDate<-blooms %>%
  group_by(year) %>%
  slice(match(TRUE, Count_blooms > 1)) %>%
  ungroup
maxBloomDate<-blooms %>%
  group_by(year) %>%
  slice(which.max(Count_blooms)) %>%
  ungroup
lastBloomDate<-blooms %>%
  group_by(year) %>%
  filter(Count_blooms>1) %>%
  filter(Date==max(Date)) %>%
  select(Date)
lastBloomDate$doy<-as.numeric(format(lastBloomDate$Date,"%j"))
# phenoDates<-cbind.data.frame(lastBloomDate$year,firstBloomDate$dummyDate,
#                              maxBloomDate$dummyDate,lastBloomDate$Date)
phenoDates<-cbind.data.frame(lastBloomDate$year,firstBloomDate$doy,
                             maxBloomDate$doy,lastBloomDate$doy,
                             bloomsYear$avgBlooms, bloomsYear$maxBlooms)
colnames(phenoDates)<-c("year","firstBloom","maxBloom","lastBloom",
                        "avgBlooms","maxBlooms")

# extract climate and combine with phenoDates
temp<-subset(dataDaily, adjYear>=2004 & adjYear<=2023)
# get Oct 1st
Oct1<-subset(temp, adjDOY==93)
# get Jan 1st
Jan1<-subset(temp, adjDOY==185)
# get June 30th
Jun30<-subset(temp, adjDOY==365)

# get freeze days
frzDays<-temp %>% group_by(adjYear) %>%
  summarize(count=sum(minT<=0, na.rm = TRUE),
            minT=min(minT, na.rm = TRUE))

# combine dfs
phenoClim<-cbind.data.frame(phenoDates,Oct1[,c("cumPrecip","gdd")],
                            Jan1[,c("cumPrecip","gdd")],
                            Jun30[,c("cumPrecip","gdd")],
                            frzDays[,c("count","minT")])
colnames(phenoClim)[7:ncol(phenoClim)]<-c("Oct1pcp","Oct1gdd",
                                          "Jan1pcp","Jan1gdd",
                                          "Jun30pcp","Jun30gdd",
                                          "frzDays","minT")

# fall and winter precip
phenoClim$OctJunPcp<-phenoClim$Jun30pcp-phenoClim$Oct1pcp
phenoClim$JanJunPcp<-phenoClim$Jun30pcp-phenoClim$Jan1pcp
# winter gdd 
phenoClim$JanJunGDD<-phenoClim$Jun30gdd-phenoClim$Jan1gdd

# corr matrix
library("PerformanceAnalytics")
temp <- phenoClim[, c(2:17)]
chart.Correlation(temp, histogram=TRUE, pch=19)

# stepwise linear regression
library(MASS)
# Fit the full model 
full.model <- lm(maxBlooms ~., data = phenoClim[,c(6,7:17)])
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)

# lm 
full.model <- lm(maxBlooms ~minT+Jun30pcp+Jun30gdd, data = phenoClim)
summary(full.model)


