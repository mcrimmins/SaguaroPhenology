# read in and analyze Peachy saguaro phenology data
# MAC 03/03/24

library(ggplot2)
library(dplyr)
library(tidyr)

# read in data
blooms <- readxl::read_excel("full_bloom_count_dataset.xlsx", col_types = c("date", "numeric"))

# add in date fields for sorting/pivoting
blooms$month<-as.numeric(format(blooms$Date,"%m"))
blooms$day<-as.numeric(format(blooms$Date,"%d"))
blooms$year<-as.numeric(format(blooms$Date,"%Y"))
blooms$doy<-as.numeric(format(blooms$Date,"%j"))
# add in dummy date
blooms$dummyDate<-as.Date(paste0("2020-",blooms$month,"-",blooms$day))

# plot data
ggplot(blooms, aes(Date,Count_blooms))+
  geom_line()

ggplot(blooms,aes(dummyDate,Count_blooms))+
  geom_line()+
  facet_wrap(.~year)

# heat map of data
ggplot(blooms, aes(x = dummyDate, y = year, fill = Count_blooms)) +
  geom_tile()+
  scale_fill_viridis_c()+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
               date_labels = "%b-%d",
               limits = as.Date(c('2020-03-01','2020-08-01')))

# doy metrics
temp<-blooms %>% group_by(doy) %>%
  summarise(median = median(Count_blooms),
            q25 = quantile(Count_blooms, probs = 0.25),
            q75 = quantile(Count_blooms, probs = 0.75))
temp$date<-as.Date(paste0("2020-",temp$doy),format = "%Y-%j")

temp<-temp %>% pivot_longer(cols=c(2:4),
                            names_to='vars',
                            values_to='value')

ggplot(temp, aes(date,value,color=vars))+
  geom_line()+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
               date_labels = "%b-%d",
               limits = as.Date(c('2020-03-01','2020-08-01')))

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
