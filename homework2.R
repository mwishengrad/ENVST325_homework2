#install packages once in project
install.packages(c("dplyr", "lubridate"))
#load packages each session
library(dplyr)
library(lubridate)

#read in uploaded data
streamH <- read.csv("/cloud/project/activity02/stream_gauge.csv")
siteInfo <- read.csv("/cloud/project/activity02/site_info.csv")
siteInfo$names

#parse data date/time
exampleDate <- c("2021-01-10 05:23:30")
ymd_hms(exampleDate)
ymd_hms(exampleDate, tz="America/New_York") 
ymd_hms(exampleDate, tz="EST")
streamH$dateF <- ymd_hm(streamH$datetime, tz="America/New_York")

#extract info from dates
year(streamH$dateF)
leap_year(streamH$dateF)
streamH$dayOfYear <- yday(streamH$dateF)
streamH$decimalDate <- decimal_date(streamH$dateF)

#subset stream data
peaceH <- streamH %>% 
  filter(siteID == 2295637)

#plot data - date v. height, lines and dots
plot(peaceH$dateF, peaceH$gheight.ft, type="b", pch=19, xlab="Date",
     ylab = "Stage height (ft)")

#preview data table
str(streamH)

siteInfo

#join siteInfo and streamH into new data frame floods
floods <- full_join(streamH, # left table
                    siteInfo, # right table
                    by="siteID") # common identifier

head(floods)

#aggregating data
mean(floods$gheight.ft)

#pipes sequence data 
height.ave <- floods %>% # data frame with pipe
  group_by(names) %>% # group data frame by unique names
  summarise(mean.height = mean(gheight.ft)) # next summarize using mean

height.ave

height.day <- floods %>% 
  group_by(names, dayOfYear) %>% # group by unique names and doy
  summarise(mean.height = mean(gheight.ft), max.height = max(gheight.ft)) # next summarize using mean and max

height.day

max.cat <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= major.ft)

max.cat

max.cat <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= major.ft) %>%
  summarise(n.major = n()) # summarise number of observations for group

max.cat

max.cat <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= major.ft) %>%
  summarise(n.major = ((n()*15)/60)/24) # find out how many days the 15 minute periods add up to

max.cat

#In class Prompts

#Prompt 1
floods #full join from tutorial

#testing other types of joins to see if change outcome
floodsL <- left_join(streamH, siteInfo, by="siteID")
floodsR <- right_join(streamH, siteInfo, by="siteID")
floodsI <- inner_join(streamH, siteInfo, by="siteID")

#For this data set because nothing is not included/missing 
#from any row/column the type of join used does NOT matter.

#Prompt 2

floods$parsedDate <- date(floods$dateF)

#Prompt 3
firstFlood <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= major.ft) %>%
  summarise(minDate = min(parsedDate))
  
firstFlood


#HOMEWORK


#Question 1

# Stream stage plot for PEACE RIVER AT US 17 AT ZOLFO SPRINGS from earlier section
peaceH <- streamH %>% 
  filter(siteID == 2295637)
plot(peaceH$dateF, peaceH$gheight.ft, type="b", pch=19, xlab="Date",
     ylab = "Stage height (ft)")

#Stream stage plot for FISHEATING CREEK AT PALMDALE
fishCreek <- streamH %>%
  filter(siteID == 2256500)
plot(fishCreek$dateF, fishCreek$gheight.ft, type="b", pch=19, xlab="Date",
     ylab = "Stage height (ft)")

#Stream stage plot for SANTA FE RIVER NEAR FORT WHITE
santaFe <- streamH %>%
  filter(siteID == 2322500)
plot(santaFe$dateF, santaFe$gheight.ft, type="b", pch=19, xlab="Date",
     ylab = "Stage height (ft)")

#WITHLACOOCHEE RIVER AT US 301 AT TRILBY
trilby <-streamH %>%
    filter(siteID == 2312000)
plot(trilby$dateF, trilby$gheight.ft, type="b", pch=19, xlab="Date",
     ylab = "Stage height (ft)")


#Question 2

