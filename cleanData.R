library(dplyr)
library(XML)
library(rvest)
library(RCurl)
library(readxl)
library(lubridate)

usdata <- read.csv("US_Accidents_Dec19.csv")
california <- usdata %>% filter(State == "CA")

url <- "https://wiki.openstreetmap.org/wiki/TMC/Event_Code_List"
urldata <- getURL(url)
tmc.tables <- readHTMLTable(urldata)
tmc.table <- tmc.tables[[1]]
tmc <- as.data.frame(tmc.table[,1:2],stringsAsFactors=FALSE)
tmc <- tmc[-1,]
california$TMC <- as.factor(california$TMC)
newcalifornia <- left_join(california,tmc,by=c("TMC"="V1"))
colnames(newcalifornia)[50] <- "TMC Code"

holidays <- read_excel("PublicholidayCA.xlsx")
holidays$Date <- as.Date(holidays$Date)

newcalifornia$date <- date(newcalifornia$Start_Time)
newcalifornia$day <- weekdays(newcalifornia$date)
newcalifornia$weekend <- ifelse(newcalifornia$day=="Saturday" | newcalifornia$day=="Sunday",1,0)
newcalifornia$ph <- ifelse(newcalifornia$date %in% holidays$Date,1,0)
newcalifornia <- newcalifornia %>% mutate(hour = substr(Start_Time,12,13), month = substr(date,6,7), year = substr(date,1,4))
newcalifornia$day <- factor(newcalifornia$day,levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
newcalifornia$typeDay <- ifelse(newcalifornia$ph == 1, "Public Holiday", ifelse(newcalifornia$weekend==1, "Weekend", "Weekday"))
newcalifornia$typeDay <- factor(newcalifornia$typeDay, levels = c("Weekday", "Weekend", "Public Holiday"))

w1 <- "Clear"
w2 <- "Cloudy"
w3 <- "Haze/Fog"
w4 <- "Drizzle"
w5 <- "Rain"
w6 <- "Snow"
w7 <- "Hail"
specificweatherlist <- c(w1,w2,w3,
                         w2,w2,w2, #6
                         w4,NA,w1,#9
                         w3,w3,w3,#12
                         w5,w4,w5,#15
                         w3,w6,w6,#18
                         w3,w3,w5,#21
                         w4,w5,w1,#24
                         w2,w3,w3,
                         w4,w3,w3,#30
                         w1,w5,w1,
                         w4,w7,w4,
                         w5,w5,w3,
                         w5,w2,w2,#42
                         w5,w5,w5,
                         w5,w5,w6,#48
                         w6,w2,w4,#51
                         w5,w1,w3,
                         w3,w1,w6,#57
                         w6,w5,w3,#60
                         w3,w5,w3,
                         w5,w6,w1,
                         w3)
weathertypes = unique(newcalifornia$Weather_Condition)
indexes <- match(newcalifornia$Weather_Condition, weathertypes)
newweatherlist <- specificweatherlist[indexes]
newcalifornia <- add_column(newcalifornia, Weather_Type = newweatherlist, .after = "Weather_Condition")

write.csv(newcalifornia,"CA.csv")
