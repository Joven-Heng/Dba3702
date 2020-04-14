library(dplyr)
library(XML)
library(RCurl)
library(lubridate)
library(readxl)

california <- read.csv("california.csv")
holidays <- read_excel("PublicholidayCA.xlsx")
holidays$Date <- as.Date(holidays$Date)

url <- "https://wiki.openstreetmap.org/wiki/TMC/Event_Code_List"
urldata <- getURL(url)

tmc.tables <- readHTMLTable(urldata)
tmc.table <- tmc.tables[[1]]
tmc <- as.data.frame(tmc.table[,1:2],stringsAsFactors=FALSE)
tmc <- tmc[-1,]

california$TMC <- as.factor(california$TMC)

newcalifornia <- left_join(california,tmc,by=c("TMC"="V1"))
newcalifornia <- newcalifornia[,-c(3,48,49,50)]
colnames(newcalifornia)[47] <- "TMC Code"
newcalifornia$date <- date(newcalifornia$Start_Time)
newcalifornia$day <- weekdays(newcalifornia$date)
newcalifornia$weekend <- ifelse(newcalifornia$day=="Saturday" | newcalifornia$day=="Sunday",
                                1,0)
newcalifornia$ph <- ifelse(newcalifornia$date %in% holidays$Date,1,0)

write.csv(newcalifornia,"CA.csv")
