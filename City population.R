library(dplyr)
library(rvest)
library(jsonlite)
library(tidyr)
library(XML)
library(RCurl)
library(curl)
library(ggplot2)



a <- read.csv("US_Accidents_Dec19.csv")
b <- read.csv("california.csv")


set.seed(123)
c<- sample_n(b, 50000)

url1 <- "https://wiki.openstreetmap.org/wiki/TMC/Event_Code_List"
url3<- curl(url1)
urldata<- readLines(url3)
mainpage <- readHTMLTable(urldata)

tmc <-mainpage[["NULL"]]
tmc <- tmc[c(1,2)]
tmc <- tmc %>% rename(tmc = V1, tmc_description = V2)
tmc <- tmc[-1,]
d <- merge(c,tmc, by = "tmc")

#Sorting out the cc-est2018
h <- read.csv("cc-est2018-alldata-06.csv", stringsAsFactors = F)
m <- h%>% filter(YEAR == 11)
n <- m[c(1,2,3,4,5,6,7,8,9,10)]

str(n)

for(i in seq(1,nrow(n))){
  z <- n[i,5]
  zz <- substr(z, 1 , nchar(z)-7)
  n[i,5]<-  zz
}


write.csv(n, "california_county_population.csv")

dd <- read.csv("CA.csv")

set.seed(123)
ee <- sample_n(dd,10000)
TMC<- ggplot( #Still need edit to make the things fatter
  data= dd, aes(x = TMC)) + geom_bar(stat = "count",width = 0.5, color = "blue", fill = "blue") + ggtitle("TMC")
)

Severity<- ggplot(
  data= dd, aes(x = Severity)) + geom_bar(stat = "count",width = 0.5, color = "blue", fill = "blue") + ggtitle("Severity")
)

