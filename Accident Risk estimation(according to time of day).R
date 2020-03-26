library(dplyr)
library(tibble)
library(ggplot2)


dd <- read.csv("CA.csv")
#Set the interval, like how many hours interval 
intervalset = 3


#Cleaning
dd[, c(6,7)] <- sapply(dd[, c(6,7)], as.character)
dd2 <- dd
dd2 <- add_column(dd2, interval = 0, .after = 6)
dd2 <- mutate(dd2, interval = as.numeric(substring(Start_Time,12,13)) %/% intervalset)
dd3 <- mutate(dd2, interval = interval * 100 * intervalset)
dd4 <- mutate(dd3, interval = sprintf("%04d", interval))

dd5<- ggplot( #Still need edit to make the things fatter
  data= dd4, aes(x = interval)) + geom_bar(stat = "count",width = 0.5, color = "blue", fill = "blue") + ggtitle("Number of accidents in a time period")
dd5
