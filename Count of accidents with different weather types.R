library(dplyr)
library(tibble)
library(ggplot2)

#This also includes the condensed weather types
#Reading data in
dd <- read.csv("CA.csv", stringsAsFactors = FALSE, na.strings = c("", NA))

#Manual sorting 
w1 = "Clear"
w2 = "Cloudy"
w3 = "haze/fog"
w4 = "drizzle"
w5 = "Rain"
w6 = "Snow"
w7 = "Hail"
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

#We have to decide if we want to delete the NAs
weathertypes = unique(dd$Weather_Condition)
indexes <- match(dd$Weather_Condition,weathertypes)
newweatherlist <- specificweatherlist[indexes]
dd6 <- add_column(dd, Weather_Type =newweatherlist, .after = "Weather_Condition")
dd7 <- ggplot(dd6, aes(x=Weather_Type)) + geom_bar() + theme(axis.text.x=element_text(angle=90))+ ggtitle("Count of accidents with different weather types")
dd7