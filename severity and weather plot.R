library(dplyr)
library(tibble)
library(ggplot2)

#This also includes the condensed weather types
#Reading data in
ca_data <- read.csv("CA.csv", stringsAsFactors = FALSE, na.strings = c("", NA))

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
weathertypes = unique(ca_data$Weather_Condition)
indexes <- match(ca_data$Weather_Condition,weathertypes)
newweatherlist <- specificweatherlist[indexes]
ca_data <- add_column(ca_data, Weather_Type =newweatherlist, .after = "Weather_Condition")

ggplot(ca_data, aes(x=Weather_Type, fill=as.character(Severity))) + geom_bar(position = "dodge")
       
       
