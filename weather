# We-love-dai
we love yaodai

ca_data <- read.csv("CA.csv", stringsAsFactors = FALSE, na.strings = c("",NA))
ca_data <- ca_data[ca_data$Weather_Condition != "NA", ]
ggplot(ca_data, aes(x=Weather_Condition)) + geom_bar() + theme(axis.text.x=element_text(angle=90))
