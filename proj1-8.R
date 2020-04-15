library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(leaflet)
library(htmlwidgets)
library(tibble)
library(leaflet.extras)
library(scales)
library(ggthemes)
library(ggrepel)

# only load once
#CA <- read.csv("CA.csv")
CA <- CA %>% mutate(hour = substr(Start_Time,12,13), month = substr(date,6,7), year = substr(date,1,4)) 

# data consolidation
data1 <- CA %>% group_by(County, year) %>% summarise(count=n())
data1 <- spread(data1, year, count)
data1 <- data1 %>% mutate(`2016-2019` = `2016`+`2017`+`2018`+`2019`)
data1<- gather(data1,`2016`,`2017`,`2018`,`2019`,`2016-2019`, key = "year", value = "count")

cal.county.pop <- read.csv("california_county_population.csv")
county.pop <- cal.county.pop %>% group_by(CTYNAME) %>% summarise(pop = sum(TOT_POP))
data1 <- left_join(data1,county.pop, by=c("County"="CTYNAME"))
data1 <- data1 %>% mutate(pop.rate = count*1000/pop)
#data1 <- data1[order(data1$pop.rate, decreasing = T),]

county.veh <- read.csv("estimated vehicles by county.csv")
county.veh <- county.veh[-59,c(-7:-11)]
county.veh$COUNTIES <- county.pop$CTYNAME
county.veh <- as.data.frame(lapply(county.veh, function(y) gsub(",", "", y))) 
county.veh$TOTAL <- as.numeric(as.character(county.veh$TOTAL))

data1 <- left_join(data1,county.veh[,c("COUNTIES","TOTAL")], by=c("County"="COUNTIES"))
data1 <- data1 %>% mutate(veh.rate = count*1000/TOTAL)
colnames(data1) <- c("County", "year", "no.accidents", "total.pop", "pop.rate", "total.veh", "veh.rate")


# clean weather types
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
weathertypes = unique(CA$Weather_Condition)
indexes <- match(CA$Weather_Condition,weathertypes)
newweatherlist <- specificweatherlist[indexes]
CA <- add_column(CA, Weather_Type = newweatherlist, .after = "Weather_Condition")

# leaflet
colors <- c("green", "yellow", "orange", "red")

severity <- c("1", "2", "3", "4")
factPal <- colorFactor(c("green", "yellow", "orange", "red"), severity, ordered = T)
m <- leaflet() %>% addTiles()
n <- leaflet() %>% addTiles()

#data with demographics
data2 <- left_join(cal.county.pop, data1, by = c("CTYNAME" = "County"))
data2$AGEGRP <- as.numeric(data2$AGEGRP)
data2$AGESTATUS <- cut(data2$AGEGRP, c(0,4,9,13,18), labels = c("Children","Young adults", "Middle Age", "Elderly"))


# app layout
header <- dashboardHeader(title = "US Accidents")

sidebar <- dashboardSidebar(
  sidebarMenu(
    # Tab 1 
    menuItem(tabName = "overview", "Overview", icon = icon("globe-americas")),
    
    # Tab 2
    menuItem(tabName = "county", "County", icon = icon("map-marker")),
    
    selectInput(inputId = "county", label = "County", choices = unique(CA$County)),
    
    selectInput(inputId = "year", label = "Year", choices = c("2016", "2017","2018","2019", "2016-2019"))
    
  )
)

body <- dashboardBody(
  
  tabItems(
    # Tab 1 content
    tabItem(tabName = "overview",
            
            fluidRow(
              valueBox("California", "Traffic Accidents (2016-2019)", icon = icon("map-marker"), width = 12)
            ),
            
            fluidRow(
              box(plotOutput("plot1"), width=12)
            ),
            
            fluidRow(
              box(plotOutput("plot2"), width=8),
              box(plotOutput("plot3"), width=4)
            )
    ),
    
    
    #Tab 2 content
    tabItem(tabName="county",
            
            fluidRow(valueBoxOutput("county", width=12)),
            
            fluidRow(
              infoBoxOutput("no.accidents"),
              infoBoxOutput("population"),
              infoBoxOutput("vehicles")
            ),
            
            fluidRow(
              infoBoxOutput("popaccidentrate", width=6),
              infoBoxOutput("vehaccidentrate", width=6)
            ),
            
            fluidRow(
              tabBox(
                title = "Locations of Traffic Accidents",
                id = "leaflet",
                width = 12,
                height = 500,
                tabPanel("Cluster Map", leafletOutput("severitymap" , height = 500)),
                tabPanel("Heat Map", leafletOutput("heatmap", height = 500)),
                tabPanel("Severity Map", leafletOutput("severitymap2", height = 500))
              )
            ),
            
            fluidRow(
              br(),
              br(),
              br(),
              tabBox(title = "Weather Conditions",
                     id = "weather",
                     tabPanel("Absolute", plotOutput("weather_absolute")),
                     tabPanel("Relative", plotOutput("weather_relative"))),
              box(plotOutput("road_safety"), title = "Road Conditions")
            ),
            
            fluidRow(
              tabBox(
                title = "Times Series of Accidents",
                id = "time",
                width = 12,
                height = 500,
                tabPanel("Hour", plotOutput("hour")),
                tabPanel("Day", plotOutput("day")),
                tabPanel("Month", plotOutput("month"))
              )
            ),
            
            #demographics
            fluidRow(
              box(plotOutput("age_groups"), title = "Age groups"),
              box(plotOutput("gender"), title = "Gender demographics")
            )
            
    )
  )
)


ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session) {
  
  #overall bar graph by county
  output$plot1 <- renderPlot({
    ggplot(data1[data1$year == input$year,]) +
      geom_bar(aes(x=reorder(County,-pop.rate), y=no.accidents), fill="black", alpha=0.5, stat="identity") +
      geom_line(aes(x=reorder(County,-pop.rate), y=pop.rate*5000, color="Accidents per 1000 people"), group=1, stat="identity") +
      geom_line(aes(x=reorder(County,-pop.rate), y=veh.rate*5000, color="Accidents per 1000 vehicles"), group=1, stat="identity") +
      scale_y_continuous(sec.axis=sec_axis(~./50000, name="Accident Rate", labels=function(x) {paste0(x,"%")})) +
      labs(title = paste("Accident Frequency By County", input$year), x="County", y="Number of Accidents") +
      scale_colour_manual(values=c("Accidents per 1000 people"="red", "Accidents per 1000 vehicles"="blue"), 
                          labels = c("Accidents per 1000 people", "Accidents per 1000 vehicles"), name="Legend:") +
      theme_economist_white() +
      theme(axis.text.x=element_text(angle=90), axis.title.y.left = element_text(size = 12),axis.title.y.right = element_text(size = 12),plot.title = element_text(size = 25, hjust = 0.5), 
            legend.text = element_text(size = 16), legend.title = element_text(size = 16))
  })
  
  # bar chart for bumps,etc
  output$plot2 <- renderPlot({
    if (input$year != "2016-2019"){
      data <- CA[CA$year == input$year,]
    }else{
      data <- CA
    }
    ggplot(data) + 
      geom_bar(aes(x="Bump",y=..count.., fill=Bump), width=.3) + 
      geom_bar(aes(x="Stop",y=..count.., fill=Stop),width =.3) +
      geom_bar(aes(x="Give_Way",y=..count.., fill=Give_Way), width=.3) + 
      geom_bar(aes(x="Traffic_Signal",y=..count.., fill=Traffic_Signal),width =.3) +
      geom_bar(aes(x="Traffic_Calming",y=..count.., fill=Traffic_Calming),width =.3) +
      theme_economist_white() +
      theme(axis.text.x = element_text(angle = 90), axis.title.x = element_blank(), plot.title = element_text(size = 20, hjust = 0.5)) +
      labs(title = "Road Conditions") +
      scale_fill_discrete(name = "", labels = c("Present", "Not Present"))
  })
  
  # severity pie chart
  output$plot3 <- renderPlot({
    # severity percentage
    sev <- CA %>% group_by(Severity, year) %>% summarise(count = n())
    sev <- spread(sev, year, count)
    sev <- sev %>% mutate(`2016-2019` = `2016`+`2017`+`2018`+`2019`)
    sev <- gather(sev,`2016`,`2017`,`2018`,`2019`,`2016-2019`, key = "year", value = "count")
    by.year <- sev %>% group_by(year) %>% summarise(sum(count))
    sev <- left_join(sev, by.year, by="year")
    sev$per <- sev$count/sev$`sum(count)`
    sev$label <- percent(sev$per, accuracy = 0.1)
    
    data <- sev[sev$year == input$year,]
    ggplot(data) +
      geom_bar(aes(x="" ,y=per, fill=as.factor(Severity)), stat="identity") +
      coord_polar("y", start=0) +
      labs(title = "Severity of Accidents by Percentage", fill = "Severity Level") +
      theme_economist_white() +
      geom_text_repel(aes(x=1, y = cumsum(per) - per/2), label=data$label, nudge_x = 1) +
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            panel.grid.major=element_blank())
  })
  
  
  # county
  output$county <- renderValueBox({
    valueBox(input$county, paste("Accidents in", input$year),  icon = icon("map-marker"))
  })
  
  # no.accidents output
  output$no.accidents <- renderInfoBox({
    infoBox("Accidents", data1[data1$County==input$county & data1$year == input$year,"no.accidents"], icon = icon("users"))
  })
  
  # population output
  output$population <- renderInfoBox({
    infoBox("Population", data1[data1$County==input$county & data1$year == input$year,"total.pop"], icon = icon("users"))
  })
  
  # vehicle output
  output$vehicles <- renderInfoBox({
    infoBox("Registered Vehicles", data1[data1$County==input$county & data1$year == input$year,"total.veh"], icon = icon("users"))
  })
  
  # popaccidentrate output
  output$popaccidentrate <- renderInfoBox({
    infoBox("Accident Rate", paste0(floor(data1[data1$County==input$county & data1$year == input$year,"pop.rate"]),"/1000 people") )
  })
  
  # vehaccidentrate output
  output$vehaccidentrate <- renderInfoBox({
    infoBox("Vehicle Accident Rate", paste0(floor(data1[data1$County==input$county & data1$year == input$year,"veh.rate"]),"/1000 vehicles"))
  })
  
  # leaflet
  output$severitymap <- renderLeaflet({
    for (i in 1:4){
      if (input$year != "2016-2019"){
        data <- CA[CA$year == input$year,]
      }else{
        data <- CA
      }
      data <- data[data$County == input$county,]
      data <- data[data$Severity  == severity[i],]
      m <- m %>% addCircleMarkers(data = data, 
                                  lng = ~Start_Lng, 
                                  lat = ~Start_Lat, 
                                  color = colors[i],
                                  fillOpacity = 1,
                                  group = severity[i],
                                  radius = 0.5,
                                  clusterOptions = markerClusterOptions())
    }
    m <- m %>% addLayersControl(overlayGroups = severity)
    m
  })
  
  # heatmap
  output$heatmap <- renderLeaflet({
    if (input$year != "2016-2019"){
      data <- CA[CA$year == input$year,]
    }else{
      data <- CA
    }
    leaflet(data = data[data$County == input$county,]) %>% addTiles() %>% addWebGLHeatmap(lng= ~Start_Lng, lat= ~Start_Lat, size = 200)
  })
  
  ##Leaflet2
  output$severitymap2 <- renderLeaflet({
  for (i in 1:4) {
    if (input$year != "2016-2019"){
      data <- CA[CA$year == input$year,]
    }else{
      data <- CA
    }
    data <- data[data$County == input$county,]
    data <- data[data$Severity  == severity[i],]
    severity.level <- data[data$Severity == severity[i],]
    
    n <- addCircleMarkers(n,
                                   data = severity.level,
                                   lng = ~Start_Lng,
                                   lat = ~Start_Lat, 
                                   popup = ~Street,
                                   radius = 0.5,
                                   fillOpacity = 1,
                                   color = factPal(severity.level$Severity),
                                   group = severity[i])
  }
  
  n <- n %>% addLayersControl(n, overlayGroups = severity)
  n
  })
  
  
  # weather boxplot
  output$weather_absolute <- renderPlot({
    if (input$year != "2016-2019"){
      data <- CA[CA$year == input$year,]
    }else{
      data <- CA
    }
    ggplot(data[data$County==input$county & !is.na(data$Weather_Type),], aes(x=Weather_Type, fill=as.character(Severity))) + 
      geom_bar(position = "dodge") +
      labs(x="Weather Condition", y="Count", fill="Severity Level") +
      theme_economist_white() + 
      theme(axis.text.x = element_text(angle = 90))
  })
  
  output$weather_relative <- renderPlot({
    if (input$year != "2016-2019"){
      data <- CA[CA$year == input$year,]
    }else{
      data <- CA
    }
    ggplot(data[data$County==input$county & !is.na(data$Weather_Type),], aes(x=Weather_Type, fill=as.character(Severity))) + 
      geom_bar(position = "fill") +
      labs(x="Weather Condition", y="Proportion", fill="Severity Level") +
      theme_economist_white() + 
      theme(axis.text.x = element_text(angle = 90))
  })
  
  
  # road safety boxplot
  output$road_safety <- renderPlot({
    if (input$year != "2016-2019"){
      data <- CA[CA$year == input$year,]
    }else{
      data <- CA
    }
    ggplot(data[data$County==input$county,]) + 
      geom_bar(aes(x="Bump",y=..count.., fill=Bump), width=.3) + 
      geom_bar(aes(x="Stop",y=..count.., fill=Stop),width =.3) +
      geom_bar(aes(x="Give_Way",y=..count.., fill=Give_Way), width=.3) + 
      geom_bar(aes(x="Traffic_Signal",y=..count.., fill=Traffic_Signal),width =.3) +
      geom_bar(aes(x="Traffic_Calming",y=..count.., fill=Traffic_Calming),width =.3) +
      theme_economist_white() + 
      theme(axis.text.x = element_text(angle = 90), axis.title.x = element_blank()) +
      scale_fill_discrete(name = "", labels = c("Present", "Not Present"))
  })
  
  # time series plot
  output$hour <- renderPlot({
    if (input$year != "2016-2019"){
      data <- CA[CA$year == input$year,]
    }else{
      data <- CA
    }
    ggplot(data=data[data$County==input$county,]) +
      geom_bar(aes(x = hour ,y = ..count..)) +
      labs(x="Hour", y="Count") +
      theme_economist_white()
  })
  
  output$day <- renderPlot({
    if (input$year != "2016-2019"){
      data <- CA[CA$year == input$year,]
    }else{
      data <- CA
    }
    ggplot(data=data[data$County==input$county,]) + 
      geom_bar(aes(x = day ,y = ..count.., fill = as.character(ph))) +
      labs(x="Day", y="Count") +
      labs(fill = "Public Holiday") + 
      theme_economist_white()
  })
  
  
  output$month <- renderPlot({
    if (input$year != "2016-2019"){
      data <- CA[CA$year == input$year,]
    }else{
      data <- CA
    }
    ggplot(data=data[data$County==input$county,]) +
      geom_bar(aes(x = month ,y = ..count..)) +
      labs(x="Month", y="Count") +
      theme_economist_white()
  })
  
  #Age group demographic
  output$age_groups <- renderPlot({
    ggplot(data2 %>% filter(CTYNAME == input$county & !is.na(AGESTATUS)), aes(x=AGESTATUS, y= TOT_POP, fill = AGESTATUS))+
      geom_bar(stat="identity") +
      labs(x = "Age group", y= "Population") + 
      theme(legend.position = "none")
  })
  
  #Gender demographics
  output$gender <- renderPlot({
    ggplot(data2 %>% filter(CTYNAME == input$county) %>% summarise(total_male = sum(TOT_MALE), total_female = sum(TOT_FEMALE))) +
      geom_bar(aes(x="Male",y=total_male), width=.3, stat = "identity", fill = "blue") +
      geom_bar(aes(x="Female",y=total_female), width=.3, stat = "identity", fill = "red") +
      labs(x="Gender", y="Population")
  })
  
}

shinyApp(ui, server)
