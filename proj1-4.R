
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(leaflet)
library(htmlwidgets)
library(tibble)
library(leaflet.extras)

#only load once
CA <- read.csv("CA.csv")
CA <- CA %>% mutate(hour = substr(Start_Time,12,13), month = substr(date,6,7))

data1 <- CA %>% group_by(County) %>% summarise(count=n())

cal.county.pop <- read.csv("california_county_population.csv")
county.pop <- cal.county.pop %>% group_by(CTYNAME) %>% summarise(pop = sum(TOT_POP))
data1 <- left_join(data1,county.pop, by=c("County"="CTYNAME"))
data1 <- data1 %>% mutate(pop.rate = count*1000/pop)
#data1 <- data1[order(data1$pop.rate, decreasing = T),]

county.veh <- read.csv("estimated vehicles by county.csv")
county.veh <- county.veh[-59,]
county.veh$COUNTIES <- county.pop$CTYNAME
county.veh <- as.data.frame(lapply(county.veh, function(y) gsub(",", "", y))) 
county.veh$TOTAL <- as.numeric(as.character(county.veh$TOTAL))

data1 <- left_join(data1,county.veh[,c("COUNTIES","TOTAL")], by=c("County"="COUNTIES"))
data1 <- data1 %>% mutate(veh.rate = count*1000/TOTAL)
colnames(data1) <- c("County", "no.accidents", "total.pop", "pop.rate", "total.veh", "veh.rate")

#Cleaning weather types
w1 <- "Clear"
w2 <- "Cloudy"
w3 <- "haze/fog"
w4 <- "drizzle"
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
CA <- add_column(CA, Weather_Type =newweatherlist, .after = "Weather_Condition")

#leaflet
colors <- c("green", "yellow", "orange", "red")
severity <- c("1", "2", "3", "4")
m <- leaflet() %>% addTiles()

header <- dashboardHeader(title = "US Accidents")

sidebar <- dashboardSidebar(
  sidebarMenu(
    #Tab 1 
    menuItem(tabName = "overview", "Overview", icon = icon("globe-americas")),
    
    #Tab 2
    menuItem(tabName = "county", "County", icon = icon("map-marker")),
    
    selectInput(inputId = "county", label = "County", choices = unique(CA$County))
    
  )
)

body <- dashboardBody(

  tabItems(
    #Tab 1 content
    tabItem(tabName = "overview",
            
            fluidRow(
              valueBox("California", "Traffic Accidents", icon = icon("map-marker"), width = 12)
            ),
            
            fluidRow(
              box(plotOutput("plot1"), width=12)
            ),
            
            fluidRow(
              box(plotOutput("plot2"), width=6),
              box(plotOutput("plot3"), width=6)
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
                title = "Leaflet",
                id = "leaflet",
                width = 10,
                height = 500,
                tabPanel("Severity Map", leafletOutput("severitymap" , height = 500)),
                tabPanel("HeatMap", leafletOutput("heatmap", height = 500))
              )
            ),
            
            fluidRow(
              br(),
              br(),
              br(),
              box(plotOutput("weathertype_accident"), title = "Weather Conditions"),
              box(plotOutput("road_safety"), title = "Presense of bumps etc")
            ),
            
            fluidRow(
              tabBox(
                title = "Time series",
                id = "time",
                width = 10,
                height = 500,
                tabPanel("Hour", plotOutput("hour")),
                tabPanel("Day", plotOutput("day")),
                tabPanel("Month", plotOutput("month"))
              )
            )
              
    )
  )
)


ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session) {
  
  #overall bar graph by county
  output$plot1 <- renderPlot({
    ggplot(data=data1) +
      geom_bar(aes(x=reorder(County,-pop.rate), y=no.accidents), fill="black", alpha=0.5, stat="identity") +
      geom_line(aes(x=reorder(County,-pop.rate), y= pop.rate*5000), group=1,  color="red", stat="identity") +
      geom_line(aes(x=reorder(County,-pop.rate), y= veh.rate*5000), group=1,  color="blue", stat="identity") +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
      theme(axis.text.x=element_text(angle=90))
  })
  
  #bar chart for bumps,etc
  output$plot2 <- renderPlot({
    ggplot(data=CA) + 
      geom_bar(aes(x="Bump",y=..count.., fill=Bump), width=.3) + 
      geom_bar(aes(x="Stop",y=..count.., fill=Stop),width =.3) +
      geom_bar(aes(x="Give_Way",y=..count.., fill=Give_Way), width=.3) + 
      geom_bar(aes(x="Traffic_Signal",y=..count.., fill=Traffic_Signal),width =.3) +
      geom_bar(aes(x="Traffic_Calming",y=..count.., fill=Traffic_Calming),width =.3)
  })
  
  output$plot3 <- renderPlot({
    ggplot(data=CA) + geom_bar(aes(x="Severity" ,y=..count.., fill = as.character(Severity))) + coord_polar("y", start=0)
  })
  
  
  #County
  output$county <- renderValueBox({
    valueBox(input$county,"Traffic Accidents by County",  icon = icon("map-marker"))
  })
  
  #no.accidents output
  output$no.accidents <- renderInfoBox({
    infoBox("Accidents", data1[data1$County==input$county,"no.accidents"], icon = icon("users"))
  })
  
  #population output
  output$population <- renderInfoBox({
    infoBox("Population", data1[data1$County==input$county,"total.pop"], icon = icon("users"))
  })
  
  #vehicle output
  output$vehicles <- renderInfoBox({
    infoBox("Registered Vehicles", data1[data1$County==input$county,"total.veh"], icon = icon("users"))
  })

  #popaccidentrate output
  output$popaccidentrate <- renderInfoBox({
    infoBox("Accident Rate", paste0(floor(data1[data1$County==input$county,"pop.rate"]),"/1000 people") )
  })
  
  #vehaccidentrate output
  output$vehaccidentrate <- renderInfoBox({
    infoBox("Vehicle Accident Rate", paste0(floor(data1[data1$County==input$county,"veh.rate"]),"/1000 vehicles"))
  })
  
  #leaflet
  output$severitymap <- renderLeaflet({
    for (i in 1:4){
      data <- CA[CA$County == input$county,]
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
  
  #heatmap
  output$heatmap <- renderLeaflet({
    leaflet(data = CA[CA$County == input$county,]) %>% addTiles() %>% addWebGLHeatmap(lng= ~Start_Lng, lat= ~Start_Lat, size = 200)
  })
  
  
  #Weather boxplot
  output$weathertype_accident <- renderPlot({
    ggplot(CA[CA$County==input$county,], aes(x=Weather_Type, fill=as.character(Severity))) + geom_bar(position = "dodge")
  })
  
  #road safety boxplot
  output$road_safety <- renderPlot({
    ggplot(data=CA[CA$County==input$county,]) + 
      geom_bar(aes(x="Bump",y=..count.., fill=Bump), width=.3) + 
      geom_bar(aes(x="Stop",y=..count.., fill=Stop),width =.3) +
      geom_bar(aes(x="Give_Way",y=..count.., fill=Give_Way), width=.3) + 
      geom_bar(aes(x="Traffic_Signal",y=..count.., fill=Traffic_Signal),width =.3) +
      geom_bar(aes(x="Traffic_Calming",y=..count.., fill=Traffic_Calming),width =.3)
  })
  
  #Time Series plot
  output$hour <- renderPlot({
    ggplot(data=CA[CA$County==input$county,]) + geom_bar(aes(x = hour ,y = ..count..))
  })
  
  output$day <- renderPlot({
    ggplot(data=CA[CA$County==input$county,]) + geom_bar(aes(x = day ,y = ..count.., fill = as.character(ph)))
  })
  
  output$month <- renderPlot({
    ggplot(data=CA[CA$County==input$county,]) + geom_bar(aes(x = month ,y = ..count..))
  })
  
}

shinyApp(ui, server)

















