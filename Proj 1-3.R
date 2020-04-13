library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

library(leaflet)
library(htmlwidgets)
library(tibble)

#only load once
#CA <- read.csv("CA.csv")
CA <- read.csv("california.csv")

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

df <- CA
df$Severity <- as.character(df$Severity)
sample.df <- sample_n(df, 60000)
sample.df <- mutate(sample.df, Coordinate= paste0(as.character(Start_Lng),sep = ", ", as.character(Start_Lat)))

ca_data <- read.csv("CA.csv", stringsAsFactors = FALSE, na.strings = c("", NA))

severity.list <- c("1", "2", "3", "4")
factPal <- colorFactor(c("green", "yellow", "orange", "red"), severity.list, ordered = T)

freshMap <- leaflet() %>%
  setView(lat = 36.7783, lng = -119.4179, zoom = 6) %>% 
  addTiles()


clusteredMap <- freshMap
layeredMap <- freshMap

for (i in 1:4) {
  severity.level <- sample.df[sample.df$Severity == severity.list[i],]
  
  layeredMap <- addCircleMarkers(layeredMap,
                                 data = severity.level,
                                 lng = ~Start_Lng,
                                 lat = ~Start_Lat, 
                                 popup = ~Street,
                                 radius = 0.5,
                                 fillOpacity = 1,
                                 color = factPal(severity.level$Severity),
                                 group = severity.list[i])
}

severityLayered <- addLayersControl(layeredMap, overlayGroups = severity.list)

##This is for the severity - clustered
for (i in 1:4) {
  severity.level <- sample.df[sample.df$Severity == severity.list[i],]
  
  clusteredMap <- addCircleMarkers(clusteredMap,
                                   data = severity.level,
                                   lng = ~Start_Lng,
                                   lat = ~Start_Lat, 
                                   popup = ~Coordinate,
                                   radius = 0.5,
                                   fillOpacity = 1,
                                   color = factPal(severity.level$Severity),
                                   group = severity.list[i],
                                   clusterOptions = markerClusterOptions())
}

severityClustered <- addLayersControl(clusteredMap, overlayGroups = severity.list)
severityClustered

#Weathered

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

weathertypes = unique(ca_data$Weather_Condition)
indexes <- match(ca_data$Weather_Condition,weathertypes)
newweatherlist <- specificweatherlist[indexes]
ca_data <- add_column(ca_data, Weather_Type =newweatherlist, .after = "Weather_Condition")


County <- unique(CA$County)

header <- dashboardHeader(title = "US Accidents")

sidebar <- dashboardSidebar(
  sidebarMenu(
    #Tab 1 
    menuItem(tabName = "visualization", "Visualization", icon = icon("globe-americas"),
             selectInput(inputId = "county", label = "County", choices = County),
             checkboxGroupInput(inputId = "severity", label = "Severity", choices = c("1","2","3","4")),
             radioButtons(inputId = "display", label = "Display", choices = c("Heatmap","Cluster Distribution"))
    ),
    
    #Tab 2
    menuItem(tabName = "Variables", "Variables", icon = icon("list"),
             textInput(inputId = "lon",label = "Lon"),
             textInput(inputId = "lat",label = "Lat"),
             checkboxGroupInput(inputId = "variablestrends", label = "Display", choices = c("Date","Day","Month","Weather","Severity")),
             radioButtons(inputId = "ph", label = "Public Holidays", choices = c("ph"))
    )
  )
)

body <- dashboardBody(
  fluidRow(
    valueBox("California", "Traffic Accidents", icon = icon("map-marker"), width = 12)
  ),
  
  fluidRow(
    box(plotOutput("plot1"), width=12)
  ),
  
  
  fluidRow(
    valueBoxOutput("county", width=12)
  ),
  
  tabItems(
    #Tab 1 content
    tabItem(tabName="visualisation",class='active', 
            
            
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
                title = "Map of all the problems in the county",
                id = "tabset1",width = "500px",
                tabPanel("Severity Map", leafletOutput("severitymap")),
                tabPanel("HeatMap", leafletOutput("clusteredmap"))
              )
            )
            
    ),
    
    tabItem(tabName="Variables",class='active',
            fluidRow(plotOutput("weathertype_accident"))
    )
  )
)


ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session) {
  
  output$plot1 <- renderPlot({
    ggplot(data=data1) +
      geom_bar(aes(x=reorder(County,-pop.rate), y=no.accidents), fill="black", alpha=0.5, stat="identity") +
      geom_line(aes(x=reorder(County,-pop.rate), y= pop.rate*5000), group=1,  color="red", stat="identity") +
      geom_line(aes(x=reorder(County,-pop.rate), y= veh.rate*5000), group=1,  color="blue", stat="identity") +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
      theme(axis.text.x=element_text(angle=90))
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
  
  #Severity Map 
  output$severitymap <- renderLeaflet({
    severityLayered
  })
  
  output$clusteredmap <- renderLeaflet({
    severityClustered
  })
  
  output$weathertype_accident <- renderPlot({
    ggplot(ca_data, aes(x=Weather_Type, fill=as.character(Severity))) + geom_bar(position = "dodge")
  })
  
  
  
}

shinyApp(ui, server)


