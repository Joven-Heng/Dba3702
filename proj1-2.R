library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

library(leaflet)
library(htmlwidgets)
library(tibble)

CA <- read.csv("california.csv")

cal.county.pop <- read.csv("california_county_population.csv")
county.pop <- cal.county.pop %>% group_by(CTYNAME) %>% summarise(pop = sum(TOT_POP))

county.veh <- read.csv("estimated vehicles by county.csv")
county.veh <- county.veh[-59,]
county.veh$COUNTIES <- county.pop$CTYNAME
county.veh <- as.data.frame(lapply(county.veh, function(y) gsub(",", "", y))) 
county.veh$TOTAL <- as.numeric(as.character(county.veh$TOTAL))
county.veh


#This is for the severity - layered 
#we have to set up the graph here due to a forloop
df = CA
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


#only need to load once so it wont take as long
#CA <- read.csv("CA.csv")
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
  tabItems(
    
    #Tab 1 content
    tabItem(tabName="visualisation", 
            
            
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
              leafletOutput("severitymap")
            )
            
    ),
    
    tabItem(tabName="Variables", class = "active",
            fluidRow(leafletOutput("clusteredmap")),
            
            fluidRow(plotOutput("weathertype_accident"))
    )
  )
)


ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session) {
  
  #county.accident <- reactive({
  #  filter(CA, County == input$county)
  #})
  
  
  #no.accidents output
  output$no.accidents <- renderInfoBox({
    county.accident <- nrow(filter(CA, County == input$county))
    infoBox("Accidents", county.accident, icon = icon("users"))
  })
  
  #population output
  output$population <- renderInfoBox({
    pop <- county.pop[county.pop$CTYNAME == input$county,"pop"]
    infoBox("Population", pop, icon = icon("users"))
  })
  
  #vehicle output
  output$vehicles <- renderInfoBox({
    veh <- county.veh[county.veh$COUNTIES == input$county,"TOTAL"]
    infoBox("Registered Vehicles", veh, icon = icon("users"))
  })
  
  #popaccidentrate output
  output$popaccidentrate <- renderInfoBox({
    county.accident <- nrow(filter(CA, County == input$county))
    pop <- county.pop[county.pop$CTYNAME == input$county,"pop"]
    accidentrate <- floor(county.accident*1000/pop)
    infoBox("Accident Rate", paste0(accidentrate,"/1000 people") )
  })
  
  #vehaccidentrate output
  output$vehaccidentrate <- renderInfoBox({
    county.accident <- nrow(filter(CA, County == input$county))
    veh <- county.veh[county.veh$COUNTIES == input$county,"TOTAL"]
    vehrate <- floor(county.accident*1000/veh)
    infoBox("Vehicle Rate", paste0(vehrate,"/1000 vehicles"))
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
