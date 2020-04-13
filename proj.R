
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

#only load once
#CA <- read.csv("CA.csv")

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
            )
            
    ),
    
    tabItem(tabName="Variables",class='active',
            box("a")
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
  

  
}

shinyApp(ui, server)

















