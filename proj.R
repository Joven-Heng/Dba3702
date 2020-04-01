
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

cal.county.pop <- read.csv("california_county_population.csv")
county.pop <- cal.county.pop %>% group_by(CTYNAME) %>% summarise(pop = sum(TOT_POP))

county.veh <- read.csv("estimated vehicles by county.csv")
county.veh <- county.veh[-59,]
county.veh$COUNTIES <- county.pop$CTYNAME
county.veh <- as.data.frame(lapply(county.veh, function(y) gsub(",", "", y))) 
county.veh$TOTAL <- as.numeric(as.character(county.veh$TOTAL))
county.veh

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
  

  
}

shinyApp(ui, server)

















