
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)


header <- dashboardHeader(title = "US Accidents")

sidebar <- dashboardSidebar(
  sidebarMenu(
    #Tab 1 
    menuItem(tabName = "visualization", "Visualization", icon = icon("globe-americas"),
             selectInput(inputId = "county", label = "County", choices = c("Solano","Alameda")),
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
              infoBoxOutput("severityrate"),
              infoBoxOutput("popaccidentrate"),
              infoBoxOutput("vehaccidentrate ")
            )
            
    ),
    
    tabItem(tabName="Variables",class='active',
            box("a")
            )
  )
)


ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
  
  #Accident output
  output$no.accidents <- renderInfoBox({
    pop <- CA %>% filter(County == input$county) 
    infoBox("Accidents", sum(col(pop)), icon = icon("users"))
  })
  
  
}

shinyApp(ui, server)

















