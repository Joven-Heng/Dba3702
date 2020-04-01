library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)


library(leaflet)
library(dplyr)
library(htmlwidgets)

#cleaning the data

CA <- read.csv("california.csv")
df <- CA
df$Severity <- as.character(df$Severity)
sample.df <- sample_n(df, 60000)




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
            ),
            
            fluidRow(
              #joven
              leafletOutput(plotOutput("severitymap"))
            )
            
    ),
    
    #Tab 2 content
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
  
  
  output$severitymap <- renderLeaflet({
    severity.list <- c("1", "2", "3", "4")
    factPal <- colorFactor(c("green", "yellow", "orange", "red"), severity.list, ordered = T)
    m <- leaflet() %>%
      setView(lat = 36.7783, lng = -119.4179, zoom = 6) %>% 
      addTiles()
    
    for (i in 1:4) {
      severity.level <- sample.df[sample.df$Severity == severity.list[i],]
      
      m <- addCircleMarkers(m,
                            data = severity.level,
                            lng = ~Start_Lng,
                            lat = ~Start_Lat, 
                            popup = ~Street,
                            radius = 0.5,
                            fillOpacity = 1,
                            color = factPal(severity.level$Severity),
                            group = severity.list[i])
    }
    
    m <- addLayersControl(m, overlayGroups = severity.list)
    
  })
  
  
  
  
}

shinyApp(ui, server)


