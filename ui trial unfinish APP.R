#Part of the code only. Not finish

library(shiny)
library(ggplot2)
library(shinyTime)
library(ggmap)
library(jsonlite)
library(shinycssloaders)

ui <- fluidPage(tabsetPanel( tabPanel("Overview",
                                       pageWithSidebar(
                                         headerPanel("US car accident"),
                                         sidebarPanel(
                                           radioButtons(inputId = "display",label="Display Type",
                                                        choices = c("Heat Map","Cluster Plot")),
                                           #Dk how do this
                                           selectInput("dataset", "Country", c("California", "Texas")),
                                           radioButtons(inputId = "display",label="Severity",
                                                        choices = c("1","2","3","4"))),
                                         mainPanel ())), 
                              tabPanel("tab 2", 
                                       pageWithSidebar(
                                         headerPanel("In depth analysis"),
                                         sidebarPanel(
                                           selectInput("dataset", "Sort by", c("Date", "Day", "Month")),
                                           radioButtons(inputId = "display",label="Severity",
                                                        choices = c("1","2","3","4")),
                                           radioButtons(inputId = "display",label="Weather",
                                                        choices = c("Clear", "Cloudy", "drizzle", "Hail", "haze/fog", "Rain", "Snow"))),
                                         mainPanel ())), 
                              tabPanel("tab 3", "contents")))




server <- function(input, output){}

shinyApp(ui=ui, server=server)