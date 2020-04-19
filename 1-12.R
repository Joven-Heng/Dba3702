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
library(tidyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(maps)
library(googleway)
library(stringr)
library(ggmap)
library(DT)

# only load once
CA <- read.csv("CA.csv")

# data consolidation
data1 <- CA %>% group_by(County, year) %>% summarise(count=n())
data1 <- spread(data1, year, count)
data1[is.na(data1)] <-0
data1 <- data1 %>% mutate(`2016-2019` = `2016`+`2017`+`2018`+`2019`)
data1<- gather(data1,`2016`,`2017`,`2018`,`2019`,`2016-2019`, key = "year", value = "count")

cal.county.pop <- read.csv("california_county_population.csv")
county.pop <- cal.county.pop %>% group_by(CTYNAME) %>% summarise(pop = sum(TOT_POP))
county.pop <- county.pop %>% mutate(pop = pop/2)
data1 <- left_join(data1,county.pop, by=c("County"="CTYNAME"))
data1 <- data1 %>% mutate(pop.rate = count*1000/pop)
#data1 <- data1[order(data1$pop.rate, decreasing = T),]


county.veh <- read.csv("estimated vehicles by county.csv")
county.veh <- county.veh[-59,c(-7:-11)]
county.veh$COUNTIES <- county.pop$CTYNAME
county.veh <- as.data.frame(lapply(county.veh, function(y) gsub(",", "", y)))
county.veh$TOTAL <- as.numeric(as.character(county.veh$TOTAL))

#datatable
caltotal <- cal.county.pop %>% filter(AGEGRP == 0)
calelderly <- cal.county.pop %>% filter(AGEGRP >= 14) %>% group_by(CTYNAME) %>% summarize(Elderly = sum(TOT_POP))
calYoung<- cal.county.pop %>% filter(AGEGRP >= 1 & AGEGRP <=3) %>% group_by(CTYNAME) %>% summarize(Children = sum(TOT_POP))
county.veh2 <- county.veh %>% select(COUNTIES, Vehicles = TOTAL)
df9 <- caltotal %>% select(County = CTYNAME, Population = TOT_POP)
df9 <- left_join(df9, calelderly, by = c("County" = "CTYNAME"))
df9 <- left_join(df9, calYoung, by = c("County" = "CTYNAME"))
df9 <- left_join(df9, county.veh2, by = c("County" = "COUNTIES"))
df9 <- mutate(df9, `% Elderly` = signif(Elderly/Population,2), `% Children` = signif(Children/Population,2), `% Vehicles` = signif(Vehicles/Population,2))


data1 <- left_join(data1,county.veh[,c("COUNTIES","TOTAL")], by=c("County"="COUNTIES"))
data1 <- data1 %>% mutate(veh.rate = count*1000/TOTAL)
colnames(data1) <- c("County", "year", "no.accidents", "total.pop", "pop.rate", "total.veh", "veh.rate")

data1 <- data1%>% group_by(year) %>% mutate(rank = dense_rank(desc(pop.rate)))
data1rank<- data1%>% select(County, rank, year)
data1rank <- data1rank%>% ungroup()




# leaflet
colors <- c("green", "yellow", "orange", "red")

severity <- c("1", "2", "3", "4")
factPal <- colorFactor(c("green", "yellow", "orange", "red"), severity, ordered = T)
m <- leaflet() %>% addTiles()
n <- leaflet() %>% addTiles()

#data with demographics
data2 <- left_join(cal.county.pop, data1, by = c("CTYNAME" = "County"))
data2$AGEGRP <- as.numeric(data2$AGEGRP)
data2$AGESTATUS <- cut(data2$AGEGRP, c(0,4,9,13,18), labels = c("Children","Young Adults", "Middle Age", "Elderly"))

# unique weekdays
years <- unique(CA$year)

df <- data.frame()
for (y in years) {
  df1 <- CA[CA$year==y,]
  df2 <- df1 %>% group_by(day) %>% summarise(total=n())
  df3 <- aggregate(df1$day, by=list(df1$date), FUN=unique)
  df4 <- df3 %>% group_by(x) %>% summarise(count=n())
  df2$count <- df4$count
  df2$avg <- df2$total/df2$count
  df2$year <- y
  df <- rbind(df, df2)
  df
}

df22<- data.frame()
for (y in years) {
  df1 <- CA[CA$year==y,]
  df2 <- df1 %>% group_by(typeDay) %>% summarise(total=n())
  df3 <- aggregate(df1$typeDay, by=list(df1$date), FUN=unique)
  df4 <- df3 %>% group_by(x) %>% summarise(count=n())
  df2$count <- df4$count
  df2$avg <- df2$total/df2$count
  df2$year <- y
  df22 <- rbind(df22, df2)
  df22
}

df$day <- factor(df$day,levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
dfsummarised <- df%>% group_by(day) %>% summarise(total = sum(total))

dftotal <- df %>% group_by(day) %>% summarise(total = sum(count))
dftotal2 <- df22 %>% group_by(typeDay) %>% summarise(total = sum(count))

# app layout
header <- dashboardHeader(title = "US Accidents")

sidebar <- dashboardSidebar(
  sidebarMenu(
    # Tab 1 
    menuItem(tabName = "overview", "Overview", icon = icon("globe-americas")),
    
    # Tab 2
    menuItem(tabName = "county", "County Demographics", icon = icon("info")),
    
    # Tab 3
    menuItem(tabName = "county2", "County", icon = icon("map-marker")),
    
    selectInput(inputId = "year", label = "Year", choices = c("2016-2019", "2016", "2017","2018","2019")),
    
    selectInput(inputId = "county", label = "County", choices = sort(unique(CA$County)))
    
  )
)

body <- dashboardBody(
  tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #00334e;
                                position: fixed
                                }
                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #00334e;
                                }
                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #00334e;
                                }
                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #145374;
                                position: fixed
                                }
                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #5588a3;
                                }
                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #145374;
                                color: #ffffff;
                                }
                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #5588a3;
                                }
                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #00334e;
                                }
                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #f3f9fb;
                                }
                                '))),
  
  tabItems(
    # Tab 1 content
    tabItem(tabName = "overview",
            
            fluidRow(
              valueBox("California", "Traffic Accidents (2016-2019)", icon = icon("map-marker"), width = 12, color = "navy")
            ),
            
            fluidRow(
              tabBox(
                title = "Distribution",
                id = "dist",
                width = 7,
                tabPanel("Distribution Across Counties", plotOutput("plot1")),
                tabPanel("Log10 Scale", plotOutput("plot5"))
              ),
              box(plotOutput("plot4"), width=5, title = "Distribution Map")
            ),
            
            fluidRow(
              tabBox(
                title = "Details of Accidents",
                id = "things",
                width = 4,
                height = 500,
                tabPanel("Road Conditions", plotOutput("plot2")),
                tabPanel("Severity Level", plotOutput("plot3"))
              ),
              tabBox(
                id = "Overviews",
                width = 8,
                tabPanel("Accident Rate Ranked by County", DT::dataTableOutput("rankdatatable")),
                tabPanel("Population Statistics", DT::dataTableOutput("overviewTable"))                
              )
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
              infoBoxOutput("popaccidentrate"),
              infoBoxOutput("vehaccidentrate"),
              infoBoxOutput("accidentrank")
            ),
            
            #demographics
            fluidRow(
              tabBox(
                title = "Demographics",
                id = "demographics",
                tabPanel("Age Groups", plotOutput("age_groups")),
                tabPanel("Gender", plotOutput("gender"))
              ),
              box(plotOutput("city"), title = "Accident Frequency by City")
            )
            
    ),
    
    #Tab 3 content
    tabItem(tabName="county2",
            
            fluidRow(valueBoxOutput("county2", width=12)),
            
            fluidRow(
              infoBoxOutput("no.accidents2"),
              infoBoxOutput("population2"),
              infoBoxOutput("vehicles2")
            ),
            
            fluidRow(
              infoBoxOutput("popaccidentrate2"),
              infoBoxOutput("vehaccidentrate2"),
              infoBoxOutput("accidentrank2")
            ),
            
            fluidRow(
              tabBox(
                title = "Locations of Traffic Accidents",
                id = "leaflet",
                width = 12,
                height = 500,
                tabPanel("Severity Map", leafletOutput("severitymap2", height = 500)),
                tabPanel("Cluster Map", leafletOutput("severitymap" , height = 500))
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
              tabBox(title = "Details of Accidents", 
                     id = "Details of Accidents",
                     tabPanel("Road Conditions",plotOutput("road_safety")),
                     tabPanel("Severity Level", plotOutput("plot3b")))
            ),
            
            fluidRow(
              tabBox(
                title = "Time",
                id = "time",
                width = 12,
                height = 500,
                tabPanel("Hour", plotOutput("hour")),
                tabPanel("Day", plotOutput("day")),
                tabPanel("Month", plotOutput("month")),
                tabPanel("Summary", plotOutput("daySummary"))
              )
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
      scale_y_continuous(sec.axis=sec_axis(~./5000, name="Accident Rate")) +
      labs(title = paste("Accident Frequency By County", input$year), x="County", y="Number of Accidents") +
      scale_colour_manual(values=c("Accidents per 1000 people"="red", "Accidents per 1000 vehicles"="blue"), 
                          labels = c("Accidents per 1000 people", "Accidents per 1000 vehicles"), name="Legend:") +
      theme_economist() +
      theme(axis.text.x=element_text(angle=90, vjust = 0.5))
  })
  
  output$plot5 <- renderPlot({
    ggplot(data1[data1$year == input$year,]) +
      geom_bar(aes(x=reorder(County,-pop.rate), y=no.accidents), fill="black", alpha=0.5, stat="identity") +
      labs(title = paste("Accident Frequency By County", input$year), x="County", y="Number of Accidents") +
      scale_y_log10() +
      scale_y_continuous(trans = log10_trans(),
                         breaks = trans_breaks("log10", function(x) 10^x),
                         labels = trans_format("log10", math_format(10^.x))) +
      theme_economist() +
      theme(axis.text.x=element_text(angle=90, vjust = 0.5))
  })
  
  # bar chart for bumps,etc
  output$plot2 <- renderPlot({
    if (input$year != "2016-2019"){
      data <- CA[CA$year == input$year,]
    } else{
      data <- CA
    }
    ggplot(data) + 
      geom_bar(aes(x="Bump",y=..count.., fill=Bump), width=.4) + 
      geom_bar(aes(x="Stop",y=..count.., fill=Stop),width =.4) +
      geom_bar(aes(x="Give_Way",y=..count.., fill=Give_Way), width=.4) + 
      geom_bar(aes(x="Traffic_Signal",y=..count.., fill=Traffic_Signal),width =.4) +
      geom_bar(aes(x="Traffic_Calming",y=..count.., fill=Traffic_Calming),width =.4) +
      theme_economist() +
      theme(axis.text.x=element_text(angle=30, vjust=.8, hjust=0.8), axis.title.x = element_blank(), plot.title = element_text(size = 20, hjust = 0.5)) +
      labs(title = "Road Conditions", x="Road Condition", y="Count") +
      scale_x_discrete(labels = c("Bump", "Give Way Sign", "Stop Sign", "Traffic Calming Marking", "Traffic Signal")) +
      scale_fill_manual(name = "", labels = c("Not Present", "Present"), values = c("firebrick1", "olivedrab1"))
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
    sev$label <- percent(sev$per, accuracy = 0.01)
    
    data <- sev[sev$year == input$year,]
    ggplot(data) +
      geom_bar(aes(x="" ,y=per, fill=as.factor(Severity)), stat="identity") +
      coord_polar("y", start=0) +
      labs(title = "Severity of Accidents by Percentage", fill = "Severity") +
      theme_economist() +
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            panel.grid.major=element_blank(),
            legend.text=element_text(size=10)) +
      scale_fill_manual(values = c("1"= "olivedrab1", "2"="lightgoldenrod1", "3"="darkorange", "4"="firebrick1"),
                        labels = paste0(data$Severity, " (", data$label, ")"))
  })
  
  
  
  # distribution across counties
  output$plot4 <- renderPlot({
    key <- 'AIzaSyDhBLI5W7LFhoRLea-ZVMQQDytw2RBE98E'
    
    world <- ne_countries(scale = "medium", returnclass = "sf")
    byCounty <- data1 %>% select(County, year, no.accidents) %>%
      filter(year==input$year)
    byCounty$County <- as.character(byCounty$County)
    
    counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))
    counties <- subset(counties, grepl("california", counties$ID))
    counties$County <- str_to_title(sub('.*,\\s*', '', counties$ID))
    counties <- left_join(counties, byCounty, by="County")
    counties$no.accidents <- ifelse(is.na(counties$no.accidents), 0, counties$no.accidents)
    
    bottom3 <- byCounty %>% 
      arrange(no.accidents) %>% 
      head(3)
    top3 <- byCounty %>% 
      arrange(desc(no.accidents)) %>% 
      head(3)
    
    calicounty <- as.data.frame(rbind(top3[,1], bottom3[,1]))
    calicounty <- add_column(calicounty, state="California", .before = "County")
    coords <- apply(calicounty, 1, function(x) {
      google_geocode(address=paste(x["County"], x["state"], sep=", "), key=key)
    })
    calicounty <- cbind(calicounty, do.call(rbind, lapply(coords, geocode_coordinates)))
    calicounty <- st_as_sf(calicounty, coords = c("lng", "lat"), remove = FALSE, 
                           crs = 4326, agr = "constant")
    
    ggplot(data = world) +
      geom_sf() +
      geom_sf(data = counties, aes(fill = no.accidents)) +
      geom_sf(data = calicounty, size = 1) +
      geom_text_repel(data = calicounty, aes(x = lng, y = lat, label = County), 
                      fontface = "bold", size = 4) +
      scale_fill_viridis_c(trans = "sqrt", alpha = .4) +
      coord_sf(xlim = c(-125, -113), ylim = c(32, 43), expand = FALSE) +
      labs(fill="Frequency", x="Longitude", y="Latitude", title=paste("Accident Frequency Map", input$year)) +
      theme_economist() +
      theme(legend.position = "right", legend.text = element_text(size=12))
  })
  
  #Datatable
  output$overviewTable <- DT::renderDataTable({
    df9
  })
  
  #Rank datatable
  output$rankdatatable <- DT::renderDataTable({
    data1rank %>% filter(year == input$year) %>% select(County, rank)
  })
  
  # county
  output$county <- output$county2 <- renderValueBox({
    valueBox(input$county, paste("Accidents in", input$year),  icon = icon("map-marker"), color = "navy")
  })
  
  # no.accidents output
  output$no.accidents <- output$no.accidents2 <- renderInfoBox({
    infoBox("Accidents", formatC(data1[data1$County==input$county & data1$year == input$year,"no.accidents"], format="d", big.mark = ","), icon = icon("car-crash"), color = "navy")
  })
  
  # population output
  output$population <- output$population2 <-renderInfoBox({
    infoBox("Population", formatC(data1[data1$County==input$county & data1$year == input$year,"total.pop"], format="d", big.mark = ","), icon = icon("users"), color = "navy")
  })
  
  # vehicle output
  output$vehicles <- output$vehicles2 <- renderInfoBox({
    infoBox("Registered Vehicles", formatC(data1[data1$County==input$county & data1$year == input$year,"total.veh"], format="d", big.mark = ","),  icon = icon("car"), color = "navy")
  })
  
  # popaccidentrate output
  output$popaccidentrate <- output$popaccidentrate2 <- renderInfoBox({
    infoBox("Accident Rate", paste0(ceiling(data1[data1$County==input$county & data1$year == input$year,"pop.rate"]),"/1,000 people"), icon = icon("percentage"), color = "navy")
  })
  
  # vehaccidentrate output
  output$vehaccidentrate <- output$vehaccidentrate2 <- renderInfoBox({
    infoBox("Vehicle Accident Rate", paste0(ceiling(data1[data1$County==input$county & data1$year == input$year,"veh.rate"]),"/1,000 vehicles"), icon = icon("percentage"), color = "navy")
  })
  
  #Accident rank output
  output$accidentrank<- output$accidentrank2 <- renderInfoBox({
    infoBox("Accident Rank", paste0(data1[data1$County==input$county & data1$year == input$year,"rank"],sep = "/",length(unique(data1$County))), icon = icon("list-ol"), color = "navy")
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
      theme_economist() + 
      scale_fill_manual(values = c("1"= "olivedrab1", "2"="lightgoldenrod1", "3"="darkorange", "4"="firebrick1"))
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
      theme_economist() +
      scale_fill_manual(values = c("1"= "olivedrab1", "2"="lightgoldenrod1", "3"="darkorange", "4"="firebrick1"))
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
      theme_economist() + 
      theme(axis.text.x=element_text(angle=30, vjust=.8, hjust=0.8), axis.title.x = element_blank(), plot.title = element_text(size = 20, hjust = 0.5)) +
      labs(x="Road Condition", y="Count") +
      scale_x_discrete(labels = c("Bump", "Give Way Sign", "Stop Sign", "Traffic Calming Marking", "Traffic Signal")) +
      scale_fill_manual(name = "", labels = c("Not Present", "Present"),values = c("firebrick1", "olivedrab1"))
  })
  
  # severity pie chart 2
  output$plot3b <- renderPlot({
    data13 <- CA
    data13 <- data13[data13$County==input$county,]
    # severity percentage
    sev <- data13 %>% group_by(Severity, year) %>% summarise(count = n())
    sev <- spread(sev, year, count)
    for (i in c("2016", "2017", "2018","2019")){
      if (!(i %in% colnames(sev))){
        sev$i <- NA
        colnames(sev)[ncol(sev)]<- i 
      }
    }
    sev[is.na(sev)] <-0
    sev <- sev %>% mutate(`2016-2019` = `2016`+`2017`+`2018`+`2019`)
    sev <- gather(sev,`2016`,`2017`,`2018`,`2019`,`2016-2019`, key = "year", value = "count")
    by.year <- sev %>% group_by(year) %>% summarise(sum(count))
    sev <- left_join(sev, by.year, by="year")
    sev$per <- sev$count/sev$`sum(count)`
    sev$label <- percent(sev$per, accuracy = 0.01)
    data <- sev[sev$year == input$year,]
    data[is.na(data)] <-0
    
    ggplot(data) +
      geom_bar(aes(x="" ,y=per, fill=as.factor(Severity)), stat="identity") +
      coord_polar("y", start=0) +
      labs(fill = "Severity") +
      theme_economist() +
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            panel.grid.major=element_blank(),
            legend.text=element_text(size=10)) +
      scale_fill_manual(values = c("1"= "olivedrab1", "2"="lightgoldenrod1", "3"="darkorange", "4"="firebrick1"),
                        labels = paste0(data$Severity, " (", data$label, ")"))
  })
  
  # time series plot
  output$hour <- renderPlot({
    if (input$year != "2016-2019"){
      data <- CA[CA$year == input$year,]
    }else{
      data <- CA
    }
    ggplot(data=data[data$County==input$county,]) +
      geom_bar(aes(x = as.factor(hour) ,y = ..count.., fill = ..count..)) +
      scale_fill_gradient(low = "green", high = "red") +
      labs(x="Hour", y="Average No. of Accidents") +
      theme_economist() +
      theme(legend.position = "none")
  })
  
  output$day <- renderPlot({
    if (input$year != "2016-2019"){
      data <- CA[CA$year == input$year,]
      data <- data[data$County==input$county,]
      df77 <- df %>% filter(year == input$year)
      df90 <- data %>% group_by(day) %>% summarise(accidents = n())
      df91 <- right_join(df90, df77, by = "day")
      df91[is.na(df91)] <-0
      df91$accidents <- as.numeric(df91$accidents)
      data <- df91 %>% mutate(rate = accidents/count)
    }else{
      data <- CA
      data <- data[data$County==input$county,]
      df77 <- dftotal
      names(df77) <- c("day", "total")
      df90 <- data %>% group_by(day) %>% summarise(accidents = n())
      df91 <- right_join(df90, df77, by = "day")
      df91[is.na(df91)] <-0
      df91$accidents <- as.numeric(df91$accidents)
      data <- df91 %>% mutate(rate = accidents/total)
    }
    data$day <- factor(data$day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    ggplot(data=data) + 
      geom_bar(aes(x = day ,y = rate, fill = rate), stat = "identity") +
      labs(x = "", y="Average No. of Accidents Per Day") +
      scale_fill_gradient(low = "green", high = "red") +
      theme_economist() +
      theme(legend.position = "none")
  })
  
  output$daySummary <- renderPlot({
    if (input$year != "2016-2019"){
      data <- CA[CA$year == input$year,]
      data=data[data$County==input$county,]
      df88 <- df22 %>% filter(year == input$year)
      df87 <- data %>% group_by(typeDay) %>% summarise(accidents = n())
      df86 <- right_join(df87, df88, by = "typeDay")
      df86[is.na(df86)] <-0
      data <- df86 %>% mutate(rate = accidents/total)
    }else{
      data <- CA
      data=data[data$County==input$county,]
      df88 <- dftotal2
      df87 <- data %>% group_by(typeDay) %>% summarise(accidents = n())
      df86 <- right_join(df87, df88, by = "typeDay")
      df86[is.na(df86)] <-0
      data <- df86 %>% mutate(rate = accidents/total)
    }
    data$typeDay <- factor(data$typeDay, levels = c("Weekday", "Weekend","Public Holiday"))
    ggplot(data= data) + 
      geom_bar(aes(x = typeDay ,y = rate, fill = rate), stat= "identity") +
      scale_fill_gradient(low = "green", high = "red") +
      labs(x = "", y="Average No. of Accidents Per Day") +
      theme_economist() + 
      theme(legend.position = "none")
  })
  
  output$month <- renderPlot({
    if (input$year != "2016-2019"){
      data <- CA[CA$year == input$year,]
    }else{
      data <- CA
    }
    data$month <- factor(data$month, levels = c("1","2","3","4","5","6","7","8","9","10","11", "12"))
    ggplot(data=data[data$County==input$county,]) +
      geom_bar(aes(x = month ,y = ..count.., fill = ..count..)) +
      labs(x="Month", y="Average No. of Accidents") +
      scale_fill_gradient(low = "green", high = "red") +
      theme_economist() + 
      theme(legend.position = "none")
  })
  
  #Age group demographic
  output$age_groups <- renderPlot({
    ggplot(data2 %>% filter(CTYNAME == input$county & !is.na(AGESTATUS)), aes(x=AGESTATUS, y= TOT_POP, fill = AGESTATUS))+
      geom_bar(stat="identity") +
      labs(x = "Age Group", y= "Population", fill = "Age Group") + 
      theme(legend.position = "none") +
      theme_economist()
  })
  
  #Gender demographics
  output$gender <- renderPlot({
    ggplot(data2 %>% filter(CTYNAME == input$county) %>% summarise(total_male = sum(TOT_MALE), total_female = sum(TOT_FEMALE))) +
      geom_bar(aes(x="Male",y=total_male), width=.3, stat = "identity", fill = "blue") +
      geom_bar(aes(x="Female",y=total_female), width=.3, stat = "identity", fill = "red") +
      labs(x="Gender", y="Population") +
      theme_economist()
  })
  
  #city plot
  output$city <- renderPlot({
    if (input$year != "2016-2019"){
      data <- CA[CA$year == input$year,]
    } else{
      data <- CA
    }
    ggplot(data=data[data$County==input$county,]) + 
      geom_bar(aes(x=City, y=..count.., fill = ..count..)) + 
      labs(x="City", y="Count") +      
      scale_fill_gradient(low = "green", high = "red") +
      theme_economist() +
      theme(axis.text.x=element_text(angle=90, vjust=0.5), legend.position = "none")
  })
  
  
}

shinyApp(ui, server)
