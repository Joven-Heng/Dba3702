library(leaflet)
library(dplyr)
library(htmlwidgets)
library(tibble)

df <- read.csv("CA.csv")
sam.df <- sample_n(df, 60000)
sample.df <- mutate(sample.df, Coordinate= paste0(as.character(Start_Lng),sep = ", ", as.character(Start_Lat)))

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
                        popup = ~Coordinate,
                        radius = 0.5,
                        fillOpacity = 1,
                        color = factPal(severity.level$Severity),
                        group = severity.list[i],
                        clusterOptions = markerClusterOptions())
}

m <- addLayersControl(m, overlayGroups = severity.list)

m

saveWidget(m, file = "layered cluster map.html")
