library(leaflet)
library(dplyr)
library(htmlwidgets)

df <- read.csv("CA.csv")
df$Severity <- as.character(df$Severity)

severity.list <- c("1", "2", "3", "4")
factPal <- colorFactor(c("green", "yellow", "orange", "red"), severity.list, ordered = T)

m <- leaflet() %>%
  setView(lat = 36.7783, lng = -119.4179, zoom = 6) %>% 
  addTiles()

for (i in 1:4) {
  severity.level <- df[df$Severity == severity.list[i],]
  
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
m

saveWidget(m, "heatmap.html")
