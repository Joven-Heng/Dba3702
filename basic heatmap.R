library(leaflet)
library(leaflet.extras)
library(dplyr)
library(htmlwidgets)

df <- read.csv("CA.csv")
sample.df <- sample_n(df, 60000)

m <- leaflet() %>% 
  setView(lat = 36.7783, lng = -119.4179, zoom = 6) %>% 
  addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
  addWebGLHeatmap(data = sample.df, 
                  lng = ~Start_Lng, 
                  lat = ~Start_Lat, 
                  opacity = 0.5)

m

saveWidget(m, file = "heatmap.html")
