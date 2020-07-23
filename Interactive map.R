install.packages("raster")
install.packages("sf")
install.packages("leaflet")

library(raster)
library(sf)
library(leaflet)
library(sp)
library(dplyr)

data <- read.csv("wave3data.csv")

help(leaflet)

# Pull out coordinates of each cluster from data
cluster_coords <- data %>%
  group_by(cluster.id) %>%
  slice(1) %>%
  dplyr::select(lat, long, forest.ha)

#GreenLeaves
greenLeafIcon <- makeIcon(
  iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
  iconWidth = 38, iconHeight = 95,
  iconAnchorX = 22, iconAnchorY = 94,
  shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
  shadowWidth = 50, shadowHeight = 64,
  shadowAnchorX = 4, shadowAnchorY = 62
)

leaflet(data = quakes[1:4,]) %>% addTiles() %>%
  addMarkers(~cluster_coords$long, ~cluster_coords$lat, icon = greenLeafIcon)


leaflet() %>%
  addTiles() %>%
  setView(lng = 35, lat = -5, 
          zoom = 6) %>%
  addMarkers(lng = cluster_coords$long, 
             lat = cluster_coords$lat, 
             icon = greenLeafIcon,
             label = round(cluster_coords$forest.ha),
             clusterOptions = markerClusterOptions())


           
