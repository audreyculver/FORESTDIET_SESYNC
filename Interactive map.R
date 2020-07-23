install.packages("raster")
install.packages("sf")
install.packages("leaflet")
library(raster)
library(sf)
library(leaflet)

# Pull out coordinates of each cluster from data
# slice function to take first row from each cluster group
# dplyr select to distinguish from select fn in raster package

cluster_coords <- wave3data %>%
  group_by(cluster.id) %>%
  slice(1) %>%
  dplyr::select(lat, long, forest.ha)

leaflet() %>%
  addTiles() %>%
  setView(lng = 35, lat = -5, 
          zoom = 6) %>%
  addMarkers(lng = cluster_coords$long, 
             lat = cluster_coords$lat, 
             label = round(cluster_coords$forest.ha))
