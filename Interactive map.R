install.packages("raster")
install.packages("sf")
install.packages("leaflet")
library(raster)
library(sf)
library(leaflet)
leaflet() %>%
  addTiles() %>%
  setView(lng = 35, lat = -5, 
          zoom = 6)


