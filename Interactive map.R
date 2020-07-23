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

# slice function to take first row from each cluster group
# dplyr select to distinguish from select fn in raster package

cluster_coords <- wave3data %>%

  group_by(cluster.id) %>%
  slice(1) %>%
  dplyr::select(lat, long, forest.ha)


####creating cluster dataset####

library(dplyr)
ClusterData <- data %>%
  group_by(cluster.id) %>%
  dplyr::summarise(MeanWealth=mean(wealth.score), MeanMarketDistance=mean(dist.market), countclusters=n())

####content of markers#####
content <- paste(sep="", "Average wealth score ", round(ClusterData$MeanWealth,digits = 2), "<br>", 
                 "Average distance to market ", round(ClusterData$MeanMarketDistance, digits = 2), "<br>",
                 "Number of households ", ClusterData$countclusters )


#####add markers####
leaflet() %>%
  addTiles() %>%
  setView(lng = 35, lat = -5, 
          zoom = 6) %>%
  addMarkers(lng = cluster_coords$long, 
             lat = cluster_coords$lat, 
             icon = greenLeafIcon,
             label = round(cluster_coords$forest.ha),
             popup = content,
             clusterOptions = markerClusterOptions())

#^^this code will generate map with icons that cluster as you zoom out, pop up with cluster info

####GreenLeaves####
greenLeafIcon <- makeIcon(
  iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
  iconWidth = 38, iconHeight = 45,
  iconAnchorX = 22, iconAnchorY = 45,
  shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
  shadowWidth = 50, shadowHeight = 64,
  shadowAnchorX = 4, shadowAnchorY = 62
)

leaflet(data = quakes[1:4,]) %>% addTiles() %>%
  addMarkers(~cluster_coords$long, ~cluster_coords$lat, icon = greenLeafIcon)



####add colors####
vec_breaksC <- c(0,2,4, Inf)
vec_colors <- c("#000000", "#cc9900", "#669900")

ClusterData$WealthGroup <- ClusterData$MeanWealth
for (i in 1: length(ClusterData$WealthGroup)) {
  ClusterData$color[i] <- vec_colors[min(which(vec_breaksC > ClusterData$WealthGroup[i])) -1]
}

warnings()
