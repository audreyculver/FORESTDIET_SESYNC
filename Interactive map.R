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
  dplyr::summarise(MeanWealth=mean(wealth.score), MeanMarketDistance=mean(dist.market), countclusters=n(),
                   MeanDDS=mean(mhdds9), ForestCover=mean(forest.ha))

####content of markers#####
content <- paste(sep="", "CLUSTER INFORMATION", "<br>", "Average wealth score: ", round(ClusterData$MeanWealth,digits = 1), "<br>", 
                 "Average distance to market (km): ", round(ClusterData$MeanMarketDistance, digits = 1), "<br>",
                 "Number of households: ", ClusterData$countclusters, "<br>",
                 "Average dietary diversity score: ", round(ClusterData$MeanDDS,digits = 1), "<br>",
                 "Forest Cover (ha): ", round(ClusterData$ForestCover, digits = 1))


#####add markers####
leaflet() %>%
  addTiles() %>%
  setView(lng = 35, lat = -5, 
          zoom = 6) %>%
  addCircleMarkers(lng = cluster_coords$long, 
             lat = cluster_coords$lat, 
             label = round(cluster_coords$forest.ha),
             popup = content,
             clusterOptions = markerClusterOptions(),
             color = vec_colors)

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

rm(greenLeafIcon)

####add colors to wealth groups####
vec_breaksC <- c(0,2,4, Inf)
vec_colors <- c("#de2d26", "#fa9fb5", "#c51b8a")

ClusterData$WealthGroup <- ClusterData$MeanWealth
for (i in 1: length(ClusterData$WealthGroup)) {
  ClusterData$color[i] <- vec_colors[min(which(vec_breaksC > ClusterData$WealthGroup[i])) -1]
}

####add colors to forest cover groupss####
##Add forest cover groups
#Group the data in equal groups
install.packages("Hmisc")
library(Hmisc)
ClusterData$ForestCoverGroups <- cut2(ClusterData$ForestCover, g=4)
count(ClusterData, ForestCoverGroups)

#new group names

levels(ClusterData$ForestCoverGroups)<-c("very low cover", "low cover", "medium cover", "high cover" )


vec_colorsfc <- c("#edf8e9", "#bae4b3", "#74c476", "#238b45")


####add colors to DDS groupss####
##Add DDS groups
#Group the data in equal groups
ClusterData$DDSgroups <- cut2(ClusterData$MeanDDS, g=4)
count(data, DDSgroups)

#new group names
levels(ClusterData$DDSgroups)<-c("very low DDS", "low DDS", "medium DDS", "high DDS" )

