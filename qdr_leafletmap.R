library(raster)
library(sf)
library(leaflet)
library(sp)
library(dplyr)

data <- read.csv("wave3data.csv")

####add colors####
vec_breaksC <- c(0,2,4, Inf)
vec_colors <- c("#de2d26", "#fa9fb5", "#c51b8a")

# Pull out coordinates of each cluster from data

# slice function to take first row from each cluster group
# dplyr select to distinguish from select fn in raster package

cluster_coords <- data %>%
  group_by(cluster.id) %>%
  slice(1) %>%
  dplyr::select(lat, long, forest.ha)


####creating cluster dataset####

ClusterData <- data %>%
  group_by(cluster.id) %>%
  dplyr::summarise(MeanWealth=mean(wealth.score), MeanMarketDistance=mean(dist.market), countclusters=n(),
                   MeanDDS=mean(mhdds9), ForestCover=mean(forest.ha), ForestPatches=mean(forest.patches))


ClusterData$WealthGroup <- ClusterData$MeanWealth
ClusterData$color <- NA
for (i in 1: length(ClusterData$WealthGroup)) {
  ClusterData$color[i] <- vec_colors[min(which(vec_breaksC > ClusterData$WealthGroup[i])) -1]
}

##Add forest cover groups
#Group the data in equal groups
library(Hmisc)
ClusterData$ForestCoverGroups <- cut2(ClusterData$ForestCover, g=4)
count(ClusterData, ForestCoverGroups)


#new group names
levels(ClusterData$ForestCoverGroups)<-c("very low cover", "low cover", "medium cover", "high cover" )

##Add DDS groups
#Group the data in equal groups
ClusterData$mDDSgroups <- cut2(ClusterData$MeanDDS, g=4)
count(ClusterData, mDDSgroups)

#new group names
levels(ClusterData$mDDSgroups)<-c("very low DDS", "low DDS", "medium DDS", "high DDS" )


####content of markers#####
content <- paste(sep="", "CLUSTER INFORMATION", "<br>", 
                 "Average dietary diversity score: ", round(ClusterData$MeanDDS,digits = 1), "<br>",
                 "Forest Cover (ha): ", round(ClusterData$ForestCover, digits = 1), "<br>", 
                 "Forest patches: ", ClusterData$ForestPatches, "<br>",
                 "Average wealth score: ", round(ClusterData$MeanWealth,digits = 1), "<br>", 
                 "Average distance to market (km): ", round(ClusterData$MeanMarketDistance, digits = 1), "<br>",
                 "Number of households: ", ClusterData$countclusters, "<br>")

                
                
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
             color = ClusterData$color)

#^^this code will generate map with icons that cluster as you zoom out, pop up with cluster info

#### Below is Quentin's attempt to modify code from https://rstudio.github.io/leaflet/showhide.html
#### to have a menu to show different layers for the different coloring types.

# palette going from yellow to green to show amt of forest
forest_palette <- colorQuantile("YlGn", ClusterData$ForestCover)

# palette going from blue to dark blue to show amt of dds
dds_palette <- colorQuantile("Blues", ClusterData$MeanDDS)

threelayermap <- leaflet() %>%
  addTiles(group = 'base map') %>%
  setView(lng = 35, lat = -5, 
          zoom = 6) %>%
  addCircleMarkers(lng = cluster_coords$long, 
                   lat = cluster_coords$lat, 
                   label = round(cluster_coords$forest.ha),
                   popup = content,
                   clusterOptions = markerClusterOptions(),
                   fillColor = ClusterData$color, 
                   color = NA,
                   group = 'Colored by wealth') %>%
  addCircleMarkers(lng = cluster_coords$long, 
                   lat = cluster_coords$lat, 
                   label = round(cluster_coords$forest.ha),
                   popup = content,
                   clusterOptions = markerClusterOptions(),
                   color = NA,
                   fillColor = forest_palette(ClusterData$ForestCover), 
                   group = 'Colored by forest cover') %>%
  addCircleMarkers(lng = cluster_coords$long, 
                   lat = cluster_coords$lat, 
                   label = round(cluster_coords$forest.ha),
                   popup = content,
                   clusterOptions = markerClusterOptions(),
                   color = NA,
                   fillColor = dds_palette(ClusterData$MeanDDS), 
                   group = 'Colored by DDS') %>%
  addLayersControl(
    baseGroups = c("base map"),
    overlayGroups = c("Colored by wealth", "Colored by forest cover", "Colored by DDS"),
    options = layersControlOptions(collapsed = FALSE)
  )


threelayermap
