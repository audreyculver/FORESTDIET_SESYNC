#add collaborator
git config --global user.name RasmuSkovOlesen 
git config --global user.email rso@ign.ku.dk


#libraries
install.packages("reshape2")
library(reshape2)
library(dplyr)
install.packages("openxlsx")
library(openxlsx)
library(tidyverse)

#Histogram over forest cover
qplot(wave3data$forest.ha,
      geom="histogram",
      main = "Forest cover", 
      xlab = "Ha", 
      fill=I("dark blue"),
      col=I("green"))

#Histogram over Dietary diversity score
qplot(wave3data$mhdds9,
      geom="histogram",
      main = "Dietary Diversity Score", 
      xlab = "DDS value", 
      fill=I("dark blue"),
      col=I("green"))

#Histogram over Dietary diversity score
qplot(wave3data$wealth.score,
      geom="histogram",
      main = "Wealth of respondents", 
      xlab = "Wealth Score (1-5)", 
      fill=I("dark blue"),
      col=I("green"))
