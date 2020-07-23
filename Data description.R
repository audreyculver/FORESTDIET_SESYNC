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

wave3data <- read_csv('wave3data.csv')

####HISTOGRAMS####
##Histogram over forest cover
qplot(wave3data$forest.ha,
      geom="histogram",
      main = "Forest cover", 
      xlab = "Ha", 
      fill=I("dark blue"),
      col=I("green"))

##Histogram over Dietary diversity score
qplot(wave3data$mhdds9,
      geom="histogram",
      main = "Dietary Diversity Score", 
      xlab = "DDS value", 
      fill=I("dark blue"),
      col=I("green"))

##Histogram over Dietary diversity score
qplot(wave3data$wealth.score,
      geom="histogram",
      main = "Wealth of respondents", 
      xlab = "Wealth Score (1-5)", 
      fill=I("dark blue"),
      col=I("green"))

ggplot(wave3data,
       aes(x = mhdds9, fill = sex.head)) +
  geom_histogram(binwidth = 1)

ggplot(wave3data,
       aes(x = mhdds9, fill = wealth.index)) +
  geom_histogram(binwidth = 1)


##Histogram over Forest Patches
qplot(wave3data$forest.patches,
      geom="histogram",
      main = "Forest Patches", 
      xlab = "Number of forest patches", 
      fill=I("dark blue"),
      col=I("green"))

##Histogram over Forest Patches
qplot(wave3data$age.head,
      geom="histogram",
      main = "Age of head of household", 
      xlab = "Age", 
      fill=I("dark blue"),
      col=I("green"))

##Histogram over Education
wave3data$education <- as.factor(wave3data$education)
#doesn't work
qplot(wave3data$education,
      geom="histogram",
      main = "Education", 
      xlab = "Educational level", 
      fill=I("dark blue"),
      col=I("green"))




####PLOTTING####

#Group the data in equal groups
install.packages("Hmisc")
library(Hmisc)
wave3data$ForestCoverGroups <- cut2(wave3data$forest.ha, g=4)
count(wave3data, ForestCoverGroups)

#new group names
levels(wave3data$ForestCoverGroups)<-c("very low cover", "low cover", "medium cover", "high cover" )


#Plot the data
par(mar=c(7,5,1,1))
# Plotting DDS for the 4 groups
boxplot(mhdds9 ~ ForestCoverGroups, data=wave3data,xlab = '',
        ylab = 'Dietary diversity score', las=2, col = 2:4)

#Add wealth groups
wave3data$wealth.score<-as.factor(wave3data$wealth.score)

#ggplot
ggplot(wave3data, aes(x=ForestCoverGroups, y=mhdds9, fill=wealth.score)) + geom_boxplot() + 
facet_grid(~wealth.score)+ scale_fill_brewer(palette = "RdYlGn")
#ggplot2 - combined
ggplot(wave3data, aes(x=ForestCoverGroups, y=mhdds9, fill=wealth.score)) + geom_boxplot() + 
scale_fill_brewer(palette = "RdYlGn")+xlab('Forest cover')+ylab('Dietary diversity score')

## EV BOXPLOTS

#DDScores for different Forest Cover Levels, grouped by Wealth Index
ggplot(wave3data, aes(x=wealth.index, y = mhdds9, fill = ForestCoverGroups)) + 
  geom_boxplot() + scale_fill_brewer(palette = "RdYlGn")+xlab('Wealth Index') + ylab('Diet Diversity Score')

#Create equal groups for Distance to Market
install.packages("Hmisc")
library(Hmisc)
wave3data$MarketDistanceGroups <- cut2(wave3data$dist.market, g=4)
count(wave3data, MarketDistanceGroups)
levels(wave3data$MarketDistanceGroups)<-c("short distance", "medium distance", "long distance", "very long distance" )

#DDScores for different Forest Cover Levels, grouped by Distance to Market (means in blue)
ggplot(wave3data, aes(x=MarketDistanceGroups, y = mhdds9, fill = ForestCoverGroups)) + 
geom_boxplot() + stat_summary(geom = 'point', fun = mean, color = 'blue', position = position_dodge(width = 0.75)) +
scale_fill_brewer(palette = "RdYlGn")+xlab('Market Distance') + ylab('Diet Diversity Score')

#Group the data in equal groups
install.packages("Hmisc")
library(Hmisc)
data$DDSgroups <- cut2(data$mhdds9, g=4)
count(data, DDSgroups)

#new group names
levels(data$DDSgroups)<-c("very low DDS", "low DDS", "medium DDS", "high DDS" )


