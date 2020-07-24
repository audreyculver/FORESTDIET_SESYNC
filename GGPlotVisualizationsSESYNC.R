####Data Visualizations for SESYNC Presentation####

#load r packages
install.packages("reshape2")
library(reshape2)
library(dplyr)
install.packages("openxlsx")
library(openxlsx)
library(tidyverse)
library(Hmisc)

wave3data <- read_csv('wave3data.csv')

##prepare the data

#create categorical variables
wave3data <- wave3data %>%
  mutate(wealth.score = factor(wealth.score),
         sex.head = factor(sex.head))
#make equal groupings for forest cover, market distance, dietary diversity
wave3data$ForestCoverGroups <- cut2(wave3data$forest.ha, g=4)
count(wave3data, ForestCoverGroups)
levels(wave3data$ForestCoverGroups)<-c("very low cover", "low cover", 
                                       "medium cover", "high cover" )

#try with just 3 forest cover groups
wave3data$ForestCoverGroups3 <- cut2(wave3data$forest.ha, g=3)
count(wave3data, ForestCoverGroups3)
levels(wave3data$ForestCoverGroups3)<-c("low cover", "medium cover", 
                                       "high cover" )


wave3data$MarketDistanceGroups <- cut2(wave3data$dist.market, g=4)
count(wave3data, MarketDistanceGroups)
str(wave3data$MarketDistanceGroups)
levels(wave3data$MarketDistanceGroups)<-c(">40 km", "40 - 80 km", 
                                          "80 - 115 km", "> 115 km")

wave3data$DDSgroups <- cut2(data$mhdds9, g=4)
count(wave3data, DDSgroups)
levels(wave3data$DDSgroups)<-c("very low DDS", "low DDS", "medium DDS", "high DDS" )

##boxplots
#DDScores for different Forest Cover Levels, grouped by Wealth Index
ggplot(wave3data, aes(x=wealth.index, y = mhdds9, fill = ForestCoverGroups)) + 
  geom_boxplot() + scale_fill_brewer(palette = "RdYlGn")+xlab('Wealth Index') + ylab('Diet Diversity Score')

#DDScores for different Forest Cover Levels, grouped by Distance to Market (means in blue)
ggplot(wave3data, aes(x=MarketDistanceGroups, y = mhdds9, fill = ForestCoverGroups)) + 
  geom_boxplot() + stat_summary(geom = 'point', fun = mean, color = 'blue', position = position_dodge(width = 0.75)) +
  scale_fill_brewer(palette = "RdYlGn")+xlab('Market Distance') + ylab('Diet Diversity Score')

#DDScores for different market distance groups, grouped by Forest Cover
ggplot(wave3data, aes(x = MarketDistanceGroups, y = mhdds9, fill = ForestCoverGroups3)) + geom_boxplot() +
                 scale_fill_brewer(palette = "RdYlGn")+xlab('Market Distance') + ylab('Diet Diversity Score')

#DDScores for different market distance groups, faceted by Forest Cover
ggplot(wave3data, aes(x = MarketDistanceGroups, y = mhdds9, fill = ForestCoverGroups3)) + geom_boxplot() + 
  xlab('Market Distance') + ylab('Diet Diversity Score') + facet_wrap( ~ ForestCoverGroups3) + scale_fill_brewer(palette = 'Greens') + ggtitle('Dietary Diversity by Market Distance for Different Levels of Forest Cover') + stat_summary(geom = 'point', fun = mean, color = 'blue', position = position_dodge(width = 0.75))

#DDScores for different wealth groups, faceted by Forest Cover
ggplot(wave3data, aes(x = wealth.index, y = mhdds9, fill = ForestCoverGroups3)) + geom_boxplot() + 
  xlab('Wealth Index') + ylab('Diet Diversity Score') + facet_wrap( ~ ForestCoverGroups3) + scale_fill_brewer(palette = 'Greens') + ggtitle('Dietary Diversity by Wealth Index for Different Levels of Forest Cover') + stat_summary(geom = 'point', fun = mean, color = 'blue', position = position_dodge(width = 0.75))
›