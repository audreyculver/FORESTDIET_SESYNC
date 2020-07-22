# Boxplot with points for the means superimposed on the boxes.

#DDScores for different Forest Cover Levels, grouped by Wealth Index
ggplot(wave3data, aes(x=wealth.index, y = mhdds9, fill = ForestCoverGroups, group = interaction(wealth.index, ForestCoverGroups))) +
  geom_boxplot() +
  stat_summary(geom = 'point', fun = mean, color = 'blue', position = position_dodge(width = 0.75)) +
  scale_fill_brewer(palette = "RdYlGn") +
  xlab('Wealth Index') + ylab('Diet Diversity Score')
