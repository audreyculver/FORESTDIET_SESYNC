# Boxplot with the fitted lines from a mixed-effects model plotted on top of the boxes
# This uses the wave3data object as created in the ggplotvisualizationssesync.r script

# Fit model
# Do NOT use the standardized coefficients in this case, because we want to 
# plot the results on the scale of the actual data.

forest_x_wealth_model <- lmer(mhdds9 ~ forest.ha + wealth.score + forest.ha:wealth.score + (1 | cluster.id),
                            data = wave3data)

# Create predicted values, ignoring random effect for the prediction so that
# we can just plot a single trendline on each facet. Otherwise it would give
# us a separate line for each of the 250 clusters!

# I searched this on stackoverflow and found this page
# https://stackoverflow.com/questions/33763089/plotting-predicted-values-from-lmer-as-a-single-plot
# this shows how to get the trendlines using the effects package

library(effects)
# Get effects from model. 
# We specify three levels for forest.ha which should correspond to your 3 levels on the plot.
effect_data <- Effect(c('forest.ha', 'wealth.score'), forest_x_wealth_model,
                      xlevels = list(forest.ha = 3, wealth.score = 5)) 
effect_data <- as.data.frame(effect_data) # Turn it into a data frame

# Modify the data frame to add the low medium and high cover text labels.
effect_data$ForestCoverGroups3 <- factor(c('low cover', 'medium cover', 'high cover'), 
                                         levels = c('low cover', 'medium cover', 'high cover'))

# Below is your boxplot, with a geom_line layer added using the data from effect_data
# I also include a dotted line for the upper and lower confidence intervals!


#DDScores for different wealth groups, faceted by Forest Cover
ggplot(wave3data, aes(x = wealth.score, y = mhdds9, fill = ForestCoverGroups3)) + 
  geom_boxplot() + 
  geom_line(data = effect_data, aes(y = fit, group = ForestCoverGroups3)) +
  geom_line(data = effect_data, aes(y = lower, group = ForestCoverGroups3), linetype = 'dotted') +
  geom_line(data = effect_data, aes(y = upper, group = ForestCoverGroups3), linetype = 'dotted') +
  xlab('Wealth Score') + ylab('Diet Diversity Score') + 
  facet_wrap( ~ ForestCoverGroups3) + 
  scale_fill_brewer(palette = 'Greens') + 
  ggtitle('Dietary Diversity by Wealth Index for Different Levels of Forest Cover') +
  theme_bw()
