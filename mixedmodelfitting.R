# Fit mixed-effects model to diet diversity data

library(lme4)

#standardize forest_ha and dist.market variables to have mean = 0, sd = 1
wave3data <- wave3data %>%
  mutate(forest_std = (forest.ha - mean(forest.ha))/sd(forest.ha),
         distmarket_std = (dist.market - mean(dist.market))/sd(dist.market))

#not-standardized model, including the interaction between forest hectares and market distance, interpreted 
#as: does the effect of forest cover on diet diversity increase with distance to market?'
dist_x_forest_model <- lmer(mhdds9 ~ forest.ha + dist.market + forest.ha:dist.market + (1 | cluster.id),
     data = wave3data)

#standardized model, giving one intercept per cluster_id with hh randomly distributed around that intercept (same as above)
dist_x_forest_model_std <- lmer(mhdds9 ~ forest_std + distmarket_std + forest_std:distmarket_std + (1 | cluster.id),
                            data = wave3data)

summary(dist_x_forest_model)
summary(dist_x_forest_model_std)

# refit the model without interaction
dist_x_forest_model_std_nointeraction <- lmer(mhdds9 ~ forest_std + distmarket_std + (1 | cluster.id),
                                data = wave3data)
summary(dist_x_forest_model_std_nointeraction)
