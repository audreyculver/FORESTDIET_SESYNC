library(tidyverse)

wave3data <- read_csv('wave3data.csv')

wave3data <- wave3data %>%
  mutate(wealth.score = factor(wealth.score),
         sex.head = factor(sex.head))

# Check whether there are missing values
summary(wave3data)

theme_set(theme_bw())

# Single variable relationships
ggplot(wave3data, aes(x = age.head)) +
  geom_histogram()

ggplot(wave3data, aes(x = wealth.score)) +
  geom_bar()

ggplot(wave3data, aes(x = education)) +
  geom_bar()

# Double variable relationships
ggplot(wave3data, aes(y= wealth.score, x = age.head)) +
  geom_boxplot()

# Summarize household sizes by cluster
hh_size_stats <- wave3data %>%
  group_by(cluster.id) %>%
  summarize(n_households = n(),
            mean_household_size = mean(hh.size),
            median_household_size = median(hh.size),
            sd_household_size = sd(hh.size))

ggplot(hh_size_stats, aes(x = mean_household_size)) +
  geom_histogram()
