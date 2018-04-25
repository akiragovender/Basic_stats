# Akira Govender
# ANOVA Exercies
# Purpose: To complete ANOVA exercises
# 24 April 2018

# Setup -------------------------------------------------------------------

library(tidyverse)

# Exercise 1 --------------------------------------------------------------

# Load data  --------------------------------------------------------------

feed_1 <- c(60.8, 57.0, 65.0, 58.6, 61.7)
feed_2 <- c(68.7, 67.7, 74.0, 66.3, 69.8)
feed_3 <- c(102.6, 102.1, 100.2, 96.5)
feed_4 <- c(87.9, 84.2, 83.1, 85.7, 90.3)

# Does feed type have an effect on the mass of pigs at 
# the end of the experiment?

# Make a dataframe

bacon <- as.tibble(data.frame(
  feed = c(
    rep("Feed 1", length(feed_1)),
    rep("Feed 2", length(feed_2)),
    rep("Feed 3", length(feed_3)),
    rep("Feed 4", length(feed_4))),
  mass = c(feed_1, feed_2, feed_3, feed_4)))

Bacon_1 <- as_tibble(bacon)
Bacon_2 <- aov(mass ~ feed, data = Bacon_1)
summary(Bacon_2)

ggplot(data = Bacon_2, aes(x = feed, y = mass)) +
  geom_boxplot(aes(fill = feed), notch = TRUE)

# There is a significant difference hence feed type has an effect on pig mass.

# Extra analysis 

TukeyHSD(Bacon_2)

# Exercise 2  -------------------------------------------------------------

# Load data

teeth <- datasets::ToothGrowth

# Determine hypothesis 

# HO: There is no difference between the length of tooth and the supplement 
# H1: There is a difference between the length of tooth and the supplement

# ANOVA Test 

teeth.aov <- aov(len ~ supp, data = teeth)
summary(teeth.aov)

# Create graph 

ggplot(data = teeth.aov, aes(x = supp, y = len)) +
  geom_boxplot(aes(fill = supp), notch = TRUE)

# Results 

# As the p value is greater than 0.05, it can be said that the supplement
# has no effect on the length of the tooth thus the null hypothesis is accepted

# Exercise 3  -----------------------------------------------------------------

# Load data 

seeds <- read.csv2("seedlings.csv")

# Determine hypothesis 

# HO: There is no difference between number of seedlings and the temperature they are grown at  
# H1: There is a difference between number of seedlings and the temperature they are grown at  

# ANOVA Test 

seeds.aov <- aov(seedlings ~ storage, data = seeds)
summary(seeds.aov)

# Create graph 

ggplot(data = seeds.aov, aes(x = storage, y = seedlings)) +
  geom_boxplot(aes(fill = storage), notch = TRUE)

# Results 

# As the p value is greater than 0.05, it can be said that the temperature
# the plants are grown at has no effect on the number of seedlings produced
# thus the null hypothesis is accepted

# End of Exercises  -------------------------------------------------------


