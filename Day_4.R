# Akira Govender
# Day_4
# The fourth day of the stats class
# Purpose: To discuss ANOVA analysis
# 19 April 2018

# Setup -------------------------------------------------------------------

library(tidyverse)

# Load data  --------------------------------------------------------------

chicks <- as_tibble(ChickWeight)

# Subset out only the sample sets to be compared

chicks_sub <- chicks %>% 
  filter(Diet %in% c(1, 2), Time == 21)

# Conducting a t test 

t.test(weight ~ Diet, data = chicks_sub)

# 1 way ANOVA -------------------------------------------------------------

# Research question:
# Is there a difference in chicken mass at day 21 as a result of them 
# eating 4 different diets?

# Null hypothesis: There is no difference in chicken mass at 21 days 
# after having been fed one of four diets 

chicks_21 <- chicks %>% 
  filter(Time == 21)

chicks.aov1 <- aov(weight ~ Diet, data = chicks_21)
summary(chicks.aov1)

# We reject the null hypothesis and accept the alternative hypothesis
# There is a significant difference in mass based on the different diets

# Creating noched boxplots 

ggplot(data = chicks.aov1, aes(x = Diet, y = weight)) +
  geom_boxplot(aes(fill = Diet), notch = TRUE)

# When notches dont overlap there is a significant difference, hence there
# is a significant difference between diet 1 and (2 and 3)

# Tukey HSD test

TukeyHSD(chicks.aov1)

# Diet 2-1 does not result in difference

# Visuals -----------------------------------------------------------------

chicks_21 <- ChickWeight %>% 
  filter(Time == 21)

# Boxplot

ggplot(data = chicks_21, aes(x = Diet, y = weight, fill = Diet)) +
  geom_boxplot(notch = TRUE, colour = "grey50") +
  geom_segment(aes(x = Diet, xend = Diet, y = weight, yend = weight + 2))

# Segments showing confidence intervals 
# Dataframe segments 

chicks_Tukey <- as.data.frame(TukeyHSD(aov(weight ~ Diet, data = chicks_21))$Diet)
chicks_Tukey$pairs <- as.factor(row.names(chicks_Tukey))

# Creating the graph

ggplot(data = chicks_Tukey, aes(x = diff, y = pairs)) +
   geom_segment(aes(x = lwr, xend = upr, y = pairs, yend = pairs)) +
   geom_abline(mapping = NULL, data = NULL, slope = 90, intercept = 0, linetype = "dotted")

# Or

plot(TukeyHSD(aov(weight ~ Diet, data = chicks_21)))

# Multiple factor ANOVA ---------------------------------------------------

# HO: There is no change in chicken mass from Day 1 to day 21

chicks_0_21 <- ChickWeight %>% 
  filter(Time %in% c(0 ,2, 21))

# Visualise the data

ggplot(data = chicks_0_21, aes(x = Time, y = weight)) +
  geom_boxplot(notch = TRUE, aes(fill = as.factor(Time)))

# Run and ANOVA

summary(aov(weight ~ as.factor(Time), data = chicks_0_21))

# Run a tukey post hoc test 

TukeyHSD(aov(weight ~ as.factor(Time), data = chicks_0_21))

# Plot the data

plot(TukeyHSD(aov(weight ~ as.factor(Time), data = chicks_0_21)))

# Looking at only Day 0 and 21 for time and diet 

summary(aov(weight ~ Diet + as.factor(Time), data = ChickWeight))

# There is an increase in df for the time factor but no increase for Diet

# Looking at the interactions between the factors 

summary(aov(weight ~ Diet * as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21))))

# Looking at the tukey results and plotting that data

TukeyHSD(aov(weight ~ Diet * as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21))))

plot(TukeyHSD(aov(weight ~ Diet * as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21)))))

# Using a line graph to better understand the data

chicks_means <- ChickWeight %>% 
  group_by(Diet, Time) %>% 
  summarise(weight_mean = mean(weight, na.rm = T))

ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_line(size = 2) +
  geom_point(shape = 15, size = 5)

# Non parametric Tests ----------------------------------------------------

# What happens if we do not have normal data?
# For a t test we best use  Wilcox rank sum test 

wilcox.test()  # Filled in like a test 

# Kruskall- Wallis test

kruskal.test(weight ~ Diet, data = chicks_0_21)

library(pgirmess)

kruskalmc(weight ~ Diet, data = chicks_0_21)

# End of Day 4 ------------------------------------------------------------

# Bonus Question  ---------------------------------------------------------

# Does feed type have an effect on the mass of pigs at 
# the end of the experiment?

feed_1 <- c(60.8, 57.0, 65.0, 58.6, 61.7)
feed_2 <- c(68.7, 67.7, 74.0, 66.3, 69.8)
feed_3 <- c(102.6, 102.1, 100.2, 96.5)
feed_4 <- c(87.9, 84.2, 83.1, 85.7, 90.3)

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

# There is a significant difference hence feed type has an effect on pig mass

# Extra analysis 

TukeyHSD(Bacon_2)

# End of Bonus question ---------------------------------------------------


