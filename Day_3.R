# Akira Govender
# Day_3 
# The third day of the stats class
# Purpose: To discuss data visualisations and distributions 
# 17 April 2018

# Setup -------------------------------------------------------------------

library(fitdistrplus)
library(logspline)

# Generate log-normal data

r_norm <- rnorm(n = 1000, mean = 13, sd = 1)

# Creating a histogram

hist(r_norm)
descdist(r_norm, discrete = FALSE, boot = 100)

# Uniformly distributed data

y <- runif(100)
par(mfrow = c(1, 1))
plot(x = c(1:100), y = y)
hist(y)
descdist(r_norm, discrete = FALSE)

# Analysing and demonstrating the statistical tests -----------------------

# Load additional packages  -----------------------------------------------

library(tidyverse)
library(plotly)

# Random normal data

r_dat <- data.frame(dat = c(rnorm(n = 1000, mean = 10, sd = 3),
                            rnorm(n = 1000, mean = 8, sd = 2)),
                    sample = c(rep("A", 1000), rep("B", 1000)))

# Checking assumptions ----------------------------------------------------

# Normality 
# We are able to use the Shapiro-Wilk test

shapiro.test(r_dat$dat)            

# This above tests the data altogether

# We want to run the test on groups instead of altogether to test normality

r_dat %>% 
  group_by(sample) %>% 
  summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2]))

# The [2] indicates that we are only interested in the second value
# which is the p value

# Data is normal when p > 0.05
# Data is non normal when p < = 0.05

# Checking homoscedasticity -----------------------------------------------

# There are numerous ways to check for homoscedasticity 
# Homoscedasticity is the similarity of variance between sample sets
# For now we say the assumptions are met when the variance of the samples are 
# not more than 2 - 4 times greater than one another

# Checking the varicance for everything 

var(r_dat$dat)

# Doing it in a better, tidier way 

r_dat %>% 
  group_by(sample) %>% 
  summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2]),
            r_norm_var = var(dat))

# The above shows the normality of the distribution & variance as well 
# as the p values 

# 2 Assumptions: 
# 1. Data was normally distributed 
# 2. Data was homoscedastic (no outliers)

# One sample t test  ------------------------------------------------------

# Create a single sample of random normal data

r_one <- data.frame(dat = rnorm(n = 20, mean = 20, sd = 5),
                    sample = "A")

# Visualising data using a density plot found at end of exercise 

t.test(r_one$dat, mu = 20)

# Run a test that will produce a significant result 

t.test(r_one$dat, mu = 30)

# Pick a side -------------------------------------------------------------

# Are the dat less than the popultion mean 

t.test(r_one$dat, mu = 20, alternative = "less")

# Or greater

t.test(r_one$dat, mu = 20, alternative = "greater")

# What about for the larger population mean, are the samples less than 
# the population of 30?

t.test(r_one$dat, mu = 30, alternative = "less")

# Or greater than 

t.test(r_one$dat, mu = 30, alternative = "greater")

# Two sample t test -------------------------------------------------------

# Creating another dataframe 

r_two <- data.frame(dat = c(rnorm(n = 20, mean = 4, sd = 1),
                            rnorm(n = 20, mean = 5, sd = 1)),
                    sample = c(rep("A", 20), rep("B", 20)))

# Run a basic test 

t.test(dat ~ sample, data = r_two, var.equal = TRUE)

# Pick a side
# Is A less than B?

t.test(dat ~ sample, data = r_two, var.equal = TRUE, alternative = "less")

# Is A greater than B?

t.test(dat ~ sample, data = r_two, var.equal = TRUE, alternative = "greater")

# A t-test workflow ------------------------------------------------------

# Worked example from manual 

# Load the data

ecklonia <- read_csv("ecklonia.csv") %>% 
  gather(key = "variable", value = "value", -species, -site, -ID)

# Visualising the data 

ggplot(data = ecklonia, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip()

# Filter the data

ecklonia_sub <- ecklonia %>% 
  filter(variable == "stipe_mass")

# Creating the new graph

ggplot(data = ecklonia_sub, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "stipe mass (kg)", x = "") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# Assumptions for a t-test:
  
# 1. Dependent variable must be continuous 
# 2. Observations in groups being compared are independent of each other
# 3. Data are normally distributed
# 4. Data are homoscedastic (no outliers)

# In order to meet assumptions 3 and 4 : 

two_assum <- function(x)
  {x_var <- var(x)
  x_norm <- as.numeric(shapiro.test(x)[2])
  result <- c(x_var, x_norm)
  return(result)}

ecklonia_sub %>% 
  group_by(site) %>% 
  summarise(stipe_mass_var = two_assum(value)[1],
            stipe_mass_norm = two_assum(value)[2])

# Running an analysis

# Traditional output

t.test(value ~ site, data = ecklonia_sub, var.equal = TRUE, 
       alternative = "greater")

# Dataframe output

compare_means(value ~ site, data = ecklonia_sub, method = "t.test", 
              var.equal = TRUE, alternative = "greater")

# Results 

# The stipe mass of Ecklonia maxima was found to be 
# significantly greater at Batsata Rock than at Boulders Beach 
# (p = 0.03, t = 1.87, df = 24) 

# Analysing another variable of the data ----------------------------------

# Filter the data

ecklonia_diam <- ecklonia %>% 
  filter(variable == "stipe_diameter")

# Visualising the data using a graph 

diameter_data <- ecklonia_diam %>%
  group_by(site) %>%
  summarise(mn_diameter = mean(value, na.rm = TRUE),
            sd_diameter = sd(value, na.rm = TRUE))

ggplot(diameter_data, aes(x = site, y = mn_diameter, fill = site)) +
  geom_col(aes(fill = site), position = "dodge",width = 0.5) +
  labs(x = "Site",y = "Average diameter",
       title = "Average stipe diameter at different locations") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_errorbar(aes(ymin = mn_diameter - sd_diameter,
                    ymax = mn_diameter + sd_diameter),
                position = "dodge",width = 0.5)

# Creating the new graph

ggplot(data = ecklonia_diam, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "Stipe diameter", x = "") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# Assumptions for a t-test:

# 1. Dependent variable must be continuous 
# 2. Observations in the groups being compared are independent of each other
# 3. Data are normally distributed
# 4. Data are homoscedastic (no outliers)

# In order to meet assumptions 3 and 4 : 

two_assum <- function(x)
{x_var <- var(x)
x_norm <- as.numeric(shapiro.test(x)[2])
result <- c(x_var, x_norm)
return(result)}

ecklonia_diam %>% 
  group_by(site) %>% 
  summarise(stipe_diam_var = two_assum(value)[1],
            stipe_diam_norm = two_assum(value)[2])

# Running an analysis

# Traditional output

t.test(value ~ site, data = ecklonia_diam, var.equal = TRUE, 
       alternative = "greater")

# Dataframe output

compare_means(value ~ site, data = ecklonia_diam, method = "t.test", 
              var.equal = TRUE, alternative = "greater")

# Results 

# We can see that the stipe diameter of Ecklonia maxima was found 
# to be significantly greater at Batsata Rock than at Boulders Beach
# as the analysis showed a p value of 0.02 

# Exercise 1 --------------------------------------------------------------

# Create random normal data
# Data represents the weight of 30 children aged 12, half of whom were
# born in Cape Town and the other half of whom were born in Durban. 

exercise <- data.frame(weight = c(rnorm(n = 15, mean = 22, sd = 1),
                            rnorm(n = 15, mean = 23, sd = 1)),
                    location = c(rep("Durban", 15), 
                                 rep("Cape Town", 15)))

# Visualising the data using a graph 

exercise_data <- exercise %>%
  group_by(location) %>%
  summarise(mn_weight = mean(weight, na.rm = TRUE),
            sd_weight = sd(weight, na.rm = TRUE))

ggplot(exercise_data, aes(x = location, y = mn_weight, fill = location)) +
  geom_col(aes(fill = location), position = "dodge",width = 0.5) +
  labs(x = "Location",y = "Average weight",
       title = "Average weight of children born in Cape town and Durban") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_errorbar(aes(ymin = mn_weight - sd_weight,
                    ymax = mn_weight + sd_weight),
                position = "dodge",width = 0.5)

# Creating a graphical representation of the data

ggplot(data = exercise, aes(x = location, y = weight, fill = location)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "Weight (kg)", x = "") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# Creating a hypothesis

# H0: Childrens weight in Cape Town is not greater than in Durban
# H1: Childrens weight in Cape Town is greater than in Durban

# In order to meet assumptions 3 and 4 : 

two_assum <- function(x)
{x_var <- var(x)
x_norm <- as.numeric(shapiro.test(x)[2])
result <- c(x_var, x_norm)
return(result)}

exercise %>% 
  group_by(location) %>% 
  summarise(weight_var = two_assum(weight)[1],
            weight_norm = two_assum(weight)[2])

# Running the analysis (Traditional and dataframe respectively)

t.test(weight ~ location, data = exercise, 
       var.equal = TRUE, alternative = "greater")

compare_means(weight ~ location, data = exercise, method = "t.test", 
              var.equal = TRUE, alternative = "greater")

# Results 

# The null hypothesis is accepted as the p value is greater than 0.05
# The childrens weight was not found to be significantly higher in 
# Cape Town than it was in Durban (p = 0.13, t = 1.12, df = 28)

# End of Day 3 ------------------------------------------------------------

# Bonus exercise  ---------------------------------------------------------

# Creating a density plot to visualise the spread of the data

# Calculate mean of the data 

r_one %>% 
  summarise(mean_one = mean(dat))

# Be more specific about what you want to be done

r_one %>% 
  group_by(sample) %>% 
  summarise(mean_one = mean(dat),
            median_one = median(dat))

# Visualising the density of the data

ggplot(data = filter(r_one), 
       aes(x = dat, fill = sample)) +
  geom_density(alpha = 0.4) +
  labs(x = "Data",y = "Density",
       title = "Density plot representing the data") +
  theme(plot.title = element_text(hjust = 0.5))

# End of bonus exercise  -------------------------------------------------


