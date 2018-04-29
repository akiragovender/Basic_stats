# Akira Govender
# Day_6
# The sixth day of the stats class
# Purpose: To discuss confidence intervals 
# 26 April 2018

# What is a confidence interval ?

# Range of estimates of the mean that represents 95% of the means we have 
# calculated 

# Setup -------------------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(rcompanion)

# Load data 

Input <- ("Student  Sex     Teacher  Steps  Rating
          a        female  Jacob    8000   7
          b        female  Jacob    9000  10
          c        female  Jacob   10000   9
          d        female  Jacob    7000   5
          e        female  Jacob    6000   4
          f        female  Jacob    8000   8
          g        male    Jacob    7000   6
          h        male    Jacob    5000   5
          i        male    Jacob    9000  10
          j        male    Jacob    7000   8
          k        female  Sadam    8000   7
          l        female  Sadam    9000   8
          m        female  Sadam    9000   8
          n        female  Sadam    8000   9
          o        male    Sadam    6000   5
          p        male    Sadam    8000   9
          q        male    Sadam    7000   6
          r        female  Donald   10000  10
          s        female  Donald    9000  10
          t        female  Donald    8000   8
          u        female  Donald    8000   7
          v        female  Donald    6000   7
          w        male    Donald    6000   8
          x        male    Donald    8000  10
          y        male    Donald    7000   7
          z        male    Donald    7000   7")

data <- read.table(textConnection(Input),header = TRUE)
summary(data)

# Calculate mean and confidence intervals 

groupwiseMean(Steps ~ 1, data = data, conf = 0.95, digits = 3)

# For male and females - One way data

groupwiseMean(Steps ~ Sex, data = data, conf = 0.95, digits = 3)

# Results including teacher as well as sex - two way data

dat1 <- groupwiseMean(Steps ~ Teacher + Sex, data = data, conf = 0.95, digits = 3)

# Create the graph (density plot) ------------------------------------------------------------

ggplot(data = data, aes(x = Rating)) +
  geom_density(aes(fill = Sex)) +
  labs(x = "Data", y = "Count")

# Creating the graph 

ggplot(data = dat1, aes(y = Mean, x = Sex)) +
  geom_point(aes(colour = Teacher)) +
  geom_errorbar(aes(ymin = Mean - Trad.lower, 
                    ymax = Mean + Trad.upper,
                    colour = Teacher)) +
  facet_wrap(~Teacher)

# Bootstrapping

groupwiseMean(Steps ~ Sex, data = data, conf = 0.95, digits = 3, R = 10000,
              boot = TRUE, traditional = FALSE, normal = FALSE,
              basic = FALSE, percentile = FALSE, bca = TRUE) 

# Chickweight data analysis -------------------------------------------------------------

chicks <- as_tibble(ChickWeight)
shapiro.test(chicks$weight)

chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(norm_wt = as.numeric(shapiro.test(weight)[2]))

# If the p-value is < 0.05 then data is non-Normal
# If the p-value is > 0.05 then data is Normal

# Log Transform 

log_data <- data %>%
  mutate(log10 = log10(Steps)) %>% 
  mutate(log = log(Steps)) %>%
  mutate(cuberoot = (Steps)) %>% 
  mutate(sqrt = sqrt(Steps)) 

# Plot histogram including all of the columns ------------------------------------

plot1 <- ggplot(data = log_data, aes(x = log10, fill = Teacher)) +
  geom_histogram(aes(fill = Teacher), position = "dodge")

plot2 <- ggplot(data = log_data, aes(x = log, fill = Teacher)) +
  geom_histogram(aes(fill = Teacher), position = "dodge")

plot3 <- ggplot(data = log_data, aes(x = cuberoot, fill = Teacher)) +
  geom_histogram(aes(fill = Teacher), position = "dodge")

plot4 <- ggplot(data = log_data, aes(x = sqrt, fill = Teacher)) +
  geom_histogram(aes(fill = Teacher), position = "dodge")

final <- ggarrange(plot1, plot2, plot3, plot4)

# Run facetted graph 

final

# Another way 

dat2 <- data %>% 
  mutate(log10 = log10(Steps)) %>% 
  mutate(log = log(Steps)) %>%
  mutate(cuberoot = (Steps)) %>% 
  mutate(sqrt = sqrt(Steps)) %>%
  select(-Student, -Rating) %>% 
  gather(key = "dat.type", value = "trans.data", - Sex, - Teacher)

# Create a graph 

ggplot(data = dat2, aes(x = trans.data)) +
  geom_histogram(binwidth = 1000, aes(fill = Sex), position = "dodge") +
  facet_grid(Sex ~ Teachers)

iris <- iris
iris.aov <- aov(data= iris, Petal.Length ~ Species)
summary(iris.aov)

# H0:There is no significant difference in petal width between the three species
# H1:There is a difference in petal width between the three species

shapiro.test(iris$Petal.Width)

# If the data is not normal then the p value is less than 0.05

iris.dat <- as.tibble(iris)

iris %>% 
  group_by(Species) %>% 
  summarise(norm_dat = as.numeric(shapiro.test(Petal.Width)[2]))

# Some species have non normal data 

# Kruskal-Wallis test

kruskal.test(Petal.Width ~ Species, data = iris)

# End of day 6  -----------------------------------------------------------

# Transforming data exercises ---------------------------------------------

# Exercise 1 --------------------------------------------------------------

# Load data

# Normal data (Day 2)

chicks %>% 
  filter(Time == 2) %>% 
  summarise(norm_wt = as.numeric(shapiro.test(weight)[2]))   # p value less than 0.05

# Not normal data (Day 16)

chicks %>% 
  filter(Time == 16) %>% 
  summarise(norm_wt = as.numeric(shapiro.test(weight)[2]))  # p value greater than 0.05

# Exercise 2 --------------------------------------------------------------

# Transforming the data 

log_data_chicks <- chicks %>%
  mutate(log10 = log10(weight)) %>% 
  mutate(log = log(weight)) %>%
  mutate(cuberoot = (weight)) %>% 
  mutate(sqrt = sqrt(weight)) 

# End of exercises  -------------------------------------------------------


