# Akira Govender
# Day_1 
# The first day of the stats class
# Purpose: To practise some concepts that we will encounter 
# 12 April 2018


# Setup -------------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(ggpubr)

# Integers ----------------------------------------------------------------

# Generate the integer data (describes discrete things, are counted in whole numbers)

integer_r <- as.integer(seq(5, 14, by = 1 ))

# Look at the complete dataset 

integer_r

# Look at a summary of the data 

summary(integer_r)

# Continous ---------------------------------------------------------------

# Generating a sequence of numeric data (numeric data is quantative)

numeric_r <- seq(23, 43, length.out = 10)

# Look at the dataset that was generated 

numeric_r

# Dates -------------------------------------------------------------------

# Performing arithmetic with dates
# Look at the difference in days, how many days are in between the two dates?

as.Date("2005-12-31") - as.Date("2005-12-12")

# or

dates_r <- seq(as.Date("2005-12-16"), as.Date("2005-12-25"), by = "day")

# View summary 

summary(dates_r)

# Dataframes  -------------------------------------------------------------

# Create the base data frame 

df_r <- data.frame(integers = integer_r,
                   numeric = numeric_r,
                   dates = dates_r)
# Make sure that all the lengths are the same in order for the command to run 

# Update into tibble form 

df_r <- as_tibble(df_r)
summary(df_r)

# View the dataframe that was just created

df_r

# Catagories  -------------------------------------------------------------
# Qualitative data tell us information about something, puts them into catagories ( Example: male and female)
# We can count the number of subjects/ observations in each catagory  

# Creating a number of different factors of classes 

# Electronics

elec_r <- as.factor(c("laptops",
                      "desktops",
                      "cell phones"))

# People 

people_r <- as.factor(c("funny",
                        "beautiful",
                        "beanies"))

# Colours 

colour_r <- as.factor(c("red", "blue"))

# Ordinal data ------------------------------------------------------------

# Sill working with qualitative data, but it is ranked in some way, 
# for example, red is better than orange (order of preference)

colour_qual <- ordered(c("blue", "green",
                            "yellow", "orange", "red"),
                          levels = c("blue", "green", "yellow",
                                     "orange", "red"))

# Levels specifically creates the ordering of importance, in terms of ecology 
# we can use high, intermediate, low density which you need to tell the computer 
# which is the most important


# Binary data -------------------------------------------------------------

# Can only take on two outcomes for example true and false or dead or alive

binary_r <- c(TRUE, FALSE, TRUE, TRUE)
summary(binary_r)


#  Character data ---------------------------------------------------------

# Just strings of words or multiple words such as Bettys Bay 

sites_r <- c("Yztervarkpunt", "Betty's Bay", 
             "Gansbaai", "Sea Point")

# Missing values  ---------------------------------------------------------
# Note that NA does not mean zero, it just means the value is absent or not recorded

chicks_nest <- c(3, 2, 0, 10, 5, 6, 8, 2, 4, NA)

# Viewing a summary of the data

summary(chicks_nest)

# Calculating the mean and standard deviation of the data respectively

mean(chicks_nest)
sd(chicks_nest)

# Inorder to have your data show up in the environment assignt it to a word
# Inroder to look at the: top of the data : head(name, row number)
#                      : bottom of the data tail(name, row number)


# Descriptive statistics --------------------------------------------------

# Create a dataframe

chicks <- as_tibble(ChickWeight)

chicks %>% 
  summarise(chicken_count = n())

# or 

nrow(chicks)

# To look at the number of observations ie. number of rows


# Measures of central tendency --------------------------------------------

# Calculate mean weight 

chicks %>% 
  summarise(mean_wt = mean(weight))

# Be more specific about what you want to be done

chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mean_wt = mean(weight),
            median_wt = median(weight))

# Visualising the density of the data

ggplot(data = filter(chicks, Time == 21), 
       aes(x = weight, fill = Diet)) +
  geom_density(alpha = 0.4)

# Diet 1 and 3 are skewed to the left and right whilst diet 4 seems to be in the middle 

# Right skewed data applies to Diet 1 

# Skewness (where is the majority of the data) ----------------------------------------------------------------

# Calculate the numeric value 

# Load new package in order to use the skewness function  -----------------

library(e1071)

#   Comapare the difference in mean and median against skewness

chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mean_wt = mean(weight),
            median_wt = median(weight),
            skew_wt = skewness(weight))

# Kurtosis ----------------------------------------------------------------

# Calculate the kurtosis of the tails of a distribution 
chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mean_wt = mean(weight),
            median_wt = median(weight),
            skew_wt = skewness(weight),
            kurt_wt = kurtosis(weight))
# Kurtosis has no tails 

exp_r <- data.frame(dat = rexp(n = 500), 
                    sample = "A")

ggplot(data = exp_r, aes(x = dat)) +
  geom_density()

kurtosis(exp_r$dat)

#  Measures of Variance  --------------------------------------------------------------

# We use standard deviation more than we use variance due to the fact that
# it is in the same unit of measurement of the data

# A summary of the many different statistical properties 

wt_summary <- chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mean_wt = mean(weight),
            median_wt = median(weight),
            var_wt = var(weight),
            sd_wt = sd(weight),
            min_wt = min(weight),
            quart1_wt = quantile(weight, 0.25),
            quart2_wt = quantile(weight, 0.5),
            quart3_wt = quantile(weight, 0.75))

# End of Day 1 ------------------------------------------------------------



