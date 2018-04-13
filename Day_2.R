# Akira Govender
# Day_2 
# The second day of the stats class
# Purpose: To discuss data visualisations and distributions 
# 13 April 2018

# Setup -------------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(ggpubr)

# Manual calculations  -----------------------------------------------------
# Calculating mean, median, standard deviation, variance 

# Firstly we generate random normal data 
# There needs to be 3 arguments (number, standard deviation, mean)

r_dat <- data.frame(dat = rnorm(n = 600, mean = 37, sd = 50), 
                    sample = "A")

# Visualising the data
# Note : In order to make the plot, a data frame needs to be created

ggplot(data = r_dat, aes(x = dat)) +
  geom_density()

# Calculating mean:
# 1. Sum of all the points 
# 2. divided by 
# 3. number of all points

r_dat %>% 
  summarise(r_sum = sum(dat),         # simplifies the data 
            r_n = n(),
            r_mean = r_sum/r_n,
            r_mean_func = mean(dat))         

# The median 
# Brute force with base R
# $ left side: data frame and right side: column in data frame

order(r_dat$dat)[length(r_dat$dat) / 2]

# Use tidy 

r_dat %>% 
  arrange(dat) %>% 
  slice(n() / 2)

# Or tidy automagic way

r_dat %>% 
  summarise(r_median = median(dat))

# Calculating Variance:
# 1. The sum of
# 2. Each value minus the mean 
# 3. Squared
# 4. Divided by 
# 5. The count of the samples minus 1

r_dat %>% 
  mutate(r_error = dat - mean(dat),
         r_error_square = r_error * r_error) %>% 
  summarise(r_squared_sum = sum(r_error_square),
            r_var = r_squared_sum/(n() - 1))

# Calculating standard devaition 

r_dat %>% 
  summarise(r_var = var(dat),
            r_sd = sqrt(r_var),
            r_sd_func = sd(dat))

# Exercise 1 --------------------------------------------------------------

# Data summary for chick weights in wt_summary is similar to summary
# returned for weight [summary(chicks)]

summary(ChickWeight$weight)

ChickWeight %>% 
  summarise(min_weight = min(weight),
            quart_1 = quantile(weight, 0.25),
            med_weight = median(weight),
            mean_weight = mean(weight),
            quart_3 = quantile(weight, 0.75),
            max_weight = max(weight))

# Visualisations ----------------------------------------------------------

# Load additional libraries 

library(RColorBrewer)
library(viridis)

# Load the data (SA time)

sa_time <- read.csv("SA_time.csv")

# Edit the data 

sa_time <- sa_time %>% 
  mutate(human = seq(1, n(), 1),
         geo = c(rep(c("Cape Town", "George", "PE"), times = 6),
                 rep("Joburg", 2)))

# Creating long data

sa_long <- sa_time %>% 
  gather(key = "time_type", value = "minutes", - human )

# Qualitative 

# Create a count of the qualitative values 

sa_count <- sa_long %>% 
  count(time_type) %>% 
  mutate(prop = n/sum(n))

# Stacked bar graph 

ggplot(data = sa_count, aes(x = "", y = n, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
  labs(title = "Stacked bar graph", subtitle = "cumulative sum",
       x = NULL, y = "Count") +
  theme_minimal()

# Stacked proportion bar graph 

ggplot(data = sa_count, aes(x = "", y = prop, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
  scale_y_continuous(breaks = c(0.00, 0.33, 0.66, 1.00)) +
  labs(title = "Stacked bar graph", subtitle = "relative proportions",
       x = NULL, y = "Proportion") +
  theme_minimal()

# Creating a pie chart 

ggplot(data = sa_count, aes(x = "", y = prop, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
  scale_y_continuous(breaks = c(0.00, 0.33, 0.66, 1.00)) +
  labs(title = "Pie chart", subtitle = "but why though?",
       x = NULL, y = NULL) +
  coord_polar("y", start = 0) +
  theme_minimal()

# Continious data ---------------------------------------------------------

# Histograms 

ggplot(data = sa_long, aes(x = minutes)) +
  geom_histogram()

# Get rid of that one value 

sa_clean <- sa_long %>% 
  filter(minutes < 300)

# Try graph again (faceted histogram) 

ggplot(data = sa_clean, aes(x = minutes)) +
  geom_histogram(aes(fill = time_type), position = "dodge") +
  facet_wrap(~time_type, ncol = 1, scales = "free_x")

# Relative proportion histogram 

ggplot(data = sa_clean, aes(x = minutes)) +
  geom_histogram(aes(y = ..density.. , fill = time_type),
                 position = "dodge", binwidth = 1) +
  facet_wrap(~time_type, ncol = 1, scales = "free_x")

# Creating a box plot (use when data is not normally distributed)

ggplot(data = sa_clean, aes(x = time_type, y = minutes)) +
  geom_boxplot(aes(fill = time_type))

# Note: The dot is the maximum value
# Interquartile range is in between quartile 1 and 3 - important 
# measurement of central tendency 
# The higher the IQR, the more variabiliy is present in the data
# Central black line represent the median 

# Creating noched boxplots 

ggplot(data = sa_clean, aes(x = time_type, y = minutes)) +
  geom_boxplot(aes(fill = time_type), notch = TRUE)

# Caluculate the summary stats for plotting over the boxplots 

sa_summary_stats <- sa_clean %>% 
  group_by(time_type) %>% 
  summarise(time_type_mean = mean(minutes))

# Plot this over the boxplot created above

ggplot(data = sa_clean, aes(x = time_type, y = minutes)) +
  geom_boxplot(aes(fill = time_type), notch = TRUE) +
  geom_point(data = sa_summary_stats, size = 6, shape = 18,
             aes(y = time_type_mean, colour = "goldenrod"))

# Relationships  ----------------------------------------------------------

# Creating a scatter plot
# Numeric values are present on both axes

ggplot(data = sa_time, aes(y = now_now, x = just_now)) +
  geom_point(aes(colour = geo)) +
  geom_smooth(aes(colour = geo), method = "lm") +
  coord_equal(xlim = c(0, 60), ylim = c(0, 60))

# Shows the relationship between the x and y axis 

# End of Day 2 ------------------------------------------------------------




