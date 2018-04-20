# Akira Govender
# Day_5
# The fifth day of the stats class
# Purpose: To discuss ANOVA analysis Part 2
# 20 April 2018

# Setup -------------------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(Rmisc)

# Load the data -----------------------------------------------------------

snakes <- read_csv("snakes.csv") %>% 
  mutate(day = as.factor(day))

# Summarise the data ------------------------------------------------------

snakes_summary <- snakes %>%
  group_by(day) %>% 
  summarise(snakes_mean = mean(openings),
            snakes_sd = sd(openings))

# Formulate a hypothesis --------------------------------------------------

# HO: There is no difference in the number of opening from day to day
# H1: There is a difference in the number of opening from day to day 

#  Calculate SE & CI ------------------------------------------------------

snakes_summary_2 <- summarySE(data = snakes,
                              measurevar = "openings",
                              groupvars = c("day"))

# Visualise the data ------------------------------------------------------

ggplot(data = snakes, aes(x = day, y = openings)) +
  geom_segment(data = snakes_summary_2, aes(x = day, xend = day, y = openings - ci, yend = openings + ci, colour = day),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = day), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)

# As there are 2 factors we need to have another hypothesis

# H0: There is no difference between snakes with respect to the number of openings at which they habituate.
# H0: There is no difference between days in terms of the number of openings at which the snakes habituate.

# Test the hypothesis -----------------------------------------------------

snakes.day.aov <- aov(openings ~ day, data = snakes)
summary(snakes.day.aov)

# Test both hypotheses

snakes.all.aov <- aov(openings ~ day + snake, data = snakes)
summary(snakes.all.aov)

# Testing assumptions 

# First visualise normality of data

snakes.residuals <- residuals(snakes.all.aov)
hist(snakes.residuals)

# Visualise homoscedasticity

plot(fitted(snakes.all.aov), residuals(snakes.all.aov))

# Apply tukey test 

snakes.tukey <- TukeyHSD(snakes.all.aov, which = "day")
plot(snakes.tukey)

# Visualise the factor interaction

ggplot(data = snakes,aes(x = as.numeric(day),
                         y = openings,
                         colour = snake)) +
  geom_line(size = 3) +
  geom_point(size = 4)

# Exercise using moth data ------------------------------------------------

# Load data 

moths <- read_csv("moth_traps.csv") %>% 
  gather(key = "trap", value = "count", - Location)

# Summarise the data ------------------------------------------------------

moth_loc_summary <- moths %>%
  group_by(Location) %>% 
  summarise(moth_mean = mean(count),
            moth_sd = sd(count))

moth_trap_summary <- moths %>%
  group_by(trap) %>% 
  summarise(moth_mean = mean(count),
            moth_sd = sd(count))

# Formulate the hypotheses --------------------------------------------------

# HO: There is no difference in the count depending of different locations
# HO: There is no difference in count depending on the different trap types

#  Calculate SE & CI ------------------------------------------------------

moth_loc_summary_2 <- summarySE(data = moths,
                              measurevar = "count",
                              groupvars = c("Location"))

moth_trap_summary_2 <- summarySE(data = moths,
                                measurevar = "count",
                                groupvars = c("trap"))

# Visualise the data ------------------------------------------------------

Location <- ggplot(data = moths, aes(x = Location, y = count)) +
  geom_segment(data = moth_loc_summary_2, aes(x = Location, xend = Location, y = count - ci, yend = count + ci, colour = Location),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = Location), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)

Trap <- ggplot(data = moths, aes(x = trap, y = count)) +
  geom_segment(data = moth_trap_summary_2, aes(x = trap, xend = trap, y = count - ci, yend = count + ci, colour = trap),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = trap), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)

Final <- ggarrange(Location, Trap,
                      ncol = 2, nrow = 1,
                      labels = ("AUTO"),
                      common.legend = TRUE)

# Test the hypothesis -----------------------------------------------------

moth.loc.aov <- aov(count ~ Location, data = moths)
summary(moth.loc.aov)

moth.trap.aov <- aov(count ~ trap, data = moths)
summary(moth.trap.aov)

# Test both hypotheses

moths.all.aov <- aov(count ~ Location + trap, data = moths)
summary(moths.all.aov)

# Testing assumptions 

# First visualise normality of data

moths.residuals <- residuals(moths.all.aov)
hist(moths.residuals)

# Visualise homoscedasticity

plot(fitted(moths.all.aov), residuals(moths.all.aov))

# Apply tukey test 

moths.loc.tukey <- TukeyHSD(moths.all.aov, which = "Location")
plot(moths.loc.tukey)

moths.trap.tukey <- TukeyHSD(moths.all.aov, which = "trap")
plot(moths.trap.tukey)

# Regression --------------------------------------------------------------

head(faithful)

# Plot a quick scatter plot

ggplot(data = faithful, aes(x = waiting, y = eruptions)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, colour = "hotpink")

# Form a hypothesis

# HO: Waiting time does not influence the duration of eruption time 
# H1: Waiting time does influence the duration of eruption time 

# Test the hypothesis

faithful_lm <- lm(eruptions ~ waiting, data = faithful)
summary(faithful_lm)

# Correlations ------------------------------------------------------------

# Load additional library -------------------------------------------------

library(corrplot)

# Load data 

ecklonia <- read.csv("ecklonia.csv")

# Formulate hypothesis 

# HO: There is no relationship between stipe diameter and stipe length
# h1: There is a relationship between stipe diameter and stipe length

# Test the hypothesis

cor.test(ecklonia$frond_mass, ecklonia$frond_length)

# Visualise the data 

ggplot(data = ecklonia, aes(x = frond_mass, y = frond_length)) +
  geom_point()

# Run hecka test 

ecklonia_sub <- ecklonia %>% 
  select(frond_mass:frond_length)

ecklonia_cor <- cor(ecklonia_sub)
ecklonia_cor

# Spearman rank test 

# This is used in cases where ordinal data is used 

ecklonia$length <- as.numeric(cut((ecklonia$stipe_length + ecklonia$frond_length), 3))

# Run the test 

cor.test(ecklonia$length, ecklonia$stipe_diameter, method = "spearman")

# Kendall rand correlation

cor.test(ecklonia$primary_blade_length, ecklonia$primary_blade_width, method = "kendall")

# Visualise what we have done ---------------------------------------------

ecklonia_pearson <- cor(ecklonia_sub)

corrplot(ecklonia_cor, method = "circle")

# End of Day 5 ------------------------------------------------------------

# Bonus Question 

# Create a heatmap using the ecklonia pearson data 

# Create data matrix
# ecklonia_matrix <- data.matrix(ecklonia_pearson) 
# In this example data is already in matrix form so this is just for 
# future knowledge

# Creating a palette

ecklonia_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 299)

# Creating the heatmap 

ecklonia_heatmap_1 <- heatmap(ecklonia_pearson, Rowv=NA, Colv=NA, 
                            col = ecklonia_palette, scale="column", 
                            margins=c(13,8), 
                            main = "Heatmap showing correlation of Ecklonia data")

# Trying other aestetic options 

library(pheatmap)
library(heatmap3)
  
ecklonia_heatmap_2 <- pheatmap(ecklonia_pearson, col = c("#ffffcc", "#d9f0a3", "#addd8e", "#78c679", "#31a354", "#006837"), 
         border_color = NA, main = "Heatmap showing the correlation between the different variables of Ecklonia data", 
         legend = TRUE)
         
library(plotly)

ecklonia_heatmap_3 <- plot_ly(z = ecklonia_pearson, type = "heatmap", colors = "Blues", 
                              x = c("D", "FM", "FL", "PBL", "SM", "PBW"),
                              y = c("PBW", "SM", "PBL", "FL", "FM", "D"))

ecklonia_heatmap_3

# End of bonus exercise ------------------------------------------------------------



