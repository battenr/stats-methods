# Title: Plotting Distributions for PS Methods

# Description: An example of plotting the distributions before and after
# IPTW of two confounders. 

# Setup ----

#... Libraries ----

library(tidyverse) # ol faithful
library(WeightIt) # used for IPTW
library(cobalt) # used to plot balance
library(patchwork) # for combining plots

# Data ----

# Keeping it simple: 
# 1. One continuous confounder - hours slept
# 2. One binary confounder - donut eaten
# 3. Binary treatment (coffee)
# 4. Continuous outcome: happiness (although not needed)

sample.size = 1000 # arbitrary sample size

df <- data.frame(
  donut_eaten = rbinom(n = sample.size, size = 1, prob = 0.3),
  hours_sleep = rnorm(n = sample.size, mean = 8, sd = 2)
  ) %>% 
  dplyr::mutate(
    coffee = rbinom(n = sample.size, size = 1, prob = plogis(0.2*hours_sleep + 0.1*donut_eaten)),
    happy = rnorm(n = sample.size, mean = 10 + 0.5 * hours_sleep + 2 * coffee + 0.1 * donut_eaten)
  )

# Calculating IPTW ----

w <- WeightIt::weightit(
  formula = coffee ~ hours_sleep + donut_eaten, # PS formula of form: trt ~ covariates
  data = df # dataframe from above
)

# Plots ----

# - Plotting both adjusted and unadjusted using the which argument (which = "both")
# - Cleaning up variable names and axis titles

#... Overall Propensity Scores

p1 <- cobalt::bal.plot(w, which = "both") + 
  ggtitle("Distribution of Propensity Score (using IPTW)") + 
  labs(x = "Propensity Score", fill = "Coffee") +
  scale_fill_discrete(labels = c("No", "Yes")) + 
  theme(plot.title = element_text(hjust = 0.5)) 

#... Hours of Sleep ----

p2 <- cobalt::bal.plot(w, var.name = "hours_sleep", which = "both") + 
  ggtitle("Distribution for Hours of Sleep") + 
  labs(x = "Hours of Sleep", fill = "Coffee") +
  scale_fill_discrete(labels = c("No", "Yes")) + 
  theme(plot.title = element_text(hjust = 0.5)) 

#... Donut Eaten ----

p3 <- cobalt::bal.plot(w, var.name = "donut_eaten", which = "both") + 
  ggtitle("Distribution for Donut Eaten") + 
  labs(x = "Donut Eaten", fill = "Coffee") +
  scale_fill_discrete(labels = c("No", "Yes")) + 
  scale_x_discrete(labels = c("No", "Yes")) + 
  theme(plot.title = element_text(hjust = 0.5)) 

# Combining Plots for Output ---

p1 / (p2 + p3) # the / makes two lines
