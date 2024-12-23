# Title: Demonstration of Region of Practical Equivalence (ROPE)

# Description: Showing how ROPE can be a helpful tool. 
# My personal preference is to show the entire distribution, however 
# we can also use ROPE for decision making. 

# Setup ----

#... Library ----

library(tidyverse) # ol faithful
library(brms)
library(bayestestR)

#... Functions ----

source("R/custom_theme.R")
source("R/sim_data.R")

# Data ----

df <- sim_data()

df <- df %>%
  mutate(across(c(x, z1, z2), scale))

# Bayesian Model ----

# Setting Priors 

priors <- c(
  prior(normal(0, 2), class = "b", coef = "x"),
  prior(normal(0, 2), class = "b", coef = "z1"), 
  prior(normal(0, 2), class = "b", coef = "z2")
)

# Scaling the Variables 



# Fitting the model 

mod <- brms::brm(y ~ x + z1 + z2, 
                 family = gaussian(link = "identity"), 
                 data = df, 
                 prior = priors)

pp_check(mod)

# Calculating ROPE ----

# The ROPE is the region of practical equivalence. This is helpful because it can help
# us determine what the region would be where we'd basically say there is no effect. 
# For example, 0 is no effect. What about 0.3? What about 0.4? 

# Note: this is meant to be an introduction, but there are 

# For more information, recommend: 
# 

