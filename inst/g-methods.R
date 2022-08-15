# Using the script to learn about g-methods

# g-formula, marginal structural models, nested mean models

# Setup ----

#... Libraries ----

library(tidyverse)
library(simstudy)
library(gfoRmula)

#... Dependencies ----

#... Pins ----

# Simulated Data ----

df = data.frame(
  age = runif(100, min = 20, max = 60),
  sex = rbinom(100, size = 1, prob = 0.80), # 1 = female
  income = rep(letters[1:4], length.out = 100),
  bzd = rbinom(100, size = 1, prob = 0.1),
  ae = rbinom(100, size = 1, prob = 0.59)
)

gfoRmula::


# Marginal structural models ----


