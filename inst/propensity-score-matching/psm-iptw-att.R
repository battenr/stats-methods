# Title: Comparing estimate from PSM to IPTW (ATT estimand)

# Also look into how PSM estimate changes/compares to the actual values when using PSM with replacement

# Setup ----

#... Libraries ----

library(tidyverse)

#... Dependencies ----

# Simulated Data ----

n.study = 773

df <- data.frame(
  id = sample(x = 1:n.study, n.study, replace = FALSE),
  age =
  ) %>% 
  dplyr::mutate(
  trt = rbinom(n = n.study, size = 1, prob = c(0.412)
)

# Logistic Regression as Base Case ----

