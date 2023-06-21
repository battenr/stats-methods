# Title: Comparing estimate from PSM to IPTW (ATT estimand)

# Also look into how PSM estimate changes/compares to the actual values when using PSM with replacement

# Setup ----

#... Libraries ----

library(tidyverse)

#... Dependencies ----

# Simulated Data ----

set.seed(07102022)

n.study = 773

df <- data.frame(
  id = sample(x = 1:n.study, n.study, replace = FALSE),
  age = runif(n = n.study, min = 18, max = 90)
  ) %>% 
  dplyr::mutate(
    trt = rbinom(n = n.study, size = 1, prob = 0.412),
    
)

?runif

# Logistic Regression as Base Case ----

