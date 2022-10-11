# Title: Simulating Longitudinal Data with Base R

# Setup ----

#... Libraries ----

library(tidyverse)
library(lubridate)

#... Dependencies ----

# Data ----

#.. BZD Data ----

n.bzd = 

df <- data.frame(
  id = sample(1:500, size = 2719, replace = TRUE),
  date = sample(seq(as.Date('2015/01/01'), as.Date('2020/12/31'), by = 'day', ), 2719, replace = TRUE),
  bzd = 
)
