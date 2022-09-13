# Title: Simulating Longitudinal Data with Base R

# Setup ----

#... Libraries ----

library(tidyverse)

#... Dependencies ----

#... Pins ----

?rep
?replicate

# Data ----

times_rep = round(runif(25, min = 1, max = 10), 0)

length(times_rep)

df <- data.frame(
  id = rep(1:25, times = times_rep)
)

df <- data.frame(
  sex = rbinom(n = 25, size = 1, prob = 0.30), 
  age = runif(n = 25, min = 5, max = 100)
)

replicate(n = 3, rnorm(5, 0, 1), simplify = FALSE )

?replicate


rep()

df = df %>% 
  dplyr::mutate(
    sex = rbinom(n = 132, size = 1, prob = (0.5*id/15))
  )

summary(df$id)

(0.5*25)/15





?rep
