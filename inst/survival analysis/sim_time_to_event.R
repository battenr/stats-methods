# Title: Simulated Survival Dat a

# Setup ----

#... Libraries ----

library(tidyverse)
library(ggsurvfit)

#... Dependencies ----

#... Pins ----

# Data ----

df <- data.frame(
  age = runif(n = 271, min = c(10, 30), max = c(18, 80)), 
  sex = rbinom(n = 271, size = 1, prob = c(0.379, 0.767)),
  beta_age = runif(n = 271, min = 0, max = 0.215),
  beta_sex = runif(n = 271, min = 0, max = 0.39),
  time_to_first_tooth = rweibull(n = 271, shape = 1.46, scale = 1)
) %>% 
  dplyr::mutate(
    prob_candy_eater = (beta_age*age + beta_sex*sex)/25,
    candy_eater = rbinom(n = 271, size = 1, prob = prob_candy_eater)
  )

summary(df$prob_candy_eater)

library(ggsurvfit)
install.packages("ggsurvfit")

# KM Curve to Test Data ----

ggsurvfit::survfit2(
  Surv(time_to_first_tooth) ~ candy_eater, data = df
) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  )

