# Title: Confounder for a different variable

# Description: If there is a confounder for a different variable, does 
# this 

# Setup ----

library(tidyverse)
library(ggdag)
library(WeightIt)
library(sandwich)
library(broom)

# DAG ----

theme_set(theme_dag())

double_dag <- ggdag::dagify( # double for double confounder
  happy ~ coffee + sleep + donut, 
  coffee ~ sleep, 
  sleep ~ donut,
  exposure = "coffee",
  outcome = "happy"
)

ggdag::ggdag(double_dag)

# Simulating Data ----

ss = 500 # sample size 

df = data.frame(
  donut = rnorm(n = ss, mean = 2, sd = 0.5)
) %>% 
  dplyr::mutate(
    sleep = 9 - 1.25*donut,
    coffee = rbinom(n = ss, size = 1, prob = plogis(0.05*sleep)),
    happy = 1.5*coffee + 0.5*sleep + 0.1*donut + rnorm(n = ss)
  )

# Model (Only with Confounder) ----

ipw <- WeightIt::weightit(formula = coffee ~ sleep, 
                          data = df, 
                          method = "glm")

mod <- glm(happy ~ coffee + sleep, 
           data = df,
           weights = ipw$weights)

robust_se <- sqrt(diag(sandwich::sandwich(mod)))[2]

theta <- broom::tidy(mod)[2,2]


# Model (With Both) ----

ipw <- WeightIt::weightit(formula = coffee ~ sleep + donut, 
                          data = df, 
                          method = "glm")

mod <- glm(happy ~ coffee + donut + sleep, 
           data = df,
           weights = ipw$weights)

robust_se <- sqrt(diag(sandwich::sandwich(mod)))[2]

theta <- broom::tidy(mod)[2,2]

# Metrics to Assess the Model ----

# Bias

# % Instead in precision
