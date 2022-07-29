# Entropy Balancing

# Playing around with entropy balancing to compare with PSM & IPTW. Initial idea is to 
# compare performance based on MSE between models 

# Setup ----

library(tidyverse)
library(simstudy)
library(ebal)
library(WeightIt)

#... Libraries ----

#... Dependencies ----

# Simulating data for binary outcome (ORR) ----

# Using covariates commonly captured in RRMM research: age, sex and ECOG status
# Outcome: objective response rate aka objective response

# Trying simulating using base R

set.seed(2022)

age = rnorm(1000, mean = 70, sd = 5)
sex = rbinom(1000, size = 1,  prob = 0.546) # 1 is male, 0 is female
ecog = rbinom(1000, size = 1, prob = 0.50)
response = rbinom(1000, size = 1, prob = 0.65)
trt = rbinom(1000, size = 1, prob = 0.50) # assuming 1 is trial and 0 is RW

df <- data.frame(
  age, sex, ecog, response, trt
) 

#... Entropy Balancing ----

# using this link: https://lost-stats.github.io/Model_Estimation/Matching/entropy_balancing.html

trial_rw <- df %>% pull(trt)

match_var <- df %>% 
  select(age, sex, ecog) %>%  
  as.matrix() # starting with only age as variable

eb <- ebal::ebalance(trial_rw, match_var)

df %>% count(trt)

eb.rw <- df %>% dplyr::filter(trt == 1) %>% dplyr::mutate(weight = 1)
df.trial <- df %>% dplyr::filter(trt == 0) 
eb.trial <- cbind(df.trial, eb$w) %>% dplyr::rename(weight = `eb$w`)

eb.weights <- rbind(eb.rw, eb.trial)

# weights for eb are only for the RW patients

#... IPTW ----

iptw <- WeightIt::weightit(trt ~ age + sex, df = df, 
                           estimand = "ATT", stabilize = TRUE,
                           threshold = c(m = 0.02),
                           )

# Logistic Regression Using Weights ----

mod.eb <- glm(response ~ trt + age + sex + ecog, 
              data = eb.weights,
              weights = weight)

mod.iptw <- glm(response ~ trt + age + sex + ecog, 
                data = df, 
                weights = iptw$weights,
                )

broom::tidy(mod.iptw)
library(sandwich)

sandwich::vcovCL(mod.iptw)

sandwich(mod.iptw)

# Calculating Mean Squared Error ----

# Data frame for predicting

# Predicting values using eb model

eb.predict <- stats::predict(mod.eb) %>% as.data.frame()

eb.predict$predicted <- eb.predict$.

predict.actual <- cbind(df, eb.predict) %>% 
  dplyr::mutate(
    diff = predicted - response, 
    squared_diff = diff^2
  )

mean(predict.actual$squared_diff) # MSE is 0.224764

# For IPTW model 

iptw.predict <- stats::predict(mod.iptw) %>% as.data.frame

iptw.predict$predicted <- iptw.predict$.

predict.actual <- cbind(df, iptw.predict) %>% 
  dplyr::mutate(
    diff = predicted - response, 
    squared_diff = diff^2
  )

mean(predict.actual$squared_diff) # 0.2235477



