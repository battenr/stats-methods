# Title: TTE Halloween

# Description: Simulating a dataset for teaching about time to event analyses 
#              using halloween as an example and how long it takes to eat 
#              all of your halloween candy (including your parents eating it too)
#              

# Notes: 
#       censoring is last time you knew your candy was there (before your parents ate it)
#       not censored means you got to eat all your candy

# Setup ----

#... Libraries ----

library(tidyverse)
library(survival)
library(survminer)

#... Dependencies ----

# Data ----

set.seed(10112022) # October 11, 2022

n.obs = 593

df = data.frame(
 age = runif(n= n.obs, min = 5, max = 22),
 sex = rbinom(n = n.obs, size = 1, prob = 0.81), # 1 = female
 costume = sample( x = c("avocado","bumblebee", "pumpkin", "lion"),
                   size = n.obs, 
                   replace = TRUE, 
                   prob = c(0.40, 0.30, 0.20, 0.10)
                   ),
 candy_amount = runif(n = n.obs, min = , max = 2), # number of pieces of candy
 candy_type = sample (x = c("skittles", "reeses cup", "m and ms", "starburst", "hot tamales"),
                      size = n.obs, 
                      replace = TRUE, 
                      prob = c(0.15, 0.30, 0.15, 0.20, 0.20)
                      )# what is the most common type of candy you have 
) %>% 
  dplyr::mutate(
    rho = 1, 
    beta_age = 0.1, 
    beta_sex = 0.2, 
    beta_costume = dplyr::case_when(
      costume == "avocado" ~ 0.30, 
      costume == "bumblebee" ~ 0.15, 
      costume == "pumpkin" ~ 0.20, 
      costume = "lion" ~ 0.1),
    beta_ca = 0.05,
    beta_candy_type = dplyr::case_when(
      costume == "avocado" ~ 0.30, 
      costume == "bumblebee" ~ 0.15, 
      costume == "pumpkin" ~ 0.20, 
      costume = "lion" ~ 0.1),
    
    
    lambda = 0.01, 
    rateC = 0.001, 
    
    x_beta = beta_age*age + beta_sex*sex + beta_costume + beta_ca*candy_amount + beta_candy, 
    lambda_wiki = lambda^(-1/rho),
    lambda_prime = lambda_wiki/exp(x_beta/rho), 
    Tlat = rweibull(n = n.obs, shape = rho, scale = lambda_prime), 
    
    C = rexp(n = n.obs, rate = rateC),
    time = pmin(Tlat, C), 
    status = as.numeric(Tlat <= C)
  )

# Fitting Cox PH Model ----

fit.coxph <- survival::coxph(Surv(time, status) ~ age + sex, 
                             data = df)

fit.coxph$coefficients

summary(fit.coxph)

# KM Curve ----

#... Overall ----

fit <- survfit(Surv(time, status) ~ 1, 
               data = df)

survminer::ggsurvplot(fit, 
                      data = df, 
                      risk.table = TRUE)

#... Stratified by Sex ----

fit <- survfit(Surv(time, status) ~ sex, 
               data = df)

survminer::ggsurvplot(fit, 
                      data = df, 
                      risk.table = TRUE)

# Using Failure Probability (i.e., like death) instead of Survival Probability ----

survminer::ggsurvplot(fit, 
                      data = df, 
                      risk.table = TRUE,
                      fun = "event")

