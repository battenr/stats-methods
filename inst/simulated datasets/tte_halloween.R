# Title: TTE Halloween

# Description: Simulating a dataset for teaching about time to event analyses 
#              using halloween as an example and how long it takes to eat 
#              all of your halloween candy
#              

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
 sex = rbinom(n = n.obs, size = 1, prob = 0.81) # 1 = female
) %>% 
  dplyr::mutate(
    rho = 1, 
    beta_age = 0.1, 
    beta_sex = 0.2, 
    beta_costume = dplyr::case_when(
      
    ).
    
    lambda = 0.01, 
    rateC = 0.001, 
    
    x_beta = beta_age*age + beta_sex*sex, 
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

