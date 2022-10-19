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

# n.obs = 593
n.obs = 100

df = data.frame(
 id = sample(x = 1:n.obs, size = n.obs, replace = FALSE),
 age = runif(n= n.obs, min = 5, max = 22),
 sex = rbinom(n = n.obs, size = 1, prob = 0.81), # 1 = female
 costume = sample( x = c("avocado","bumblebee", "pumpkin", "lion"),
                   size = n.obs, 
                   replace = TRUE, 
                   prob = c(0.40, 0.30, 0.20, 0.10)
                   ),
 candy_amount = runif(n = n.obs, min = 20, max = 100), # number of pieces of candy
 candy_type = sample (x = c("skittles", "popeye sticks", "candy corn", "lemon heads", "M&Ms"),
                      size = n.obs, 
                      replace = TRUE, 
                      prob = c(0.15, 0.30, 0.15, 0.20, 0.20)
                      )# what is the most common type of candy you have 
) %>% 
  dplyr::mutate(
    rho = 1, 
    beta_age = 0.01, 
    beta_sex = 0.2, 
    beta_costume = dplyr::case_when(
      costume == "avocado" ~ 0.01, 
      costume == "bumblebee" ~ 0.03, 
      costume == "pumpkin" ~ 0.05, 
      costume == "lion" ~ 0.02),
    beta_ca = 0.005,
    beta_candy_type = dplyr::case_when(
      # Note for self: the bigger the value, the quicker the event (i.e., quicker person eats candy)
      candy_type == "skittles" ~ 0.1, 
      candy_type == "popeye sticks" ~ 0.05, 
      candy_type == "candy corn" ~ 0.00003, 
      candy_type == "lemon heads" ~ 0.020,
      candy_type == "M&Ms" ~ 0.20),
    
    
    lambda = 0.01, 
    rateC = 0.001, 
    
    x_beta = beta_age*age + beta_sex*sex + beta_costume + beta_ca*candy_amount + beta_candy_type, 
    lambda_wiki = lambda^(-1/rho),
    lambda_prime = lambda_wiki/exp(x_beta/rho), 
    Tlat = rweibull(n = n.obs, shape = rho, scale = lambda_prime), 
    
    C = rexp(n = n.obs, rate = rateC),
    time = pmin(Tlat, C), 
    status = as.numeric(Tlat <= C)
  )

survminer::ggsurvplot(survfit(Surv(time, status) ~ candy_type, 
                              data = df), 
                      data = df, 
                      risk.table = TRUE)

survminer::ggsurvplot(survfit(Surv(time, status) ~ 1, 
                              data = df), 
                      data = df, 
                      risk.table = TRUE)

# Plot of Data ----

ggplot(data = df) + 
  geom_col(aes(x = time, y = id), width  = 3) + 
  geom_point(aes(x = time, y = id, shape = as.character(status))) +
  coord_flip()

# Barbell Plot ----

df.50id <- data.frame(
  id = sample(1:n.obs, size = 100)
)

df.50 <- df.50id %>% 
  dplyr::left_join(
    df, 
    by = "id" 
  )

ggplot(data = df.50) + 
  geom_segment(aes(x = time, y = id,
               xend = status, yend = id)) +
  geom_col(aes(y = time, y = id, color = status))


ggplot(data = df.50) + 
  geom_col(aes(x = time, y = id), width  = 0.5) + 
  geom_point(aes(x = time, y = id, shape = as.character(status))) +
  coord_flip()
  

?geom_col

# Fitting Cox PH Model ----

fit.coxph <- survival::coxph(Surv(time, status) ~ age + sex + costume + candy_type + candy_amount, 
                             data = df)

fit.coxph$coefficients

summary(fit.coxph)

# KM Curve ----

#... Overall ----

fit <- survfit(Surv(time, status) ~ 1, 
               data = df)

survminer::ggsurvplot(survfit(Surv(time, status) ~ 1, 
                              data = df), 
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

#... Stratified by Candy Type ----

# trying to make it so that candy corn lasts longer (i.e., people don't want to eat it)

fit <- survfit(Surv(time, status) ~ candy_type, 
               data = df)

survminer::ggsurvplot(fit, 
                      data = df, 
                      risk.table = TRUE)

print(cox.zph(fit.coxph))
plot(cox.zph(fit.coxph))
