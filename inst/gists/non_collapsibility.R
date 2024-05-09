# Title: Non-Collapsibility of ORs and HRs

# Setup ----

#... Libraries ----

library(tidyverse)
library(broom)

# RD ----

# Base case

sim_cont <- function(sample.size){
  
  df = data.frame(
    x = rbinom(n = sample.size, size = 1, prob = 0.5),
    z = rnorm(n = sample.size, mean = 5, sd = 2)
    #z = rbinom(n = sample.size, size = 1, prob = 0.3)
  ) %>% 
    mutate(
      y = 3*x + 0.5*z + rnorm(n = sample.size, mean = 0, sd = 1)
     # y = rbinom(n = sample.size, size = 1, prob = c(0.1 + 0.2*x + 0.3*z))
      #y = rbinom(n = sample.size, size = 1, prob = plogis(0.5*x + 0.4*z))
    )
  
  # Marginal Model 
  
  mod_m <- glm(y ~ x, 
               family = gaussian(link = "identity"), 
               data = df)
  
  estimate_m <- broom::tidy(mod_m) %>% 
    filter(term == "x") %>% 
    select(estimate)
  
  # Conditional Estimate
  
  mod_c <- glm(y ~ x + z, 
               family = gaussian(link = "identity"), 
               data = df)
  
  estimate_c <- broom::tidy(mod_c) %>% 
    filter(term == "x") %>% 
    select(estimate)
  
  
  
  result = data.frame(
    estimate_m, 
    estimate_c
  ) %>% 
    rename(
      `Marginal Estimate` = estimate,
      `Conditional Estimate` = estimate.1
    )
  
  return(result)
  
}

output_list <- replicate(1000, sim_cont(sample.size = 1000), simplify = FALSE)

df.out <- do.call(rbind, output_list)

mean(df.out$`Marginal Estimate`)
mean(df.out$`Conditional Estimate`)

# Binary (OR) ----

sim_binary <- function(sample.size){
  
  df = data.frame(
    x = rbinom(n = sample.size, size = 1, prob = 0.5),
    #z = rnorm(n = sample.size, mean = 5, sd = 2)
    z = rbinom(n = sample.size, size = 1, prob = 0.3)
  ) %>% 
    mutate(
      y = rbinom(n = sample.size, size = 1, prob = c(0.1 + 0.2*x + 0.3*z))
      #y = rbinom(n = sample.size, size = 1, prob = plogis(0.5*x + 0.4*z))
    )
  
  # Marginal Model 
  
  mod_m <- glm(y ~ x, 
               family = binomial(link = "logit"), 
               data = df)
  
  estimate_m <- broom::tidy(mod_m) %>% 
    filter(term == "x") %>% 
    select(estimate)
    
  # Conditional Estimate
  
  mod_c <- glm(y ~ x + z, 
               family = binomial(link = "logit"), 
               data = df)
  
  estimate_c <- broom::tidy(mod_c) %>% 
    filter(term == "x") %>% 
    select(estimate)
  
  
  
  result = data.frame(
    estimate_m, 
    estimate_c
  ) %>% 
    rename(
      `Marginal Estimate` = estimate,
      `Conditional Estimate` = estimate.1
    )
  
  return(result)
  
}

output_list <- replicate(1000, sim_binary(sample.size = 1000), simplify = FALSE)

df.out <- do.call(rbind, output_list)

mean(df.out$`Marginal Estimate`)
mean(df.out$`Conditional Estimate`)

df.out

