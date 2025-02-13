# Title: Doubly Robust Estimation

# Description: Demonstrating how doubly robust estimation can be 
# a useful tool, but has limitations. Specifically by looking at three different 
# scenarios and the resulting bias. 

# This script has three examples: 
# 1. Both models correctly specified
# 2. One of the models correctly specified
# 3. Neither model correctly specified. 

# We are going to use inverse probability of treatment weighting and
# a generalized linear model for the outcome

# Note there is a great paper by Funk et al. if you're interested in learning more

# Setup ----

#... Library ----

library(tidyverse) # ol faithful
library(WeightIt) # for estimating weights 
library(broom) # for tidying results

#... Functions ----

# Simulating Data Function

sim_data <- function(n = 250, # sample size 
                     beta_trt = 1.5, # treatment effect
                     # Parameters for Z1 
                     z1_mean = 5, z1_sd = 2, 
                     # Parameters for Z2
                     z2_size = 1, z2_prob = 0.5, 
                     # Confounder - Effect on X
                     z1_on_x = 0.05, z2_on_x = 0.2,
                     # Confounder - Effect on Y
                     z1_on_y = 0.5, z2_on_y = 0.3){
  
  # Creating the Dataframe
  
  df <- data.frame(
    z1 = rnorm(n = n, mean = z1_mean, sd = z1_sd), 
    z2 = rbinom(n = n, size = z2_size, prob = z2_prob)
  ) %>% 
    dplyr::mutate(
      prob = plogis(z1_on_x*z1 + z2_on_x*z2), 
      x = rbinom(n = n, size = 1, prob = prob), 
      y = beta_trt*x + z1_on_y*z1 + z2_on_y*z2 + rnorm(n = n, mean = 0, sd = 1)
    )
  
  # Return the dataframe
  
  return(df)
}

# Simulating Data ----

set.seed (654) # setting seed for reproducibility

df <- sim_data() # a dataset where there really is an effect

# Scenario 1: Both Models Correctly Specified ----

#... Fitting PS Model ----

psmod1 <- WeightIt::weightit(x ~ z1 + z2, 
                             data = df, 
                             estimand = "ATE", # estimating the ATE
                             stabilize = TRUE) # using stabilized weights

#.... Fitting Outcome Model ----


outmod1 <- glm(y ~ x + z1 + z2, 
               data = df, 
               weights = psmod1$weights,
               )

#... Repeating 1000 times ----

scenario1 <- function(n = 250, beta_trt = 1.5){
  
  df <- sim_data()
  
  psmod1 <- WeightIt::weightit(x ~ z1 + z2, 
                               data = df, 
                               estimand = "ATE",
                               stabilize = TRUE)
  
  outmod1 <- glm(y ~ x + z1 + z2, 
                 data = df, 
                 weights = psmod1$weights,
  )
  
  results <- broom::tidy(outmod1) %>% 
    filter(term == "x") %>% 
    select(term, estimate, std.error) 
  
  return(results)
  
  
}

# Repeat...Repeat....Repeat! ----

# Using the function we will repeat it 1000 times. 

check_1000 <- replicate(1000, scenario1(), simplify = FALSE)

df.out <- do.call(rbind, check_1000) %>%  
  dplyr::mutate(
    difference = estimate - 1.5, # estimating difference between estimated effect and "true" effect
    squared = (difference - mean(difference))^2
  ) 


# Calculating the mean bias and Monte Carlo SE of estimate

# See Morris et al. (2019) for details on calculating these

mean(df.out$difference) # bias  
sqrt(sum(df.out$squared)*(1 / (1000*999))) # Monte Carlo SE of bias (1000 is number of simulations, 999 is n - 1)

# Scenario 2: One Model Correctly Specified ----

# Repeating the same as scenario 1 however in this situation there is only one model that 
# is correctly specified (in this case, the propensity score model)

scenario2 <- function(n = 250, beta_trt = 1.5){
  
  df <- sim_data()
  
  psmod2 <- WeightIt::weightit(x ~ z1 + z2, 
                               data = df, 
                               estimand = "ATE",
                               stabilize = TRUE)
  
  outmod2 <- glm(y ~ x + z2, 
                 data = df, 
                 weights = psmod1$weights,
  )
  
  results <- broom::tidy(outmod2) %>% 
    filter(term == "x") %>% 
    select(term, estimate, std.error) 
  
  return(results)
  
  
}

# Repeat...Repeat....Repeat! ----

# Using the function we will repeat it 1000 times. 

check_1000 <- replicate(1000, scenario2(), simplify = FALSE)

df.out <- do.call(rbind, check_1000) %>%  # formatting it
  dplyr::mutate(
    difference = estimate - 1.5,
    squared = (difference - mean(difference))^2
  ) 


# Calculating the mean bias and Monte Carlo SE of estimate

# See Morris et al. (2019) for details on calculating these

mean(df.out$difference) # bias  
sqrt(sum(df.out$squared)*(1 / (1000*999))) # Monte Carlo SE of bias (1000 is number of simulations, 999 is n - 1)

# Situation 3: Neither Model Correctly Specified ----

# Repeating the same as scenario 1 & 2 however in this situation neither model is 
# correctly specified (i.e., both are wrong)

scenario3 <- function(n = 250, beta_trt = 1.5){
  
  df <- sim_data()
  
  psmod3 <- WeightIt::weightit(x ~ z2, 
                               data = df, 
                               estimand = "ATE",
                               stabilize = TRUE)
  
  outmod3 <- glm(y ~ x + z1, 
                 data = df, 
                 weights = psmod3$weights,
  )
  
  results <- broom::tidy(outmod3) %>% 
    filter(term == "x") %>% 
    select(term, estimate, std.error) 
  
  return(results)
  
  
}

# Repeat...Repeat....Repeat! ----

# Using the function we will repeat it 1000 times. 

check_1000 <- replicate(1000, scenario3(), simplify = FALSE)

df.out <- do.call(rbind, check_1000) %>%  # formatting it
  dplyr::mutate(
    difference = estimate - 1.5,
    squared = (difference - mean(difference))^2
  ) 


# Calculating the mean bias and Monte Carlo SE of estimate

# See Morris et al. (2019) for details on calculating these

mean(df.out$difference) # bias  
sqrt(sum(df.out$squared)*(1 / (1000*999))) # Monte Carlo SE of bias (1000 is number of simulations, 999 is n - 1)


