# Title: Same Method, Different Answers - Causal Estimands

# Description: Demonstrating how you can use the same method (IPW), but end up with
# different answers. Each of them is valid, but each answer is for 
# a different question. 

# This code simulates data for two study designs: 
# - An RCT
# - An observational study (RWE)

# This code demonstrates how for an RCT they will be similar, but
# not for an observational study. 

# Setup ----

#... Packages ----

library(tidyverse) # ol faithful
library(WeightIt) # for IPW 
library(broom) # for tidying outputs

#... Functions ----

# Title: Simulating Data Skeleton

# Description: Skeleton for Simulating Data 

sim_data <- function(n = 200, # sample size 
                     beta_trt = 1.5, # treatment effect
                     # Parameters for Z1 
                     z1_mean = 5, z1_sd = 3, 
                     # Parameters for Z2
                     z2_size = 1, z2_prob = 0.5, 
                     # Parameters for Z3
                     z3_mean = 10, z3_sd = 2,
                     # Confounder - Effect on X
                     z1_on_x = 0.5, z2_on_x = 0.2, z3_on_x = 0.02,
                     # Confounder - Effect on Y
                     z1_on_y = 1.5, z2_on_y = 2, z3_on_y = 2.5){
  
  # Creating the Dataframe
  
  df <- data.frame(
    z1 = rnorm(n = n, mean = z1_mean, sd = z1_sd), 
    z2 = rbinom(n = n, size = z2_size, prob = z2_prob),
    z3 = rnorm(n = n, mean = z3_mean, sd = z3_sd) 
  ) %>% 
    dplyr::mutate(
      prob = plogis(z1_on_x*z1 + z2_on_x*z2 + z3_on_x*z3), 
      x_rct = rbinom(n = n, size = 1, prob = 0.5), 
      x_rwe = rbinom(n = n, size = 1, prob = prob), 
      y_rct = beta_trt*x_rct + z1_on_y*z1 + z2_on_y*z2 + z3_on_y*z3 + rnorm(n = n, mean = 0, sd = 1),
      y_rwe = beta_trt*x_rwe + z1_on_y*z1 + z2_on_y*z2 + z3_on_y*z3 + rnorm(n = n, mean = 0, sd = 1)
    )
  
  # Return the dataframe
  
  return(df)
}

# Simulating Data ----

set.seed(456) # setting seed for reproducibility

df <- sim_data() # creating a dataset 

# RCT ----

# Fitting a model using weighting for each estimand. 
# For the outcome model including the covariates used in the PS model 

estimands <- c("ATE", "ATT", "ATC", "ATO") # causal estimands that we'll estimate

# Fitting models for each estimand

results_rct <- map_dfr(estimands, function(e) {
  ps.mod <- weightit(x_rct ~ z1 + z2 + z3,
                     data = df,
                     method = "glm",
                     estimand = e)
  
  tidy_result <- glm_weightit(
    y_rct ~ x_rct + z1 + z2,
    data = df,
    weightit = ps.mod
  ) %>%
    tidy() %>%
    filter(term == "x_rct") %>%
    mutate(
      estimand = e,
      lower_ci = round(estimate - 1.96 * std.error, 2),
      upper_ci = round(estimate + 1.96 * std.error, 2),
      estimate = round(estimate, 2),
      result = paste0(estimate, " 95% CI: ", lower_ci, " to ", upper_ci)
    ) %>%
    select(estimand, result)
  
  tidy_result
})

print(results_rct)

# RWE ----

# Fitting a model using weighting for each estimand. 
# For the outcome model including the covariates used in the PS model 

estimands <- c("ATE", "ATT", "ATC", "ATO") # causal estimands that we'll estimate

# Fitting models for each estimand


estimands <- c("ATE", "ATT", "ATC", "ATO")

results_rwe <- map_dfr(estimands, function(e) {
  ps.mod <- weightit(x_rwe ~ z1 + z2 + z3,
                     data = df,
                     method = "glm",
                     estimand = e)
  
  tidy_result <- glm_weightit(
    y_rwe ~ x_rwe + z1 + z2,
    data = df,
    weightit = ps.mod
  ) %>%
    tidy() %>%
    filter(term == "x_rwe") %>%
    mutate(
      estimand = e,
      lower_ci = round(estimate - 1.96 * std.error, 2),
      upper_ci = round(estimate + 1.96 * std.error, 2),
      estimate = round(estimate, 2),
      result = paste0(estimate, " 95% CI: ", lower_ci, " to ", upper_ci)
    ) %>%
    select(estimand, result)
  
  tidy_result
})

print(results_rwe)


