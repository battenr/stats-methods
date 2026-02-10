# Title: Coverage Probability 

# Description: Demonstrating how coverage probability is useful for causal inference. 
# This code uses the example of calculating variance based on three methods: 
# a) sandwich estimator, b) M-estimation, c) bootstrapping. 
# The coverage probability is calculated for each method then compared. 

# Notes: 
# 1. The bootstrap step may take a while to run
# 2. This code was generated with the help of Gemini
# 3. This code is for demonstrative purposes. The number of simulations,
# boostrap replicates, etc. may be different if using for decision making. 

# Setup ----

#... Libraries ----

library(tidyverse) # ol faithful
library(WeightIt) # for estimating weights and M-estimation
library(sandwich) # for sandwich estimation
library(boot) # for bootstrapping
library(rsample) # for bootstrapping

#... Functions ----

# Simulating Data 

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

# Parameters ----

set.seed(456) # for reproducibility
true_beta <- 1.5 # true effect 
n_sim <- 1000 # number of repetitions

# Robust Sandwich Estimator ----

robust_coverage <- replicate(n_sim, {
  # Simulating Data 
  dat <- sim_data(beta_trt = true_beta)
  # Estimating IP Weights
  W <- weightit(x ~ z1 + z2, data = dat, method = "ps", estimand = "ATE")
  
  # Extracting Necessary Information
  
  fit <- glm(y ~ x, data = dat, weights = W$weights)
  est <- coef(fit)["x"]
  se  <- sqrt(diag(vcovHC(fit, type = "HC0")))["x"]
  
  lower <- est - 1.96 * se
  upper <- est + 1.96 * se
  
  (true_beta >= lower & true_beta <= upper) # checking if within interval
})

# Printing Result

cat("Robust Sandwich Coverage:", mean(robust_coverage), "\n")

# M-Estimation ---

m_est_coverage <- replicate(n_sim, {
  
  # Simulating Data 
  
  dat <- sim_data(beta_trt = true_beta)
  
  # glm_weightit handles the PS model and outcome model simultaneously
  
  fit_m <- glm_weightit(
    y ~ x, 
    data = dat, 
    weightit = weightit(x ~ z1 + z2, data = dat, method = "ps", estimand = "ATE"),
    vcov = "asympt" # for M-estimation
  )
  
  # Extracting Information from Model 
  
  summ <- summary(fit_m)
  est <- coef(fit_m)["x"]
  se  <- summ$coefficients["x", "Std. Error"]
  
  lower <- est - 1.96 * se
  upper <- est + 1.96 * se
  
  # Checking if within interval
  
  (true_beta >= lower & true_beta <= upper)
})

# Printing Result

cat("M-Estimation Coverage:", mean(m_est_coverage), "\n")

# Bootstrapping ----

#... Helper Function  ----

# Need this function for later 

fit_iptw_boot <- function(split) {
  df_boot <- rsample::analysis(split)
  
  # 1. Estimate weights on the resampled data
  w_obj <- weightit(x ~ z1 + z2, data = df_boot, method = "ps", estimand = "ATE")
  
  # 2. Fit the outcome model
  fit <- glm(y ~ x, data = df_boot, weights = w_obj$weights)
  
  # Return just the coefficient for x
  return(coef(fit)["x"])
}

#... Simulation ----

# Adding a note that it may take a while to run the code 

cat("Running Bootstrap Simulation (this will take a while)...\n")

boot_coverage <- replicate(n_sim, {
  dat <- sim_data(beta_trt = true_beta)
  
  # Create bootstrap samples
  boot_samples <- rsample::bootstraps(dat, times = 100)
  
  # Use map_dbl to get the treatment effect for each split (as per your style)
  boot_results <- boot_samples %>%
    mutate(estimate = map_dbl(splits, ~ fit_iptw_boot(.x)))
  
  # Calculate Percentile Confidence Interval
  # (Finding the 2.5th and 97.5th percentiles of the 'estimate' column)
  ci <- quantile(boot_results$estimate, probs = c(0.025, 0.975))
  
  (true_beta >= ci[1] & true_beta <= ci[2])
})

mean(boot_coverage) # calculating coverage

# Printing the result

cat("Bootstrap Coverage:", mean(boot_coverage), "\n")
