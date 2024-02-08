# Want to see about using bootstrapping with TTE outcomes (or sandwich estimators)

library(tidyverse)

# library(simsurv)

# Bender et al. is a fantastic resource for simulating TTE data 

# Simulating Data ----

# Using base R for now, but can use simsurv package instead if I want too 






# Set the parameters for simulation
set.seed(123) # for reproducibility
n <- 100  # number of patients
beta <- c(-2, 0.5)  # coefficients including intercept and treatment effect
covariates <- data.frame(
  trt = rbinom(n, 1, 0.5)  # treatment assignment (0 or 1)
)

# Simulate event times
linear_predictors <- beta[1] + beta[2] * covariates$trt
rate <- exp(linear_predictors)  # Convert to rate parameter for the exponential distribution
time <- rexp(n, rate)

# Simulate censoring times, assuming a censoring rate
censoring <- rexp(n, rate = 0.1)

# Determine the observed time and status (event or censored)
observed_time <- pmin(time, censoring)
status <- as.numeric(time <= censoring)  # 1 if event, 0 if censored

# Create the final dataset
surv_data_base <- data.frame(
  time = observed_time,
  status = status,
  trt = covariates$trt
)




# FROM CHATGPT ----

library(survival)
library(boot)

# Function to simulate survival data under PH assumption
simulate_PH_data <- function(n, beta, lambda, confounders) {
  # Simulating confounders
  X <- matrix(rnorm(n * confounders), ncol = confounders)
  
  # Simulating baseline hazards
  T <- rexp(n, rate = lambda * exp(X %*% beta))
  
  # Censoring (for simplicity, we'll assume a 50% censoring rate)
  C <- runif(n, 0, 2 * max(T))
  delta <- as.numeric(T <= C)
  T <- pmin(T, C)
  
  # Return a data frame
  data.frame(time = T, status = delta, X)
}

# Function to simulate survival data under non-PH assumption
simulate_nonPH_data <- function(n, beta, lambda, confounders, time_effect) {
  X <- matrix(rnorm(n * confounders), ncol = confounders)
  
  # Time-varying effect
  T <- rexp(n, rate = lambda * exp(X %*% beta))
  T <- T * (1 + time_effect * log(T))
  
  C <- runif(n, 0, 2 * max(T))
  delta <- as.numeric(T <= C)
  T <- pmin(T, C)
  
  data.frame(time = T, status = delta, X)
}

# Function for fitting a Cox model and computing robust SE with bootstrapping
cox_model_boot <- function(data, indices) {
  # Fit the model to the bootstrapped sample
  d <- data[indices,] # Bootstrapped dataset
  fit <- coxph(Surv(time, status) ~ X1 + X2, data = d, cluster = d$cluster)
  
  # Return the coefficients
  coef(fit)
}

# Simulate data under PH assumption
set.seed(123)
ph_data <- simulate_PH_data(100, beta = c(0.5, -0.5), lambda = 0.1, confounders = 2)

# Simulate data under non-PH assumption with a time-varying effect
nonph_data <- simulate_nonPH_data(100, beta = c(0.5, -0.5), lambda = 0.1, confounders = 2, time_effect = 0.02)

# Bootstrapping with 1000 resamples
set.seed(123)
boot_ph <- boot(ph_data, cox_model_boot, R = 1000)
boot_nonph <- boot(nonph_data, cox_model_boot, R = 1000)

# Compute robust SE using the boot objects
robust_se_ph <- sqrt(diag(vcov(boot_ph)))
robust_se_nonph <- sqrt(diag(vcov(boot_nonph)))

# Output the robust SE for both PH and non-PH models
list(
  PH_robust_SE = robust_se_ph,
  NonPH_robust_SE = robust_se_nonph
)
