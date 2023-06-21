# Title: Simulating Data 

# Description: Playing around with ways to improve the speed of simulating data 

# Setup ----

#... Libraries ----

library(tidyverse)

#... Functions ----

# Load all functions

lapply(list.files("R/"), function(x) source(paste0("R/", x)))

#... Dependencies ----

# Simulated Data ----


# From ChatGPT ----

generate_data <- function(n1, n2, n_obs) {
  # Generate continuous covariates
  cont_covariates <- matrix(rnorm(n_obs * n1), nrow = n_obs)
  
  # Generate binary covariates
  bin_covariates <- matrix(rbinom(n_obs * n2, size = 1, prob = 0.5), nrow = n_obs)
  
  # Combine the covariates
  covariates <- cbind(cont_covariates, bin_covariates) 
  
  # Return the covariate matrix
  return(covariates)
}

# Define the number of covariates and observations
n1 <- 5   # number of continuous covariates
n2 <- 3   # number of binary covariates
n_obs <- 100   # sample size

# Define the number of repetitions
n_reps <- 100

# Generate the data
data_list <- replicate(n_reps, generate_data(n1, n2, n_obs), simplify = FALSE)

# Combine the data into a single matrix
data_matrix <- do.call(rbind, data_list)
