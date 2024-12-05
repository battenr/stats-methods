# Title: Maximum Likelihood Estimate ----

# Description: Explaining what exactly a maximum likelihood estimate is. 
# The likelihood is the probability of it occuring. Basically "how
# likely is it"

# Setup ----

library(tidyverse)

# Showing how to tell what a likelihood estimate is 

age <- rnorm(n = 100, mean = 60, sd = 10)

likelihood <- dnorm(age, mean = 60, sd = 10)

max(likelihood)

# Using weight example from

# https://online.stat.psu.edu/stat415/lesson/1/1.2

value = c(115, 122, 130, 
          127, 149, 160,
          152, 138, 149,
          180)

value_grid <- seq(from = 100, to = 200, by = .5)

likelihood <- dnorm(value_grid, mean = 100, sd = 20)

max(likelihood)

sum(value*max(likelihood))/10


log_likelihood = function(b0, b1){
  # Use the following x and y values
  x = c(4, 0, 3, 4, 7, 0, 0, 3, 0, 2)
  y = c(53, 56, 37, 55, 50, 36, 22, 75, 37, 42)
  
  # Compute the yhat and residuals based on the two input values
  yhats = b0 + b1*x
  errors = y - yhats
  
  # Compute the sd of the residuals
  sigma = sd(errors)
  
  # Compute the log-likelihood
  log_lik = sum(dnorm(errors, mean = 0, sd = sigma, log = TRUE))
  
  # Output the log-likelihood
  return(log_lik)
}



likelihood <- dnorm(wght, mean = 100, sd = 10)

max(likelihood)

qnorm(0.0129)

# Plot of likelihood

# Working Notes ----

# From Statistical Rethinking

# Grid method

p_grid <- seq(from = 0, to = 1, length.out = 20) # our actual data. not a probability necessarily 
# could use a mean or draw from continuous values 

p_grid

prior <- rep(1, 20) # so our prior is just 1 

likelihood <- dbinom(6, size = 9, prob = p_grid) # calculate the likelihood at each value of the prior
# so this is the probability whereas rbinom() would give the

mean(likelihood) # maximum likelihood estimate. It's the "peak" of the likelihood
median(likelihood) # maximum likelihood estimate. 

# Both the mean and median are shitty at finding the maximum likelihood estimate. This is 
# why a GLM, doesn't use these to find it. Instead

# insert how to find the peak of this. 

# We can clearly see from the graph we'd want the maximum to be something ~ 0.25

max(likelihood) # much much better. 
