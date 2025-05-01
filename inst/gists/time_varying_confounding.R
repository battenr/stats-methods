# Title: Time-Varying Confounding (MSM vs IPTW)

# Description: Demonstrating how marginal structural models provide 
# less biased estimates compared to using IPTW at baseline only, when 
# there is time-varying confounding. 

# Setup ----

#... Libraries ----

library(tidyverse) # ol faithful
library(WeightIt) # for creating weights
library(geepack) # for using GEEs
library(broom) # for cleaning outputs from models 

# Function for Simulating Data ----

# Note for transparency: 

# I wrote code for baseline, and used ChatGPT to help assist with: 
# - adding multiple timepoints
# - turning this into a function 

simulate_data_and_model <- function(n = 500, Tmax = 5) {
  # Tmax is the total number of timepoints. For simplicity, 
  # included that each individual had the same number of timepoints. 

  # Initialize a list. This will be used to add the simulated data
  data_list <- list()
  
  # Simulate data for each individual
  
  # This for loop is for each individual. The code works by running through each timepoint
  # one individual at a time. Then it moves on to the next individual. 
  
  for (i in 1:n) {
    # Baseline values (at time 0)
    # The rationale for using the term _prev, is the "current" timepoint will become the 
    # "previous" for the next timepoint. The term baseline only applies to the first
    # measurement. 
    
    sleep_prev <- runif(n = 1, min = 2, max = 10)
    weightlifting_prev <- rbinom(n = 1, size = 1, prob = 0.4)
    coffee_prev <- rbinom(n = 1, size = 1, prob = plogis(0.5 * weightlifting_prev - 0.1 * sleep_prev))
    happiness_prev <- 1.5 * coffee_prev + 0.5 * sleep_prev + 2 * weightlifting_prev + rnorm(n = 1, mean = 0, sd = 1)
    
    # Storing this data, since this will be the baseline values. 
    
    data_list[[length(data_list) + 1]] <- data.frame(
      id = i,
      time = 0,
      sleep = sleep_prev,
      weightlifting = weightlifting_prev,
      coffee = coffee_prev,
      happiness = happiness_prev
    )
    
    # Loop through time points (from )
    for (t in 1:Tmax) {
      
      # Simulate time-varying confounders and exposure
      sleep_curr <- runif(n = 1, min = 2, max = 10) + 1.5 * weightlifting_prev - 0.5 * coffee_prev
      weightlifting_curr <- rbinom(n = 1, size = 1, prob = plogis(0.4 * sleep_prev + 0.3 * coffee_prev))
      coffee_curr <- rbinom(n = 1, size = 1, prob = plogis(0.5 * weightlifting_curr - 0.1 * sleep_curr))
      happiness_curr <- 1.5 * coffee_curr + 0.5 * sleep_curr + 2 * weightlifting_curr + rnorm(n = 1, mean = 0, sd = 1)
      
      # Store the simulated data for this individual at time t
      data_list[[length(data_list) + 1]] <- data.frame(
        id = i,
        time = t,
        sleep = sleep_curr,
        weightlifting = weightlifting_curr,
        coffee = coffee_curr,
        happiness = happiness_curr
      )
      
      # Update previous values for the next time point
      sleep_prev <- sleep_curr
      weightlifting_prev <- weightlifting_curr
      coffee_prev <- coffee_curr
      happiness_prev <- happiness_curr
    }
  }
  
  # Combine into a long-format data frame
  df <- do.call(rbind, data_list)
  
  # Weighting at baseline. Here, using IPTW (through the WeightIt package to create weights). 
  # This is done assuming that the confounders don't change over time (which we know is wrong)
  wghts_bl <- WeightIt::weightit(
    coffee ~ sleep + weightlifting, 
    data = df %>% filter(time == 0),
    stabilize = TRUE, # want stabilized weights
    estimand = "ATE"
  )
  
  # Creating Dataframe with IDs and weights
  
  wghts_bl <- data.frame(
    id = unique(df$id),
    wght = wghts_bl$weights
  )
  
  # Adding weights the the dataframe 
  
  df_ipw <- df %>% 
    left_join(wghts_bl, by = "id")
  
  # Fit the model with IPW at baseline
  mod_ipw <- geeglm(happiness ~ coffee, 
                    data = df_ipw,
                    id = id,
                    weights = wght)
  
  # MSM Weights. 
  # These are similar to IPTW, however the weights change over time 
  
  msm_weights <- WeightIt::weightit(
    coffee ~ sleep + weightlifting, 
    data = df,
    stabilize = TRUE
  )
  
  wghts_msm <- data.frame(
    id = df$id,
    time = df$time, 
    wght = msm_weights$weights
  )
  
  df_msm <- df %>% 
    left_join(wghts_msm, by = c("id", "time"))
  
  # Fit the model with MSM weights
  mod_msm <- geeglm(happiness ~ coffee, 
                    data = df_msm,
                    id = id,
                    weights = wght)
  
  # Return the estimates. Later these will be used to calculate bias and Monte Carlo SE of bias. 
  results = data.frame(
    mod_ipw = broom::tidy(mod_ipw) %>% filter(term == "coffee") %>% select(estimate) %>% rename(ipw_estimate = estimate),
    mod_msm = broom::tidy(mod_msm) %>% filter(term == "coffee") %>% select(estimate) %>% rename(msm_estimate = estimate)
  )
  
  return(results)
}

# Repeat, Repeat, Repeat Again! ----

# Here we are repeating 1000 times. A couple of notes: 
# - 1000 is arbitrary. In reality, there are ways to determine the number of repeitions required
# - This code may take a while to run. If interested in a smaller example, try changing the 
# number of reptitions to a smaller number. 

set.seed(456) # setting seed for reproducibility

output_list <- replicate(1000, # this is number of repeitions (make smaller if you want quicker run time)
                         simulate_data_and_model(), 
                         simplify = FALSE)

# Calculating Metrics ----

# Using bias for metric. 

# These formulas are from Morris et al. 2019. 

df.out <- do.call(rbind, output_list) %>% 
  mutate(
    bias_ipw = 1.5 - ipw_estimate, # it's 1.5 because that's what we defined the treatment effect as 
    bias_msm = 1.5 - msm_estimate, 
    squared_ipw = (bias_ipw - mean(bias_ipw)) ^2, # will be used later 
    squared_msm = (bias_msm - mean(bias_msm)) ^2 # will be used later
  )

#... Calculating Bias ----

mean(df.out$bias_ipw)
mean(df.out$bias_msm)

#... Monte Carlo SE of Estimate ----

sqrt(sum(df.out$squared_ipw)*(1 / (1000*999))) # Monte Carlo SE of bias (1000 is number of simulations, 999 is n - 1)

sqrt(sum(df.out$squared_msm)*(1 / (1000*999))) # Monte Carlo SE of bias (1000 is number of simulations, 999 is n - 1)
