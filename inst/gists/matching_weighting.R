# Title: Matching vs Weighting 

# Description: Demonstrating how matching and weighting can estimate 
# different things. IPTW can also estimate ATT. The goal is to demonstrate how they 
# may give different answers

# Setup ----

#... Library ----

library(tidyverse) # ol faithful
library(WeightIt) # for estimating weights 
library(MatchIt) # for matching 
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

# Matching ----

#... Fitting PS Model ----

# Note: there are not enough controls to match all the treated subjects. This can be changed when simulating the data

matchmod <- MatchIt::matchit(x ~ z1 + z2, 
                             data = df,
                             distance = "glm", 
                             link = "logit",
                             estimand = "ATT")

#... Matched Data ----

matchdat <- match.data(matchmod)

#.... Fitting Outcome Model ----

outmod1 <- glm_weightit(y ~ x, 
                        data = matchdat, 
                        weights = weights,
)

broom::tidy(outmod1) # tidying the outputs of the model

# Weighting ----

#... Fitting PS Model ----

wmod <- WeightIt::weightit(x ~ z1 + z2, 
                           data = df,
                           method = "glm",
                           estimand = "ATE") # intentionally estimating different estimand

#.... Fitting Outcome Model ----

outmod2 <- glm_weightit(y ~ x, 
                        data = df, 
                        weights = wmod$weights,
)

broom::tidy(outmod2)

# Result ----

# Note: this is from running this one time. If we were to actually compare results, 
# we would need to run more simulations (1000, 10,000, etc)

# Matching: 

broom::tidy(outmod1)

# Weighting:

broom::tidy(outmod2)