# Doubly Robust Estimation ----

# Description: Doubly robust estimation can be helpful for estimating 
# causal effects. This code is demonstrating how there is reduced bias
# compared to IPTW only or using the outcome model only

# Setup ----

#... Packages ----

library(tidyverse)
library(broom)
library(WeightIt)

#... Functions ----

# For simulating data

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

df <- sim_data()


# Outcome Model ----

# Fitting an outcome model. In this case, a generalized linear model. 
# The function is created so that later the process can be repeated 


outcome_model_sim <- function(){
  
  df <- sim_data() # function from above 
  
  mod <- glm(y ~ x + z1 + z2, 
             family = gaussian(link = "identity"),
             data = df)
  
  result <- broom::tidy(mod) %>% 
    filter(term == "x") %>% 
    dplyr::select(
      estimate
    )
  
  return(result)
  
}

# Repeating 1000 times 

output_list <- replicate(1000, outcome_model_sim(), simplify = FALSE)

df.outmod <- do.call(rbind, output_list) # making it a data frame so we can work with it

calcs_for_bias <- df.outmod %>% 
  mutate(
    difference = estimate - 1.5,
    squared = (estimate - mean(estimate))^2
  )

bias.outmod <- mean(calcs_for_bias$difference)

monte.se.bias.outmod <- sqrt(sum(calcs_for_bias$squared)/(1000*(1000-1)))

# IPTW ----

# Function for estimating effects using IPTW

iptw_sim <- function(){
  df <- sim_data() # function from above 
  
  
  psmod <- WeightIt::weightit(x ~ z1 + z1, 
                              method = "glm", 
                              estimand = "ATE",
                              stabilize = TRUE,
                              data = df)
  
  mod <- glm(y ~ x, 
             family = gaussian(link = "identity"),
             weights = psmod$weights,
             data = df)
  
  result <- broom::tidy(mod) %>% 
    filter(term == "x") %>% 
    dplyr::select(
      estimate
    )
  
  return(result)
  
}

# Repeating 1000 times 

output_list <- replicate(1000, iptw_sim(), simplify = FALSE)

df.iptw <- do.call(rbind, output_list) # making it a data frame so we can work with it

calcs_for_bias <- df.iptw %>% 
  mutate(
    difference = estimate - 1.5,
    squared = (estimate - mean(estimate))^2
  )

bias.iptw <- mean(calcs_for_bias$difference)

monte.se.bias.iptw <- sqrt(sum(calcs_for_bias$squared)/(1000*(1000-1)))


# Doubly Robust -----

# Note: Doubly robust methods are a category of methods. This is like saying 
# propensity score Mmthods. For this code, we are focusing on one example. This 
# method is the simplest way to use doubly robust estimation: 
# including covariates in the outcome model (the same covariates that were in 
# the PS model)


dr_sim <- function(){
  df <- sim_data() # function from above 
  
  
  psmod <- WeightIt::weightit(x ~ z1 + z1, 
                              method = "glm", 
                              estimand = "ATE",
                              stabilize = TRUE,
                              data = df)
  
  mod <- glm(y ~ x + z1 + z2, 
             family = gaussian(link = "identity"),
             weights = psmod$weights,
             data = df)
  
  result <- broom::tidy(mod) %>% 
    filter(term == "x") %>% 
    dplyr::select(
      estimate
    )
  
  return(result)
  
}

# Repeating 1000 times 

output_list <- replicate(1000, dr_sim(), simplify = FALSE)

df.dr <- do.call(rbind, output_list) # making it a data frame so we can work with it

calcs_for_bias <- df.dr %>% 
  mutate(
    difference = estimate - 1.5,
    squared = (estimate - mean(estimate))^2
  )

bias.dr <- mean(calcs_for_bias$difference)

monte.se.bias.dr <- sqrt(sum(calcs_for_bias$squared)/(1000*(1000-1)))

# Results -----

# Combining results into one dataframe

data.frame(
  Method = c("IPTW", "Outcome Model", "Doubly Robust"), 
  bias = c(bias.iptw, bias.outmod, bias.dr),
  monte_carlo_se_of_bias = c(monte.se.bias.iptw, monte.se.bias.outmod, monte.se.bias.dr)
)









