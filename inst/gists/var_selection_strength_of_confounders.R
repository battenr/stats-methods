# Title: Strength of Confounders

# Description: When using real-world data, an important aspect is choosing the 
# dataset to use. Not all variables are always available in each data source. 
# For example, eletronic health records vs claims data vs registry data each 
# have strengths and benefits (one of which, is the variables available)

# Setup ----

#... Packages ----

library(tidyverse) # ol' faithful
library(WeightIt) # for IPW
library(broom) # for tidying model output

#... Functions ----

# For simulating data. 

sim_data <- function(n = 250, # sample size 
                     beta_trt = 1.5, # treatment effect
                     # Parameters for Z1 
                     z1_mean = 5, z1_sd = 2, 
                     # Parameters for Z2
                     z2_size = 1, z2_prob = 0.5, 
                     # Parameters for Z3 
                     z3_mean = 2, z3_sd = 1, 
                     # Confounder - Effect on X
                     z1_on_x = 0.05, z2_on_x = 0.2, z3_on_x = 0.1,
                     # Confounder - Effect on Y
                     z1_on_y = 0.5, z2_on_y = 0.3, z3_on_y = 0.4){
  
  # Creating the Dataframe
  
  df <- data.frame(
    z1 = rnorm(n = n, mean = z1_mean, sd = z1_sd), 
    z2 = rbinom(n = n, size = z2_size, prob = z2_prob),
    z3 = rnorm(n = n, mean = z3_mean, sd = z3_sd)
  ) %>% 
    dplyr::mutate(
      prob = plogis(z1_on_x*z1 + z2_on_x*z2 + z3_on_x*z3), 
      x = rbinom(n = n, size = 1, prob = prob), 
      y = beta_trt*x + z1_on_y*z1 + z2_on_y*z2 + z3_on_y*z3 + rnorm(n = n, mean = 0, sd = 1)
    )
  
  # Return the dataframe
  
  return(df)
}

# Setting Seed for Reproducibility ----

set.seed(456)

# Scenario 1: Z1 & Z2 ----

# Inverse probability weighting is used, including confounders Z1 & Z2. 
# For the outcome model, the confounders are also included. 

#... Function ----

# Function used to simulate data, estimate weights and fit outcome model. 
# The value returned is the estimate 

z1_z2 <- function(sample.size){
  
  df <- sim_data(n = sample.size)
  
  ipw <- WeightIt::weightit(x ~ z1 + z2, 
                            data = df, 
                            method = "glm", 
                            estimand = "ATE", 
                            stabilize = TRUE)
  
  out.mod <- glm(y ~ x + z1 + z2, 
                 data = df, 
                 weights = ipw$s.weights)
  
  result <- out.mod %>% 
    broom::tidy() %>% 
    filter(term == "x") %>% 
    select(estimate) #%>% 
  #as.numeric()
  
  return(result)
  
}

#... Repeat 1000 times ----

# 1000 times is arbitrary for this example. 
# Note: there are ways to calculate the appropriate number of iterations required. 

output_list_z1_z2 <- replicate(1000, 
                          z1_z2(sample.size = 250), simplify = FALSE)

# Reformatting and adding columns required for calculating metrics. 

df.out_z1_z2 <- do.call(rbind, output_list_z1_z2) %>% # reformatting
  
  # Adding a new column that will be used to estimate the Monte Carlo SE of the estimate
  mutate(
    bias_for_one = 1.5 - estimate, 
    squared = (bias_for_one - mean(bias_for_one))^2
  )

#... Calculating Bias ---

# See Morris et al. (2019) for details on calculating these

mean(df.out_z1_z2$bias_for_one) # bias  
sqrt(sum(df.out_z1_z2$squared)*(1 / (1000*999))) # Monte Carlo SE of bias (1000 is number of simulations, 999 is n - 1)

# Scenario 2: Z2 & Z3 -----

# Inverse probability weighting is used, including confounders Z2 & Z3. 
# For the outcome model, the confounders are also included. 

#... Function ----

# Function used to simulate data, estimate weights and fit outcome model. 
# The value returned is the estimate 

z2_z3 <- function(sample.size){
  
  df <- sim_data(n = sample.size)
  
  ipw <- WeightIt::weightit(x ~ z2 + z3, 
                            data = df, 
                            method = "glm", 
                            estimand = "ATE", 
                            stabilize = TRUE)
  
  out.mod <- glm(y ~ x + z2 + z3, 
                 data = df, 
                 weights = ipw$s.weights)
  
  result <- out.mod %>% 
    broom::tidy() %>% 
    filter(term == "x") %>% 
    select(estimate) #%>% 
  #as.numeric()
  
  return(result)
  
}

#... Repeat 1000 times ----

# 1000 times is arbitrary for this example. 
# Note: there are ways to calculate the appropriate number of iterations required. 

output_list_z2_z3 <- replicate(1000, 
                               z2_z3(sample.size = 250), simplify = FALSE)

# Reformatting and adding columns required for calculating metrics. 

df.out_z2_z3 <- do.call(rbind, output_list_z2_z3) %>% # reformatting
  
  # Adding a new column that will be used to estimate the Monte Carlo SE of the estimate
  mutate(
    bias_for_one = 1.5 - estimate, 
    squared = (bias_for_one - mean(bias_for_one))^2
  )

#... Calculating Bias ---

# See Morris et al. (2019) for details on calculating these

mean(df.out_z2_z3$bias_for_one) # bias  
sqrt(sum(df.out_z2_z3$squared)*(1 / (1000*999))) # Monte Carlo SE of bias (1000 is number of simulations, 999 is n - 1)

# Scenario 3: Z1 & Z3 ----

# Inverse probability weighting is used, including both confounders. 
# For the outcome model, the predictor is not included. 

#... Function ----

# Function used to simulate data, estimate weights and fit outcome model. 
# The value returned is the estimate 

z1_z3 <- function(sample.size){
  
  df <- sim_data(n = sample.size)
  
  ipw <- WeightIt::weightit(x ~ z1 + z3, 
                            data = df, 
                            method = "glm", 
                            estimand = "ATE", 
                            stabilize = TRUE)
  
  out.mod <- glm(y ~ x + z1 + z3, 
                 data = df, 
                 weights = ipw$s.weights)
  
  result <- out.mod %>% 
    broom::tidy() %>% 
    filter(term == "x") %>% 
    select(estimate) #%>% 
  #as.numeric()
  
  return(result)
  
}

#... Repeat 1000 times ----

# 1000 times is arbitrary for this example. 
# Note: there are ways to calculate the appropriate number of iterations required. 

output_list_z1_z3 <- replicate(1000, 
                               z1_z3(sample.size = 250), simplify = FALSE)

# Reformatting and adding columns required for calculating metrics. 

df.out_z1_z3 <- do.call(rbind, output_list_z1_z3) %>% # reformatting
  
  # Adding a new column that will be used to estimate the Monte Carlo SE of the estimate
  mutate(
    bias_for_one = 1.5 - estimate, 
    squared = (bias_for_one - mean(bias_for_one))^2
  )

#... Calculating Bias ---

# See Morris et al. (2019) for details on calculating these

mean(df.out_z1_z3$bias_for_one) # bias  
sqrt(sum(df.out_z1_z3$squared)*(1 / (1000*999))) # Monte Carlo SE of bias (1000 is number of simulations, 999 is n - 1)
