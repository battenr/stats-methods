# Title: Stepwise Selection is a Bad Idea for Causal Inference 

# Description: Demonstrating how using stepwise selection can lead to 
# a biased estimate when the goal is causal inference

# Setup ----

#... Libraries ----

library(tidyverse) # ol' faithful
library(broom) # for tidying output
library(MASS) # for stepwise selection (stepAIC)

#... Functions ----

# Title: Simulating Data Skeleton

# Description: Skeleton for Simulating Data 

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
      y = beta_trt*x + z1_on_y*z1 + z2_on_y*z2 + rnorm(n = n, mean = 0, sd = 1),
      c1 = rbinom(n = n, size = 1, prob = plogis(x + y)), # collider 1
      c2 = rnorm(n = n, mean = x + y, sd = 3) # collider 2 
    ) 
  
  # Return the dataframe
  
  return(df)
}

# Setting Seed ----

set.seed(456) # setting seed for reproducibility

# Stepwise Selection ----

# Using stepwise selection and the output from this model to 
# get the estimated treatment effect 

step_selection <- function(sample.size){
  
  # See above for notes on detail of variables
  
  #sample.size = 250
  
  trt_effect = 1.5 # same as that in the sim_data() function
  
  df <- sim_data()
  
  # Stepwise selection
  
  step_mod <- MASS::stepAIC(glm(y ~ x + z1 + z2 + c1 + c2, 
                             data = df),
                      direction = "both")
  
  
  expected_value <- broom::tidy(step_mod)$estimate[2]
  
  bias_for_one = expected_value - trt_effect
  
  df_bias = data.frame(
    bias_for_one
  )
  
  return(df_bias)
  
}

# Repeating 1000 times using a sample size of 1000

output_list <- replicate(1000, step_selection(sample.size = 250), simplify = FALSE)

df.out <- do.call(rbind, output_list) %>% # reformatting
  
  # Adding a new column that will be used to estimate the Monte Carlo SE of the estimate
  mutate(
    squared = (bias_for_one - mean(bias_for_one))^2
  )


# Calculating the mean bias and Monte Carlo SE of estimate

# See Morris et al. (2019) for details on calculating these

mean(df.out$bias_for_one) # bias  
sqrt(sum(df.out$squared)*(1 / (1000*999))) # Monte Carlo SE of bias (1000 is number of simulations, 999 is n - 1)

# Using a DAG ----

# Repeating process as for stepwise selection but using a DAG. 
# By using a DAG, we know we shouldn't adjust for c1 & c2 (only z1 & z2)

dag_selection <- function(sample.size){
  
  # See above for notes on detail of variables
  
  #sample.size = 250
  
  trt_effect = 1.5 # same as that in the sim_data() function
  
  df <- sim_data()
  
  dag_mod <- glm(y ~ x + z1 + z2, 
                          data = df)
  
  
  expected_value <- broom::tidy(dag_mod)$estimate[2]
  
  bias_for_one = expected_value - trt_effect
  
  df_bias = data.frame(
    bias_for_one
  )
  
  return(df_bias)
  
}

# Repeating 1000 times using a sample size of 1000

output_list <- replicate(1000, dag_selection(sample.size = 250), simplify = FALSE)

df.out <- do.call(rbind, output_list) %>% # reformatting
  
  # Adding a new column that will be used to estimate the Monte Carlo SE of the estimate
  mutate(
    squared = (bias_for_one - mean(bias_for_one))^2
  )


# Calculating the mean bias and Monte Carlo SE of estimate

# See Morris et al. (2019) for details on calculating these

mean(df.out$bias_for_one) # bias  
sqrt(sum(df.out$squared)*(1 / (1000*999))) # Monte Carlo SE of bias (1000 is number of simulations, 999 is n - 1)
