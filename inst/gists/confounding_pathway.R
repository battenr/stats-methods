# Title: Confounding as a Pathway

# Description: Confounding is sometimes mixed with the term confounder. This
# code demonstrates how we can adjust for either variable on a confounding pathway 
# to reduce bias (assuming there are no colliders)

# Setup ---- 

#... Libraries ----

library(tidyverse) # ol' faithful
library(ggdag) # used for creating the DAG in R
library(broom) # for tidying model outputs

#... Functions ----

# Simulating Data

# This will be used throughout the code to generate data that we can use to 
# fit different models and compare. 

sim_data <- function(sample.size = 250, 
                     trt.effect = 2){
  df <- data.frame(
    hours_sleep = rnorm(n = sample.size, mean = 8, sd = 2) # continuous for hours of sleep. assuming follows normal distribution
  ) %>% 
    dplyr::mutate( 
      lifting = rbinom(n = sample.size, size = 1, prob = plogis(0.1 + 0.1*hours_sleep)), # binary for lifting (yes/no)
      coffee = rbinom(n = sample.size, size = 1, prob = plogis(0.2*hours_sleep)), # binary for coffee
      happy = 5 + trt.effect*coffee + 1.5*lifting # continuous for happiness
    )
}

# DAG ----

theme_set(theme_dag()) # setting the theme of the plot 

# Creating the DAG to guide simulating data

dag = ggdag::dagify(
  happy ~ coffee + lifting,
  lifting ~ sleep,
  coffee ~ sleep,
  exposure = "coffee",
  outcome = "happy"
)

# Viewing the DAG

dag |> 
  ggdag::ggdag(
    layout = "nicely")

# Scenarios ----

# Using the function above (based on the DAG), we can simulate three scenarios: 
# 1) Adjusting for sleep 
# 2) Adjusting for lifting
# 3) Adjusting for sleep and lifting

set.seed(456) # for reproducibility

# For this, we will repeat each scenario 1000 times. Note: there are much better ways to 
# choose the number of simulations required. For this purpose we'll use 1000 but 
# it's an aribitrary number. 

# Adjusting for Sleep ----

# The below code generates data for each of the variables, then fits a generalized linear model. 
# The effect estimate from this model is then compared to the "true" treatment effect. For this example, 
# a GLM was used, however there are other adjustment techniques that could be explored too (such as IPW). 

adjust_sleep <- function(sample.size){
  
  trt_effect = 2
  
  df <- sim_data(sample.size = sample.size)
  
  # Fitting the model but only adjusting for hours of sleep
  
  mod <- glm(happy ~ coffee + hours_sleep, 
             family = gaussian(link = "identity"), 
             data = df)
  
  expected_value <- broom::tidy(mod)$estimate[2] # this is the estimate
  
  bias_for_one = expected_value - trt_effect # comparing the estimate to the "true" effect 
  
  df_bias = data.frame(
    bias_for_one
  )
  
  return(df_bias)
  
}

# Repeating 1000 times using a sample size of 250

output_list <- replicate(1000, adjust_sleep(sample.size = 250), simplify = FALSE)


df.out <- do.call(rbind, output_list) %>% # reformatting
  
  # Adding a new column that will be used to estimate the Monte Carlo SE of the estimate
  mutate(
    squared = (bias_for_one - mean(bias_for_one))^2
  )


# Calculating the mean bias and Monte Carlo SE of estimate

# See Morris et al. (2019) for details on calculating these

mean(df.out$bias_for_one) # bias  
sqrt(sum(df.out$squared)*(1 / (1000*999))) # Monte Carlo SE of bias (1000 is number of simulations, 999 is n - 1)

# Adjusting for Lifting ----

adjust_lifting <- function(sample.size){
  
  trt_effect <- 2 # what we want the effect to be on the outcome. Basically what effect coffee
  # has 
  
  df <- sim_data(sample.size = sample.size)
  
  # Fitting the model but only adjusting for lifting
  
  mod <- glm(happy ~ coffee + lifting, 
             family = gaussian(link = "identity"), 
             data = df)
  
  expected_value <- broom::tidy(mod)$estimate[2] # this is the estimate
  
  bias_for_one = expected_value - trt_effect # comparing the estimate to the "true" effect 
  
  df_bias = data.frame(
    bias_for_one
  )
  
  return(df_bias)
  
}

# Repeating 1000 times using a sample size of 250

output_list <- replicate(1000, adjust_lifting(sample.size = 250), simplify = FALSE)


df.out <- do.call(rbind, output_list) %>% # reformatting
  
  # Adding a new column that will be used to estimate the Monte Carlo SE of the estimate
  mutate(
    squared = (bias_for_one - mean(bias_for_one))^2
  )


# Calculating the mean bias and Monte Carlo SE of estimate

# See Morris et al. (2019) for details on calculating these

mean(df.out$bias_for_one) # bias  
sqrt(sum(df.out$squared)*(1 / (1000*999))) # Monte Carlo SE of bias (1000 is number of simulations, 999 is n - 1)

# Adjusting for Both ----

# Adjusting for both sleep and lifting

adjust_both <- function(sample.size){
  
  trt_effect <- 2 # what we want the effect to be in the outcome. Basically what effect coffee
  # has 
  
  df <- sim_data(sample.size = sample.size)
  
  # Fitting the model but adjusting for both sleep and lifting
  
  mod <- glm(happy ~ coffee + hours_sleep + lifting, 
             family = gaussian(link = "identity"), 
             data = df)
  
  expected_value <- broom::tidy(mod)$estimate[2] # this is the estimate
  
  bias_for_one = expected_value - trt_effect # comparing the estimate to the "true" effect 
  
  df_bias = data.frame(
    bias_for_one
  )
  
  return(df_bias)
  
}

# Repeating 1000 times using a sample size of 250

output_list <- replicate(1000, adjust_both(sample.size = 250), simplify = FALSE)


df.out <- do.call(rbind, output_list) %>% # reformatting
  
  # Adding a new column that will be used to estimate the Monte Carlo SE of the estimate
  mutate(
    squared = (bias_for_one - mean(bias_for_one))^2
  )


# Calculating the mean bias and Monte Carlo SE of estimate

# See Morris et al. (2019) for details on calculating these

mean(df.out$bias_for_one) # bias  
sqrt(sum(df.out$squared)*(1 / (1000*999))) # Monte Carlo SE of bias (1000 is number of simulations, 999 is n - 1)
