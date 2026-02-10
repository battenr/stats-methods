# Title: Why Adjusting for All Variables is Problematic

# Description: It's easy to think that adjusting for all variables is helpful. 
# For causal inference, it's very problematic. This code simulates data, then fits a 
# generalized linear model. Bias and the Monte Carlo Standard Error of bias are calculated
# to compare results from two scenarios: 

# 1. Adjusting for all variables
# 2. Adjusting for correct variables (only confounder, not mediator, not collider)

# Simulating Data ----

# This code is based on the DAG found in the accompanying LinkedIn post. 
# There are five variables: 
# - Coffee (the exposure)
# - Sleep (confounder) 
# - Lifting (mediator)
# - Dancing (collider)
# - Happy (the outcome) 

# Arbitrarily using sample size of 250 
# Treatment effect of 1.5 

sample.size = 250

trt_effect = 1.5

# This is the test data frame. This code will be used in both of the functions 
# to determine bias from both scenarios. 

test = data.frame(
  sleep = rnorm(n = sample.size, mean = 8, sd = 2)
) %>% 
  mutate(
    coffee = rbinom(n = sample.size, size = 1, prob = plogis(0.05*sleep)),
    lifting = rbinom(n = sample.size, size = 1, prob = plogis(0.2 + 0.3*coffee)),
    happy = trt_effect*coffee + 2*lifting + 3*sleep + rnorm(n = sample.size, mean = 0, sd = 1),
    dancing = 5 + 2*coffee + 2.5*happy + rnorm(n = sample.size, mean = 0, sd = 1)
  )

# Adjusting for All Variables ----

# This code simulates data, fits a generalized linear model with all variables and
# calculates the bias. This function is run repeatedly later. 

adjust_all <- function(sample.size){
  
  # Simulating Data 
  
  trt_effect = 1.5
  
  df = data.frame(
    sleep = rnorm(n = sample.size, mean = 8, sd = 2)
  ) %>% 
    mutate(
      coffee = rbinom(n = sample.size, size = 1, prob = plogis(0.05*sleep)),
      lifting = rbinom(n = sample.size, size = 1, prob = plogis(0.2 + 0.3*coffee)),
      happy = trt_effect*coffee + 2*lifting + 3*sleep + rnorm(n = sample.size, mean = 0, sd = 1),
      dancing = 5 + 2*coffee + 2.5*happy + rnorm(n = sample.size, mean = 0, sd = 1)
    )
  
  # Fitting Model 
  
  mod <- glm(happy ~ coffee + sleep + lifting + dancing, 
             family = gaussian(link = "identity"),
             data = df)
  
  # Extracting value and calculating bias
  
  expected_value <- broom::tidy(mod)$estimate[2]
  
  bias_for_one = expected_value - trt_effect
  
  df_bias = data.frame(
    bias_for_one
  )
  
  return(df_bias)
  
  
  
}

# Repeating 1000 times using a sample size of 1000

output_list <- replicate(1000, adjust_all(sample.size = 1000), simplify = FALSE)

df.out <- do.call(rbind, output_list) %>% # reformatting
  
  # Adding a new column that will be used to estimate the Monte Carlo SE of the estimate
  mutate(
    squared = (bias_for_one - mean(bias_for_one))^2
  )

# Calculating the mean bias and Monte Carlo SE of estimate

# See Morris et al. (2019) for details on calculating these

mean(df.out$bias_for_one) # bias  
sqrt(sum(df.out$squared)*(1 / (1000*999))) # Monte Carlo SE of bias (1000 is number of simulations, 999 is n - 1)

# Adjusting for Only Confounders ----

# This code simulates data, fits a generalized linear model with only the confounder (correct 
# variable selection) and calculates the bias. This function is run repeatedly later. 

adjust_confounder <- function(sample.size){
  
  # Simulating Data 
  
  trt_effect = 1.5
  
  df = data.frame(
    sleep = rnorm(n = sample.size, mean = 8, sd = 2)
  ) %>% 
    mutate(
      coffee = rbinom(n = sample.size, size = 1, prob = plogis(0.05*sleep)),
      lifting = rbinom(n = sample.size, size = 1, prob = plogis(0.2 + 0.3*coffee)),
      happy = trt_effect*coffee + 2*lifting + 3*sleep + rnorm(n = sample.size, mean = 0, sd = 1),
      dancing = 5 + 2*coffee + 2.5*happy + rnorm(n = sample.size, mean = 0, sd = 1)
    )
  
  # Fitting Model 
  
  mod <- glm(happy ~ coffee + sleep, 
             family = gaussian(link = "identity"),
             data = df)
  
  # Extracting value and calculating bias
  
  expected_value <- broom::tidy(mod)$estimate[2]
  
  bias_for_one = expected_value - trt_effect
  
  df_bias = data.frame(
    bias_for_one
  )
  
  return(df_bias)
  
}

# Repeating 1000 times using a sample size of 1000

output_list <- replicate(1000, adjust_confounder(sample.size = 1000), simplify = FALSE)

df.out <- do.call(rbind, output_list) %>% # reformatting
  
  # Adding a new column that will be used to estimate the Monte Carlo SE of the estimate
  mutate(
    squared = (bias_for_one - mean(bias_for_one))^2
  )

# Calculating the mean bias and Monte Carlo SE of estimate

# See Morris et al. (2019) for details on calculating these

mean(df.out$bias_for_one) # bias  
sqrt(sum(df.out$squared)*(1 / (1000*999))) # Monte Carlo SE of bias (1000 is number of simulations, 999 is n - 1)


