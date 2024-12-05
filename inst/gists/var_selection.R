# Title: Adjusting for Correct Variables vs Not 

# Description: The idea for this came from trying to show what happens 
# if the wrong variables are selected. Basically, when we use variable selection methods
# based on the p-value the results can be biased due to model misspecification. Furthermore, 
# how would that look if it wasn't just one variable that was wrong...but two!

# Setup ---- 

#... Libraries ----

library(tidyverse) # ol' faithful

# Simulated Data ----

# Using the DAG from the LinkedIn post (dated 26Sep2024) above as a guide
# We can simulate data to see what would happen if we adjusted for correct variables vs 
# did not. 

set.seed(123) # for reproducibility

# For this, we will repeat it 1000 times. Note: there are much better ways to 
# choose the number of simulations required. For this purpose we'll use 1000 but 
# it's an aribitrary number. 

#... Not adjusting for the variable ----

# The below code simulates code for each of the variables required and 
# fit a generalized linear model. The estimate from this is then compared to the 
# treatment effect. For this we used a GLM, but there are other adjustment techniques 
# that could be explored too such as IPW.  

wrong_adjust <- function(sample.size){
  
  trt_effect <- 2 # what we want the effect to be in the outcome. Basically what effect coffee
  # has 
  
  df <- data.frame(
    sunshine = rbinom(n = sample.size, size = 1, prob = 0.65) # using a binary variable for sunshine
  ) %>% 
    dplyr::mutate( 
      hours_sleep = rnorm(n = sample.size, mean = 4 + 1.5*sunshine, sd = 2), # continuous for hours of sleep
      coffee = rbinom(n = sample.size, size = 1, prob = plogis(0.2*hours_sleep + 0.1*sunshine)), # binary for coffee
      lifting = rbinom(n = sample.size, size = 1, prob = 0.1 + 0.3*sunshine + 0.2*coffee), # binary for lifting
      happy = rnorm(n = sample.size, mean = 10 + 0.5 * hours_sleep + 2 * coffee + 3 * sunshine, sd = 3) # continuous for happiness
    )
  
  # Fitting the model but wrongly adjusting for sleep & lifting 
  
  mod <- glm(happy ~ coffee + sunshine + lifting, 
             family = gaussian(link = "identity"), 
             data = df)
  
  expected_value <- broom::tidy(mod)$estimate[2] # this is the estimate
  
  bias_for_one = expected_value - trt_effect # comparing the estimate to the "true" effect 
  
  df_bias = data.frame(
    bias_for_one
  )
  
  return(df_bias)
  
}

# Repeating 1000 times using a sample size of 1000

output_list <- replicate(1000, wrong_adjust(sample.size = 1000), simplify = FALSE)

df.out <- do.call(rbind, output_list) %>% # reformatting
  
  # Adding a new column that will be used to estimate the Monte Carlo SE of the estimate
  mutate(
    squared = (bias_for_one - mean(bias_for_one))^2
  )

# Calculating the mean bias and Monte Carlo SE of estimate

# See Morris et al. (2019) for details on calculating these

mean(df.out$bias_for_one) # bias  
sqrt(sum(df.out$squared)*(1 / (1000*999))) # Monte Carlo SE of bias (1000 is number of simulations, 999 is n - 1)

#... Adjusting ----

correct_adjust <- function(sample.size){
  
  # See above for notes on detail of variables
  
  trt_effect <- 2 # what we want the effect to be in the outcome. Basically what effect coffee
  # has 
  
  df <- data.frame(
    sunshine = rbinom(n = sample.size, size = 1, prob = 0.65) # using a binary variable for sunshine
  ) %>% 
    dplyr::mutate( 
      hours_sleep = rnorm(n = sample.size, mean = 4 + 1.5*sunshine, sd = 2), # continuous for hours of sleep
      coffee = rbinom(n = sample.size, size = 1, prob = plogis(0.2*hours_sleep + 0.1*sunshine)), # binary for coffee
      lifting = rbinom(n = sample.size, size = 1, prob = 0.1 + 0.3*sunshine + 0.2*coffee), # binary for lifting
      happy = rnorm(n = sample.size, mean = 10 + 0.5 * hours_sleep + 2 * coffee + 3 * sunshine, sd = 3) # continuous for happiness
    )
  
  # Fitting model adjusting for sleep and sunshine
  
  mod <- glm(happy ~ coffee + hours_sleep + sunshine, 
             family = gaussian(link = "identity"), 
             data = df)
  
  expected_value <- broom::tidy(mod)$estimate[2]
  
  bias_for_one = expected_value - trt_effect
  
  df_bias = data.frame(
    bias_for_one
  )
  
  return(df_bias)
  
}

# Repeating 1000 times using a sample size of 1000

output_list <- replicate(1000, correct_adjust(sample.size = 1000), simplify = FALSE)

df.out <- do.call(rbind, output_list) %>% # reformatting
  
  # Adding a new column that will be used to estimate the Monte Carlo SE of the estimate
  mutate(
    squared = (bias_for_one - mean(bias_for_one))^2
  )


# Calculating the mean bias and Monte Carlo SE of estimate

# See Morris et al. (2019) for details on calculating these

mean(df.out$bias_for_one) # bias  
sqrt(sum(df.out$squared)*(1 / (1000*999))) # Monte Carlo SE of bias (1000 is number of simulations, 999 is n - 1)
