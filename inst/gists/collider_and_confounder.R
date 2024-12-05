# Title: A collider and a confounder - To adjust or not adjust? 

# Description: Should we adjust for a variable if it's both a collider and a confounder? 
# The idea for this came from an article I recently read - Liu et al.(2012)
# https://academic.oup.com/aje/article/176/10/938/92975#246684420

# The idea for this code was to keep the situation simple to start. It can be built upon 
# rather easily. A few things to try altering would be the strength of the causal relationships, 
# continuous vs binary vs TTE variables, more/less variables, etc. 

# Setup ---- 

#... Libraries ----

library(tidyverse) # ol' faithful
library(ggdag) # used for creating the DAG in R

# DAG ----

theme_set(theme_dag()) # setting the theme of the plot 

# Creating the DAG to guide the scenario

dag = ggdag::dagify(
  happy ~ coffee + sleep + sunshine,
  lifting ~ sunshine, 
  sleep ~ sunshine + lifting, 
  coffee ~ sleep + lifting, 
  exposure = "coffee",
  outcome = "happy"
)

# Viewing the DAG

dag |> 
  ggdag::ggdag(
    layout = "nicely")

?bootstraps

# Simulated Data ----

# Using the DAG above as a guide, we can simulate data to see what would happen 
# if we adjusted vs if we did not adjust. 

set.seed(123) # for reproducibility

# For this, we will repeat it 1000 times. Note: there are much better ways to 
# choose the number of simulations required. For this purpose we'll use 1000 but 
# it's an aribitrary number. 

#... Not adjusting for the variable ----

# The below code simulates code for each of the variables required and 
# fit a generalized linear model. The estimate from this is then compared to the 
# treatment effect. For this we used a GLM, but there are other adjustment techniques 
# that could be explored too such as IPW.  

no_adjust <- function(sample.size){
  
  trt_effect <- 2 # what we want the effect to be in the outcome. Basically what effect coffee
  # has 
  
  df <- data.frame(
    sunshine = rbinom(n = sample.size, size = 1, prob = 0.65) # using a binary variable for sunshine
    ) %>% 
    dplyr::mutate( 
    lifting = rbinom(n = sample.size, size = 1, prob = 0.2 + .3*sunshine), # binary for lifting
    hours_sleep = rnorm(n = sample.size, mean = 4 + 1.5*lifting + 1*sunshine, sd = 2), # continuous for hours of sleep
    coffee = rbinom(n = sample.size, size = 1, prob = plogis(0.2*hours_sleep + 0.1*lifting)), # binary for coffee
    happy = rnorm(n = sample.size, mean = 10 + 0.5 * hours_sleep + 2 * coffee + 3 * lifting, sd = 3) # continuous for happiness
    )
  
  # Fitting the model but not adjusting for hours of sleep
  
  mod <- glm(happy ~ coffee, 
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

output_list <- replicate(1000, no_adjust(sample.size = 1000), simplify = FALSE)

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

adjust <- function(sample.size){
  
  # See above for notes on detail of variables
  
  trt_effect <- 2 # what we want the effect to be in the outcome. Basically what effect coffee
  # has 
  
  df <- data.frame(
    sunshine = rbinom(n = sample.size, size = 1, prob = 0.65)
  ) %>% 
    dplyr::mutate( 
      lifting = rbinom(n = sample.size, size = 1, prob = 0.2 + .3*sunshine),
      hours_sleep = rnorm(n = sample.size, mean = 4 + 1.5*lifting + 1*sunshine, sd = 2),
      coffee = rbinom(n = sample.size, size = 1, prob = plogis(0.2*hours_sleep + 0.1*lifting)),
      happy = rnorm(n = sample.size, mean = 10 + 0.5 * hours_sleep + trt_effect * coffee + 3 * lifting, sd = 3)
    )
  
  # Fitting model adjusting for sleep 
  
  mod <- glm(happy ~ coffee + hours_sleep, 
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

output_list <- replicate(1000, no_adjust(sample.size = 1000), simplify = FALSE)

df.out <- do.call(rbind, output_list) %>% # reformatting
  
  # Adding a new column that will be used to estimate the Monte Carlo SE of the estimate
  mutate(
    squared = (bias_for_one - mean(bias_for_one))^2
  )


# Calculating the mean bias and Monte Carlo SE of estimate

# See Morris et al. (2019) for details on calculating these

mean(df.out$bias_for_one) # bias  
sqrt(sum(df.out$squared)*(1 / (1000*999))) # Monte Carlo SE of bias (1000 is number of simulations, 999 is n - 1)



