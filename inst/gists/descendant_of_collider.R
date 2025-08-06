# Title: Descendant of a Collider - To adjust or not adjust? 

# Description: Should we adjust for a variable that is a descendant of a collider? 
# It is commonly accepted that we should not, since it will increase bias. 
# The goal of this code is to prove that through simulation. 

# Setup ---- 

#... Libraries ----

library(tidyverse) # ol' faithful
library(ggdag) # used for creating the DAG in R

# DAG ----

theme_set(theme_dag()) # setting the theme of the plot 

# Creating the DAG to guide the scenario

dag = ggdag::dagify(
  happy ~ coffee + sleep,
  # nothing causes sleep
  strength ~ lifting, 
  lifting ~ coffee + sleep,
  # nothing causes coffee
  exposure = "coffee",
  outcome = "happy"
)

# Viewing the DAG

dag |> 
  ggdag::ggdag(
    layout = "nicely")


# Simulated Data ----

# Using the DAG above as a guide, we can simulate data to see what would happen 
# if we adjusted vs if we did not adjust. 

set.seed(456) # for reproducibility

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
    hours_sleep = rnorm(n = sample.size, mean = 6, sd = 2),
    coffee = rbinom(n = sample.size, size = 1, prob = 0.5)
    # using a binary variable for sunshine
  ) %>% 
    dplyr::mutate( 
      lifting = rbinom(n = sample.size, size = 1, prob = plogis(1.5*coffee + 0.4*hours_sleep)),
      strength = 2 + 5*lifting, 
      happy = 0.5*hours_sleep + trt_effect*coffee 
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

output_list <- replicate(1000, no_adjust(sample.size = 250), simplify = FALSE)

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
    hours_sleep = rnorm(n = sample.size, mean = 6, sd = 2),
    coffee = rbinom(n = sample.size, size = 1, prob = 0.5)
    # using a binary variable for sunshine
  ) %>% 
    dplyr::mutate( 
      lifting = rbinom(n = sample.size, size = 1, prob = plogis(1.5*coffee + 0.4*hours_sleep)),
      strength = 2 + 5*lifting, 
      happy = 0.5*hours_sleep + trt_effect*coffee 
    )
  
  # Fitting the model but not adjusting for hours of sleep
  
  mod <- glm(happy ~ coffee + strength, 
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

output_list <- replicate(1000, adjust(sample.size = 250), simplify = FALSE)

df.out <- do.call(rbind, output_list) %>% # reformatting
  
  # Adding a new column that will be used to estimate the Monte Carlo SE of the estimate
  mutate(
    squared = (bias_for_one - mean(bias_for_one))^2
  )


# Calculating the mean bias and Monte Carlo SE of estimate

# See Morris et al. (2019) for details on calculating these

mean(df.out$bias_for_one) # bias  
sqrt(sum(df.out$squared)*(1 / (1000*999))) # Monte Carlo SE of bias (1000 is number of simulations, 999 is n - 1)




