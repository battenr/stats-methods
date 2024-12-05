# Title: Demonstration of Power Loss 

# Description: Demonstrating the power lost by dichotomizing a covariate. 

# Setup ----

library(tidyverse) # ol faithful 
library(broom) # tidying the model results

# Function for Estimating Treatment Effect ----

# The below function estimates the effect based upon a simple scenario. From 
# the simple scenario, other parts can be added one (additional covariates, etc). 

# - Generalized linear model is used (GLM)
# - Continuous outcome
# - One continuous covariate 
# - Assume treatment/effect is binary 


estimate_effects <- function(){
  
  n = 500 # sample size, arbitrarily set to 500 
  
  # Creating a dataframe 
  
  df <- data.frame(
    x = rbinom(n = n, size = 1, prob = 0.6), # binary treatment 
    c1 = rnorm(n = n, mean = 10, sd = 5) # continuous covariate
  ) %>% 
    mutate(
      y = 3 + 0.3*x + 0.5*c1 + rnorm(n), # continuous outcome
      binary_c1 = ifelse(c1 >= 13, "group1", "group2") # dichotomizing covariate
    )
  
  # Continuous Covariate
  
  # Fitting the model with a continuous covariate 
  
  mod_cont <- glm(y ~ x + c1, 
             family = gaussian(), 
             data = df)
  
  # Extracting the p-value
  
  p_cont <- broom::tidy(mod_cont)[2,5] %>% as.numeric()
  
  # Using a significance value of 0.05. If the p-value is less than that
  # then true. If it's not, then false 
  
  cont_result <- ifelse(p_cont <= 0.05, TRUE, FALSE) 
  
  # Binary Covariate
  
  mod_binary <- glm(y ~ x + binary_c1, 
                  family = gaussian(), 
                  data = df)
  
  # Extracting p-value
  
  p_binary <- broom::tidy(mod_binary)[2,5] %>% as.numeric()
  
  # Changing to logical based on 0.05 significance level 
  
  binary_result <- ifelse(p_binary <= 0.05, TRUE, FALSE)
  
  # Return the result
  
  return(
    data.frame(
      "ContResult" = cont_result,
      "BinaryResult" = binary_result
    )
  )
  
}

# Repeating Over and Over ----

# Repeating the above 1000 times. This is also arbitrary, in reality there are 
# good resources for determining this. Suggest Morris et al. (2019) for more details

output <- replicate(1000, estimate_power(), simplify = FALSE) 

output <- do.call(rbind, output) # reformatting 

# Calculating Power ----

mean(output$ContResult) # power for continuous outcome
mean(output$BinaryResult) # power for binary outcome

