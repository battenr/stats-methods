# Demonstration of Power Loss ----

# Setup ----

library(tidyverse) # ol faithful 

# Function for Estimating Effects ----

# The below function estimates the effect. 

# - Generalized linear model is used (GLM)
# - Continuous outcome
# - 1 covariate (can increase from there)
# - Assume treatment/effect is binary 


estimate_effects <- function(){
  
  n = 500 # sample size 
  
  df <- data.frame(
    x = rbinom(n = n, size = 1, prob = 0.6),
    c1 = rnorm(n = n, mean = 10, sd = 5)
  ) %>% 
    mutate(
      y = 3 + 0.3*x + 0.5*c1 + rnorm(n),
      binary_c1 = ifelse(c1 >= 13, "group1", "group2")
    )
  
  # Continuous 
  
  mod_cont <- glm(y ~ x + c1, 
             family = gaussian(), 
             data = df)
  
  p_cont <- broom::tidy(mod_cont)[2,5] %>% as.numeric()
  
  cont_result <- ifelse(p_cont <= 0.05, TRUE, FALSE)
  
  # Binary 
  
  
  mod_binary <- glm(y ~ x + binary_c1, 
                  family = gaussian(), 
                  data = df)
  
  p_binary <- broom::tidy(mod_binary)[2,5] %>% as.numeric()
  
  binary_result <- ifelse(p_binary <= 0.05, TRUE, FALSE)
  
  return(
    data.frame(
      "ContResult" = cont_result,
      "BinaryResult" = binary_result
    )
  )
  
}

# Repeating Repeatedly ----

output <- replicate(1000, estimate_power(), simplify = FALSE) 

output <- do.call(rbind, output)

# Calculating Power ----

mean(output$ContResult)
mean(output$BinaryResult)

