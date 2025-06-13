# Title: Adjusting for Predictor Variable

# Description: Directed acyclic graphs (DAGs) are fantastic for variable selection. 
# Sometimes there are variable that it's not clear if we should include or not. 
# This code demonstrates one example: a variable that causes one of the confounders
# and the outcome. 

# Both bias and relative increase in precision are explored for this situation. 
# Inverse probability weighting, estimating the average treatment effect, is 
# used. When adjusting for the predictor, it is only included in the outcome model

# Setup ----

#... Packages ----

library(tidyverse) # ol' faithful
library(WeightIt) # for IPW
library(broom) # for tidying model output

#... Functions ----

# For simulating data. 
# Note: an additional variable was included that is a cause of Z1 & Y 
# (referred to as p1)

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
    z2 = rbinom(n = n, size = z2_size, prob = z2_prob),
    p1 = rnorm(n = n, mean = 3, sd = 1)
  ) %>% 
    dplyr::mutate(
      z1 = rnorm(n = n, mean = z1_mean, sd = z1_sd) + 1.5*p1,
      prob = plogis(z1_on_x*z1 + z2_on_x*z2), 
      x = rbinom(n = n, size = 1, prob = prob), 
      y = beta_trt*x + z1_on_y*z1 + z2_on_y*z2 + 2*p1 + rnorm(n = n, mean = 0, sd = 1)
    )
  
  # Return the dataframe
  
  return(df)
}

# Setting Seed for Reproducibility ----

set.seed(456)

# Scenario 1: IPW (no predictor) ----

# Inverse probability weighting is used, including both confounders. 
# For the outcome model, the predictor is not included. 

#... Function ----

# Function used to simulate data, estimate weights and fit outcome model. 
# The value returned is the estimate 

no_predictor <- function(sample.size){
  
  df <- sim_data(n = sample.size)
  
  ipw <- WeightIt::weightit(x ~ z1 + z2, 
                               data = df, 
                               method = "glm", 
                               estimand = "ATE", 
                               stabilize = TRUE)
  
  out.mod <- glm(y ~ x, 
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

output_listA <- replicate(1000, 
                         no_predictor(sample.size = 250), simplify = FALSE)

# Reformatting and adding columns required for calculating metrics. 

df.outA <- do.call(rbind, output_listA) %>% # reformatting
  
  # Adding a new column that will be used to estimate the Monte Carlo SE of the estimate
  mutate(
    bias_for_one = 1.5 - estimate, 
    squared = (bias_for_one - mean(bias_for_one))^2
  )

#... Calculating Bias ---

# See Morris et al. (2019) for details on calculating these

mean(df.outA$bias_for_one) # bias  
sqrt(sum(df.outA$squared)*(1 / (1000*999))) # Monte Carlo SE of bias (1000 is number of simulations, 999 is n - 1)

#... Empirical SE ----

mean_estimateA <- mean(df.outA$estimate) # mean of the estimates

# SSE 

sseA <- df.outA %>% 
  mutate(
    error_squares = (estimate - mean_estimateA)^2
  ) %>% 
  summarise(
    sse = sum(error_squares)
  ) %>% as.numeric()

# Calculating Empirical SE 

emp_seA <- sqrt((1/1000)*sseA)


# Scenario 2: IPW (including predictor) -----

# Inverse probability weighting is used, including both confounders. 
# For the outcome model, the predictor included. 

#... Function ----

# Function used to simulate data, estimate weights and fit outcome model. 
# The value returned is the estimate 

adjust_predictor <- function(sample.size){
  
  df <- sim_data(n = sample.size)
  
  ipw <- WeightIt::weightit(x ~ z1 + z2, 
                            data = df, 
                            method = "glm", 
                            estimand = "ATE", 
                            stabilize = TRUE)
  
  out.mod <- glm(y ~ x + p1, 
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

output_listB <- replicate(1000, 
                         adjust_predictor(sample.size = 250), simplify = FALSE)

# Reformatting and adding values for calculating metrics 

df.outB <- do.call(rbind, output_listB) %>% # reformatting
  
  # Adding a new column that will be used to estimate the Monte Carlo SE of the estimate
  mutate(
    bias_for_one = 1.5 - estimate, 
    squared = (bias_for_one - mean(bias_for_one))^2
  )

#... Calculating Bias ----

# See Morris et al. (2019) for details on calculating these

mean(df.outB$bias_for_one) # bias  
sqrt(sum(df.outB$squared)*(1 / (1000*999))) # Monte Carlo SE of bias (1000 is number of simulations, 999 is n - 1)

#... Empirical SE ----

mean_estimateB <- mean(df.outB$estimate)

sseB <- df.outB %>% 
  mutate(
    error_squares = (estimate - mean_estimateB)^2
  ) %>% 
  summarise(
    sse = sum(error_squares)
  ) %>% as.numeric()

emp_seB <- sqrt((1/1000)*sseB)

# Relative % Increase in Precision (B vs A) ----

# For this example, we are interested about 
# Increase in Precision of Including Predictor vs Not Including Predictor, so: 
# B = including predictor in outcome model 
# A = not including predictor 

# Formulas are from Morris et al. 2019

100*(((emp_seA/emp_seB)^2)-1)

#... Monte Carlo SE of Precision ----

corrAB<- cor(df.outA$estimate, df.outB$estimate)

(200*(emp_seA/emp_seB)^2)*sqrt((1-corrAB^2)/(1000-1))

