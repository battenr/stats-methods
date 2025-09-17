# Title: Truncating Weights for Inverse Probability Weighting 

# Description: When using inverse probability weights, it is recommended to stabilize weights. 
# If there are extreme weights, truncating them can help reduce variance. 

# This code demonstrates the benefit of truncating the weights 
# (that is setting the values > some threshold, to that threshold). 
# For example, setting weights > 95th percentile to the 95th percentile

# Setup ----

#... Libraries ----

library(tidyverse) # ol' faithful
library(WeightIt) # for IPW weighting
library(broom) # tidying output from model

#... Functions ----

# Simulating Data 

sim_data <- function(n = 250, # sample size 
         beta_trt = 1.5, # treatment effect
         # Parameters for Z1 
         z1_mean = 5, z1_sd = 2, 
         # Parameters for Z2
         z2_size = 1, z2_prob = 0.5, 
         # Confounder - Effect on X
         z1_on_x = 0.05, z2_on_x = 0.5,
         # Confounder - Effect on Y
         z1_on_y = 0.5, z2_on_y = 0.3){
  
  # Creating the Dataframe
  
  df <- data.frame(
    z1 = rnorm(n = n, mean = z1_mean, sd = z1_sd), 
    z2 = rbinom(n = n, size = z2_size, prob = z2_prob),
    u1 = rnorm(n = n, mean = z1_mean, sd = z1_sd) # adding unmeasured confounder 
  ) %>% 
    dplyr::mutate(
      prob = plogis(-z1_on_x*u1 + z1_on_x*z1^2 + z2_on_x*z2), 
      x = rbinom(n = n, size = 1, prob = prob), 
      y = beta_trt*x + z1_on_y*z1 + z2_on_y*z2 + rnorm(n = n, mean = 0, sd = 1)
    )
  
  # Return the dataframe
  
  return(df)
}

# Checking for Extreme Weights ----

# This is the example for one run 

df <- sim_data() # simulating data 

# Fitting propensity score model

ps.mod <- WeightIt::weightit(x ~ z1 + z2, 
                             data = df, 
                             method = "glm", 
                             estimand = "ATE", 
                             stabilize = TRUE)

# Checking Weights to See if Any Extreme

summary(ps.mod$weights)

hist(ps.mod$weights)

# Truncating Weights ----

truncate <- function(sample.size){
  
  trt_effect <- 1.5 # same as in sim_data() function 
  
  # Simulating data
  
  df <- sim_data()
  
  # Fitting PS model - estimating the average treatment effect and stabilizing the weights
  
  ps.mod <- WeightIt::weightit(x ~ z1 + z2, 
                               data = df, 
                               method = "glm", 
                               estimand = "ATE", 
                               stabilize = TRUE)
  
  ipw <- ps.mod$weights
  
  # Truncating the weights at the 95th percentile. 
  # This means if a weight is > the 95th percentile, set it to the 95th percentile
  
  ipw <- ifelse(ipw > quantile(ipw, 0.95),
                quantile(ipw, 0.95),
                         ipw)
  
  # Fitting outcome model 
  
  mod <- glm_weightit(y ~ x, # note: not doubly robust for this example
             family = gaussian(link = "identity"), 
             data = df,
             weights = ipw)
  
  estimate <- broom::tidy(mod)$estimate[2] # this is the estimate
  
  bias_for_one = estimate - trt_effect # comparing the estimate to the "true" effect 
  
  df_bias = data.frame(
    bias_for_one,
    estimate # need for estimating relative precision later 
  )
  
  return(df_bias)
  
}

# Repeating 1000 times using a sample size of 250

output_list <- replicate(1000, truncate(sample.size = 250), simplify = FALSE)

df.out.truncate <- do.call(rbind, output_list) %>% # reformatting
  
  # Adding a new column that will be used to estimate the Monte Carlo SE of the estimate
  mutate(
    squared = (bias_for_one - mean(bias_for_one))^2,
    residuals_squared = (estimate - mean(estimate))^2
  )


# Calculating the mean bias and Monte Carlo SE of estimate

# See Morris et al. (2019) for details on calculating these

mean(df.out.truncate$bias_for_one) # bias  
sqrt(sum(df.out.truncate$squared)*(1 / (1000*999))) # Monte Carlo SE of bias (1000 is number of simulations, 999 is n - 1)

# Emperical SE 

emp.se.truncate <- sqrt(sum(df.out.truncate$residuals_squared)/999)

emp.se.truncate / sqrt(2*999)


# Not Truncated ----

no.truncate <- function(sample.size){
  
  trt_effect <- 1.5 # same as in sim_data() function 
  
  # Simulating data
  
  df <- sim_data()
  
  # Fitting PS model 
  
  ps.mod <- WeightIt::weightit(x ~ z1 + z2, 
                               data = df, 
                               method = "glm", 
                               estimand = "ATE", 
                               stabilize = TRUE)
  
  # Outcome model 
  
  mod <- glm_weightit(y ~ x, # note: not doubly robust 
                      family = gaussian(link = "identity"), 
                      data = df,
                      weights = ps.mod$weights)
  
  estimate <- broom::tidy(mod)$estimate[2] # this is the estimate
  
  bias_for_one = estimate - trt_effect # comparing the estimate to the "true" effect 
  
  df_bias = data.frame(
    bias_for_one,
    estimate # need for estimating relative precision later 
  )
  
  return(df_bias)
  
}

# Repeating 1000 times using a sample size of 250

output_list <- replicate(1000, no.truncate(sample.size = 250), simplify = FALSE)

df.out.no.truncate <- do.call(rbind, output_list) %>% # reformatting
  
  # Adding a new column that will be used to estimate the Monte Carlo SE of the estimate
  mutate(
    squared = (bias_for_one - mean(bias_for_one))^2,
    residuals_squared = (estimate - mean(estimate))^2
  )


# Calculating the mean bias and Monte Carlo SE of estimate

# See Morris et al. (2019) for details on calculating these

mean(df.out.no.truncate$bias_for_one) # bias  
sqrt(sum(df.out.no.truncate$squared)*(1 / (1000*999))) # Monte Carlo SE of bias (1000 is number of simulations, 999 is n - 1)

# Emperical SE 

emp.se.no.truncate <- sqrt(sum(df.out.no.truncate$residuals_squared)/999)

emp.se.no.truncate / sqrt(2*999)

# Percent Increase in Precision ----

# Truncating vs Not Truncating 

100*(((emp.se.no.truncate/emp.se.truncate)^2) - 1)

# Monte Carlo SE of Estimate

corr.a.b <- cor(df.out.no.truncate$estimate,
                df.out.truncate$estimate)

200*((emp.se.no.truncate/emp.se.truncate)^2)*sqrt((1 - (corr.a.b^2)) / (1000-1))
