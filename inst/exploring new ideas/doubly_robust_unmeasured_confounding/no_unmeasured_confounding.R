# Doubly Robust When Data is Missing? 

# Part 1: When there is no unmeasured confounding 

# Title: 

# Description: 

# Setup ----

#... Libraries ----

library(tidyverse) # ol' faithful
library(WeightIt)

#... Functions ----

sim_data <- function(n = 250, # sample size 
                     beta_trt = 1.5, # treatment effect
                     # Parameters for Z1 
                     z1_mean = 5, z1_sd = 2, 
                     # Parameters for Z2
                     z2_size = 1, z2_prob = 0.5, 
                     # Parameters for U1
                     u1_mean = 3, u1_sd = 2,
                     # Confounder - Effect on X
                     z1_on_x = 0.05, z2_on_x = 0.5, u1_on_x = 0.07,
                     # Confounder - Effect on Y
                     z1_on_y = 0.5, z2_on_y = 0.3, u1_on_y = 0.4
                     ){
  
  # Creating the Dataframe
  
  df <- data.frame(
    z1 = rnorm(n = n, mean = z1_mean, sd = z1_sd), 
    z2 = rbinom(n = n, size = z2_size, prob = z2_prob),
    u1 = rnorm(n = n, mean = u1_mean, sd = u1_sd)
  ) %>% 
    dplyr::mutate(
      beta_trt = 1.5, 
      # beta_trt = dplyr::case_when(
      #   s1 == 1 ~ rnorm(n = n, mean = 3, sd = 1), 
      #   s1 == 0 ~ rnorm(n = n, mean = 0, sd = 1)
      # ),
      prob = plogis(z1_on_x*z1 + z2_on_x*z2), 
      x = rbinom(n = n, size = 1, prob = prob), 
      #y = beta_trt*x + z1_on_y*z1 + z2_on_y*z2 + u1_on_y*u1 + rnorm(n = n, mean = 0, sd = 1)
      y = beta_trt*x + z1_on_y*z1 + z2_on_y*z2 + rnorm(n = n, mean = 0, sd = 1)
    )
  
  # Return the dataframe
  
  return(df)
}

# Code ----

df <- sim_data()


# IPW ----

ipw <- function(sample.size){
  
  trt_effect <- 1.5 # same as in sim_data() function 
  
  # Simulating data
  
  df <- sim_data()
  
  # Fitting PS model - estimating the average treatment effect and stabilizing the weights
  
  ps.mod <- WeightIt::weightit(x ~ z1, 
                               data = df, 
                               method = "glm", 
                               estimand = "ATE", 
                               stabilize = TRUE)
  
  # Fitting outcome model 
  
  mod <- glm_weightit(y ~ x, # note: not doubly robust for this example
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

output_list <- replicate(1000, ipw(sample.size = 250), simplify = FALSE)

df.out.ipw <- do.call(rbind, output_list) %>% # reformatting
  
  # Adding a new column that will be used to estimate the Monte Carlo SE of the estimate
  mutate(
    squared = (bias_for_one - mean(bias_for_one))^2,
    residuals_squared = (estimate - mean(estimate))^2
  )


# Calculating the mean bias and Monte Carlo SE of estimate

# See Morris et al. (2019) for details on calculating these

ipw.bias <- mean(df.out.ipw$bias_for_one) # bias  
ipw.se.of.bias <- sqrt(sum(df.out.ipw$squared)*(1 / (1000*999))) # Monte Carlo SE of bias (1000 is number of simulations, 999 is n - 1)

# Outcome Model ----

out_mod <- function(sample.size){
  
  trt_effect <- 1.5 # same as in sim_data() function 
  
  # Simulating data
  
  df <- sim_data()
  
  # Fitting outcome model 
  
  mod <- glm(y ~ x + z2, # note: not doubly robust for this example
             family = gaussian(link = "identity"), 
             data = df)
  
  estimate <- broom::tidy(mod)$estimate[2] # this is the estimate
  
  bias_for_one = estimate - trt_effect # comparing the estimate to the "true" effect 
  
  df_bias = data.frame(
    bias_for_one,
    estimate # need for estimating relative precision later 
  )
  
  return(df_bias)
  
}

# Repeating 1000 times using a sample size of 250

output_list <- replicate(1000, out_mod(sample.size = 250), simplify = FALSE)

df.out.outmod <- do.call(rbind, output_list) %>% # reformatting
  
  # Adding a new column that will be used to estimate the Monte Carlo SE of the estimate
  mutate(
    squared = (bias_for_one - mean(bias_for_one))^2,
    residuals_squared = (estimate - mean(estimate))^2
  )


# Calculating the mean bias and Monte Carlo SE of estimate

# See Morris et al. (2019) for details on calculating these

outmod.bias <- mean(df.out.outmod$bias_for_one) # bias  
outmod.se.of.bias <- sqrt(sum(df.out.outmod$squared)*(1 / (1000*999))) # Monte Carlo SE of bias (1000 is number of simulations, 999 is n - 1)

# Doubly Robust ----

dr <- function(sample.size){
  
  trt_effect <- 1.5 # same as in sim_data() function 
  
  # Simulating data
  
  df <- sim_data()
  
  # Fitting PS model - estimating the average treatment effect and stabilizing the weights
  
  ps.mod <- WeightIt::weightit(x ~ z1, 
                               data = df, 
                               method = "glm", 
                               estimand = "ATE", 
                               stabilize = TRUE)
  
  # Fitting outcome model 
  
  mod <- glm_weightit(y ~ x + z2, # note: not doubly robust for this example
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

output_list <- replicate(1000, dr(sample.size = 250), simplify = FALSE)

df.out.dr <- do.call(rbind, output_list) %>% # reformatting
  
  # Adding a new column that will be used to estimate the Monte Carlo SE of the estimate
  mutate(
    squared = (bias_for_one - mean(bias_for_one))^2,
    residuals_squared = (estimate - mean(estimate))^2
  )


# Calculating the mean bias and Monte Carlo SE of estimate

# See Morris et al. (2019) for details on calculating these

dr.bias <- mean(df.out.dr$bias_for_one) # bias  
dr.se.of.bias <- sqrt(sum(df.out.dr$squared)*(1 / (1000*999))) # Monte Carlo SE of bias (1000 is number of simulations, 999 is n - 1)


# Results ----

results <- data.frame(
  method = c("IPW", "Outcome Model", "Doubly Robust"), 
  bias = c(ipw.bias, outmod.bias, dr.bias), 
  se_of_bias = c(ipw.se.of.bias, outmod.se.of.bias, dr.se.of.bias)
)

results





