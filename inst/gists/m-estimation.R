# Title: M-Estimation After IPTW ----

# Description: Demonstrating M-estimation after IPTW. 
# Showing how it can be helpful, and comparing to if we didn't use anything 
# (in this case using the SEs from GLM)

# Note: code in this script was largely inspiried by Ross et al. (2024). 
# The code supplied in that paper was used to create this code 
# (the authors used a binary outcome, while this code uses a continuous outcome)

# Setup ----

#... Packages ----

library(tidyverse) # ol' faithful
library(WeightIt) # for IP weights & M-Estimation
library(broom) # tidying results

#... Functions -----
 
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
    z1 = rnorm(n = n, mean = z1_mean, sd = z1_sd), 
    z2 = rbinom(n = n, size = z2_size, prob = z2_prob)
  ) %>% 
    dplyr::mutate(
      prob = plogis(z1_on_x*z1 + z2_on_x*z2), 
      x = rbinom(n = n, size = 1, prob = prob), 
      y = beta_trt*x + z1_on_y*z1 + z2_on_y*z2 + rnorm(n = n, mean = 0, sd = 1)
    )
  
  # Return the dataframe
  
  return(df)
}


# Example (One Iteration) ----

set.seed(456) # setting seed for reproducibility 
 
df <- sim_data() # simulating data

#... PS Model/IPTW ----

psmod <- WeightIt::weightit(x ~ z1 + z1, 
                   method = "glm", 
                   estimand = "ATE",
                   stabilize = TRUE,
                   data = df)


#... Fitting Outcome Model Using GLM ----

mod <- glm(y ~ x, 
           family = gaussian(link = "identity"),
           weights = psmod$weights,
           data = df)

# Cleaning the results and calculating the 95% CIs 

broom::tidy(mod) %>% 
  dplyr::mutate(
    lower_ci = round(estimate - 1.96*std.error, 2),
    upper_ci = round(estimate + 1.96*std.error, 2),
    estimate = round(estimate, 2),
    result = paste0(estimate, " 95% CI: ", lower_ci, " to ", upper_ci)
  ) %>% 
  dplyr::filter(term == "x") %>% 
  dplyr::select(result)

#... M-Estimation ----

# Using M-Estimation for the standard errors. For this, using the glm_weightit
# function and the weightit object (in this case that's psmod)

glm_weightit(y ~ x, 
             data = df,
             weightit = psmod) %>% # this is a weightit object, from above
  summary() 

# Replicating 1000 times ----

# If we are going to compare the methods, we can't only use one iteration. 
# So we are going to use 1000. In reality, we would use more than 1000 and put 
# some thought into choosing the number of iterations. 

# For this example, I picked 1000 arbitrarily/so it wouldn't take too long to run. 

#... GLM ----

glm_sim <- function(){
  df <- sim_data() # function from above 
  
  
  psmod <- WeightIt::weightit(x ~ z1 + z1, 
                              method = "glm", 
                              estimand = "ATE",
                              stabilize = TRUE,
                              data = df)
  
  mod <- glm(y ~ x, 
             family = gaussian(link = "identity"),
             weights = psmod$weights,
             data = df)
  
  result <- broom::tidy(mod) %>% 
    dplyr::mutate(
      lower_ci = estimate - 1.96*std.error,
      upper_ci = estimate + 1.96*std.error
    ) %>% 
    filter(term == "x") %>% 
    mutate(
      contains_true = ifelse(between(1.5, lower_ci, upper_ci), TRUE, FALSE)
    ) %>% 
    dplyr::rename(
      se = std.error
    ) %>% 
    dplyr::select(
      lower_ci, 
      upper_ci, 
      estimate, 
      se,
      contains_true)
  
  return(result)
  
}

# Repeating 1000 times 

output_list <- replicate(1000, glm_sim(), simplify = FALSE)

df.glm <- do.call(rbind, output_list) # making it a data frame so we can work with it

# Calculating the Relative % Error

meanse <- mean(df.glm$se) # Average ModSE

empse <- sqrt(var(df.glm$estimate)) # Empircal SE

100*(meanse/empse - 1) # relative % error

#... M-Estimation ----

# Same as what was done using GLM but now using M-estimation for the SEs

m_est_sim <- function(){
  df <- sim_data() # function from above 
  
  
  psmod <- WeightIt::weightit(x ~ z1 + z1, 
                              method = "glm", 
                              estimand = "ATE",
                              stabilize = TRUE,
                              data = df)
  
  mod <- WeightIt::glm_weightit(y ~ x, 
                       data = df,
                       weightit = psmod) %>% # this is a weightit object, from above
    summary() 
  
  result <- data.frame(
    lower_ci = mod$coefficients[2,1] - 1.96*mod$coefficients[2,2] ,
    upper_ci = mod$coefficients[2,1] + 1.96*mod$coefficients[2,2],
    estimate = mod$coefficients[2,1],
    se = mod$coefficients[2,2]
  ) %>% 
    mutate(
      contains_true = ifelse(between(1.5, lower_ci, upper_ci), TRUE, FALSE)
    ) %>% 
    dplyr::select(lower_ci, upper_ci, estimate, contains_true, se)
  
  return(result)
  
}

output_list <- replicate(1000, m_est_sim(), simplify = FALSE) # Repeating 1000 times

df.mest <- do.call(rbind, output_list) # combining to a dataframe 

 # Relative % Error in SE 

meanse <- mean(df.mest$se) # Average ModSE

empse <- sqrt(var(df.mest$estimate)) # Empirical SE 

100*(meanse/empse - 1) # Relative % error 




