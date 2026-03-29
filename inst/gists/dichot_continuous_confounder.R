# Title: Why Dichotomizing a Continuous Confounder is a Bad Idea 

# Description: Dichotomizing a continuous confounder is a bad idea. 
# This code demonstrates why it's a bad idea, and how it can lead to 
# bias. 

# Setup ----

#... Packages ----

library(tidyverse) # ol faithful
library(WeightIt) # for IP weighting
library(cobalt) # for visualizing balance 

#... Functions ----

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

# Simulating Data ----

# For this data we will have: 
# - Binary treatment
# - Two confounders: one continuous, one binary
# - Continuous outcome

#set.seed(456) # for reproducibility, if wanted 

df.test <- sim_data() %>% 
  mutate(
    # Making a version of the continuous confounder, that is binary. 
    # Picking arbitrary value of 7 as cutpoint
    z1_binary = ifelse(z1 > 7, 1, 0)
  ) 

# Propensity Score Models ----

#... Correct Model (z1 is continuous)

ps.cont <- WeightIt::weightit(x ~ z1 + z2, 
                                 data = df.test, 
                                 estimand = "ATE")

# Checking for balance 

cobalt::bal.plot(ps.cont, which = "both", var = "z1")

#... Incorrect Model (z1 is binary)

ps.bin <- WeightIt::weightit(x ~ z1_binary + z2, 
                                 data = df.test, 
                                 estimand = "ATE")

# Checking for balance 

cobalt::bal.plot(ps.bin, which = "both", var = "z1_binary")

# Outcome Models ----

#... Continuous ----

WeightIt::glm_weightit(y ~ x, 
                       data = df.test, 
                       weightit = ps.cont)

#... Binary ----

WeightIt::glm_weightit(y ~ x, 
                       data = df.test, 
                       weightit = ps.bin)

# Repeating to assess bias ----

# Scenario 1: Correctly Modelling z1 as continuous ----

scenario1 <- function(n = 250, beta_trt = 1.5){
  
  df <- sim_data()
  
  psmod <- WeightIt::weightit(x ~ z1 + z2, 
                               data = df, 
                               estimand = "ATE",
                               stabilize = TRUE)
  
  outmod <- WeightIt::glm_weightit(y ~ x, 
                 data = df, 
                 weightit = psmod
  )
  
  results <- broom::tidy(outmod) %>% 
    filter(term == "x") %>% 
    select(term, estimate, std.error) 
  
  return(results)
}

# Repeat...Repeat....Repeat! 

# Using the function we will repeat it 1000 times. 

check_1000.scen1 <- replicate(1000, scenario1(), simplify = FALSE)

df.out.scen1 <- do.call(rbind, check_1000.scen1) %>%  # formatting it
  dplyr::mutate(
    difference = estimate - 1.5,
    squared = (difference - mean(difference))^2
  ) 


# Calculating the mean bias and Monte Carlo SE of estimate

# See Morris et al. (2019) for details on calculating these

bias.scenario1 <- mean(df.out.scen1$difference) # bias  
se.scenario1 <- sqrt(sum(df.out.scen1$squared)*(1 / (1000*999))) # Monte Carlo SE of bias (1000 is number of simulations, 999 is n - 1)


# Scenario 2: Z1 is binary ----

scenario2 <- function(n = 250, beta_trt = 1.5){
  
  df <- sim_data() %>% 
    mutate(z1_binary = ifelse(z1 > 7, 1, 0))
  
  psmod <- WeightIt::weightit(x ~ z1_binary + z2, 
                              data = df, 
                              estimand = "ATE",
                              stabilize = TRUE)
  
  outmod <- WeightIt::glm_weightit(y ~ x, 
                                   data = df, 
                                   weightit = psmod)
  
  results <- broom::tidy(outmod) %>% 
    filter(term == "x") %>% 
    select(term, estimate, std.error) 
  
  return(results)
}

# Repeat...Repeat....Repeat! 

# Using the function we will repeat it 1000 times. 

check_1000.scen2 <- replicate(1000, scenario2(), simplify = FALSE)

df.out.scen2 <- do.call(rbind, check_1000.scen2) %>%  # formatting it
  dplyr::mutate(
    difference = estimate - 1.5,
    squared = (difference - mean(difference))^2
  ) 


# Calculating the mean bias and Monte Carlo SE of estimate

# See Morris et al. (2019) for details on calculating these

bias.scenario2 <- mean(df.out.scen2$difference) # bias  
se.scenario2 <- sqrt(sum(df.out.scen2$squared)*(1 / (1000*999))) # Monte Carlo SE of bias (1000 is number of simulations, 999 is n - 1)

# Results ----

data.frame(
  scenario = c("Continuous Z1", "Binary Z1"),
  bias = c(bias.scenario1, bias.scenario2),
  montese = c(se.scenario1, se.scenario2)
)
