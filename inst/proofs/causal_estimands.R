# Title: Causal Estimands

# Description: Demonstrating different causal estimands give different results. 
# Each result isn't necessarily correct or incorrect. Each one gives a 
# different answer to a different question. 

# Setup ----

#... Libraries ----

library(tidyverse) # ol' faithful
library(WeightIt) # For Weighting 
library(broom)

# Simulated Data ----


sim_data <- function(n = 1000, # sample size 
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

df <- sim_data()

# Fitting Different Estimands ----

#... ATE ----

ip.mod <- WeightIt::weightit(x ~ z1 + z2, 
                             data = df,
                             stabilize = TRUE, 
                             estimand = "ATE")

ip.mod$weights

mod <- WeightIt::glm_weightit(y ~ x, 
                              data = df, 
                              weights = ip.mod$weights)

ate <- mod %>% 
  broom::tidy() %>% 
  filter(term == "x") %>% 
  select(term, estimate, std.error) %>% 
  mutate(
    estimand = "ATE"
  )


#... ATT ----

ip.mod <- WeightIt::weightit(x ~ z1 + z2, 
                             data = df,
                             estimand = "ATT")

mod <- WeightIt::glm_weightit(y ~ x, 
                              data = df, 
                              weights = ip.mod$weights)

att <- mod %>% 
  broom::tidy() %>% 
  filter(term == "x") %>% 
  select(term, estimate, std.error) %>% 
  mutate(
    estimand = "ATT"
  )


#... ATU ----

ip.mod <- WeightIt::weightit(x ~ z1 + z2, 
                             data = df,
                             estimand = "ATU")

mod <- WeightIt::glm_weightit(y ~ x, 
                              data = df, 
                              weights = ip.mod$weights)

atu <- mod %>% 
  broom::tidy() %>% 
  filter(term == "x") %>% 
  select(term, estimate, std.error) %>% 
  mutate(
    estimand = "ATU"
  )

#... ATO ----

ip.mod <- WeightIt::weightit(x ~ z1 + z2, 
                             data = df,
                             estimand = "ATO")

mod <- WeightIt::glm_weightit(y ~ x, 
                              data = df, 
                              weights = ip.mod$weights)

ato <- mod %>% 
  broom::tidy() %>% 
  filter(term == "x") %>% 
  select(term, estimate, std.error) %>% 
  mutate(
    estimand = "ATO"
  )

# Viewing All Results ----

rbind(ate, att, atu, ato)

