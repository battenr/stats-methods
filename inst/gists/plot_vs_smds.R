# Title: Checking Balance After IPTW: SMDs & Plots

# Description: This code demonstrates how standardized mean differences (SMDs) 
# can be a useful tool to check for balance but may not show the entire picture. 

# Setup ----

#... Packages ----

library(tidyverse) # ol faithful
library(WeightIt) # for IPTW
library(cobalt) # for checking balance

#... Functions ----

# Simulating Data
# - Binary Treatment
# - Continuous Outcome
# - Two Confounders (one continuous, one binary)

sim_data <- function(n = 250, 
                     beta_trt = 1.5,
                     z1_mean = 1, z1_sd = 5,
                     z2_size = 1, z2_prob = 0.5,
                     z1_on_x = 0.4, z2_on_x = 0.2,
                     z1_on_y = 0.5, z2_on_y = 0.3){
  
  df <- data.frame(
    z1 = rnorm(n = n, mean = z1_mean, sd = z1_sd), 
    z2 = rbinom(n = n, size = z2_size, prob = z2_prob)
  ) %>%
    mutate(
      prob = plogis(z1_on_x * z1 + z2_on_x * z2),
      x = rbinom(n = n, size = 1, prob = prob),
      y = beta_trt * x + z1_on_y * z1 + z2_on_y * z2 + rnorm(n, 0, 1)
    )
  
  return(df)
}

# Simulating Data ----

set.seed(456) # setting seed for reproducibility

df <- sim_data() # using the function from above to simulate a dataset

# Inverse Probability Weighting ----

ps.model <- WeightIt::weightit(x ~ z1 + z2 , 
                               data = df, 
                               method = "glm", 
                               estimand = "ATE", 
                               stabilize = TRUE)

df$w <- ps.model$s.weights # adding weights to the dataset

# Checking Balance ----

#... Standardized Mean Differences (SMDs) ----

cobalt::bal.tab(ps.model, un = TRUE)

#... Plots ----

# Checking Z1

cobalt::bal.plot(ps.model, which = "both", var.name = "z1") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    text = element_text(size = 20)
  ) +
  labs(x = "Z1", title = "Balance for Z1", subtitle = "Standardized Mean Difference - Unadjusted 1.66; Adjusted: 0.017")

# Optional (for checking propensity score and z2)

cobalt::bal.plot(ps.model, which = "both", var.name = "prop.score")
cobalt::bal.plot(ps.model, which = "both", var.name = "z2")

