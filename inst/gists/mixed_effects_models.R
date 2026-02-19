# Title: Linear Mixed Effects Models for Causal Inference 

# Description: Mixed effects models can be useful for causal inference, in particular
# when there are clusters of groups that we want to account for. If we do not account
# for these clusters, we could get an incorrect result 
# (specifically we need to account for the differences within and between clusters)

# Note: This code was created with the help of Gemini

# Setup ----

#... Libraries ----

library(tidyverse) # ol' faithful
library(lme4) # for a mixed effect model
library(broom) # to clean output from model 
library(broom.mixed) # to clean output from a mixed model 

#... Functions ----

# For simulating data 

sim_data <- function(n_groups = 20, # number of clusters 
                        n_per_group = 15, # number of individuals per group
                        beta_trt = 1.5) { # "true" treatment effect 
  
  # Creating the cluster-level effect (u1)
  # This will be considered "unobserved", something that we didn't measure
  
  groups <- data.frame(
    cluster_id = 1:n_groups,
    u1 = rnorm(n_groups, mean = 0, sd = 3) # The "Hidden" cluster effect
  )
  
  # Expanding to the individual levels (so one row per person)
  
  df <- expand.grid(cluster_id = 1:n_groups, 
                    rep = 1:n_per_group) %>%
    left_join(groups, by = "cluster_id") %>%
    mutate(
      # Individual level covariates
      z1 = rnorm(n = n(), 5, 2),
      # Treatment assignment (randomized within clusters)
      x = rbinom(n = n(), 1, 0.5),
      # Outcome: Includes the hidden cluster effect (u1)
      y = beta_trt * x + 0.5 * z1 + u1 + rnorm(n(), 0, 1)
    )
  
  return(df)
}

# Simulating Data ----

set.seed(123) # setting seed for reproducibility

data <- sim_data() # for simulating data, function in above section

# Fitting Models ----

#... GLM ----

# Fitting a generalized linear model, accounting for the cluster_id but not 
# for the random effects (a key part of linear mixed effects models)

model_glm <- glm(y ~ x + z1 + cluster_id, 
             family = gaussian(link = "identity"),
             data = data)

#... Mixed Effects Model ----

# Fitting a linear mixed effects model

model_mem <- glmer(y ~ x + z1 + (1 | cluster_id), 
                  family = gaussian(link = "identity"),
                  data = data)

# Comparing Results ----

#... Cleaning Outputs from Models ----

summary_glm <- tidy(model_glm, conf.int = TRUE) %>% 
  mutate(model = "Standard GLM (Ignoring Random Effects)")

summary_mem <- tidy(model_mem, conf.int = TRUE) %>% 
  filter(effect == "fixed") %>% 
  mutate(model = "Mixed Effects (Accounting for Random Effects)")

#... Comparing Models ----

comparison <- bind_rows(summary_glm, summary_mem) %>%
  filter(term == "x") %>%
  select(model, estimate, conf.low, conf.high)

print(comparison)
