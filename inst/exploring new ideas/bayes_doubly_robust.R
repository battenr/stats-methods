# Title: Simultaneously modelling both the outcome and the treatment. 

# Description: Causal effects typically model the confounder-outcome relationshp
# and/or the confounder-treatment relationship. Doubly robust methods, becoming
# more popular recently, have aimed to solved this problem. 

# The goal of this script is to explore the possibility of modelling both y
# and x at the same time. There are a few benefits to this: 
# - Typical benefits of Bayesian method 
# - 


library(tidyverse)
library(brms)
library(tidybayes)
library(bayesplot)

# Playing around with modelling the treatment at same time as outcome

source("R/sim_data.R")

df <- sim_data()

# beta trt is 1.5 

# Testing Ordinary GLM 

glm(y ~ x + z1 + z2, 
    family = gaussian(), 
    data = df) %>% 
  broom::tidy()

# Bayes Approach

priors <- c(
  prior(normal(0, 2), class = "b", coef = "x"),
  prior(normal(0, 2), class = "b", coef = "z1"), 
  prior(normal(0, 2), class = "b", coef = "z2")
)

bform <- bf(y ~ x + z1 + z2) +
  bf(x ~ z1 + z2) + set_rescor(FALSE)

bmod <- brms::brm(bform, 
                  data = df, 
                  family = gaussian(),
                  prior = prior(normal(0,2)) # using same prior for all 
                  )


summary(bmod)


# test = tidybayes::add_epred_draws(newdata = df %>% select(x, z1, z2), 
#                            bmod, 
#                            ndraws = 10)

test2 = tidybayes::add_epred_draws(newdata = df %>% select(x, z1, z2), 
                           bmod, 
                           value = "b_x",
                           ndraws = 100)

# 
# ggplot(data = test, 
#        mapping = aes(x = .epred)) +
#   stat_halfeye()

ggplot(data = test2, 
       mapping = aes(x = b_x)) +
  stat_halfeye()

median_hdci(test2$b_x)

0.689 (0.417 to 5.90) # this is wildly wrong. 

?tidybayes::median_hdci()

?median_hdci()
  
  
  geom_stat_halfeye()

?add_epred_draws

bmod %>% 
  spread_draws(x) %>% 
  ggplot() + 
  geom_stat_halfeye()

conditional_effects(bmod, "x", resp = "y")

# Now Let's Repeat This ----

# Only Modelling Confounder-Treatment Relationship ----


