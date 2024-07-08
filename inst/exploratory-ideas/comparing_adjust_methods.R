# Comparing Confounder Methods

# Title for Blog Post: 
# Something about Comparing Adjustment Techniques

# Working Title:  Comparison is the Thief of Joy 

# - Outcome Regression
# - Matching
# - IPTW
# - Entropy Balancing

# This code is going to be used for a blog post comparing these different methods. 

# Preview of blog: 
# - talk about how overall want to compare different methods. Things to keep in mind

# Setup ----

library(tidyverse)
library(WeightIt)
library(MatchIt)
library(broom) # for tidying the output results
library(sandwich) # robust sandwich estimators

set.seed(123)
n <- 1000
df <- tibble(
  trt = rbinom(n, 1, 0.5),
  cov1 = rnorm(n),
  cov2 = rnorm(n),
  outcome = 0.5 * trt + 0.3 * cov1 + 0.2 * cov2 + rnorm(n)
)


# Using One Step ----

# For now, using this one step. later this will be made into a function and re-run 

#... IPTW ----

weighit_out <- WeightIt::weightit(trt ~ cov1 + cov2, 
                                  data = df, 
                                  method = "ps",
                                  estimand = "ATT")

iptw_mod <- glm(outcome ~ trt, 
                data = df, 
                weights = weighit_out$weights)


iptw_se <- sqrt(diag(sandwich::sandwich(iptw_mod))) # 0.0696

iptw_trt_se <- iptw_se[2]

iptw_theta <- broom::tidy(iptw_mod)[2,2]

#... Entropy Balancing ----

eb_out <- WeightIt::weightit(trt ~ cov1 + cov2, 
                             data = df, 
                             method = "ebal",
                             estimand = "ATT")

eb_mod <- glm(outcome ~ trt, 
                data = df, 
                weights = eb_out$weights)


eb_se <- sqrt(diag(sandwich::sandwich(eb_mod, type = "HCO"))) # 0.0696

eb_trt_se <- eb_se[2]

eb_theta <- broom::tidy(eb_mod)[2,2]

#... Outcome Model ----

out_mod <- glm(outcome ~ trt + cov1 + cov2, 
               data = df)

broom::tidy(out_mod)

out_se <- broom::tidy(out_mod)

out_trt_se <- out_se[2,3]

out_trt_se

out_theta <- broom::tidy(out_mod)[2,2]

#... Matching ----

match_out <- MatchIt::matchit(trt ~ cov1 + cov2, 
                              data = df, 
                              method = "nearest")

# Get matched data
matched_data <- match.data(match_out)

# Fit outcome model on matched data
match_mod <- glm(outcome ~ trt, 
                   data = matched_data)

match_se <- sqrt(diag(sandwich::sandwich(match_mod))) 

match_trt_se <- match_se[2]

match_trt_se

match_theta <- broom::tidy(iptw_mod)[2,2]

