# Title: Adjusting for Mediator ----

# Description: This code demonstrates how adjusting for a mediator doesn't 
# estimate the total causal effect. 

# Setup ----

library(tidyverse) # ol' faithful
library(ggdag) # for drawing DAGs
library(broom) # tidying model outputs 

# Directed Acyclic Graph (DAG) ----

theme_set(ggdag::theme_dag()) # setting the theme for the DAG

# Let's create a DAG. We want: 
# - Exposure (Treatment)
# - Outcome
# - Two Confounders
# - One Mediator

dag <- ggdag::dagify(
  y ~ x + z1 + z2 + m1, 
  x ~ z1 + z2, 
  m1 ~ x,
  exposure = "x",
  outcome = "y"
) 

dag %>% 
  ggdag::ggdag()

# Which variables do we need to adjust for? 

dag %>% 
  ggdag::ggdag_adjustment_set()


#... Simulating Data ----

set.seed(456) # setting seed for reproducibility

n = 250 # arbitrary sample size
beta_trt = 1.5 # treatment effect 

# Simulating Data 

df <- data.frame(
  z1 = rnorm(n = n, mean = 5, sd = 2), 
  z2 = rbinom(n = n, size = 1, prob = 0.5)
) %>% 
  dplyr::mutate(
    prob = plogis(0.05*z1 + 0.2*z2), 
    x = rbinom(n = n, size = 1, prob = prob),
    m1 = 1 + 3*x + rnorm(n = n, mean = 0, sd = 1), # added noise so m1 and x are not colinear
    y = beta_trt*x + 0.5*z1 + 0.3*z2 + 1*m1 + rnorm(n = n, mean = 0, sd = 1)
  )


# Fitting Model ----

#... Adjusting for Confounders Only ----

glm(y ~ x + z1 + z2, 
           family = gaussian(), 
           data = df) %>% 
  broom::tidy() %>% 
  mutate(
    lower.ci = round(estimate -1.96*std.error, 3),
    upper.ci = round(estimate + 1.96*std.error, 3),
  ) %>% 
  select(
    term, estimate, lower.ci, upper.ci)
  )

#... Adjusting for Confounders and Mediator ----

glm(y ~ x + z1 + z2 + m1, 
    family = gaussian(), 
    data = df) %>% 
  broom::tidy()  %>% 
  mutate(
    lower.ci = round(estimate -1.96*std.error, 3),
    upper.ci = round(estimate + 1.96*std.error, 3),
  ) %>% 
  select(
    term, estimate, lower.ci, upper.ci)
)


  
  
  
  
  



