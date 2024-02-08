# Description: Variables to Add to PSM

# Ideas for code: 
# - What variables should be added to the propensity score? My initial thought before testing 
# is that it should only be confounders/based on a DAG. Does this include effect modifiers? 

# - How do adding effect modifiers affect: 
# 1. Bias
# 2. Precision
# 3. Power
# 4. What metric to use to check PS? 
# 5. Is there a difference between this for IPTW (stabilized) and PSM? 


# Libraries ----

library(tidyverse)
library(ggdag)
theme_set(theme_dag())

# DAG ----

# Four variables: 
# 1. Binary trt
# 2. Continuous Outcome
# 3. 1 Confounder
# 4. 1 Effect modifier (E)

dag = dagify(
  x ~ c, 
  y ~ c + e,
  exposure = "x",
  outcome = "y"
)

dag |> 
  ggdag(
    layout = "nicely")

dag |> ggdag()

dag |> 
  ggdag(
    layout = "star")

ggdag_adjustment_set(dag) # No need to adjust for anything

# Simulating Data ----

# y - continuous outcome
# x - binary trt 
# c - continuous
# e - binary

n = 250

c = rnorm(n = n, mean = 5, sd = 2)
e = rbinom(n = n , size = 1, prob = )

df = data.frame(
  x, c, e, y
)









