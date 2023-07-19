# does including a continuous variable in a model increase power? compared to if we 
# put it in as a categorical variable

# Title: Including a continuous variable increases power

# Description: Trying to simulate data to show that including a continuous variable increases power

# References: Kahan et al. (2014) - risks and rewards...

# Setup ----

#... Libraries ----

library(tidyverse)
library(ggdag)
theme_set(theme_dag())
# library(MASS) # can use for a multivariate normal distribution (kinda need real data first)

#... Functions ----

# Load all functions

# lapply(list.files("R/"), function(x) source(paste0("R/", x)))

#... Dependencies ----

# DAG ----

dag = dagify(
  y ~ x + C, # assuming continuous outcome to start 
  # variable c that is prognostic of the outcome 
  exposure = "x",
  outcome = "y"
)

dag |> 
  ggdag(
    layout = "star")

ggdag_adjustment_set(dag) # Need to adjust for c since a confounder

# Simulated Data ----
