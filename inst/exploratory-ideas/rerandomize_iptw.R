# Title: Rerandomization from Observational Data

# Description: Re-randomizing patients using observational data, then reweighting 

# Description: Trying out a new method for causal inference. Calling it 
# pseudo-randomization. The idea is to use all patients and randomize them, 
# similar to a clinical trial where all patients are randomized. We then 
# reweight patients using IPTW to estimate the ATE 

# Initial idea is to reweight for the group. if the groups correspond then receive weight of 1 

# In control group and actually received control then 1
# In control group and received treat, receive weight that you'd normally use ATE

# In treatment group and actually received trt then 1
# In treatment group and received control, receive weight that you'd normally use for ATE

# Setup ----

#... Libraries ----

library(tidyverse)
library(ggdag)

#... Functions ----

# Load all functions

lapply(list.files("R/"), function(x) source(paste0("R/", x)))

#... Dependencies ----

# DAG ----

theme_set(theme_dag())

# Starting simple with a binary treatment, continuous outcome and one continuous confounder

dag = dagify(
  x ~ c, 
  y ~ x + c,
  exposure = "x",
  outcome = "y"
)

dag |> 
  ggdag(
    layout = "nicely")

# Simulated Data ----







