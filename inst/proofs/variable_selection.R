# Title: Variable selection

# Description: Looking at different variable selection methods

# 1. Comparing stepwise selection (based on p-values), with DAGs 
#    compared MSE, bias, etc (altho obvious collider bias)
# 2. Only effect modifiers
# 3. Only confounders
# 4. All variables (obviously not a good idea with colliders)

# Setup ----

#... Libraries ----

library(tidyverse)
library(ggdag)
theme_set(theme_dag())

#... Functions ----

# Load all functions

# lapply(list.files("R/"), function(x) source(paste0("R/", x)))

#... Dependencies ----

# DAG ----

dag = dagify(
  x ~ c, 
  y ~ x + c, 
  exposure = "x", 
  outcome = "y"
)

dag |> ggdag()

dag |> ggdag(layout = "nicely") 

# Simulated Data ----

# Note: an obvious drawback of stepwise selection is including a collider. Therefore going to exclude colliders
# for this exercise

# Variable Selection ----

#... Stepwise Selection ----

#... Using DAGs ----

# Comparing Stepwise vs DAGs ----

