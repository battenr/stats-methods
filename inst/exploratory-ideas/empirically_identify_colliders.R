# Title: How to identify a collider empirically

# Description: Trying to figure out if there is a way to empirically determine if a variable is in fact
# a collider. Can use the Schneweiss paper about IVs across a range of IVs (strong, not strong, etc) 
# and do something simple for collider

# Setup ----

#... Libraries ----

library(tidyverse)
library(ggdag)

#... Functions ----

# Load all functions

lapply(list.files("R/"), function(x) source(paste0("R/", x)))

#... Dependencies ----

# DAG ----



# Simulated Data ----