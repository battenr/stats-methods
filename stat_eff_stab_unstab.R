# Title: Statistical Efficency of Stabilized IPTW vs unstabilized IPTW

# Description: Comparing the use of stabilized vs unstabilized IPTW 

# Setup ----

#... Libraries ----

library(tidyverse)
library(ggdag)

#... Dependencies ----

# Simulated Data ----

set.seed(123)

# Note: 
# 1. Assuming a binary treatment 
# 2. Comparing these two weights across MSE, bias and coverage 

# To do/ideas: 
# 1. Are stabilized weights more efficient vs non-stabilized? 
# 2. Under what conditions are they pretty similar? Under what conditions are they different? 
# 3. W

# Formulas for measures from page 13 of morris et al. 2019

# Assuming X, Y and Z ----

#... DAG ----

dag <- dagify(
  y ~ x + z, 
  x ~ z,
  exposure = "x", 
  outcome = "y"
)

ggdag::ggdag(dag, layout = "circle")

ggdag_paths(dag)

ggdag_adjustment_set(dag)

ggdag_dseparated(dag)

ggdag_dseparated(dag, controlling_for = "z")

#.... Data ----

df <- data.frame(
  X  =
)



u_stab <- 

w_stab <- function(df, treat, outcome){
  
  
  
  
}
