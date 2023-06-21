# Title: PSM - Gradient Boosted Models

# Description: Working through using gradient boosted models 
# Ref: https://cran.r-project.org/web/packages/IRexamples/vignettes/Ex-01-Propensity-Score-Weights-Using-GBM.html#estimating-propensity-score-weights-using-the-twang-package

# Setup ----

#... Libraries ----

library(tidyverse)

#... Functions ----

# Load all functions

lapply(list.files("R/"), function(x) source(paste0("R/", x)))

#... Dependencies ----

# Simulated Data ----