# Title: Bayesian Parametric G-Formula as a Sensitivity Analysis

# Description: This code is for a presentation that I gave at Bayes Pharma 2024. 
# It discusses using the Bayesian parametric g-formula as a sensitivity analysis. 

# This particular script focuses on simulating data that will be used 
# to create an example 

# Outcome type: risk difference 

# Setup ----

library(tidyverse) # ol' faithful
library(brms) # for bayesian statistics
library(tidybayes) # for creating plots afterwards for the Bayesian part
library(ggdag) # for drawing the dag 
library(WeightIt) # for IPTW
library(broom) # for cleaning results from model 

# DAG ---- 

# First we draw a DAG 

theme_set(theme_dag()) # setting the theme 

dag = dagify(
  x ~ l1 + l2, 
  y ~ l1 + l2, 
  exposure = "x", 
  outcome = "y"
)

dag |> 
  ggdag(
    layout = "nicely")

ggdag::ggdag_adjustment_set(dag) # need to adjust for L1 and L2 


# Simulating Data ----

set.seed(2345) # for reproducibility

# There are two confounders, one binary and one continuous. They have different strengths 
# on the outcome. 

# If it's hard to think in terms of abstract variables, can use an example. 
# Think of high blood pressure and a treatment. The two confounders could be 
# age and sex. (Note this is just for understanding, the below data isn't setup to 
# replicate this scenario)

n = 300 # Arbitrarily chosen sample size. 

df <- data.frame(
  l1 = rnorm(n = n, mean = 5, sd = 1), 
  l2 = rbinom(n = n, size = 1, prob = 0.6)
) %>% 
  dplyr::mutate(
    x = rbinom(n = n, size = 1, prob = 0.05 + 0.1*l1 + 0.2*l2), # binary treatment
    y = 0.15*x + 0.5*l1 + 1.5*l2 + rnorm(n = n) # known treatment effect of 0.15 (risk difference)
  )

# df %>% count(x) # checking how many are in each group (making sure there are no NAs)


