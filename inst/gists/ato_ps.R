# Title: Non-Overlapping Propensity Scores

# Description: Understanding what exactly the overlap population is 
# can be tricky. The goal of this script is to demonstrate 
# where there are non-overlapping propensity scores. This can then be shown 
# how the average treatment effect in the overlap (ATO) can be a useful 
# causal estimand. 

# Setup ----

#... Libraries ----

library(tidyverse) # ol' faithful
library(WeightIt) # for weighting for ATO
library(cobalt) # showing both plots for unadjusted and adjusted sample 

# Simulating Data  ----

set.seed(654) # setting seed for reproducibility 

sample.size = 250 # artbitrarily choosing a sample size of 250 

# Creating simulated data 

# - Binary Treatment 
# - Continuous Outcome 
# - Two confounders (one binary, Z1, one continuous Z2)

# The goal here is to make it so that the groups have a lack of overlap in 
# propensity scores. This is achieved by having z2 differ by z1. 
# However, there  may be a better way to do this 


df <- data.frame(
  z1 = rbinom(n = sample.size, size = 1, prob = 0.5)  # binary confounder
) %>%
  dplyr::mutate(
    z2 = dplyr::case_when(
      z1 == 1 ~ rnorm(n = sample.size, mean = 16, sd = 2),  # z2 when z1 is 1 
      z1 == 0 ~ rnorm(n = sample.size, mean = 4, sd = 2)    # z1 when z1 is 0 
    ),
    
    x = rbinom(n = sample.size, size = 1, prob = plogis(0.4 * z1 + 0.2 * z2)),  # Stronger influence of z2 than z1
    y = 1.5*x + 3*z1 + 2*z2  # Outcome variable
  )

# Calculating Propensity Scores ----

# Now we want to calculate the propensity scores. There are several ways to do this. 
# We could do this ourselves, using a logistic regression, however the goal is to show 
# estimating the average treatment effect in the overlap. 

# To do this we'll use the weightit() function from the WeightIt package. 

wm <- WeightIt::weightit(x ~ z1 + z2, 
                         data = df,
                         method = "glm", 
                         estimand = "ATO")

cobalt::bal.plot(wm, var.name = "prop.score", which = "both") +  
  ggtitle("Overlap Weighting", "Estimating the Average Treatment Effect in the Overlap Population") + 
  labs(x = "Propensity Score") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 24), 
    plot.subtitle = element_text(hjust = 0.5),
    text = element_text(family = "Jost", size = 18)
  )
