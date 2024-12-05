# Title: Overlap Weighting

# Description: A demonstration of overlap weighting using the propensity score

# Setup ----

#... Libraries ----

library(tidyverse) # ol' faithful
library(WeightIt) # for weighting
library(cobalt) # for balance plots

#... Simulating Data ----

set.seed(456) # setting seed for reproducibility

# Sample size 
n = 250 # arbitrarily choosing sample size 

# Simulating data with:
# - Two confounders: one binary, one continuous
# - Binary Treatment
# - Continuous outcome

df <- data.frame(
  c1 = rbinom(n = n, size = 1, prob = 0.65), # binary confounder
  c2 = rnorm(n = n, mean = 5, sd = 2) # continuous confounder
) %>% 
  mutate(
    x = rbinom(n = n, size = 1, prob = (0.05*c2 + 0.5*c1)), # treatment 
    y = 3*x + 0.5*c1 + 1.5*c2 + rnorm(n = n) # continuous outcome
  )

# Checking how many "treated" individuals there are 

table(df$x)

# Fitting Propensity Score Model ----

# Specifying the ATO uses overlap weights: 
# - PS for untreated patients
# - (1-PS) for treated patients

ow <- WeightIt::weightit(
  x ~ c1 + c2,
  data = df, 
  method = "glm", # Using logistic regression for propensity score
  estimand = "ATO" # targeting the overlap population
)

# Plot for Balance ----

# Using the cobalt package to plot the distribution of the propensity score 
# pre-/post-weighting then reformatting plot for readability

cobalt::bal.plot(ow, 
         "prop.score", 
         which = "both") +
  ggtitle("Propensity Score Distribution using Overlap Weighting") + 
  labs(x = "Propensity Score", fill = "Treatment Status") +
  scale_fill_discrete(labels = c("Untreated", "Treated")) + 
  theme(
    plot.title = element_text(hjust = 0.5),
    text = element_text(size = 20)) +
  lims(x = c(0, 1))
