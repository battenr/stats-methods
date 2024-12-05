# Title: Stabilized vs Nonstabilized Weights 

# Description: Showing stabilized vs nonstabilized weights using a density plot. 

# Setup ----

#... Libraries ----

library(tidyverse) # ol' faithful
library(broom) # cleaning model outputs

# Simulating Data ----

set.seed(456) # setting seed for reproducibility

# Sample size 

n = 250 # arbitrarily choosing sample size 

# Simulating data with:
# - Two confounders
# - Two variables impacting the outcome
# - Binary Treatment
# - Continuous outcome

df <- data.frame(
  c1 = rbinom(n = n, size = 1, prob = 0.65),
  c2 = rnorm(n = n, mean = 10, sd = 2),
  c3 = rnorm(n = n, mean = 5, sd = 2),
  c4 = rbinom(n = n, size = 1, prob = 0.5)
) %>% 
  mutate(
    x = rbinom(n = n, size = 1, prob = plogis(0.1*c2 + 0.1*c1)),
    y = 3*x + 0.5*c1 + 1.5*c2 + 5*c3 + 2*c4 + rnorm(n = n) 
  )

# Fitting Propensity Score Models ----

# Using inverse probability of treatment weighting. First fitting a model using 
# stabilized weights. 

ipw.stab <- WeightIt::weightit(
  x ~ c1 + c2,
  data = df, 
  method = "glm",
  estimand = "ATE",
  stabilize = TRUE
)

# Fitting a model using nonstabilized weights

ipw.unstab <- WeightIt::weightit(
  x ~ c1 + c2,
  data = df, 
  method = "glm",
  estimand = "ATE",
  stabilize = FALSE
)

# Extracting Results for Plotting ----

# Making a dataframe called "results" that is the weights for both methods. 
# This is basically reformatting the above so that they can be used in a plot. 

results <- bind_rows(
  broom::tidy(ipw.stab$weights) %>% 
    mutate(Method = "Stabilized"),
  broom::tidy(ipw.unstab$weights) %>% 
    mutate(Method = "Nonstabilized")
) %>% 
  rename(
    weights = x
  )

# Plot ----

ggplot(results, aes(x = weights, fill = Method)) + 
  
  # Creating a density plot
  
  geom_density() + 
  
  # Using the minimal theme as a base
  
  theme_minimal() + 
  
  # Adding the labels for the axes  and titles
  
  labs(
    title = "Inverse Probability of Treatment Weights (ATE)",
    subtitle = "Stabilized vs. Nonstabilized Weights",
    y = "Density",
    x = "Weights"
  ) +
  
  # Customize the theme to center the plot and text elements
  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    text = element_text(size = 20),
    axis.text = element_text(color = "black")
  ) 

