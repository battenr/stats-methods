# Title: Plotting Propensity Scores and Weights 

# Description: Plotting propensity scores and weights is helpful for checking the 
# entire distribution and assumptions. This code gives an example of doing that. 

# Setup ----

library(tidyverse) # ol' faithful
library(WeightIt) # for using IPTW 
library(patchwork) # for combining plots

# Simulating Data ----

# Simulating multiple covariates here. 
# - Two continuous variables (one confounder, one not)
# - Two binary confounders (one confounder, one not)
# - Continuous outcome
# - Binary treatment

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

df %>% count(x) # check what the number of patients with treatment status = 1 are

# IPTW ----

# Using weightit from the WeightIt package to calculate stabilized IPW. 

ipw.weights <- WeightIt::weightit(x ~ c1 + c2, 
              data = df,
              stabilize = TRUE
              )

# Weights Dataframe ----

# Putting the weights and propensity score into a dataframe for plotting later. 

df_weights <- data.frame(
  weights = ipw.weights$weights,
  ps = ipw.weights$ps
)

# Plots! ----

# Creating two plots: one for the weights and one for the propensity score. 

#... Plot of the Weights ----

p1 <- ggplot(df_weights, aes(x = weights)) +
  geom_density(color = "darkblue") +
  geom_histogram(aes(y = after_stat(density)), fill = "pink", alpha = 0.5) +
  labs(
    title = "Distribution of Weights",
    subtitle = "Using IPTW for the Average Treatment Effect",
    x = "Stabilized Weights",
    fill = "Scenario",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    text = element_text(size = 20),
    axis.text = element_text(color = "black")
  ) 

#... Plot of the Propensity Score ----

p2 <- ggplot(df_weights, aes(x = ps)) +
  geom_density(color = "darkblue") +
  geom_histogram(aes(y = after_stat(density)), fill = "pink", alpha = 0.5) +
  labs(
    title = "Distribution of Propensity Scores",
    subtitle = "Using IPTW for the Average Treatment Effect",
    x = "Propensity Score",
    fill = "Scenario",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    text = element_text(size = 20),
    axis.text = element_text(color = "black")
  ) 

# Combining Plots ----

p1 / p2 # combining the plots into one using the patchwork package
