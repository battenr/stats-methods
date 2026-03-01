# Playing Around with the simcausal package 

library(tidyverse)
library(simcausal)


# Setting Empty DAG ----

D <- DAG.empty()

# Adding Variables ----

Dvars <- D +
  # Confounder: Sleep (Hours of sleep, normally distributed)
  node("sleep",
       distr = "rnorm",
       mean = 7, 
       sd = 1.5) +
  # Exposure: Coffee (Binary: 1 = Drank Coffee, 0 = No Coffee)
  # People who sleep less (lower Sleep value) are more likely to drink coffee.
  
  node("coffee",
       distr = "rbinom",
       size = 1,
       prob = plogis(2.0 - 0.5 * sleep)) + 
  
  # Outcome: Happiness (Binary: 1 = Happy, 0 = Not Happy)
  # Happiness is boosted by Coffee, but also strongly influenced by Sleep.
  
  node("happiness",
       distr = "rnorm",
       mean = -2.0 + 1.2 * coffee + 0.5 * sleep,
       sd = 1)

# Finalize the network ----

Dset <- set.DAG(Dvars)

# Plotting the DAG as a check ----

plotDAG(Dset, xjitter = 0.3, yjitter = 0.04,
        edge_attrs = list(width = 0.5, 
                          arrow.width = 0.4, 
                          arrow.size = 0.8), 
        vertex_attrs = list(size = 12, label.cex = 0.8))

# Simulating Data based on the DAG ----

Odat <- sim(DAG = Dset, n = 250, rndseed = 456)

glm(happiness ~ coffee + sleep, 
    family = gaussian(link = "identity"),
    data = Odat )

# Simulating Counterfactual Outcomes ----

# First we need to add the two potential treatments 

# Define the interventions with your custom names
Dset <- Dset + 
  action("coffee1", nodes = node("coffee", distr = "rbern", prob = 1)) +
  action("coffee0", nodes = node("coffee", distr = "rbern", prob = 0))

# Simulate 1000 people in both "Coffee" and "No Coffee" universes
Xdat1 <- sim(DAG = Dset, 
             actions = c("coffee1", "coffee0"), 
             n = 1000, 
             rndseed = 123)

Xdat1$coffee1$happiness

df_plot <- data.frame(
  happiness = c(Xdat1$coffee1$happiness, Xdat1$coffee0$happiness),
  group = rep(c("Coffee", "No Coffee"), each = 1000)
)

# Can use it to plot the two counterfactuals 

# This is actually a really cool plot! Because it demonstrates the causal effect of 
# happpiness. 

# We can use this to test the g-formula, plotting our results from predicting each 
# outcome under the different treatments 

# Note: these are the same distibutions, but do they have to be? Couldn't, in theory
# the intervention under coffee0 have a different distribution 
# (note to self: discuss with gemini)

ggplot(df_plot, aes(x = happiness, fill = group)) +
  geom_density(alpha = 0.5) + # alpha makes them see-through
  scale_fill_manual(values = c("Coffee" = "purple", "No Coffee" = "pink")) +
  labs(title = "The Causal Effect of Coffee on Happiness",
       x = "Happiness Score",
       y = "Density",
       fill = "Group") +
  theme_minimal()

mean(Xdat1$coffee1$happiness) - 
mean(Xdat1$coffee0$happiness)

mean(Xdat1$coffee1$happiness)

mean(Xdat1$coffee0$happiness)

# Use this to explain causal estimands (ATE, ATT, ATU, ATO)

# ATE (different scales) ----

# Could use this to demonstrate importance of choosing the right way to measure your model. 
# For example, risk difference vs ratios 

Dset <- set.targetE(Dset, outcome = "happiness", param = "coffee1/coffee0") 
eval.target(Dset, data = Xdat1)$res # can be misleading 

Dset <- set.targetE(Dset, outcome = "happiness", param = "coffee1-coffee0") 

eval.target(Dset, data = Xdat1)$res # what the truth is 





##  Ratio_Y
## 1.344715



