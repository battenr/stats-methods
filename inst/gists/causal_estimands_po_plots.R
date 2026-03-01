# Title: Plots of Causal Estimands 

# Description: Understanding different causal estimands is a key part of causal inference. 
# Pictures are worth a thousand words applies to causal estimands too. 

# This code simulates data under the following conditions: 

# Research Question: Does coffee cause happiness?

# Three variables: 
# 1. Coffee - binary (yes/no)
# 2. Sleep - a confounder 
# 3. Happiness - a continuous outcome

# Note: for this example, we will be using potential outcomes. This means we 
# will know the values for all possible outcomes (i.e., happiness under coffee: yes 
# and happiness under coffee: no). In reality, this isn't possible (since we 
# only observe one outcome)




# Plotting estimands 














library(tidyverse)
library(patchwork)
library(rlang)

set.seed(42)
n <- 10000

# 1. Sleep (Confounder)
sleep <- rnorm(n, 7, 1)

# 2. Propensity Score: Tired people drink more coffee
prob_coffee <- plogis(-2 * (sleep - 7)) 
coffee <- rbinom(n, 1, prob_coffee)

# 3. Heterogeneous Potential Outcomes
# The effect of coffee is now (10 - sleep) * 0.5
# If sleep = 5, effect = 2.5 | If sleep = 9, effect = 0.5
y0 <- 0.5 * sleep + rnorm(n, 0, 0.5)
y1 <- y0 + (10 - sleep) * 0.5 

df <- data.frame(sleep, coffee, y0, y1, prob_coffee)

# By definition, the average treatment effect in the treated is: 

# E[Y1 - Y0 | X == 1]

# So it's the average treatment effect in only the treated. 

mean(df$y1[coffee == 1]) - mean(df$y0[coffee == 1])

# 4. Define ATO (Overlap Weights)
# ATO targets the population where there is most clinical equipoise (p approx 0.5)
df$weights_ato <- ifelse(df$coffee == 1, 1 - df$prob_coffee, df$prob_coffee)

# Updated Plotting Function
plot_causal <- function(data, title, group_filter = NULL, use_weights = FALSE) {
  
  if (!is.null(group_filter)) {
    plot_df <- data %>% filter(eval(parse_expr(group_filter)))
  } else {
    plot_df <- data
  }
  
  # Calculate Weighted or Unweighted Means
  if (use_weights) {
    mu0 <- sum(plot_df$y0 * plot_df$weights_ato) / sum(plot_df$weights_ato)
    mu1 <- sum(plot_df$y1 * plot_df$weights_ato) / sum(plot_df$weights_ato)
  } else {
    mu0 <- mean(plot_df$y0)
    mu1 <- mean(plot_df$y1)
  }
  
  ggplot(plot_df) +
    geom_density(aes(x = y0, fill = "No Coffee"), alpha = 0.4) +
    geom_density(aes(x = y1, fill = "Coffee"), alpha = 0.4) +
    geom_vline(xintercept = mu0, color = "darkblue", linetype = "dashed") +
    geom_vline(xintercept = mu1, color = "darkred", linetype = "dashed") +
    annotate("segment", x = mu0, xend = mu1, y = 0.05, yend = 0.05, 
             arrow = arrow(length = unit(0.2, "cm"), ends = "both")) +
    annotate("label", x = (mu0 + mu1)/2, y = 0.08, label = paste("Effect:", round(mu1 - mu0, 2))) +
    labs(title = title, x = "Happiness", y = "Density", fill = "Scenario") +
    theme_minimal() +
    scale_fill_manual(values = c("Coffee" = "#E69F00", "No Coffee" = "#56B4E9")) + 
    # need to fix axes to be same across all
}

# Generate distinct plots
p1 <- plot_causal(df, "ATE (Everyone)")
p2 <- plot_causal(df, "ATT (Tired Coffee Drinkers)", "coffee == 1")
p3 <- plot_causal(df, "ATU (Rested Non-Drinkers)", "coffee == 0")
p4 <- plot_causal(df, "ATO (Weighted Overlap)", use_weights = TRUE)

(p1 + p2) / (p3 + p4)