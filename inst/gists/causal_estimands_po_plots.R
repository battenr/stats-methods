# Title: Plotting Causal Estimands using Potential Outcomes

# Description: Understanding different causal estimands is a key part of causal inference. 
# Using the potential outcomes framework can help with this! 

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

# Note: This code was generated with the help of Google Gemini

# Setup ----

#... Libraries ----

library(tidyverse) # ol' faithful
library(patchwork) # for combining plots 
library(rlang)

# Simulating Data ----

set.seed(456) # for reproducibility
n <- 10000 # using 10,000 people for this example

# Simulating the dataset 

df <- data.frame(
  sleep = rnorm(n, mean = 7, sd = 1) 
) %>% 
  mutate(
    prob_coffee = plogis(-2 * (sleep - 7)), # probability of receiving coffee ("true" propensity score)
    rounded_prob = round(prob_coffee, 1), # this is used later for the ATO
    coffee = rbinom(n, 1, prob_coffee), # coffee status (1 = yes, 0 = no) - what they "actually received"
    # Outcomes
    # Here we are simulating both potential outcomes: 
    # y0 - what happens if someone doesn't receive coffee
    # y1 - what happens if someone does receive coffee
    y0 = 0.5 * sleep + rnorm(n, mean = 0, sd = 0.5), 
    y1 = y0 + (10 - sleep) * 0.5 + rnorm(n, mean = 0, sd = 0.5), # for this example, same level of happiness 
    y = ifelse(coffee == 1, y1, y0), # this is what was ACTUALLY observed 
    # as if we don't receive coffee plus an increase 
    ice = y1 - y0 # this is the individual causal effect for one person. 
    # The ICE is not used below, but can explore if you're interested!
  )

# Different Causal Estimands ----

# By definition: 
# Average treatment effect (ATE): E[Y | X = 1] - E[Y | X = 0]

mean(df$y1) - mean(df$y0)

# Average treatment effect (ATT): E[Y | X = 1] - E[Y | X = 0]

mean(df$y1[df$coffee == 1]) - mean(df$y0[df$coffee == 1]) 

# Average treatment effect (ATU): E[Y | X = 1] - E[Y | X = 0]

mean(df$y1[df$coffee == 0]) - mean(df$y0[df$coffee == 0]) 

# Average treatment effect (ATO): E[Y | PS = 0.5] - E[Y | X = 0.5]

# Here, these patients have the same characteristics so there is no difference. 
# In reality, the ATO is often driven by the statistical method and difficult to 
# explain a priori. 

# However, for completeness, here we can try what happens if we use PS of 0.5. 

mean(df$y1[df$rounded_prob == 0.5]) - mean(df$y0[df$rounded_prob == 0.5]) 

# You'll notice it's the same as the ATE. That's because in this simple example
# There is no difference in these patients vs the total patients 

# Plotting! ----

# Individual numbers are good, plots are better! 

# Here we are going to write a function, then plot three causal estimands: 
# - Average Treatment Effect (ATE)
# - Average Treatment Effect in the Treated (ATT)
# - Average Treatment Effect in the Untreated (ATU)

# Parameters for function
# Data: a dataframe with each individual and both potential outcomes
# Title: title of the plot
# Subtitle: the subtitle
# Group Filter: This is the group we're filtering to (i.e., coffee = 1, coffee = 0 or none [the total sample])

plot_estimand <- function(data, 
                          title, 
                          subtitle, 
                          group_filter = NULL) {
  
  # This first part is to filter the data. Filtering is only needed 
  # for the ATT and ATU. 
  
  if (!is.null(group_filter)) {
    plot_df <- data %>% filter(eval(parse_expr(group_filter)))
  } else {
    plot_df <- data
  }
  
  # Means under both potential outcomes
  
  mu0 <- mean(plot_df$y0)
  mu1 <- mean(plot_df$y1)
  
  # Plot itself
  
  ggplot(plot_df) +
    geom_density(aes(x = y0, fill = "No Coffee"), alpha = 0.4) +
    geom_density(aes(x = y1, fill = "Coffee"), alpha = 0.4) +
    geom_vline(xintercept = mu0, color = "darkblue", linetype = "dashed") +
    geom_vline(xintercept = mu1, color = "darkred", linetype = "dashed") +
    annotate("segment", x = mu0, xend = mu1, y = 0.75, yend = 0.75, 
             arrow = arrow(length = unit(0.2, "cm"), ends = "both")) +
    annotate("label", x = (mu0 + mu1)/2, y = 0.80, label = paste("Effect:", round(mu1 - mu0, 2)), size = 6) +
    labs(title = title, subtitle = subtitle, x = "Happiness", y = "Density", fill = "Scenario") +
    theme_minimal() +
    scale_fill_manual(values = c("Coffee" = "#E69F00", "No Coffee" = "#56B4E9")) +
    lims(x = c(0, 8), y = c(0, 1)) + 
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      text = element_text(size = 20)
    )
}

# Generate Plots for Each Estimand ---

p1 <- plot_estimand(df, "Average Treatment Effect", "ATE: Everyone")
p2 <- plot_estimand(df, "Average Treatment Effect in the Treated", "ATT: Coffee Drinkers", "coffee == 1")
p3 <- plot_estimand(df, "Average Treatment Effect in the Untreated", "ATU: Coffee Non-Drinkers", "coffee == 0")

# Combine into one plot 

p1 / (p2 + p3)

