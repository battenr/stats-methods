# Title: How Complete Case Analysis Impacts Results

# Description: This code demonstrates how complete case analysis can impact your results
# differently for different missing data mechanisms. 

# Setup ----

#... Libraries ----

library(tidyverse) # ol faithful
library(modelsummary) # for plotting model results

# Simulate Data ----

set.seed(456) # setting seed for reproducibility

n <- 250 # artibrary sample size

beta_trt <- 1.5 # true treatment effect 

# For simulating data 

df <- data.frame(
  z1 = rnorm(n, mean = 5, sd = 1), # continuous confounder
  z2 = rbinom(n, 1, prob = 0.5) # binary confounder 
) %>%
  mutate(
    # Binary treatment (i.e., coffee)
    
    prob_x = plogis(-4 + 0.8 * z1 + 0.4*z2), # probability of treatment
    x = rbinom(n, 1, prob = prob_x),
    
    # Outcome (Y) - True relationship
    y = beta_trt * x + 2.5 * z1 + 4.0 * z2 + rnorm(n, 0, 1),
    
    # --- The Three Missingness Mechanisms ---
    
    # 1. MCAR: 30% of data randomly deleted
    is_mcar = rbinom(n, 1, 0.3),
    
    # 2. MAR: Missingness depends on confounder z1 & z2
    is_mar = rbinom(n, 1, plogis(-0.5*z1 + 4*z2)),
    
    # 3. MNAR: Missingness depends on the outcome and treatment
    # If y is higher than the mean and treatment is 1 (i.e., received coffee) then 
    # data is missing. So basically all values that are above mean are 0. 
    
    is_mnar = ifelse(y > mean(y) & x == 1, 1, 0),
    
    # Creating the "Complete Case" Outcome Columns
    y_mcar = ifelse(is_mcar == 1, NA, y),
    y_mar  = ifelse(is_mar == 1, NA, y),
    y_mnar = ifelse(is_mnar == 1, NA, y)
  )

# Fitting Models ----

# Fitting a model for each type of missing data. For each model, the 
# missing values are dropped (complete cases kept)

# Formatting into a list that can be used by modelsummary package for 
# visualization later

models <- list(
  "MCAR (Dropped Rows)"    = glm(y_mcar ~ x + z1 + z2, data = df),
  "MAR (Dropped Rows)"     = glm(y_mar ~ x + z1 + z2, data = df),
  "MNAR (Dropped Rows)"    = glm(y_mnar ~ x + z1 + z2, data = df)
)

# Visualization ----

# This plot shows the results from each of the models. 

p1 <- modelplot(models, 
                coef_omit = 'Inter|z1|z2') +
  geom_vline(xintercept = 1.5, 
             size = 2, 
             linetype = "dashed", 
             color = "red", 
             size = 1) +
  annotate("text", 
           x = 1.5, 
           y = 1.5, 
           label = "True Effect (1.5)", 
           color = "red", 
           hjust = 0, 
           size = 6) +
  labs(
    title = "The Problem with Complete Case Analysis",
    subtitle = "How 'Dropping NAs' Impacts the Treatment Effect for Different Missing Data Mechanisms",
    x = "Estimated Treatment Effect",
    y = ""
  ) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c(
    "MCAR (Dropped Rows)"   = "#5dade2", # Sky Blue
    "MAR (Dropped Rows)"    = "#f39c12", # Orange
    "MNAR (Dropped Rows)"   = "#e74c3c"  # Red
  )) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5, size = 26),
    plot.subtitle = element_text(hjust = 0.5, size = 24),
    axis.text.y = element_text(size = 18, face = "bold", hjust = 0.5),
    text = element_text(size = 16)
  ) +
  # Focus the scale to see the drift
  coord_cartesian(xlim = c(-0.5, 2.5))

# Making lines and points larger 

p1$layers[[1]]$geom$default_aes$linewidth = 1.5
p1$layers[[1]]$geom$default_aes$size = 0.75

# Final Plot ----

p1


