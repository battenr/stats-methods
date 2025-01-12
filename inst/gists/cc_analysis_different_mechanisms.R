# Title: Complete Case Analysis Can Be Problematic 

# Description: Demonstrating that complete case can be problematic. The goal
# of this script is to demonstrate how complete case could lead to 
# incorrect conclusions

# Note: This is just an example. If you're curious about 
# the bias for each type of missing data pattern, I highly recommend repeating
# this process through a simulation study. 

# Setup ----

#... Packages ----

library(tidyverse) # ol faithful
library(modelsummary) # use to plot the results from the models 

# Simulating Data ----

# Using a simple dataset as an example
# - Two confounders: one continuous, one binary
# - Binary treatment
# - Continuous outcome

set.seed(456) # setting seed for reproducibility

n = 250 # sample size, arbitrarily chose 250
beta_trt = 1.5 # "true" effect 

df <- data.frame(
  z1 = rnorm(n = n, mean = 5, sd = 1), # continuous confounder
  z2 = rbinom(n = n, size = 1, prob = 0.5) # binary confounder
) %>%
  dplyr::mutate(
    #prob = plogis(0.05*z1 + 0.2*z2), # this is an intermediate variable
    prob = plogis(0.05*z1 + 1.5*z2), # this is an intermediate variable
    x = rbinom(n = n, size = 1, prob = prob), 
    y = beta_trt*x + 2*z1 + 4*z2 + rnorm(n = n, mean = 0 , sd = 1)
  ) %>% 
  dplyr::mutate(
    # Is variable missing 
    is_mcar = rbinom(n = n, size = 1, prob = 0.5), 
    is_mar = rbinom(n = n, size = 1, prob = plogis(0.1*z1)),
    is_mnar = ifelse(y > 13, 1, 0),
    
    # Outcomes for y_mcar, y_mar and y_mnar
    y_mcar = ifelse(is_mcar == 1, NA, y), 
    y_mar = ifelse(is_mar == 1, NA, y),
    y_mnar = ifelse(is_mnar == 1, NA, y)
  )

# Check what percent of each missing data pattern type is missing 

lapply(df %>% select(contains("y")), \(x)is.na(x) %>% mean())  

# Fitting Models with Different Missing Data Patterns ----

# Fitting three different models: 
# - Outcome has missingness with missing completely at random 
# - Outcome has missingness with missing at random 
# - Outcome has missingness with missing not at random 

# Fitting Different Models ----

models <- list(
  "MCAR" = glm(y_mcar ~ x + z1 + z2, data = df),
  "MAR" = glm(y_mar ~ x + z1 + z2, data = df),
  "MNAR" = glm(y_mnar ~ x + z1 + z2, 
               data = df))

# Results ----

modelsummary::modelsummary(models, 
                           statistic = 'conf.int') # this can be used to view the results in a table

# Plotting Results ----

# Colors to use in the plots\

clrs <- c(
  "#FFBE00",  # MCRN yellow
  "#54191B",  # MCRN brown
  "#2660ae"   # Blue from MCR flag
)

modelsummary::modelplot(models, 
                        coef_omit = c(1,3,4),
                        size = 0.75,
                        linewidth = 0.75) +
  scale_color_manual(values = clrs) +
  ggtitle("Models with Different Missing Data Mechanisms",
          "True Effect of 1.5") + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),
    plot.subtitle = element_text(hjust = 0.5, size = 20),
    text = element_text(size = 20),
    axis.text.y = element_blank()
  ) + 
  labs(x = "Effect Estimate and 95% Confidence Interval",
       y = "") +
  lims(x = c(0, 3)) +
  geom_vline(xintercept = 1.5,
             linetype = "dotted"
             )
