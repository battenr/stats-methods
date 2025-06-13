# Title: Estimating Sample Size Required Using Precision -----

# Description: Sample sizes are primarly based upon an assumed power. For some studies, 
# such as observational studies with large samples, there isn't an issue with power. However, 
# there may be a need to understand the required sample size (i.e., how much data to request from vendor, etc)

# In these situations, precision can be used instead! 

# Setup ----

#... Packages -----

library(tidyverse) # ol faithful
library(precisely) # for estimating sample size based on precision

# Example / Test Code ----

n_risk_difference(precision = seq(0.1, 0.5, by = 0.05), 
                  exposed = 0.7, 
                  unexposed = 0.2, 
                  group_ratio = 1, 
                  ci = 0.95)

# Calculating Sample Size for Different Levels of Precision ----

df <- map_precisely(
  n_risk_difference, 
  precision = seq(0.1, 0.5, by = 0.05), 
  exposed = 0.7, 
  unexposed = 0.2, 
  group_ratio = 1
) 

# Plotting Results ----

ggplot(data = df, mapping = aes(x = n_total, y = precision)) + 
  geom_line(color = "purple", linewidth = 1.5) +
  labs(
    title = "Sample Size Estimation Based on Precision",
    subtitle = "Risk Difference of 0.5",
    x = "Sample Size",
    y = "Precision (width of interval)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    text = element_text(size = 20),
    axis.text = element_text(color = "black")
  ) +
  scale_x_continuous(limits = c(0, 1200))
  
  