# Title: Stabilizing and Truncating Weights 

# Description: When using weights, for example IPTW, it is often helpful to 
# stabilize and/or truncate them. This code is meant to demonstrate how we 
# can do that, and why. 

# Setup ----

#... Packages ----

library(tidyverse) # ol' faithful
library(WeightIt) # for plotting weights 
library(cobalt) # for plotting balance
library(splines) # used for fitting non-linearity of Z1 on X
library(patchwork) # for combining plots into one

#... Functions ----

# Simulating Data 
# Some notes: 
# - Binary treatment
# - Two confounders
# - Relationship between Z1 and X is non-linear

sim_data <- function(n = 1000, # sample size 
                     beta_trt = 1.5, # treatment effect
                     # Parameters for Z1 
                     z1_mean = 10, z1_sd = 3, 
                     # Parameters for Z2
                     z2_size = 1, z2_prob = 0.5, 
                     # Confounder - Effect on X
                     z1_on_x = 0.1, z2_on_x = 0.2,
                     # Confounder - Effect on Y
                     z1_on_y = 0.5, z2_on_y = 0.3){
  
  # Creating the Dataframe
  
  df <- data.frame(
    z1 = rnorm(n = n, mean = z1_mean, sd = z1_sd), 
    z2 = rbinom(n = n, size = z2_size, prob = z2_prob)
  ) %>% 
    dplyr::mutate(
      prob = plogis((z1_on_x*z1)^3 + z2_on_x*z2), 
      x = rbinom(n = n, size = 1, prob = prob), 
      y = beta_trt*x + z1_on_y*z1 + z2_on_y*z2 + rnorm(n = n, mean = 0, sd = 1)
    )
  
  # Return the dataframe
  
  return(df)
}

# Simulating Data ----

set.seed(456)

df <- sim_data()

# Inverse Probability Weighting ----

#... PS Model ----

# Using spline to account for non-linear relationship between Z1 & X

# Not stabilized 

w_unstab <- WeightIt::weightit(x ~ ns(z1,4) + z2, 
                               data = df, 
                               method = "glm", 
                               estimand = "ATE",
                               stabilize = FALSE)

# Stabilized 

w_stab <- WeightIt::weightit(x ~ ns(z1,4) + z2, 
                             data = df, 
                             method = "glm", 
                             estimand = "ATE",
                             stabilize = TRUE)

#... Adding to Data ----

df.ipw <- df %>% 
  mutate(
    ipw = w_unstab$weights,
    ipwstab = w_stab$weights,
    ipwtrunc = dplyr::case_when(
      ipwstab > quantile(ipwstab, 0.99) ~ quantile(ipwstab, 0.99), 
      ipwstab < quantile(ipwstab, 0.01) ~ quantile(ipwstab, 0.01), 
      ipwstab >= quantile(ipwstab, 0.01) & ipwstab <= quantile(ipwstab, 0.99) ~ ipwstab
    )
  )

# Summary Statistics ----

summary(df.ipw$ipw)
summary(df.ipw$ipwstab)
summary(df.ipw$ipwtrunc)

# Plots ----

df.plot <- df.ipw %>% 
  tidyr::pivot_longer(
    cols = c("ipw", "ipwstab", "ipwtrunc"),
    names_to = "weight_type",
    values_to = "weight"
  )

#... Distribution of Weights ----

# No Adjustment 

p1 <- ggplot(data = df.ipw, 
             mapping = aes(x = ipw)) + 
  geom_density(linewidth = 1, color = "hotpink", fill = "hotpink") +
  ggtitle("No Adjustment to Weights") + 
  theme_minimal() + 
  labs(x = "Value of Weight", y = "Density") + 
  scale_fill_discrete(labels = c("No", "Yes")) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(size = 26),
        legend.position = "none"
  ) 

# Stabilized 

p2 <- ggplot(data = df.ipw, 
             mapping = aes(x = ipwstab)) + 
  geom_density(linewidth = 1, color = "hotpink", fill = "hotpink") +
  ggtitle("Stabilized") + 
  theme_minimal() + 
  labs(x = "Value of Weight", y = "Density") + 
  scale_fill_discrete(labels = c("No", "Yes")) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(size = 26),
        legend.position = "none"
  ) 

# Stabilized and Truncated

p3 <- ggplot(data = df.ipw, 
             mapping = aes(x = ipwtrunc)) + 
  geom_density(linewidth = 1, color = "hotpink", fill = "hotpink") +
  ggtitle("Stabilized and Truncated") + 
  theme_minimal() + 
  labs(x = "Value of Weight", y = "Density") + 
  scale_fill_discrete(labels = c("No", "Yes")) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(size = 26),
        legend.position = "none"
  ) 

# Combining Plots

p1 / (p2 + p3)