# Title: Demonstration of Confidence Intervals

# Description: The purpose of this script is to simulate data to show what a 
# confidence interval is. The goal is to highlight that the 95% has to do 
# with us repeatedly sampling the value. 

# Setup ----

library(tidyverse) # ol faithful
library(ggridges) # used for ridge plot
library(gridExtra) # used for combining plots

# Simulating Data ----

# Population Parameters 

# Choosing the population parameters. These are really just arbitrary

population_mean <- 75 # just picking 75 since nice round number
population_sd <- 50 # arbitrary. 
sample_size <- 100 # sample size of 100
num_samples <- 25 # drawing the sample 25 times. In practice, we'd use more. 
conf_level <- 0.95 # confidence interval to use. For this case, 95%

# Calculating the Confidence Interval ----

# Writing a function to calculate the confidence interval 

# Function to calculate the confidence interval
calc_ci <- function(sample, # sample size 
                    conf_level # confidence interval 
) {
  
  # Calculate point estimate first. In this case, it's the mean 
  sample_mean <- mean(sample)
  
  # Calculate the standard error. This is based on the formula: 
  # se = sd/sqrt(n)
  sample_se <- sd(sample) / sqrt(length(sample)) 
  
  # Calculating the error margin (Z*SE)
  
  error_margin <- qnorm(1 - (1-conf_level)/2) * sample_se
  
  # Lower bound for confidence interval: point estimate - error margin
  
  lower_bound <- sample_mean - error_margin
  
  # Upper bound for confidence interval: point estimate + error margin
  upper_bound <- sample_mean + error_margin
  
  # Return the upper and lower bound
  
  c(lower_bound, upper_bound)
}

# Generating Samples and CIs ----

# Creating the samples that will be used. This is done by repeatedly sampling from 
# a distribution. 

samples <- replicate(num_samples, # number of samples (in this case 25)
                     rnorm(sample_size, # drawing from a normal distribution 
                           mean = population_mean, 
                           sd = population_sd), 
                     simplify = FALSE) # formatting output for later. 

# Applying the function above to each of the samples that are created. 

cis <- t(sapply(samples, calc_ci, conf_level = conf_level)) # Confidence intervals

sample_means <- sapply(samples, mean) # means for each sample. 

# Creating a data frame of the samples. This is needed for the ridgeplots

df_samples <- data.frame(
  Sample = rep(1:num_samples, each = sample_size),
  Value = unlist(samples)
)

# Creating a data frame for confidence intervals. This consists of the 
# sample number (1 to number of samples, like an ID), mean, lower bound and upper bound. 
# 

df_ci <- data.frame(
  Sample = 1:num_samples, # which number. For example, was it first sample taken? 
  Mean = sample_means, # means 
  Lower = cis[, 1], # lower CI bound
  Upper = cis[, 2], # upper CI bound
  # Determining if the mean sample mean is within the 95% CI 
  ContainsMean = ifelse(population_mean >= cis[, 1] & population_mean <= cis[, 2], "Yes", "No")
)

# Plots Time! ----

#... Creating a ridge plot. This is to show that each CI is for a different sample. 

ridge_plot <- ggplot(
  df_samples, 
  aes(x = Value, y = as.factor(Sample), fill = ContainsMean)
) +
  geom_density_ridges(
    aes(fill = "gray"), alpha = 0.5, scale = 1.5
  ) +
  labs(title = "Sample Values",
       x = "Value of Data Point",
       y = "Sample Number") +
  scale_fill_manual(values = c("gray" = "gray")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )

#... Creating a Plot for Confidence Intervals ----

# Plotting the confidence intervals. 

ci_plot <- ggplot(
  df_ci, 
  aes(y = as.factor(Sample))) +
  geom_segment(
    aes(y = as.factor(Sample), yend = as.factor(Sample), x = Lower, xend = Upper, color = ContainsMean), size = 1
  ) +
  geom_point(
    aes(x = Mean, color = ContainsMean), size = 1.5
  ) +
  geom_vline(
    xintercept = population_mean, linetype = "dashed", color = "red"
  ) +
  labs(title = "95% Confidence Intervals",
       #subtitle = "Population Mean of 100",
       y = "Sample Number",
       x = "Confidence Intervals",
       color = "Contains Mean") +
  scale_color_manual(values = c("Yes" = "darkblue", "No" = "hotpink")) +
  theme_minimal() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
  )

# Combining Plots ----

combined_plot <- grid.arrange(ridge_plot, ci_plot, ncol = 2) 