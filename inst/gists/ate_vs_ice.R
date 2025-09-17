# Title: Average Treatment Effects (ATE) vs Individual Treatment Effects (ITE)

# Description: Demonstrating how it's important to distinguish between individual treatment effects 
# and the average treatment effect. 

# Setup ----

#... Libraries ----

library(tidyverse) # ol faithful
library(patchwork) # for combining plots 

# Function for Plotting Individual Effects ----

# This function takes the sample size (n) and the true effect as arguments. 
# It then simulates data for a dataframe that is the number of individuals 
# specified from a Gaussian/normal distribution. 

# For this example, the true_effect is 1.5. 

plot_ice <- function(n, 
                     true_effect = 1.5){
  
  # Simulating Data 
  
  df <- data.frame(
    y = rnorm(n = n, mean = true_effect, sd = 1), # if you change the SD the individual effects will move more
    id = seq(1:n)
  )
  
  # Calculating the mean 
  
  mean_value <- round(mean(df$y),2) %>% as.numeric() 
  
  # Plotting the data. The plot is ID vs Outcome
  
  ggplot(data = df, 
         mapping = aes(x = id, y = y)) + 
    geom_point(color = "purple") + 
    # Adding horizontal line for the mean 
    geom_hline(yintercept = mean_value, 
               color = "pink", 
               size = 1) +
    # Adding horizontal line for the true effect 
    geom_hline(yintercept = true_effect, 
               color = "lightgreen", 
               size = 1) +
    # Formatting theme
    theme_minimal() +
    # Adding title 
    ggtitle(paste0("Sample Size: ", n),
            paste0("Mean Effect: ",  
                   mean_value, 
                   "\n True Effect: ", 
                   true_effect)) + 
    # Formatting theme - round 2 
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      text = element_text(size = 16)
    ) + 
    # Adding labels and limits for y-axis. 
    labs(x = "Subject ID", y = "Effect") + 
    lims(y = c(-5, 5))

  
}

# Different Sample Sizes ----

# These were arbitrarily chosen 

ss <- c(20, 50, 100, 250, 500, 1000)

# Plot for Each Sample Size ----

plots <- purrr::map(ss, plot_ice)

# Combining Plots ----

# Combining Plots and Additing Overall Title 

wrap_plots(plots, ncol = 3) +  # or nrow = 2
  plot_annotation(
    title = "Individual Treatment Effects by Sample Size",
    subtitle = "Pink = Sample Mean | Green = True Mean (1.5)",
    caption = "Each panel shows a random sample from N(1.5, 2)"
  ) & 
  theme_minimal(base_size = 20) + 
  theme(      plot.title = element_text(hjust = 0.5, face = "bold"),
              plot.subtitle = element_text(hjust = 0.5)
  )
