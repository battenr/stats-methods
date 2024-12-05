# Title: Always Plot Your Data

# Description: A piece of advice I got from my supervisor was to always 
# plot your data first. This is invaluable. The code below is meant to show 
# one reason why 

# Setup ----

#... Libraries ----

library(tidyverse) # ol' faithful
library(patchwork) # for combining plots

# Different Sample Sizes ----

# Sample sizes
sample_sizes <- c(10,
                  50,
                  100, 
                  500,
                  1000)

# Function for Making Plots ----

# Function to generate a plot for each sample size
create_plot <- function(sample.size) {
  
  # Generate data for the sample size. In this case, using a normal distribution 
  # with a mean of 50 and standard deviation of 10. 
  
  df <- data.frame(
    x = rnorm(n = sample.size, 
                mean = 50, 
                sd = 10)
  )
  
  # Create a plot of the empirical density
  
  ggplot(data = df, mapping = aes(x = x) ) +
    geom_density(color = "purple", alpha = 1.5) +
    labs(title  = "Mean of 50, SD of 10", 
         subtitle = paste("Sample Size:", sample.size),
         x = "Value", 
         y = "Density") +
    theme_minimal() + 
    lims(x = c(20, 80)) + 
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"), 
      plot.subtitle = element_text(hjust = 0.5),
      text = element_text(size = 20)
    )
    
}

# Iterating over the sample sizes ----

# Map over sample_sizes and generate a list of plots

plots <- purrr::map(sample_sizes, create_plot)

# Combine Plots Into One! 

purrr::reduce(plots, `+`) + 
  plot_layout(ncol = 2)  # Arrange them in 2 columns



