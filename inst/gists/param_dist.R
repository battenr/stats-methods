# Title: Parametric vs Nonparametric Statistics 

# Description: An example of what is actually meant by parametric with parametric statistics. 

# Setup ----

#... Libraries ----

library(tidyverse) # ol faithful
library(glue) # for easy string formatting
library(patchwork) # for combining plots

# Function to create a plot for each distribution ----

generate_plot <- function(df, # the dataframe
                          aes_x, # the data point
                          distribution_name, # Name of the distribution
                          code_text, # Text that had the code (now outdated)
                          params_text # Text with the parameters
) {
  # Creating a plot that is a histogram with a density plot overlaid. Personally, I find it 
  # easier to visualize the distribution with a line than strictly a histogram. 
  
  ggplot(df, aes(x = {{aes_x}})) +
    geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "skyblue", color = "black", alpha = 0.7) +
    geom_density(color = "red", size = 1) +
    
    # Adding the labels. Some of these (the ones with glue) input to the function
    
    labs(
      title = glue("{code_text}"),
      subtitle = glue("{params_text}"),
      x = distribution_name, 
      y = "Density"
    ) +
    
    # Using minimal theme as the base
    
    theme_minimal() +
    
    # Centering the plot titles and increasing the font size. 
    
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(color = "blue", face = "bold", hjust = 0.5),
      text = element_text(size = 20)
    )
}

# Simulating Data! ----

# Using a sample size of 1000, completely arbitrary

n <- 1000

# Using the sample size, creating two different distributions. However, this could be 
# used for any type of distribution: normal, binomial, etc. 

data <- list(
  normal = data.frame(value = rnorm(n, mean = 5, sd = 2)),
  poisson = data.frame(value = rpois(n, lambda = 4))
)

# Creating Plots ----

# Using the function from above to create a plot for each distribution. 

# For the normal distribution

p1 <- generate_plot(data$normal, 
                    value, 
                    "", # artifact. really this should be removed as should the argument in the function
                    "Normal Distribution", 
                    "Parameters: Mean, SD")

# For the Poisson distribution

p2 <- generate_plot(data$poisson, 
                    value, 
                    "",
                    "Poisson Distribution", 
                    "Parameter: Lambda (rate)")

# Combining Plots! ----

# Combining the plots using the patchwork package. 

# Combine the plots using patchwork
combined_plot <- p1 /  p2 


combined_plot # Display the combined plot
