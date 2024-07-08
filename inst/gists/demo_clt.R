# Title: Demonstration of the Central Limit Theorem

# Description: The purpose of this script is to simulate data for three different 
# distributions (normal, logistic and poisson) for four different sample sizes. 
# Each of these is then plotted to demonstrate how the sampling distribution 
# changes with increasing sizes of the sample. 


# Setup ----

library(tidyverse) # ol faithful
library(patchwork) # to combine graphs together

# Simulating Data ----

#... Function for Generating The Means ----

# Three distributions: normal, logit and poisson. Parameters were chosen at random
# The process is like so: 
# 1. Select a sample of some size (i.e., 10) 
# 2. Calculate the statistic (in this case the mean)
# 3. Repeat steps 1 and 2, 1000 times (so there are 1000 means)

generate_sample_means <- function(dist_name, # name of distribution 
                                  n, # sample size 
                                  num_samples = 1000) # times that it's repeated) 
                                  {
  replicate(num_samples, { # repeating the process of selecting a sample of n 
    samples <- switch(dist_name, # based on the input distribution pick the appropriate distribution. 
                      "normal" = rnorm(n, mean = 5, sd = 3), # normal distribution
                      "logistic" = rbinom(n, size = 1, prob = plogis(rnorm(n, mean = 0, sd = 1))), # logistic distribution
                      "poisson" = rpois(n, lambda = 2)) # poisson distribution
    
    # Calculating the mean. This is done once per sample. 
    
    mean(samples)
  })
}

#... Sample sizes ----

n_sizes <- c(10, 30, 100, 200) # Setting the sample size of 10, 30, 100 and 200. Each was chosen arbitrarily

distributions <- c("normal", "logistic", "poisson") # need this for later. List of the distributions used

# Enter ggplot2 ----

#... Formatting ----

# Setting the y axis limits. Want it to be the same across the graphs 

y_limits_list <- list(
  normal = c(0, 2),
  logistic = c(0, 12),
  poisson = c(0, 4)
)

# Setting the x axis limits. Want it to be the same across the graphs. 

x_limits_list <- list(
  normal = c(2, 8),
  logistic = c(0, 1),
  poisson = c(-1, 5)
)

#... Workhorse Section ----

# Where it's all put together! 


plots <- map(distributions, ~{ # map across each distribution and apply the function below
  
  dist_name <- .x # the distribution name. Each element in the distributions object
  
  y_limits <- y_limits_list[[dist_name]] # Get y-axis limits from earlier
  x_limits <- x_limits_list[[dist_name]] # Get x-axis limits from earlier 
  
  plots_for_dist <- map(n_sizes, ~{ # map within a map. This time for each sample size (i.e., 10, 30, etc)
    
    sample_means <- generate_sample_means(dist_name, .x) # use function from earlier to get the mean 
    
    ggplot(mapping = aes(x = sample_means)) + # x axis will be the sample means
      geom_histogram(aes(y = ..density..), bins = 30, color = "black", fill = "lightblue") + # using a histogram to show how many means are in a bucket
      geom_density(color = "red") + # drawing a denisty over the histogram. This shows it "nicely"
      ggtitle(tools::toTitleCase(sprintf("%s (n = %s)", dist_name, .x))) + # adding the title. toTitleCase, makes the text titlecase
      
      # Specifying additional formatting
      
      theme_minimal() +
      ylab("Density") +
      xlab("Sample Means") +
      ylim(y_limits) + 
      xlim(x_limits) +
      theme(
        plot.title = element_text(hjust = 0.5)
      )
  })
  reduce(plots_for_dist, `+`) + plot_layout(ncol = length(n_sizes)) # combining all the plots together for a distribution
})


# Output ----

# Combining all of the plots

final_plot <- reduce(plots, `/`) + # combining all the distribution plots into one overall plot. 
  # Adding text for clarity
  plot_annotation(title = "Demonstration of the Central Limit Theorem",
                  subtitle = "Normal, Logistic and Poisson Distributions using 1000 samples",
                  theme = theme(plot.title = element_text(hjust = 0.5),
                                plot.subtitle = element_text(hjust = 0.5)))

print(final_plot) # print the graph! Yahooooo! 
