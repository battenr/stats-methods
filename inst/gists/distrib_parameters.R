# Title: Understanding Distributions and Parameters

# Description: The below plots show which parameters are needed for four different
# distributions: normal, exponential, binomial and poisson distributions. 

# Setup ----

#... Libraries ----

library(tidyverse) # ol faithful
library(patchwork) # for combining plots

#... Functions ----

# This function creates plots based on the distribution. The dataframe 
# is values randomly selected from the distribution. The distribution argument
# is the name of the distribution (lower case) in quotation marks

create_plot <- function(df, distribution) {
  
  # Based on the distribution argument, the plot will either be 
  # a density plot or it will show the probability for 
  # two discrete distributions. 
  
  # Note: This is only for four distributions but can be 
  # applied to others 
  
  
  if (distribution == "normal") {
    title <- "Normal"
    subtitle <- "Parameters: Mean, SD"
    color <- "purple"
    # For continuous distributions, use geom_density
    plot <- ggplot(data = df, aes(x = x)) + 
      geom_density(size = 1.5, color = color) +
      labs(x = "Values", y = "Density") +
      theme_minimal() +
      ggtitle(paste0(title, " Distribution"), subtitle) + 
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(size = 16)
      )
    
  } else if (distribution == "exponential") {
    title <- "Exponential"
    subtitle <- "Parameter: Rate"
    color <- "green"
    # For continuous distributions, use geom_density
    plot <- ggplot(data = df, aes(x = x)) + 
      geom_density(size = 1.5, color = color) +
      labs(x = "Values", y = "Density") +
      theme_minimal() +
      ggtitle(paste0(title, " Distribution"), subtitle) + 
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(size = 16)
      )
    
  } else if (distribution == "poisson") {
    title <- "Poisson"
    subtitle <- "Parameter: Lambda"
    color <- "cyan"
    
    plot <- ggplot(data = df, aes(x = x)) + 
      # for discrete using bars
      geom_bar(aes(y = ..count.. / sum(..count..)), stat = "count", fill = color, color = color, width = 0.5) + 
      #geom_smooth(stat = "count", aes(y = ..count.. / sum(..count..)), color = "black", size = 1) +
      labs(x = "Number of Occurences", y = "Probability") +
      theme_minimal() +
      ggtitle(paste0(title, " Distribution"), subtitle) + 
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(size = 16)
      )
    
  } else if (distribution == "binomial") {
    title <- "Binomial"
    subtitle <- "Parameters: Number of Experiments (n), Probability of Success (p)"
    color <- "pink"
    
    # For discrete using bars 
    plot <- ggplot(data = df, aes(x = x)) + 
      geom_bar(aes(y = ..count.. / sum(..count..)), stat = "count", fill = color, color = color, width = 0.5) + 
      #geom_smooth(stat = "count", aes(y = ..count.. / sum(..count..)), color = color, size = 1.5) +
      labs(x = "Values", y = "Probability") +
      theme_minimal() +
      ggtitle(paste0(title, " Distribution"), subtitle) + 
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(size = 16)
      )
    
  } 
  
  return(plot)
}

# Simulating Data from the Distributions ----

n = 10000 # arbitrarily choosing 10,000 to have enough sample size to see the distribution adequately

norm_dist <- data.frame(x = rnorm(n, mean = 0, sd = 1)) # normal distribution
binom_dist <- data.frame(x = rbinom(n, size = 1, prob = 0.6)) # binomial distribution
poisson_dist <- data.frame(x = rpois(n, lambda = 3)) # poisson distribution
exp_dist <- data.frame(x = rexp(n, rate = 1)) # exponential distribution

#... Combining the distributions into one, with the name of the distribution for each

four_dist <- list(
  list(df = norm_dist, dist = "normal"),
  list(df = exp_dist, dist = "exponential"),
  list(df = binom_dist, dist = "binomial"),
  list(df = poisson_dist, dist = "poisson")
  
)

# Plotting Time! ----

# Applying the function create_plot across each distribution that is in the 
# four_dist object. Map applies the function across each of the different 
# values in the list. 

plots <- purrr::map(four_dist, function(dist_info) {
  create_plot(dist_info$df, dist_info$dist)
})

# Combining all four graphs! 

purrr::reduce(plots, `+`) + 
  plot_layout(ncol = 2)  # Arrange them in 2 columns