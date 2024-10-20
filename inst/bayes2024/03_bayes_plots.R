# Title: Bayesian Parametric G-Formula as a Sensitivity Analysis

# Description: This code is for a presentation that I gave at Bayes Pharma 2024. 
# It discusses using the Bayesian parametric g-formula as a sensitivity analysis. 

# This particular script focuses on outputting plots from the previous scripts. 

# Scenario 1: Weak prior 
# Scenario 2: Informative prior
# Scenario 3: Overly strong prior

# Setup ----

# source("inst/bayes2024/02_bayes_gformula.R") - commented out but need this for the actual bayesian results

# Libraries ----

library(tidybayes)
library(brms)
library(tidyverse)
library(ggdist)
library(distributional)

# Function for Plotting Priors ----

plot_priors <- function(df_priors, subtext){
  
  priors %>%
    parse_dist(prior) %>%
    
    ggplot(aes(y = paste("For", coef, ":", " ", class, "~", format(.dist_obj)), xdist = .dist_obj)) +
    stat_halfeye(subguide = subguide_inside(position = "right", title = "density"), color = "purple", fill = "pink") +
    labs(
      title = "Prior Distributions",
      subtitle = subtext,
      x = NULL,
      y = NULL
    ) + 
    theme_minimal() +
    theme(text = element_text(hjust = 0.5, size = 20),
          plot.title = element_text(hjust = 0.5, face = "bold"), 
          plot.subtitle = element_text(hjust = 0.5)
    )
}


# Showing the Priors ----

# Note we do not need any simulated data for this. 

#... Weak Priors ----

priors = c(
  prior(normal(0, 10), class = b, coef = "X"),
  prior(normal(0, 10), class = b, coef = "L1"), 
  prior(normal(0, 10), class = b, coef = "L2")
)

plot_priors(priors, "Weak Priors")

#... Assuming Some Information ----

priors = c(
  prior(normal(0, 2.5), class = "b", coef = "x"), # flat prior for X
  prior(normal(1, 2.5), class = "b", coef = "l1"), # flat prior for l1
  prior(normal(1, 1), class = "b", coef = "l2")
)


plot_priors(priors, "Some Information")

# Too Strong for L2 ----

priors = c(
  prior(normal(0, 2.5), class = b, coef = "X"),
  prior(normal(0, 2.5), class = b, coef = "L1"), 
  prior(normal(2, 1), class = b, coef = "L2")
)

plot_priors(priors, "Strong Priors")


# Too Weak for L2 ----

priors = c(
  prior(normal(0, 2.5), class = b, coef = "X"),
  prior(normal(0, 2.5), class = b, coef = "L1"), 
  prior(normal(0, 1), class = b, coef = "L2")
)

plot_priors(priors, "Too Weak for L2")

