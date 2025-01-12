# Title: Plotting Different Priors 

# Description: Choosing an appropriate prior is a key part of any Bayesian analysis. 
# This code shows different priors to try and highlight which one(s) might 
# be reasonable. 

# Setup ----

#... Packages ----

library(tidyverse) # ol' faithful
library(brms) 
library(tidybayes)
library(bayesplot)

#... Setting Theme ----

custom_theme <- function() {
  theme_minimal() %+replace% # basing this on the theme_minimal() function but editing some of the components
    theme(
      plot.title = element_text(hjust = 0.5, family = "Jost", face = "bold", size = 26),
      plot.subtitle = element_text(hjust = 0.5, size = 24),
      text = element_text(family = "Jost", size = 24)
    ) 
}

#... Setting Colors ----

# These colors came from a blog post from Andrew Heiss

clrs <- c(
  "#FFBE00",  
  "#B92F0A",  
  "#2660ae",   
  "#792A26" 
)

# Priors ----

# For this we're going to look at using two different distributions: 
# Normal Distribution and Student's t. 
# We'll also look at two different types within these distributions. 

priors <- c(brms::prior(normal(4, 2), class = b),
            brms::prior(student_t(3, 4, 2), class = b),
            brms::prior(normal(2, 1), class = b),
            brms::prior(student_t(3, 2, 1), class = b)
)

# Plotting Priors ----

priors |> 
  tidybayes::parse_dist() |> 
  mutate(prior = fct_inorder(prior)) |> 
  ggplot(aes(y = 0, dist = .dist, args = .args, fill = prior)) +
  stat_slab(normalize = "panels") +
  scale_fill_manual(values = clrs, guide = "none") +
  labs(x = NULL, y = NULL) +
  facet_wrap(facets = vars(prior)) + 
  custom_theme() +
  ggtitle("Different Prior Distributions")