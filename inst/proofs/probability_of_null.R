# Title: Showing what the probability of the null is given our data 

# Description: Often the p-value is misinterpreted as the probability of the null. 
# The goal of this post is to show what the probability of the null actually is. 
# We can do this using Bayesian statistics. 

# Note: the p-value is the probability of a test-statistic at least as extreme assuming
# the null distribution

# Setup ----

#... Libraries ----

library(tidyverse) # ol' faithful
library(brms) # for Bayesian analyses
library(tidybayes) # makes working with parts of the analysis easier in tidyverse-style code
library(bayesplot) # useful for plots
library(broom) # for tidying results from GLM

# Simulating Data ----

# For this example we want to keep it very simple. We'll use just two variables. 
# - X: the treatment (binary) 
# - Y: the outcome (a continuous outcome)

set.seed(42) # for reproducibility

n <- 250 # sample size of 250, arbitrarily chosen

x <- rbinom(n = n, size = 1, prob = 0.5) # treatment 

trt_effect <- 0 # effect of treatment. For this, we're going to assume it's 0 

y <- 5 + trt_effect*x + rnorm(n = n, mean = 0, sd = 1) # continuous outcome

#.. Combining into a data frame 

df <- data.frame(
  x = x, 
  y = y
)

# Fitting Bayesian Model ----

# We're going to use a prior, where we assume that the mean effect is 0 with 
# a standard deivation of 1. 

# This prior is set for ease. However, we can tinker with it if we want/can justify it. You can make 
# it more flat (larger SD) or change the location (mean). For example, if we thought there was a treatment 
# effect, you could try something like mean of 1 and keep, or change, the SD. 

# The prior is the distribution, in this example, for the coefficient. 
# Remember the variable earlier "effect_trt"? Here we are using a prior, the distribution 
# with mean of 0 and SD of 1, for this. 

# Fitting a model using brms 

model <- brm(y ~ x, 
             data = df, 
             family = gaussian(),
             prior = c(
               prior(normal(0, 1), 
                     class = "b") # Prior on the coefficient trt_effect. The "b" is for beta (as in the coefficients)
               ) 
             )  


# Extracting Posterior Samples ----

# What we get as a result is a posterior distribution. It's helpful to plot this. So
# let's take some samples from the posterior distribution so we can see what it looks like. 

posterior <- model %>% 
  spread_draws(b_x) # the b_x here is which coefficient we want to extract draws from. This is because 
# for each coefficient there will be a posterior distribution. In this case, there is only one 
# independent variable (x). However that won't always be the case

# Plotting ----

# First let's look at the median and high density interval

tidybayes::median_hdi(posterior) # 95% probability of true value being between the lower and upper limits

hdi_data <- tidybayes::median_hdi(posterior) # we will use this as text later

# Time to Plot! Here we are plotting the posterior distribution

ggplot(data = posterior, 
       mapping = aes(x = b_x)) + 
  stat_halfeye(fill = "pink", 
               color = "purple",
               size = 6,
               point_interval = "median_hdi"
               ) + 
  labs(x = "Treatment Effect",
       y = "Density") + 
  ggtitle("Posterior Distribution",
          subtitle = "Prior ~ N(0,1)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    text = element_text(size = 20)
  ) +
  geom_text(data = hdi_data, 
            aes(x = b_x, 
                y = 0, 
                label = paste0("Median: ", 
                               round(b_x, 2),
                               "; HDI: (",
                               round(.lower, 2),
                               ", ",
                               round(.upper, 2), 
                               ")"
                               )),
            color = "purple", 
            size = 6, 
            vjust = -1) 

# Let's calculate the probability of the value being null! ----

# Now to do this, we need to think of what range are we okay with. Let's start with 0.1 on either side. 
# This would be the probability of it being between -0.1 and 0.1

posterior %>%  
  mutate(is_near_zero = ifelse(-0.1 < b_x & b_x < 0.1, 
                               1, 
                               0)
         ) %>%  
  summarise(probability_near_zero = mean(is_near_zero))

# What if we chose something "stricter". Let's try with 0.05 on either side. 

posterior %>%  
  mutate(is_near_zero = ifelse(-0.05 < b_x & b_x < 0.05, 
                               1, 
                               0)
  ) %>%  # Define "near zero"
  summarise(probability_near_zero = mean(is_near_zero))

# We can see how this changes the probability. This highlights the uncertainty and helps
# when thinking about the certainty in an estimate. 

# It's important to note this is NOT the p-value. The p-value is the probability of at least 
# as extreme of a test statistic, given the null is true. 

glm(y ~ x, 
    data = df) %>% 
  broom::tidy()
