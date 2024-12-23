# Title: Demonstration of the Region of Practical Equivalence (ROPE)

# Description: Showing how ROPE can be a helpful tool. 
# My personal preference is to show the entire distribution, however we can also use ROPE for decision making. 

# This script has two examples: 
# 1. There is actually an effect (1.5)
# 2. There is no effect (0)

# Setup ----

#... Library ----

library(tidyverse) # ol faithful
library(brms) # for fitting the model 
library(tidybayes) # for working with the results 
library(bayestestR) # for certain things. In this case, for calculating ROPE

#... Functions ----

# Custom Theme 

# This custom theme is used for the plots 

custom_theme <- function() {
  theme_minimal() %+replace% # basing this on the minimal theme with some adjustments
    theme(
      plot.title = element_text(hjust = 0.5, 
                                family = "JostRoman-bold", 
                                face = "bold", 
                                size = 30),
      #axis.title = element_text(family = "Jost Medium"),
      plot.subtitle = element_text(hjust = 0.5, size = 24),
      text = element_text(family = "Jost", size = 24)
    ) 
}

# Simulating Data Function

sim_data <- function(n = 250, # sample size 
                     beta_trt = 1.5, # treatment effect
                     # Parameters for Z1 
                     z1_mean = 5, z1_sd = 2, 
                     # Parameters for Z2
                     z2_size = 1, z2_prob = 0.5, 
                     # Confounder - Effect on X
                     z1_on_x = 0.05, z2_on_x = 0.2,
                     # Confounder - Effect on Y
                     z1_on_y = 0.5, z2_on_y = 0.3){
  
  # Creating the Dataframe
  
  df <- data.frame(
    z1 = rnorm(n = n, mean = z1_mean, sd = z1_sd), 
    z2 = rbinom(n = n, size = z2_size, prob = z2_prob)
  ) %>% 
    dplyr::mutate(
      prob = plogis(z1_on_x*z1 + z2_on_x*z2), 
      x = rbinom(n = n, size = 1, prob = prob), 
      y = beta_trt*x + z1_on_y*z1 + z2_on_y*z2 + rnorm(n = n, mean = 0, sd = 1)
    )
  
  # Return the dataframe
  
  return(df)
}

# Simulating Data ----

set.seed (654) # setting seed for reproducibility

df_with_effect <- sim_data() # a dataset where there really is an effect

df_with_no_effect <- sim_data(beta_trt = 0) # a dataset where there really is no effect

# Bayesian Model ----

# Setting Priors 

priors <- c(
  prior(normal(0, 2), class = "b", coef = "x"),
  prior(normal(0, 2), class = "b", coef = "z1"), 
  prior(normal(0, 2), class = "b", coef = "z2")
)

# Fitting models 

#... Model with an effect

mod1 <- brms::brm(y ~ x + z1 + z2, 
                  family = gaussian(link = "identity"), 
                  data = df_with_effect, 
                  prior = priors)

pp_check(mod1) # checking posterior distribution

#... Model with dataset with no effect 

mod2 <- brms::brm(y ~ x + z1 + z2, 
                  family = gaussian(link = "identity"), 
                  data = df_with_no_effect, 
                  prior = priors)

pp_check(mod2) # checking posterior distribution

# Let's Plot the Parameter! ----

# Here we are just plotting the parameter to see what the results look like

# For when there is an effect

mod1 %>% 
  spread_draws(b_x) %>% 
  ggplot(aes(x = b_x)) +
  stat_halfeye(fill = "pink", 
               color = "purple",
  ) + 
  labs(x = "Effect Estimate",
       y = "Density") + 
  # Edit the titile
  ggtitle("Effect Estimate",
          subtitle = "Prior ~ N(0,2)") +
  custom_theme()

# For when there is no effect 

mod2 %>% 
  spread_draws(b_x) %>% 
  ggplot(aes(x = b_x)) +
  stat_halfeye(fill = "pink", 
               color = "purple",
  ) + 
  labs(x = "Effect Estimate",
       y = "Density") + 
  # Edit the titile
  ggtitle("Effect Estimate",
          subtitle = "Prior ~ N(0,2)") +
  custom_theme()


# Calculating ROPE ----

# The ROPE is the region of practical equivalence. This is helpful because it can help
# us determine what the region would be where we'd basically say there is no effect. 
# For example, 0 is no effect. What about 0.3? What about 0.4? 

# Note: this is meant to be an introduction. For more details, recommend the bayestestR vignette here: 
# https://easystats.github.io/bayestestR/reference/rope.html

#... When There Is an Effect ----

rope(mod1) # we can see what percent is in ROPE

percentage_in_rope <- rope(mod1, parameters = "x")

plot(percentage_in_rope) + 
  custom_theme() +
  labs(subtitle = "True Effect is 1.5",
       x = "Possible parameter values for effect estimate",
       y = "Density",
       fill = "Credible Interval")

#... When There is No Effect ----

# For All Parameters: 

rope(mod2) # we can see what percent is in ROPE

percentage_in_rope2 <- rope(mod2, parameters = "x")

plot(percentage_in_rope2) + 
  custom_theme() +
  labs(subtitle = "True Effect is 0",
       x = "Possible parameter values for effect estimate",
       y = "Density",
       fill = "Credible Interval")