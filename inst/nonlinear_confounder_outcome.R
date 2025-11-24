# Title: Splines for Causal Inference 

# Description: Demonstrating how splines are helpful for causal inference. 
# Specifically, they are helpful when we have a non-linear relationship. 

# The goal with causal inference is to block backdoor paths. To do this, we
# adjust for confounders. We need to model the relationship between the 
# confounder and exposure, between confounder and outcome or both. 

# Sometimes these relationships are non-linear. So what do we do? 

# One solution is splines

# Setup ----

#... Libraries ----

library(tidyverse)
library(splines)
library(broom)
library(performance) # used for fitting splines

#... Functions ----

# Description: Skeleton for Simulating Data 

sim_data <- function(n = 250, # sample size 
                     beta_trt = 1.5, # treatment effect
                     # Parameters for Z1 
                     z1_mean = 5, z1_sd = 2, 
                     # Parameters for Z2
                     z2_size = 1, z2_prob = 0.5, 
                     # Confounder - Effect on X
                     z1_on_x = 0.5, z2_on_x = 0.2,
                     # Confounder - Effect on Y
                     z1_on_y = 0.2, z2_on_y = 0.3){
  
  # Creating the Dataframe
  
  df <- data.frame(
    z1 = rnorm(n = n, mean = z1_mean, sd = z1_sd), 
    z2 = rbinom(n = n, size = z2_size, prob = z2_prob)
  ) %>% 
    dplyr::mutate(
      prob = plogis(z1_on_x*z1 + z2_on_x*z2), 
      x = rbinom(n = n, size = 1, prob = prob), 
      y = beta_trt*x + z1_on_y*z1^2 + z2_on_y*z2 + rnorm(n = n, mean = 0, sd = 1)
    )
  
  # Return the dataframe
  
  return(df)
}

# Simulating Data ----

set.seed(456)

df <- sim_data()

# Plots ----

ggplot(data = df, 
       mapping = aes(x = z1, y = y)) + 
  geom_point(color = "purple", size = 2) +
  theme_minimal() +
  labs(x = "Hours of Sleep", 
       y = "Happiness") + 
  ggtitle("Hours of Sleep vs Happiness") + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 26),
    plot.subtitle = element_text(hjust = 0.5, size = 24),
    text = element_text(size = 24)
  ) 


# Fitting Models ----

#... Model 1 ----

# Only linear terms

mod1 <- glm(y ~ x + z1 + z2, 
            family = gaussian(),
            data = df)

mod1 %>% 
  broom::tidy(conf.int = TRUE)

#... Model 2 ----

# Using a spline for z1

mod2 <- glm(y ~ x + splines::ns(z1, 4) + z2, 
            family = gaussian(),
            data = df)

mod2 %>% 
  broom::tidy(conf.int = TRUE)




# Bonus! ----

# When fitting a model, we need to understand the number of knots to use. 
# One way to do this, is to use the predictive posterior check. 

performance::check_predictions(mod1) # shows how it doesn't work with the data
performance::check_predictions(mod2) # better 

# Checking For collinearity, model fit using pp_check, and for outliers 

#... Model 1 Checks 

performance::check_model(mod1, 
                         check = c("vif", 
                                   "pp_check", 
                                   "outliers", 
                                   "qq"),
                         residual_type = "deviance")

#... Model 2 Checks

performance::check_model(mod2, 
                         check = c("vif", 
                                   "pp_check", 
                                   "outliers", 
                                   "qq"),
                         residual_type = "deviance")

boot::glm.diag.plots(mod2)
