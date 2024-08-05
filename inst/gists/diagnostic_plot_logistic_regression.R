# Title: Diagnostic Plots for Logistic Regression 

# Description: These diagnostic plots are recommended for logistic regression in 
# Hosmer & Lemeshow. Creating this code to show how to make them in R. 

# Setup ----

#... Libraries ----

library(tidyverse) # ol faithful
library(patchwork) # used for combining multiple plots

# Simulating Data ----

n = 1000 # sample size, arbitrarily of 1000

# Creating the data: 
# - Binary treatment
# - Two binary confounders and one continuous confounder
# - Binary outcome fit using a logistic probability 


df <- data.frame(
  
  # Three confounders with arbitrary parameters
  
  c1 = rnorm(n = n, mean = 5, sd = 1), 
  c2 = rbinom(n = n, size = 1, prob = 0.4), 
  c3 = rbinom(n = n, size = 1, prob = 0.6)
) %>% 
  dplyr::mutate(
    c1 = round(c1, 0), # rounding this so we can use m-asymptotics. Thinking of 
    # an example where we have age as a confounder but it's been rounded to a whole number. 
    # Otherwise, we'd have to use n-asymptotics or increase sample size. 
    
    # Binary treatment. Using logistic distribution but not sure if we really need this. 
    # Could just use the prob statment most likely. 
    
    trt = rbinom(n = n, size = 1, prob = plogis(0.1 * c1 - 0.1 * c2 + 0.2 * c3)),
    
    # Binary outcome using logistic distribution. 
    
    outcome = rbinom(n = n, size = 1, prob = plogis(0.1 * trt - 0.1*c1 + 0.2 * c2 + 0.2 * c3))
  )

# Fitting Logistic Regression ----

# If you're interested in learning more about the plots. I'd recommend looking at plots 
# that we know are "correct" then ones that we know are misspecified. 

mod <- glm(outcome ~ trt + c1 + c2 + c3, 
           data = df,
           family = binomial(link = "logit") # logit means logistic regression 
           )

# Diagnostics ----

# This is based on Applied Logistic Regression by Hosmer and Lemeshow. 

#... First Group by Covariate Pattern ----

df_cov_pattern <- df %>% 
  
  # A covariate pattern is the unique combo of covariates. So here, the group_by() 
  # function includes all of the covariates that we used in our model above. 
  
  group_by(
    c1, 
    c2, 
    c3, 
    trt
  ) %>% 
  
  # mj just means the covariate patterns. It's a number almost like a 
  # participant ID. 
  
  mutate(
    mj = cur_group_id()
  ) %>% 
  
  # Ungrouping 
  
  ungroup() 

#... Diagnostics ----

# Time to calculate the diagnostics! 

df_diag <- df_cov_pattern %>% 
  
  # Creating the diagnostics. Using j to denote the covariate pattern, the 
  # same as outlined in Applied Logistic Regression by Hosmer and Lemeshow. 
  
  mutate(
    rsj = stats::rstandard(mod, type = "pearson"), # Pearson standardized residuals
    dj = stats::residuals(mod, type = "deviance"), # deviance residuals
    hj = stats::hatvalues(mod), # leverage 
    pred_prob = predict(mod, type = "response")
  ) %>% 
  group_by(
    mj
  ) %>% 
  
  # Only keeping one covariate pattern. 
  
  slice_head() %>% 
  ungroup() %>% 
  
  # Calculating what will actually be used: 
  # - Change in chi-squared
  # - Change in estimated coefficients. Note that this is for all coefficients, not 
  # for just a particular one. 
  # - Change in deviance. 
  
  # Each of these are changes, so basically the effect of removing a particular covariate pattern. 
  
  mutate(
    deltaChisq = rsj^2,
    deltaB = (rsj^2*hj) / (1-hj), 
    deltaD = dj^2 / (1-hj)
  )

# Plots Away! ----

# Delta Deviance 

p1 <- ggplot(data = df_diag, aes(x = pred_prob, y = deltaD)) + 
  geom_point() + 
  labs(title = "\u0394D vs Estimated Probability", 
       x = "Estimated Logistic Probability",
       y = "\u0394D") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  
  # Careful about the limits chosen here. X is 0 to 1 because it's a probability 
  # (so should always be 0 to 1). However Y can considerably change how your plot looks 
  # (no different from any other plot). Same comment applies to below graphs. 
  
  lims(
    x = c(0, 1),
    y = c(0, 5))

# Delta Chi-Squared

p2 <- ggplot(data = df_diag, aes(x = pred_prob, y = deltaChisq)) + 
  geom_point() +
  labs(title = "\u0394\u03C7\u00B2 vs Estimated Probability", 
       x = "Estimated Logistic Probability",
       y = "\u0394\u03C7\u00B2") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  lims(x = c(0, 1), 
       y = c(0, 10))

# Delta Coeff

p3 <- ggplot(data = df_diag, aes(x = pred_prob, y = deltaB)) + 
  geom_point() +
  labs(title = "\u0394\u03B2 vs Estimated Probability", 
       x = "Estimated Logistic Probability",
       y = "\u0394\u03B2") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  lims( x = c(0, 1))

# Combine the Plots! ----

# Combining the plots. The patchwork package is great for this. 

(p1 + p2) / p3 + # the syntax is + or / 
  
  # Adding an overall title and the theme for the "overall plot". Mainly the text and the 
  # text for the title. 
  
  plot_annotation(title = "Diagnostic Plots for Logistic Regression",
                  theme = theme(
                    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
                    text = element_text(size = 14)
                  )
  )





