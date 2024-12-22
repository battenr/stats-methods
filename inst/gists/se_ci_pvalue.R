# Title: Standard Error is Used for Confidence Intervals and P-Values

# Description: Demonstrating how the standard error is used to calculate 
# confidence intervals and p-values. This particular example, uses a 
# generalized linear model to estimate both. 

# Setup ----

#... Libraries ----

library(tidyverse) # ol' faithful
library(broom) # for cleaning output from model 

# Simulating Data ----

set.seed(654) # setting seed for reproducibility

n = 250 # sample size, arbitrarily chosen
beta_trt = 2 # "true" effect

# Simulating data with the following variables: 
# - Continuous Outcome
# - Binary Treatment (i.e., coffee)
# - Two confounders: one continuous, one binary

df <- data.frame(
  z1 = rnorm(n = n, mean = 5, sd = 1), 
  z2 = rbinom(n = n, size = 1, prob = 0.5)
) %>%
  dplyr::mutate(
  prob = plogis(0.05*z1 + 0.2*z2), # this is an intermediate variable. Used for creating the treatment (x) 
  x = rbinom(n = n, size = 1, prob = prob), 
  y = beta_trt*x + 3*z1 + 4*z2 + rnorm(n = n)
)

# Fitting the Model ----

# Fitting a generalized linear model. Here we are assuming no model misspecification. 
# We are also assuming we are able to adjust for all confounders (i.e., z1 & z2)

mod <- glm(y ~ x + z1 + z2, 
           family = gaussian(link = "identity"),
           data = df)

# Results! ----

# Using tidy() to clean the results. Calculating the 95% CIs and reformatting

broom::tidy(mod) %>% 
  dplyr::mutate(
    lower_ci = round(estimate - 1.96 * std.error, 2), # 1.96 because using an alpha of 0.05 (95% CI)
    upper_ci = round(estimate + 1.96 * std.error,2),
    estimate = round(estimate, 2),
    result = paste0(
      estimate, 
      " (", 
      lower_ci, 
      "; ", 
      upper_ci, 
      ")"
    )
  ) %>% 
  filter(term == "x") %>% 
  select(result, 
         p.value)

# Test Statistic ----

# Based on the model above, calculating the test statistic. 
# In this case, we are using the Wald test. 

beta_x <- coef(mod)["x"]  # Estimate of the coefficient for `x`
se_x <- summary(mod)$coefficients["x", "Std. Error"]  # Standard error of `x`

# Calculate the test statistic. For the Wald test, this is 
# Beta / SE of Beta

test_statistic <- beta_x / se_x

2*pnorm(abs(z_statistic), lower.tail = FALSE) # p-value. Multipled by 2 because assuming two-tailed test

