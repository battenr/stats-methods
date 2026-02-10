# Title: Bayesian Joint Modelling Approach

# Description: Demonstrating how a Bayesian approach can be useful. For this example, we are going to model 
# the outcome and the treatment at the same time. 

# Setup ----

#... Libraries ----

library(tidyverse) # ol faithful 
library(brms) # fitting a Bayesian model 
library(tidybayes) # for working with results of Bayesian model
library(ggdist) # for plotting results 
library(bayesplot) # for plotting Bayesian results 

# Bonus - Frequentist doubly robust approach
library(WeightIt)
library(survey)

#... Functions ----

# Simulating Data 

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

# Simulate Data ----

set.seed(456) # for reproducibility

df <- sim_data()

# Fitting Model ----

#... Formula ----

# Formula for the outcome (Gaussian)
bf_outcome <- bf(y ~ x + z1 + z2)

# Formula for the treatment (Bernoulli / Propensity Score)
bf_propensity <- bf(x ~ z1 + z2, family = bernoulli())

#... Fitting Model ----

# We use 'rescor = FALSE' because Bernoulli models don't have a residual 
# variance parameter to correlate with the Gaussian model

fit <- brms::brm(
  bf_outcome + bf_propensity + set_rescor(FALSE),
  data = df,
  chains = 4, 
  iter = 2000, 
  warmup = 1000,
  cores = 4
)

# Check Model Fit ----

#... Posterior Predictive Check ----

# For Outcome

pp_check(fit, resp = "y")

# For Treatment 

pp_check(fit, resp = "x")

#... Check Trace Plot ----

# Note: should look like fuzzy catepillar 

import_draws <- as.array(fit)

# Traditional Trace Plot
mcmc_trace(import_draws, pars = c("b_y_Intercept", "b_x_Intercept"))

#... Optional: Checking Rank Plot ----

# Rank Plots (often better for spotting convergence issues)
mcmc_rank_overlay(import_draws, pars = "b_y_Intercept")

# Doubly Robust - IPTW Approach (Frequentist) ----

#... Calculate weights (ATE) ----

W <- WeightIt::weightit(x ~ z1 + z2, data = df, method = "glm", estimand = "ATE")

#... Outcome Model ----

fit_iptw <- WeightIt::glm_weightit(y ~ z1 + z2, 
                                   data = df, 
                                   family = gaussian(), 
                                   weight = W)

#... Tidy Output 

fit_iptw_tidy <- broom::tidy(fit_iptw, conf.int =TRUE)

#... Format Result ----

# Extract IPTW estimate and CI
iptw_res <- data.frame(
  model = "IPTW",
  estimate = fit_iptw_tidy %>% filter(term == "(Intercept)") %>% select(estimate) %>% as.numeric(),
  conf.low = fit_iptw_tidy %>% filter(term == "(Intercept)") %>% select(conf.low) %>% as.numeric(),
  conf.high = fit_iptw_tidy %>% filter(term == "(Intercept)") %>% select(conf.high) %>% as.numeric()
)

# Plots ----

#... Prep Data (Bayesian Approach) ----


# We extract the 'b_y_x' parameter (the effect of x on y)

bayes_draws <- fit %>%
  spread_draws(b_y_x) %>%
  mutate(model = "Joint Model \n (Bayesian)")

#... Plot ----

ggplot(bayes_draws, aes(x = b_y_x, y = model)) +
  stat_halfeye(
  # Bayesian Posterior (Density + 95% Credible Interval)
    .width = c(0.95), 
    fill = "pink", 
    alpha = 0.7,
    color = "purple",
    size = 10,
    linewidth = 2
  ) +
  # IPTW Estimate (Point + 95% Confidence Interval)
  geom_pointrange(
    data = iptw_res,
    aes(x = estimate, xmin = conf.low, xmax = conf.high, y = model),
    color = "purple",
    size = 1
  ) +
  geom_vline(xintercept = 1.5, linetype = "dashed", color = "gray50") + # True Effect
  labs(
    title = "Bayesian Joint Modelling vs. IPTW",
    subtitle = "Dashed line represents true simulated effect (1.5)",
    x = "Estimated Effect of X on Y",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 24), 
    plot.subtitle = element_text(hjust = 0.5, size = 20), 
    text = element_text(size = 18),
    axis.text = element_text(size = 18)
  ) +
  xlim(x = 0, 2.5)

#... Numerical Results ----

iptw_res
summary(fit)
