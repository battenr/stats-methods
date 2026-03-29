# Title: Bayesian Sensitivitiy Analysis 


# Note: this code works but get errors about Rhat. Need to explore more in depth before sharing


# Description: Often unmeasured confounding can cause problems. Part of the reason is that the variable is 
# unmeasured. Luckily in recent years there are a variety of sensivitiy analyses that can be done for this. 

# One solution is to use a Bayesian approach. We can model the variable that is unmeasured 
# by choosing a prior and treating it as being a missing variable. 

# Note: this code is exploratory and experimental. Recommend consulting with a statistician 
# before implementing in your own research. 

# Setup ----

#... Packages ----

library(tidyverse)
library(WeightIt) 
library(brms) 
library(tidybayes) 


#... Functions ----

sim_data <- function(n = 250, # sample size 
                     beta_trt = 1.5, # treatment effect
                     # Parameters for Z1 
                     z1_mean = 5, z1_sd = 2, 
                     # Parameters for Z2
                     z2_size = 1, z2_prob = 0.5, 
                     # Confounder - Effect on X
                     z1_on_x = 0.05, z2_on_x = 0.2,
                     # Confounder - Effect on Y
                     z1_on_y = 0.5, z2_on_y = 0.3,
                     # Parameters for U
                     u_mean = 4, u_sd = 1, 
                     # Unmeasured Confounder - Effect on X and Y
                     u_on_x = 0.5, u_on_y = 0.5
                     ){
  
  # Creating the Dataframe
  
  df <- data.frame(
    z1 = rnorm(n = n, mean = z1_mean, sd = z1_sd), 
    z2 = rbinom(n = n, size = z2_size, prob = z2_prob),
    u = rnorm(n = n, mean = u_mean, sd = u_sd)
  ) %>% 
    dplyr::mutate(
      prob = plogis(z1_on_x*z1 + z2_on_x*z2 + u_on_x*u), 
      x = rbinom(n = n, size = 1, prob = prob), 
      y = beta_trt*x + z1_on_y*z1 + z2_on_y*z2 + u_on_y*u + rnorm(n = n, mean = 0, sd = 1)
    ) %>% 
    mutate( u = NA_real_)
  
  # Return the dataframe
  
  return(df)
}

# Simulate Data ----

set.seed(456) 

df <- sim_data()

# Frequentist Method (Doubly Robust) ----

#... IP Weighting ----

W <- WeightIt::weightit(x ~ z1 + z2, 
              data = df, 
              method = "glm", 
              stabilize = TRUE, 
              estimand = "ATE")

#... Outcome Model w/ Covariates ----

WeightIt::glm_weightit(y ~ x + z1 + z2, 
                       data = df, 
                       weightit = W) %>% 
  broom::tidy() %>% 
  filter(term == "x") 

# The above is our result. However, we know there is unmeasured confounding. 
# So how can we assess the impact of that? 

# Bayesian Modelling ----

#... Set Formula ----

# We are going to model three different variables at the same time. This is 
# known as joint modelling. 

# For u, we have no data, so instead we are going to treat as a missing variable
# that has complete missingness. 

# y depends on x, z1, z2 and the missing u
bf_y <- bf(y ~ x + z1 + z2 + mi(u))

# x depends on z1 and the missing u (using probit for easier latent scaling)

bf_x <- bf(x ~ z1 + z2 + mi(u), family = bernoulli(link = "logit")  )

# u is modeled as a standard normal latent variable
bf_u <- bf(u | mi() ~ 1)

# 3. Refined Sensitivity Priors
# To reach convergence, we must be very specific about the scale of U
sens_priors <- c(
  # PRIOR 1: Effect of U on Outcome (Sensitivity Parameter)
  prior(normal(2.0, 0.1), class = "b", coef = "miu", resp = "y"),
  
  # PRIOR 2: Effect of U on Treatment (Sensitivity Parameter)
  # On probit scale, 0.8 is a moderately strong effect
  prior(normal(1, 3), class = "b", coef = "miu", resp = "x"),
  
  # PRIOR 3: Anchor the Latent Variable
  # We fix mean to 0 and SD to 1 so the 'miu' coefficients have a stable scale
  prior(normal(0, 0.01), class = "Intercept", resp = "u")#,
  #prior(constant(1), class = "sigma", resp = "u")
)

# 4. Fit with High Precision
fit_mi_final <- brm(
  bf_y + bf_x + bf_u + set_rescor(FALSE),
  data = df,
  prior = sens_priors,
  #backend = "cmdstanr",
  chains = 4, 
  iter = 4000, 
  warmup = 2000,
  control = list(
    adapt_delta = 0.999, # Maximize careful steps to stop divergences
    max_treedepth = 15
  ),
  refresh = 500
)



pp_check(fit_mi_final, resp = "y")
mcmc_plot(fit_mi_final, type = "rank_overlay")

# Focus only on the parameters of interest to avoid a massive grid
plot(fit_mi_final)



library(tidybayes)

# 5. Extract and Plot the 'De-biased' ATE
fit_mi_final %>%
  spread_draws(b_y_x) %>%
  ggplot(aes(x = b_y_x)) +
  stat_halfeye(fill = "royalblue", alpha = 0.7) +
  geom_vline(xintercept = 1.5, linetype = "dashed", color = "red") + # True Effect
  labs(
    title = "Bayesian Sensitivity Analysis (Latent MI)",
    subtitle = "Posterior of X on Y after adjusting for unmeasured confounder U",
    x = "Treatment Effect (Beta)",
    y = "Posterior Density"
  ) +
  theme_minimal()




# Bonus: For comparison (bayesian model to a bayesian model) ----






