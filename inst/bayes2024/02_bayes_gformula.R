# Title: Bayesian Parametric G-Formula as a Sensitivity Analysis

# Description: This code is for a presentation that I gave at Bayes Pharma 2024. 
# It discusses using the Bayesian parametric g-formula as a sensitivity analysis. 

# This particular script focuses on the Bayesian Parametric G-Formula. 
# Three scenarios are used. Each with a different prior. 

# Scenario 1: Weak prior 
# Scenario 2: Informative prior
# Scenario 3: Overly strong prior

# Setup ----

source("inst/bayes2024/00_simulating_data.R") # only need to run this once. 
library(brms)
library(tidybayes)

# Bayesian G-Formula ----

# Now we can do this Bayesianly. For why we want to do it Bayesianly, 
# see the slides on the conference website. 

# Weak Priors ----

# Fit a Bayesian linear model with weak priors

bayes_mod <- brm(
  y ~ x + l1 +l2, 
  data = df,
  family = gaussian(),
  prior = c(
    prior(normal(0, 10), class = "b", coef = "x"), # flat prior for X
    prior(normal(0, 10), class = "b", coef = "l1"), # flat prior for l1
    prior(normal(0, 10), class = "b", coef = "l2")
  ),
  iter = 4000, 
  chains = 4
)

# Checking how model compares to the model used for the frequentist versinon. G-formula

tidy(mod)
bayes_mod

# Posterior predictive samples for Y under X = 1 and X = 0
Y_X1_samples <- posterior_epred(bayes_mod, newdata = data.frame(x = 1, l1 = df$l1, l2 = df$l2))
Y_X0_samples <- posterior_epred(bayes_mod, newdata = data.frame(x = 0, l1 = df$l1, l2 = df$l2))

# Estimate the risk difference (Average Treatment Effect) by averaging posterior draws
median(rowMeans(Y_X1_samples - Y_X0_samples))
quantile(rowMeans(Y_X1_samples - Y_X0_samples), c(0.025, 0.975))

mean(rowMeans(Y_X1_samples - Y_X0_samples)) # average treatment effect 

# Similar Result to Outcome Model but more accurate/in line with IPTW b/c we're drawing 
# from the predictive posterior distribution rather than having to bootstrap. 

# Median: 0.231 95% CI: -0.0232 to 0.486

# this compares to out model that we got before. Not to our result after 

# Assuming Some Information ----

bayes_mod <- brm(
  y ~ x + l1 +l2, 
  data = df,
  family = gaussian(),
  prior = c(
    prior(normal(0, 2.5), class = "b", coef = "x"), # flat prior for X
    prior(normal(0, 2.5), class = "b", coef = "l1"), # flat prior for l1
    prior(normal(1, 2.5), class = "b", coef = "l2")
  ),
  iter = 4000, 
  chains = 4
) 

# Checking how model compares to the model used for the frequentist versinon. G-formula

tidy(mod)
bayes_mod

# Posterior predictive samples for Y under X = 1 and X = 0
Y_X1_samples <- posterior_epred(bayes_mod, newdata = data.frame(x = 1, l1 = df$l1, l2 = df$l2))
Y_X0_samples <- posterior_epred(bayes_mod, newdata = data.frame(x = 0, l1 = df$l1, l2 = df$l2))

# Estimate the risk difference (Average Treatment Effect) by averaging posterior draws
median(rowMeans(Y_X1_samples - Y_X0_samples))
quantile(rowMeans(Y_X1_samples - Y_X0_samples), c(0.025, 0.975))

mean(rowMeans(Y_X1_samples - Y_X0_samples)) # average treatment effect 

# Too Strong for L2 ----

bayes_mod <- brm(
  y ~ x + l1 +l2, 
  data = df,
  family = gaussian(),
  prior = c(
    prior(normal(0, 2.5), class = "b", coef = "x"), # flat prior for X
    prior(normal(0, 2.5), class = "b", coef = "l1"), # flat prior for l1
    prior(normal(2, 1), class = "b", coef = "l2")
  ),
  iter = 4000, 
  chains = 4
) 

# Checking how model compares to the model used for the frequentist versinon. G-formula

tidy(mod)
bayes_mod

# Posterior predictive samples for Y under X = 1 and X = 0
Y_X1_samples <- posterior_epred(bayes_mod, newdata = data.frame(x = 1, l1 = df$l1, l2 = df$l2))
Y_X0_samples <- posterior_epred(bayes_mod, newdata = data.frame(x = 0, l1 = df$l1, l2 = df$l2))

# Estimate the risk difference (Average Treatment Effect) by averaging posterior draws
median(rowMeans(Y_X1_samples - Y_X0_samples))
quantile(rowMeans(Y_X1_samples - Y_X0_samples), c(0.025, 0.975))

mean(rowMeans(Y_X1_samples - Y_X0_samples)) # average treatment effect 

# Too Weak for L2 ----

bayes_mod <- brm(
  y ~ x + l1 +l2, 
  data = df,
  family = gaussian(),
  prior = c(
    prior(normal(0, 2.5), class = "b", coef = "x"), # flat prior for X
    prior(normal(0, 2.5), class = "b", coef = "l1"), # flat prior for l1
    prior(normal(0, 1), class = "b", coef = "l2")
  ),
  iter = 4000, 
  chains = 4
) 

# Checking how model compares to the model used for the frequentist versinon. G-formula

tidy(mod)
bayes_mod

# Posterior predictive samples for Y under X = 1 and X = 0
Y_X1_samples <- posterior_epred(bayes_mod, newdata = data.frame(x = 1, l1 = df$l1, l2 = df$l2))
Y_X0_samples <- posterior_epred(bayes_mod, newdata = data.frame(x = 0, l1 = df$l1, l2 = df$l2))

# Estimate the risk difference (Average Treatment Effect) by averaging posterior draws
median(rowMeans(Y_X1_samples - Y_X0_samples))
quantile(rowMeans(Y_X1_samples - Y_X0_samples), c(0.025, 0.975))

mean(rowMeans(Y_X1_samples - Y_X0_samples)) # average treatment effect 






