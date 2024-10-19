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
tidy(bayes_mod)


# Posterior predictive samples for Y under X = 1 and X = 0
Y_X1_samples <- posterior_epred(bayes_mod, newdata = data.frame(x = 1, l1 = df$l1, l2 = df$l2))
Y_X0_samples <- posterior_epred(bayes_mod, newdata = data.frame(x = 0, l1 = df$l1, l2 = df$l2))

# Estimate the risk difference (Average Treatment Effect) by averaging posterior draws
median(rowMeans(Y_X1_samples - Y_X0_samples))
quantile(rowMeans(Y_X1_samples - Y_X0_samples), c(0.025, 0.975))

# Median: 0.231 95% CI: -0.0232 to 0.486

# this compares to out model that we got before. Not to our result after 

# Using 

# Using Strong Prior for one of them ----

# Fit a Bayesian linear model with weak 

bayes_mod <- brm(
  y ~ x + l1 +l2, 
  data = df,
  family = gaussian(),
  prior = c(
    prior(normal(0, 10), class = "b", coef = "x"), # flat prior for X
    prior(normal(0, 2.5), class = "b", coef = "l1"), # flat prior for l1
    prior(normal(1, 2.5), class = "b", coef = "l2") # overly strong
  ),
  iter = 4000, 
  chains = 4
)

# Checking how model compares to the model used for the frequentist versinon. G-formula

tidy(mod)
bayes_mod

Y_X1_samples <- posterior_epred(bayes_mod, newdata = data.frame(x = 1, l1 = df$l1, l2 = df$l2))
Y_X0_samples <- posterior_epred(bayes_mod, newdata = data.frame(x = 0, l1 = df$l1, l2 = df$l2))

# Posterior predictive samples for Y under X = 1 and X = 0
median(rowMeans(Y_X1_samples - Y_X0_samples))
quantile(rowMeans(Y_X1_samples - Y_X0_samples), c(0.025, 0.975))

# Slight improvement 

# Using Same Prior for Both ----

bayes_mod <- brm(
  y ~ x + l1 +l2, 
  data = df,
  family = gaussian(),
  prior = c(
    prior(normal(0, 10), class = "b", coef = "x"), # flat prior for X
    prior(normal(1, 2.5), class = "b", coef = "l1"), # flat prior for l1
    prior(normal(1, 2.5), class = "b", coef = "l2") # overly strong
  ),
  iter = 4000, 
  chains = 4
)

Y_X1_samples <- posterior_epred(bayes_mod, newdata = data.frame(x = 1, l1 = df$l1, l2 = df$l2))
Y_X0_samples <- posterior_epred(bayes_mod, newdata = data.frame(x = 0, l1 = df$l1, l2 = df$l2))

# Posterior predictive samples for Y under X = 1 and X = 0
median(rowMeans(Y_X1_samples - Y_X0_samples))
quantile(rowMeans(Y_X1_samples - Y_X0_samples), c(0.025, 0.975))


# Using Overly Strong Prior for both ----


bayes_mod <- brm(
  y ~ x + l1 +l2, 
  data = df,
  family = gaussian(),
  prior = c(
    prior(normal(0, 10), class = "b", coef = "x"), # flat prior for X
    prior(normal(1, 1), class = "b", coef = "l1"), # flat prior for l1
    prior(normal(1, 1), class = "b", coef = "l2") # overly strong
  ),
  iter = 4000, 
  chains = 4
)

Y_X1_samples <- posterior_epred(bayes_mod, newdata = data.frame(x = 1, l1 = df$l1, l2 = df$l2))
Y_X0_samples <- posterior_epred(bayes_mod, newdata = data.frame(x = 0, l1 = df$l1, l2 = df$l2))

# Posterior predictive samples for Y under X = 1 and X = 0
median(rowMeans(Y_X1_samples - Y_X0_samples))
quantile(rowMeans(Y_X1_samples - Y_X0_samples), c(0.025, 0.975))

?posterior_epred()

# Plots! ----

draws <- bayes_mod %>% 
  epred_draws(newdata = df)

draws_y1 <- data.frame(
  y1 = rowMeans(Y_X1_samples)
)

draws_y0 <- data.frame(
  y0 = rowMeans(Y_X0_samples)
)

draws <- cbind(draws_y1, draws_y0) %>% 
  mutate(effect = y1-y0) %>% 
  tidyr::pivot_longer(
    everything(), 
    names_to = "outcome"
  ) 

draws %>% 
  filter(outcome != "effect") %>% 
  ggplot(aes(x = value, y = outcome, fill = as.factor(outcome))) +
  stat_halfeye(color = "purple", fill = "lightpink", point_interval = NULL) + 
  labs(x = "Y", y = "Treatment Group") +
  scale_y_discrete(labels = c(y0 = "X = 0", y1 = "X = 1")) +
  theme_minimal() + 
  theme(text = element_text(size = 20),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5)
  ) +
  ggtitle("Potential Outcomes", 
          subtitle = "Draws of Expected Value from the Posterior Distribution")  


# Forest Plot ----

library(ggdist)
library(brms)

priors = c(
  prior(normal(1, 10), class = b),
  prior(normal(0, 2.5), class = b)
  # lb = 0 sets a lower bound of 0, i.e. a half-Normal distribution
)

priors

?parse_dist

priors %>% 
  parse_dist(prior) %>% 
  ggplot(aes(y = prior, xdist = .dist_obj)) +
  stat_halfeye() +
  scale_y_discrete(labels = c("b ~ norm(1, 10)", 
                                         "b ~ norm(0, 2.5)"))

?stat_halfeye

# TESTING----

library(ggdist)
library(dplyr)
library(ggplot2)

# Sample priors data (you would replace this with your actual priors)
priors <- data.frame(
  prior = c("normal(1, 10)", "normal(0, 2.5)"),
  class = c("b", "b")
)

# Parse distribution objects
priors <- priors %>%
  mutate(.dist_obj = case_when(
    prior == "normal(1, 10)" ~ dist_normal(1, 10),
    prior == "normal(0, 2.5)" ~ dist_normal(0, 2.5)
  ))

# Create a cleaned y-label column for easier plotting
priors <- priors %>%
  mutate(prior_label = factor(prior, levels = prior))

# Plot with ggdist and correct y-axis labels
priors %>% 
  ggplot(aes(y = prior_label, xdist = .dist_obj)) +
  stat_halfeye() +
  scale_y_discrete(labels = c(
    "normal(1, 10)" = "b ~ norm(1, 10)",
    "normal(0, 2.5)" = "b ~ norm(0, 2.5)"
  )) +
  labs(
    title = "Priors Visualization",
    subtitle = "Custom Y-axis Labels",
    x = "Value",
    y = "Priors"
  )





