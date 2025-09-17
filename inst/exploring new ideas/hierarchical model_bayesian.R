# Bayesian Method 

# Using Bayesian outcome model with propenstiy scores
# 


# Setup ----

library(tidyverse)
library(WeightIt)
library(brms)
library(tidybayes)

#install.packages("brms")

source("R/sim_data.R")

df <- sim_data()

ps.mod <- WeightIt::weightit(x ~ z1 + z2, 
                   data = df, 
                   method = "glm", 
                   estimand = "ATE",
                   stabilize = TRUE)

ps.mod$ps

df.ps <- df %>% 
  mutate(
    ps = ps.mod$ps
  )

# bayesmod <- brms::brm(
#   bf(y ~ (x | z1) + (x | z2) + z1 + z2, family = gaussian()), 
#   data = df)

#summary(bayesmod)


# Some ideas to try 

# Maybe try propensity score in model then x so lik e

# (x | ps )


# Modelling Both Same Time ----

# Note: this method works, now need to understand how to compare  

formula <- bf(x ~ z1 + z2, family = bernoulli(link = "logit")) +
  bf(y ~ x + z1 + z2, family = gaussian()) +
  set_rescor(FALSE)

bayesmod <- brm(
  formula = formula,
  data = df
)

# bayesmod %>% summary()

posterior_interval(bayesmod)


# Extract posterior samples
posterior_samples <- bayesmod %>%
  spread_draws(b_y_x)

# Plot posterior distribution
ggplot(posterior_samples, aes(x = b_y_x)) +
  geom_density(fill = "skyblue", alpha = 0.7) +
  geom_vline(xintercept = 1.5, linetype = "dashed", color = "red") +
  labs(
    title = "Posterior Distribution of b_x_y (Effect of x on y)",
    x = "Estimate",
    y = "Density"
  ) +
  theme_minimal()

#... Including PS as a Covariate ----

df.ps$ps

formula <- bf(x ~ z1 + z2, family = bernoulli(link = "logit")) +
  bf(y ~ x + z1 + z2, family = gaussian()) +
  set_rescor(FALSE)

bayesmod <- brm(
  formula = y ~ x + z1 + z2 + ps,
  data = df.ps
)

bayesmod <- brm(
  formula = y ~ x + ps,
  data = df.ps
)

# bayesmod %>% summary()

posterior_interval(bayesmod)



# Extract posterior samples
posterior_samples <- bayesmod %>%
  spread_draws(b_x)

# Plot posterior distribution
ggplot(posterior_samples, aes(x = b_x)) +
  geom_density(fill = "skyblue", alpha = 0.7) +
  geom_vline(xintercept = 1.5, linetype = "dashed", color = "red") +
  labs(
    title = "Posterior Distribution of b_x_y (Effect of x on y)",
    x = "Estimate",
    y = "Density"
  ) +
  theme_minimal()

# Frequentist Approach ----

freqmod <- glm(y ~ x + z1 + z2, 
               data = df.ps, 
               weights = ps.mod$weights)

freqmod %>% broom::tidy(confint())

confint(freqmod)

freqmod <- glm(y ~ x + ps, 
               data = df.ps)

freqmod

broom::tidy(freqmod)

?tidy

confint(freqmod)


update(bayesmod, df1)

?posterior_predict

summary(bayesmod)

library(marginaleffects)

marginaleffects::avg_comparisons(bayesmod)

bayesmod %>% 
  tidybayes::add_draws()

draws <- bayesmod %>% 
  epred_draws(newdata = df)

draws %>% head() %>% view()

draws %>% head()

unique(draws$.category)

# Testing if Faster ----

library(brms)
library(future)
plan(multisession)  # For parallelization

# Define the multivariate formula template
make_formula <- function() {
  bf(x ~ z1 + z2, family = bernoulli("logit")) +
    bf(y ~ x + z1 + z2, family = gaussian()) +
    set_rescor(FALSE)
}

set.seed(123)  # for reproducibility

# Create 100 simulated datasets
datalist <- replicate(10, sim_data(), simplify = FALSE)


# Let's assume `datalist` is a list of 100 data frames
# Example: datalist <- list(df1, df2, ..., df100)

# Use `future_lapply` for parallel evaluation
library(future.apply)

library(future)
library(future.apply)
plan(multisession)  # Run models in parallel

# Fit all models
library(furrr)
plan(multisession, workers = parallel::detectCores())

fits <- future_map(1:2, function(i){
  
  data_i <- sim_data()
                     
brm(
  formula = make_formula(),
  data = data_i,
  chains = 1,
  iter = 500,
  warmup = 250,
  algorithm = "meanfield",
  refresh = 0
)
}
)








# 1.33 to 1.82 



ggplot(data = draws %>% filter(.category == "x"), 
       mapping = aes(x = .epred) ) + 
  geom_density()

brms::cred

draws %>% 
  filter(outcome != "effect") %>% 
  ggplot(aes(x = value, y = outcome, fill = as.factor(outcome))) +
  stat_halfeye(color = "#C32048", fill = "lightpink", point_interval = "mean_qi", size = 12) + 
  labs(x = "Y", y = "Treatment Group") +
  scale_y_discrete(labels = c(y0 = "X = 0", y1 = "X = 1")) +
  theme_minimal() + 
  theme(text = element_text(size = 20),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5)
  ) +
  ggtitle("Potential Outcomes", 
          subtitle = "Draws of Expected Value from the Posterior Predictive Distribution")  +
  lims(x = c(2.5, 4.5))



bayesmod %>% gather_emmeans_draws()

bayesmod <- brm(
  formula = bf,
  data = df
)

summary(bayesmod)

bayesmod %>% 
  spread_draws()

bayesmod$model

# try adjusting for PS in outcome model and adding prior 

library(tidybayes)

posterior_df <- bayesmod %>%
  spread_draws()
  
  
  spread_draws(y_x, .value = TRUE, regex = TRUE) %>%
  filter(.variable == "y_x", .response == "y")
