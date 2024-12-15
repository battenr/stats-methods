# Title: Using Only the Prior for a Model ----

# Description: A strength of Bayesian methods is the ability to set a prior and 
# a model, then draw from only the prior. This can help to show how a model 
# would perform and if our priors are reasonable. 

# Setup ----

#... Libraries ----

library(tidyverse) # ol' faithful
library(brms) # for Bayesian model
library(tidybayes) # for working with bayes models
library(patchwork) # for combining plots

# Simulated Data  ----

n = 250 # sample size 
beta_trt = 1.5

df <- data.frame(
  z1 = rnorm(n = n, mean = 5, sd = 1), 
  z2 = rbinom(n = n, size = 1, prob = 0.5)
) %>%
  dplyr::mutate(
  prob = plogis(0.05*z1 + 0.2*z2), # this is an intermediate variable 
  x = rbinom(n = n, size = 1, prob = prob), 
  y = beta_trt*x + 0.5*z1 + 0.3*z2
)

# Priors ----

# For this situation, we are going to try out two difference priors. 
# The first will be a normal distribution with mean of 0 and sd of 5 
# The second will also be a normal distribution but with a mean of 0 and sd of 0.2

priors1 <- c(
  prior(normal(0, 5), class = "b"),    # Prior for the regression coefficients (b)
  prior(normal(0, 5), class = "Intercept")  # Prior for the intercept
)

priors2 <- c(
  prior(normal(0, 0.2), class = "b"),    # Prior for the regression coefficients (b)
  prior(normal(0, 0.2), class = "Intercept")  # Prior for the intercept
)


# Usings priors of N(0, 1) ----

# We know that there are two confounders, z1 & z2. So let's try a model 
# with only X. Remember that here we aren't using any data for the outcome yet.
# Only sampling from the prior 

mod1 <- brm(y ~ x + z1 + z2,
           family = gaussian(link = "identity"),
           data = df, # specifying data here but it won't actually be used.  
           prior = priors1, 
           sample_prior = "only") # this means we will only sample the prior

#... Plotting Outcome ----

pred_val1 <- tidybayes::add_epred_draws(newdata = df, 
                                              object = mod1) # how many draws per value

pred_plot1 <- ggplot(data = pred_val1, 
                         aes(x = .epred)) + 
  stat_halfeye(fill = "pink", 
               color = "purple") + 
  labs(x = "Outcome",
       y = "Density") + 
  ggtitle("Predicted Outcome",
          subtitle = "Prior ~ N(0,5)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    text = element_text(size = 20)
  ) 

# Adjusting for Z1 & Z2 (aka the Correct Model) ----

# We know from our simulated data that the correct model would be to 
# adjust for Z1 & Z2 since they are both confounders. 


# We can repeat the process. Remember we aren't including any data for the outcome
# yet. 

mod2 <- brm(y ~ x + z1 + z2,
           family = gaussian(link = "identity"),
           data = df, # specifying data here but it won't actually be used.  
           prior = priors2,
           sample_prior = "only") # this means we will only sample the prior

#... Plotting Outcome ----

pred_val2 <- tidybayes::add_epred_draws(newdata = df, 
                                              object = mod2)

pred_plot2 <- ggplot(data = pred_val2, 
                    aes(x = .epred)) + 
  stat_halfeye(fill = "pink", 
               color = "purple",
               size = 6,
               point_interval = "median_hdi"
  ) + 
  labs(x = "Outcome",
       y = "Density") + 
  ggtitle("Predicted Outcome",
          subtitle = "Prior ~ N(0, 0.2)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    text = element_text(size = 20)
  ) 

# Plotting the actual outcome ----

actual_outcome <- ggplot(data = df, aes(x = y)) + 
  geom_density(fill = "pink") +
  labs(x = "Outcome (Y)",
       y = "Density") + 
  ggtitle("Outcome2",
          subtitle = "Prior ~ N(0,1)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    text = element_text(size = 20)
  ) 

# Now if we're okay with this, we can continue and add some data! 

# Combining Plots ----

(pred_plot1 + pred_plot2)

