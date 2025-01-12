# Title: Simulation Study with Bayesian Models

# Description: Conducting simulation studies with a frequentist approach 
# is somewhat more straightforward. The goal of this code is to 
# help provide some code that can be used when exploring Bayesian 
# approaches 

# Library ----

library(tidyverse) # ol faithful
library(brms) # Bayesian models
library(tidybayes) # for working with bayes to make it tidier
library(bayesplot) # for plotting results from Bayesian methods (i.e., posterior distribution)

# Sample Code for Simulation ----

# We can update a model with new data (per )

n = 250 # sample size 
beta_trt = 1.5

df <- data.frame(
  z1 = rnorm(n = n, mean = 5, sd = 1), 
  z2 = rbinom(n = n, size = 1, prob = 0.5)
) %>%
  dplyr::mutate(
  prob = plogis(0.05*z1 + 0.2*z2), # this is an intermediate variable 
  x = rbinom(n = n, size = 1, prob = prob), 
  y = beta_trt*x + 0.5*z1 + 0.3*z2 + rnorm(n = n, mean = 0, sd = 1)
)

table(df$x)

# Fitting Model ----

#... Setting 

priors <- c(
  prior(normal(0, 5), class = "b", coef = "x"),    # Prior for the regression coefficients (b)
  prior(normal(0, 5), class = "Intercept"),  # Prior for the intercept
  prior(normal(0, 0.5), class = "b", coef = "z1"),
  prior(normal(0, 0.5), class = "b", coef = "z2")
)


mod <- brms::brm(y ~ x + z1 + z2, 
     data = df, 
     family = gaussian(), 
     prior = priors)

plot(mod)



dfp <- mod %>% 
  spread_draws(b_x)

ggplot(data = dfp, 
       mapping = aes(x = b_x)) + 
  stat_halfeye()

median_hdci(dfp$b_x) %>% 
  select(y, ymin, ymax) %>% 
  lapply(\(x)round(x, 2)) %>% 
  as.data.frame()

# Reusable Code ----

update_model <- function(mod){
  
  # New Data Frame. 
  # New in this sense means that it's just been rerun 
  # (aka new values were created for each )
  
  newdf <- data.frame(
    z1 = rnorm(n = n, mean = 5, sd = 1), 
    z2 = rbinom(n = n, size = 1, prob = 0.5)
  ) %>%
    dplyr::mutate(
      prob = plogis(0.05*z1 + 0.2*z2), # this is an intermediate variable 
      x = rbinom(n = n, size = 1, prob = prob), 
      y = beta_trt*x + 0.5*z1 + 0.3*z2 + rnorm(n = n, mean = 0, sd = 1)
    )
  
  #
  
  mod2 <- update(mod, newdata = newdf)
  
  # Updating the posterior 
  
  dfp2 <- mod2 %>% 
    spread_draws(b_x)
  
  results <- median_hdci(dfp2$b_x) %>% 
    select(y, ymin, ymax) %>% 
    lapply(\(x)round(x, 2)) %>% 
    as.data.frame()
  
  return(results)
  
}

update_model()

n.sim = 100 

ten_samples <- replicate(n = n.sim, expr = update_model(), simplify = FALSE)

results <- do.call(rbind, ten_samples)

dfbias <- results %>% 
  mutate(
    bias = y - beta_trt # 1.5 is "true (from earlier in the code)
  )

mean(dfbias$bias)

# Monte Carlo SE of Estimate 

sqrt(1/(n.sim*(n.sim -1)) * sum((dfbias$bias - mean(dfbias$bias))^2))

0.0018 (0.0128)

# Showing Priors and Posterior ----



