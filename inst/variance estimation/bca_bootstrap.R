# Title: Bias-Corrected and Accelerated Bootstrap Interval

# Description: 

# Resource: https://stats.stackexchange.com/questions/437477/calculate-accelerated-bootstrap-interval-in-r

# Setup ----

#... Libraries ----

library(tidyverse)
library(bootstrap)

?bootstrap::bootstrap

bootstrap

# Simulated Data ----

n.obs = 756

df <- rnorm(n = n.obs, mean = 10, sd = 12)


# Bootstrapping ----

?replicate

#... Calculating by Hand ----

# Using mean 

theta <- replicate(10000, sample(df, length(df), replace = TRUE) %>% mean())

hist(theta)

quantile(df, 0.025)
quantile(df, 0.975)

mean(theta)

9.87 (-12.4 - 32.6)

bootstrap(df, 10000, theta = mean, func = function(x){quantile(x, 0.95)})


?bootstrap

#Plot bootstrap distribution
hist(theta_boot, breaks=30, xlab='theta', main='Bootstrap distribution')
abline(v=0.32, lwd=2, col='orange')

# JackKnife Sampling ----

