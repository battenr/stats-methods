# Title: Simulating Data using Base R

# Learning how to simulate data using base R and this information:
# https://aosmith.rbind.io/2018/08/29/getting-started-simulating-data/#overview

# Setup ----

#... Libraries ----

library(tidyverse)

#... Dependencies ----

# Learning how to simulate data using base R and this information:
# https://aosmith.rbind.io/2018/08/29/getting-started-simulating-data/#overview

# Central Limit Theorem ----

# Seeing how data changes with increasing sample size 

# n = 10

n = 5

age <- rnorm(n = n, mean = 40, sd = 5) 
sex <- rbinom(n = n, 1, prob = 0.30)

df = data.frame(
  age, sex
)


ggplot(data= df, 
       mapping = aes(x = age)) + 
  geom_density()

# as the sample size increases, the data approximates a normal distribution

# Pulling Random Variables From Different Normal Distributions Simultaneously ----

# Sometimes this can be helpful because you want the same mean but different variance
# (like an ANOVA test)

multiple.dis <- rnorm(n = 10, mean = c(5, 25, 20), sd = 1)

# Uniform Distribution ----

# Uniform distribution if one with a minimum and a maximum but strictly positive and continuous

# (perhapsa good example to use for age, instead of )

# Poisson Distribution ----

# note: lambda is the mean

# Checking to see how increasing lambda affects graph

lambda.25 <- rpois(n = 15, lambda = 2.5) 
lambda.30 <- rpois(n = 15, lambda = 3.0)
lambda.100 <- rpois(n = 15, lambda = 100)

poisson.data <- data.frame(
  lambda.25, lambda.30, lambda.100
)

# Question: if we increased the sample size, does the CLT work for all distributions?

ggplot(data = poisson.data) + 
  geom_density(mapping = aes(x = lambda.25)) +
  geom_density(mapping = aes(x = lambda.30)) +
  geom_density(mapping = aes(x = lambda.100))

ggplot(data = poisson.data, mapping = aes(x = lambda.100)) + 
  geom_density()

# Negative Binomial Distribution ----

library(MASS)

# negative binomial is like poisson etc. not

MASS::rnegbin()

?rnegbin()

negbin <- rnegbin(n = 20, mu = 10,theta = 4) %>% as.data.frame()
negbin$x <- negbin$.

ggplot(data = negbin, mapping = aes(x = x)) + geom_density()

# Repeating Letters (for categorical variables) ----



