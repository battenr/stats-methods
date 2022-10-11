# Title:

# Setup ----

#... Libraries ----

library(tidyverse)

#... Dependencies ----

# Data ----

# a shape value of 1 means exponential distribution

df <- data.frame(
  
)
  
  imulWeib <- function(N, lambda, rho, beta, rateC)
  {
    # covariate --> N Bernoulli trials
    x <- sample(x=c(0, 1), size=N, replace=TRUE, prob=c(0.5, 0.5))
    
    # Weibull latent event times
    v <- runif(n=N)
    Tlat <- (- log(v) / (lambda * exp(x * beta)))^(1 / rho)
    
    # censoring times
    C <- rexp(n=N, rate=rateC)
    
    # follow-up times and event indicators
    time <- pmin(Tlat, C)
    status <- as.numeric(Tlat <= C)
    
    # data set
    data.frame(id=1:N,
               time=time,
               status=status,
               x=x)
  }


# Using code I found online ----

# code from here: https://stats.stackexchange.com/questions/135124/how-to-create-a-toy-survival-time-to-event-data-with-right-censoring

library(survival)
library(tidyverse)
library(survminer)

# baseline hazard: Weibull

# N = sample size    
# lambda = scale parameter in h0()
# rho = shape parameter in h0()
# beta = fixed effect parameter
# rateC = rate parameter of the exponential distribution of C

simulWeib <- function(N, lambda, rho, beta, rateC)
{
  # covariate --> N Bernoulli trials
  x <- sample(x=c(0, 1), size=N, replace=TRUE, prob=c(0.5, 0.5))
  
  # Weibull latent event times
  v <- runif(n=N)
  Tlat <- (- log(v) / (lambda * exp(x * beta)))^(1 / rho)
  
  # censoring times
  C <- rexp(n=N, rate=rateC)
  
  # follow-up times and event indicators
  time <- pmin(Tlat, C)
  status <- as.numeric(Tlat <= C)
  
  # data set
  data.frame(id=1:N,
             time=time,
             status=status,
             x=x)
}

fit <- coxph(Surv(time, status) ~ x, data = dat)

fit

survminer::ggsurvplot(
  fit = survfit(Surv(time, status) ~ x, data = df ), 
  xlab = "Days", 
  ylab = "Overall survival probability")

set.seed(1234)
betaHat <- rep(NA, 1e3)

for(k in 1:1e3)
{
  dat <- simulWeib(N=100, lambda=0.01, rho=1, beta=-0.6, rateC=0.001)
  fit <- coxph(Surv(time, status) ~ x, data=dat)
  betaHat[k] <- fit$coef
}

mean(betaHat)
