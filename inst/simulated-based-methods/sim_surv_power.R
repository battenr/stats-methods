# Title: Simulated power analysis for survival data 

# Description: Calculating power through simulation for survival analysis

# Notes: 

# Credit to Tomas Bencomo for inspiration/ideas at link here: 
# https://discourse.datamethods.org/t/simulating-survival-data-to-evaluate-statistical-power-for-survival-analysis/2486

# Setup ----

#... Libraries ----

library(tidyverse)
library(survival)

# Simulating Data Function ----

simulate_data <- function(nc, nt, hr,
                          propc, propt,
                          maxtime, test = "cox", 
                          alpha = 0.05) {
  # Note: Bender et al 2005 has good info on simulating survival data 
  
  N = nc + nt # total N as sum of both arms
  covariate <- c(rep(1, nt), rep(0, nc)) # treating treatment group as only covariate
  cens <- maxtime * runif(N) # create censoring times (unrelated to survival). calculated as a proportion of max time
  beta <- log(hr) # convert hazard ratio to beta
  h <- exp(beta*covariate) # convert back to HR after multiplying by covariate
  dt <- -log(runif(N))/h # taken from above link but Tomas referred to Bender article. 
  # Bender formula for times T ~ Exponential(baseline_hazard)
  
  # e <- ifelse(dt <= cens, 1, 0)
  ndc <- round(nc*propc,0) # number of events in control group
  ndt <- round(nt*propt,0) # number of events in trial group
  nsc <- nc - ndc # number of nonevents in control group
  nst <- nt - ndt  # number of nonevents in trial group
  
  # Simulating based on proportion of event in each arm
  e <- c(rbinom(nt, size = 1, prob = propt), rbinom(nc, size = 1, prob = propc)) 
  # If you don't want to simulate 
  
  # e <- c(rep(1, ndt), rep(0, nst), rep(1, ndc), rep(0, nsc)) # if you want to not simulate data
  
  dt <- pmin(dt, cens) # taking minimum of calculated dt above or the censored time
  S <- Surv(dt, e) # can just input into formula or make S here instead 
  
  if (test == "cox"){ # if testing using cox model
    
    fit = survival::coxph(S ~ covariate) %>% summary()
  
    beta = fit$coefficients[1]
    se = fit$coefficients[3]
    test_stat = beta/se
  
    crit = abs(qnorm(alpha/2))
  
    stat_sig = abs(test_stat)>crit
  } else if (test == "logrank") { # if testing using the logrank test
    summary = survdiff(S ~ covariate)
    test_stat = summary$chisq
    df = 1
    
    pvalue = pchisq(test_stat, df = 1, lower.tail = FALSE)
    
    stat_sig = pvalue < alpha
  }  else { # if neither are picked then let use know to pick one or double check spelling
    "Please specify cox or logrank test or double check that you spelled your option correctly"
}
  
  return(data.frame(stat_sig))

}

# Speedy Check ----

# Using 100 iterations to quickly check everything works
test <- replicate(100, simulate_data(nt = 104, 
                                       nc = 300, 
                                       hr = 1.1, 
                                       propt = 0.7, 
                                       propc = 0.7,
                                       maxtime = 27,
                                       test = "logrank"), simplify = 'dataframe') %>% unlist()

mean(test)

# Testing Function by Iterating 10000 Times ----

# Note for speed ca

test <- replicate(10000, simulate_data(nt = 104, 
                                       nc = 300, 
                                       hr = 1.1, 
                                       propt = 0.7, 
                                       propc = 0.7,
                                       maxtime = 27), simplify = 'dataframe') %>% unlist()

mean(test)

# Comparing to powerSurvEpi package ----

powerSurvEpi::powerCT.default(
  nE = 104, 
  nC = 330,
  pE = 0.7,
  pC = 0.7, 
  RR = 1.1
)

