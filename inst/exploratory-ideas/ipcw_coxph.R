library(simsurv)
library(survival)
library(tidyverse)

# Details 

# - Need to simulate different datasets to compare performance of methods
# - Compare bias vs 0 for each method. 
# - Hypothesis is that pre-IPCW, there will be bias

cov <- data.frame(
  id = 1:200, 
  trt = rbinom(200, 1, 0.5)
) 

dat <- simsurv::simsurv(dist = "exponential", 
                        lambdas = 0.1, 
                        betas = c(trt = -0.5), 
                        x = cov, 
                        maxt = 5)

df <- dat %>% 
  dplyr::left_join(
    cov
  )

mod <- survival::coxph(Surv(eventtime, status) ~ trt, 
                 data = df)

mod$coefficients[["trt"]]



sim_run <- function() {
  # Create a data frame with the subject IDs and treatment covariate
  cov <- data.frame(id = 1:200,
                    trt = rbinom(200, 1, 0.5),
                    x1 = rnorm(n = 200, mean = 5, sd = 2))
  
  # Simulate the event times
  dat <- simsurv::simsurv(dist = "exponential", 
                          lambdas = 0.1, 
                          betas = c(trt = -0.5, x1 = -0.3), 
                          x = cov, 
                          maxt = 5)
  
  # Merge the simulated event times onto covariate data frame
  dat <- merge(cov, dat)
  
  # Fit a Weibull proportional hazards model
  mod <- survival::coxph(Surv(eventtime, status) ~ trt, 
                         data = dat)
  
  # Obtain estimates, standard errors and 95% CI limits
  est <- mod$coefficients[["trt"]]
  ses <- sqrt(mod$var)
  cil <- est + qnorm(.025) * ses
  ciu <- est + qnorm(.975) * ses
  
  # Return bias and coverage indicator for treatment effect
  c(bias = est - (-0.5), 
    coverage = ((-0.5 > cil) && (-0.5 < ciu)))
}

# Set seed for simulations
set.seed(908070)

# Perform 100 replicates in simulation study
rowMeans(replicate(100, sim_run()))

# least to bias of 0.02223858 and coverage of 0.9300

# IPCW ----------





sim_run <- function() {
  # Create a data frame with the subject IDs and treatment covariate
  cov <- data.frame(id = 1:200,
                    trt = rbinom(200, 1, 0.5),
                    x1 = rnorm(n = 200, mean = 5, sd = 2))
  
  # Simulate the event times
  dat <- simsurv::simsurv(dist = "exponential", 
                          lambdas = 0.1, 
                          betas = c(trt = -0.5, x1 = -0.3), 
                          x = cov, 
                          maxt = 5)
  
  # Merge the simulated event times onto covariate data frame
  dat <- merge(cov, dat)
  
  denom.cens <- glm(status ~ trt + x1, 
                    family = binomial(link = "logit"), 
                    data = dat)
  
  
  pd.cens <- 1-predict(denom.cens, type = "response")
  
  # estimation of numerator of ip weights for C
  numer.cens <- glm(status ~ trt, family = binomial(), data = dat)
  summary(numer.cens)
  pn.cens <- 1-predict(numer.cens, type = "response")
  
  dat$sw.c <- pn.cens/pd.cens
  
  # Fit a Weibull proportional hazards model
  mod <- survival::coxph(Surv(eventtime, status) ~ trt, 
                         data = dat,
                         weights = sw.c)
  
  # Obtain estimates, standard errors and 95% CI limits
  est <- mod$coefficients[["trt"]]
  ses <- sqrt(mod$var)
  cil <- est + qnorm(.025) * ses
  ciu <- est + qnorm(.975) * ses
  
  # Return bias and coverage indicator for treatment effect
  c(bias = est - (-0.5), 
    coverage = ((-0.5 > cil) && (-0.5 < ciu)))
}

rowMeans(replicate(100, sim_run()))

