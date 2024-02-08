# Simulating Survival Data 

# Comparing simulating data in R using base R (rexp, etc) and using simsurv package. 
# I also used ChatGPT to help guide me, but working through the results from ChatGPT 
# myself to confirm/understand

library(tidyverse)
library(simsurv)
library(survival)
library(survminer)

# Data ----

# Using Base R 

set.seed(123) # setting seed for reproducibility

n = 362 # got it from runif(1, min = 100, max = 500)

# Setting coefficients for the variables

intercept = -2
beta.trt = 0.2
beta.c1 = 0.3

c1 = rnorm(n = n, mean = 5, sd = 2) # confounder 1 
trt = rbinom(n = n, size = 1, prob = plogis(beta.trt*c1))

linear_predictors <- intercept + beta.trt*trt + beta.c1*c1
rate <- exp(linear_predictors) # converting the linear predictors to 
time <- rexp(n = n, rate = rate) # using the expoential distribution 

# Censoring (does this depend on the rate? I think so, but not sure. Can the distribution be different?)
censoring <- rexp(n = n, rate = 0.1)

# Observed time and status (event or censored)
observed_time <- pmin(time, censoring)
status <- as.numeric(time <= censoring) # 1 if event, 0 if censored

# Kaplan-Meier curve
km <- survfit(Surv(observed_time, status) ~ 1)

# Plot the Kaplan-Meier curve
ggsurvplot(km, data = data.frame(observed_time, status), conf.int = TRUE)

# Again but stratified by treatment
km <- survfit(Surv(observed_time, status) ~ trt, data = data.frame(observed_time, status, trt))

ggsurvplot(km, data = data.frame(observed_time, status), conf.int = TRUE)

# Calculate difference in survival curves using the log-rank test
 
survdiff(Surv(observed_time, status) ~ trt, data = data.frame(observed_time, status, trt))

# Fit a model to the data
fit <- survfit(Surv(observed_time, status) ~ trt, data = data.frame(observed_time, status, trt))

adj.fit <- coxph(Surv(observed_time, status) ~ trt + c1, data = data.frame(observed_time, status, trt, c1))

broom::tidy(adj.fit)

exp(broom::tidy(adj.fit)[1,2])

broom::tidy(adj.fit)[1,2] - beta.trt
broom::tidy(adj.fit)[2,2] - beta.c1

broom::tidy(fit)

# If we don't adjust for c1, it'll show the HR is non-collapsible

unadj.fit <- coxph(Surv(observed_time, status) ~ trt, data = data.frame(observed_time, status, trt, c1))

broom::tidy(unadj.fit)[1,2] - beta.trt

exp(broom::tidy(unadj.fit)[1,2])


# Create the final dataset
df <- data.frame(
  time = observed_time,
  status = status,
  trt = trt,
  c1 = c1,
  pt = seq(1:n) # patient ID 
)

# Using IPTW ----

# loading function that I wrote
source("R/ipw_stab.R") # for now, using stabilized weights. Could also use the WeightIt package. 
# Consider, using unstabilized and see what happens 

df_wghts <- ipw_stab(data = df, treat = trt, confounder = c1) 

df_wghts |> colnames()

# Fit a model to the data ( could also check what to do if using doubly robust method)
fit <- coxph(Surv(time, status) ~ trt, 
             data = df_wghts,
             weights = weights, 
             robust = TRUE)

broom::tidy(fit)

# Bootstrapping

library(boot)

boot_coxph <- function(data, indices) {
  # Resample the data
  d <- data[indices, ]
  # Fit Cox model
  fit <- coxph(Surv(time, status) ~ trt, data = d, weights = weights)
  # Return the coefficient
  return(coef(fit))
}


boot_results <- boot(data = df, statistic = boot_coxph, R = 100)


library(survival)
library(boot)

# Assuming df is your dataframe with columns: time, status, trt, and weights

# Define the bootstrapping function for the Cox model
boot_coxph <- function(data, indices) {
  # Resample the data and corresponding weights
  d <- data[indices, ]
  
  # Fit Cox model with IPTW
  fit <- coxph(Surv(time, status) ~ trt, data = d, weights = d$weights)
  
  # Return the coefficient of interest
  return(coef(fit))
}

# Perform the bootstrapping
boot_results <- boot(data = df, statistic = boot_coxph, R = 100)

# You can view the results and calculate the standard error
print(boot_results)
boot_se <- sd(boot_results$t)

# Using BCA bootstrap

# Calculate BCa confidence interval



bca_conf_int <- boot.ci(boot_results, type = "bca")

# Print the BCa confidence interval
print(bca_conf_int)

# SE Results 
# sandwich: 0.119 (by specifying in the coxph function)
# boot: 0.109 (by bootstrappip)
# boot: 
# 

print(boot_se)













boot::boot(data = df, statistic = )

sqrt(diag(sandwich::vcovHC(fit, type = "HC3")))

?vcovHC

sandwich::vcovHC(fit)

# Use sandwich estimator for SEs with coxph
# https://www.rdocumentation.org/packages/sandwich/versions/3.0-1/topics/vcovHC




# Calculating SEs ----

# Using adjusted model, we can get the SEs for the coefficients. 
# Calculating SEs using bootstrap and then also using the sandwich package

se_mod <- function(formula, data) {
  fit <- coxph(formula, data = data)
  broom::tidy(fit)[2,2]
}

sandwich::vcovHC(adj.fit)

se_mod(Surv(time, status) ~ trt + c1, data = df)

boot::boot(data = df, statistic = se_mod, R = 1000)

boot::boot(data = df, statistic = function(data, i) {
  fit <- coxph(Surv(time, status) ~ trt + c1, data = data[i,])
  broom::tidy(fit)[2,2]
}, R = 1000)

?boot::boot()


# Using Simsurv ----

library(simsurv)

# Parameters are the same as the ones used for the base R simulation
# The lambda parameter here corresponds to the baseline hazard rate (exp(-2) in this case)

# Simulate survival data
surv_data_simsurv <- simsurv(
  lambda = exp(-2),  # Baseline hazard function
  #gamma = 0,  # Weibull shape parameter (gamma=0 for exponential)
  betas = c(trt = 0.5),  # We're excluding the intercept here because it's included in lambda
  x = covariates,
  dist = "exp"  # Specify the exponential distribution
)

# Add the censoring
surv_data_simsurv$status <- with(surv_data_simsurv, ifelse(time < censoring, 1, 0))
surv_data_simsurv$time <- with(surv_data_simsurv, pmin(time, censoring))

