# Title: Checking the Proportional Hazards Assumption 

# Description: Two ways to test the proportional hazards assumption from a Cox 
# PH model. One is the Grambsch-Therneau test, while another is plotting the 
# Schoenfeld residuals over time. Other methods will be for future posts

# Setup ----

#... Libraries ----

library(tidyverse) # ol' faithful
library(simsurv) # for simulating survival (TTE) data 
library(survival) # for analyzing survival data
library(survminer) # for analyzing/plotting survival data 
library(patchwork) # combining plots

# Note: it's been commented out, however there is the option to add a covariate (c1) if interested. 

# When Proportional Hazards Assumption Holds ----

set.seed(456) # setting seed for reproducibility

#... Simulating Data ----

# Specifying parameters for the simulation

n <- 500 # arbitrarily choosing sample size of 500
lambda <- 0.1 
beta <- c(trt = log(2)) #, c1 = log(1.5))  # Hazard ratio of 2 for treatment and 1.5 for covariate 1

# Covariate
covariates <- tibble(id = 1:n, 
                     trt = rbinom(n, 1, prob = 0.5)#, 
                     #c1 = rnorm(n, mean = 10, sd = 2)
)

# Simulate survival times
data_ph <- simsurv::simsurv(
  dist = "weibull", # using a Weibull distribution
  lambdas = lambda,
  gammas = 1,  # PH assumption met
  betas = beta,
  x = covariates,
  maxt = 5 # maximum time of 5 years
)

# Combine simulated survival data with covariates
data_ph <- left_join(data_ph, covariates, by = "id")

#... Fitting Cox PH Model ----

# Create a survival object

surv_ph <- survival::Surv(time = data_ph$eventtime, event = data_ph$status)

# Fit a Cox proportional hazards model
cox_ph <- survival::coxph(surv_ph ~ trt, # + c1, 
                          data = data_ph)

#... Test the proportional hazards assumption ----

ph_test_ph <- cox.zph(cox_ph)

# Show the results
print(ph_test_ph) # Grambsch-Therneau test
plot(ph_test_ph)  # Schoenfeld plots 

# When Proportional Hazards Assumption Doesn't Hold ----

#... Simulating Data ----

# Note, this is the same as above however we are adding a time-dependent 
# component. Using simsurv this is the tde and tdefunction arguments in the 
# simsurv() function. 

# Simulate survival times. 
# Note: for this case, the treatment will have non-PH however the covariate 1 
# actually will meet the PH assumption. 

data_nonph <- simsurv::simsurv(
  dist = "weibull", # Weibull distribution again
  lambdas = lambda,
  gammas = 1,  # PH assumption met
  betas = beta,
  x = covariates,
  tde = c(trt = 0.5), # specifying the time dependent function
  tdefunction = "log", # making time dependent
  maxt = 5 # maximum time of 5 years
)

# Combine simulated survival data with covariates
data_nonph <- left_join(data_nonph, covariates, by = "id")

#.. Fitting Cox PH Model ----

# Create a survival object
surv_nonph <- Surv(time = data_nonph$eventtime, event = data_nonph$status)

# Fit a Cox proportional hazards model

cox_nonph <- coxph(surv_nonph ~ trt, # + c1,
                   data = data_nonph)

# Test the proportional hazards assumption
ph_test_nonph <- cox.zph(cox_nonph)

# Print results
print(ph_test_nonph)
plot(ph_test_nonph, 
     var = "trt")

# Plots! ----

ph_met <- survminer::ggcoxdiagnostics(cox_ph, 
                                      type = "schoenfeld",
                                      ox.scale = "time") +
  ggtitle("Schoenfeld Residuals vs Time",
          subtitle = "Proportional Hazards Assumption Met") +
  labs(x = "Time") + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
    plot.subtitle = element_text(hjust = 0.5, face = "bold"),
    text = element_text(size = 16)
  ) +
  lims(y = c(-0.75, 0.75))

ph_notmet <- survminer::ggcoxdiagnostics(cox_nonph, 
                                         type = "schoenfeld",
                                         ox.scale = "time") +
  ggtitle("Schoenfeld Residuals vs Time",
          subtitle = "Proportional Hazards Assumption Not Met") +
  labs(x = "Time") + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
    plot.subtitle = element_text(hjust = 0.5, face = "bold"),
    text = element_text(size = 16)
  ) +
  lims(y = c(-0.75, 0.75))

# Combining into one plot

ph_met / ph_notmet