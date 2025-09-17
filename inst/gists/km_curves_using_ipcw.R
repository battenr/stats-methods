# Title: IPCW for KM Curves

# Description: Demonstrating how using inverse probability of censoring weights for 
# Kaplan-Meier curves can be helpful in reducing bias

# Shoutout to Joy Shi, Sean McGrath and Tom Palmer for making the code for 
# What If: Causal Inference by Hernan and Robins freely available. 
# The calculation for censoring weight is based on that. Highly recommend 
# reading that book if you're interested in learning more. 

# Setup ----

#... Libraries ----

library(tidyverse) # ol faithful
library(simsurv) # simulating survival data 
library(survival) # package for survival analysis
library(survminer) # useful for plotting results from survival analysis 

#... Custom Theme -----

# Setting a custom theme can be extremely useful. 
# Here we are doing this because later we will want to use a theme when creating the 
# KM curves. 

custom_theme <- function() {
  theme_survminer() %+replace% # basing this on the theme_survminer() but editing some of the components
    theme(
      plot.title=element_text(hjust=0.5, face = "bold", size = 20)
    )
}

# Simulating Data ----

set.seed(456) # seed for reproducibility

n = 250 # setting the sample size. Arbitrarily chose 250

# Defining the baseline hazard function parameters
baseline <- list(shape = 1.5, scale = 0.001) 

#... Covariates ----

# Keeping covariates to a minimum for this example: 
# ID - patient ID
# age - age of the patient
# treatment - binary 

covariates <- data.frame(
  id = 1:n, # a participant ID
  age = rnorm(n, mean = 50, sd = 10), # mean of 50 for age with SD of 10
  treatment = rbinom(n = n, size = 1, prob = 0.5), # binary treatment with probability of 0.5
  sex = rbinom(n = n, size = 1, prob = 0.65) 
)

#... Simulating Survival Data ----

# Using the simsurv function to simulate the data based on the above 
# baseline values and the covariates. 

# Simulate survival data
sim_data <- simsurv(
  lambdas = baseline$scale, # scale (from earlier)
  gammas = baseline$shape, # shape (from earlier)
  x = covariates, # covariates from earlier 
  maxt = 365, # 1 year so 365 days
  idvar = "id",
  ids = covariates$id, # subject ID
  betas = c(treatment = -0.5, age = -0.005, sex = -0.05) # the treatment effect here would be a HR of 0.60. So log(0.6) = -0.5
)

# Generating Censoring ----

# There are two ways to do this. One is to use censoring times and if the 
# event happens before the censoring time then the patient isn't censored. Another way 
# is to determine the probability of being censored (this is what's being done below)

# Setting this up, so that censoring depends on treatment and sex. 

sim_data$censoring <- rbinom(n = n, size = 1, prob = plogis(1*covariates$treatment + 1*covariates$sex))

sim_data <- sim_data %>% 
  dplyr::rename(
    death = status, # renaming this from earlier. so that 1 means death 0 = censored
  ) %>% 
  mutate(
    status = case_when(
      death == 1 & censoring == 0 ~ 1, 
      death == 1 & censoring == 1 ~ 0, # then censored (aka not dead because they're censored)
      death == 0 ~ 0 
    )
  )

# Merging Censoring with Simulated Data ----

# Combining the data.
# Note: this isn't great practice to use the same object on both sides. Ideally
# should be named something new/different. 

sim_data <- sim_data %>% 
  left_join(covariates, by = "id")

# Unweighted KM Curve ----

# Fit the survival model
fit <- survfit(Surv(eventtime, status) ~ treatment, 
               data = sim_data)

# Plot the KM curve
km_plot <- ggsurvplot(
  fit,
  data = sim_data,
  pval = FALSE, 
  conf.int = FALSE,
  risk.table = FALSE,
  risk.table.col = "strata",
  xlab = "Time (days)",
  ylab = "Survival probability",
  legend.labs = c("Control", "Treatment"),
  palette = c("#E7B800", "#2E9FDF"),
  title = "Unweighted Analysis",
  ggtheme = custom_theme() # this was theme we created earlier
) 

print(km_plot)

# Weighted (IPCW) KM Curve ----

#... Estimating Weights ----

# Fit a logistic regression model to estimate the probability of censoring
# For this analysis assuming it's based on treatment and age. 

# First fitting a model that will be the denominator of the model 

denom_censor_model <- glm(censoring ~ treatment + sex, 
                          family = binomial, 
                          data = sim_data)

# Fitting a model that will be the numerator of the model

num_censor_model <- glm(censoring ~ treatment, 
                        family = binomial, 
                        data = sim_data)


# Calculating the IPCW. Note for the purpose of this code, we are only using IPCW. 
# However, these are typically used with IPTWs as well. In that case you can calculate the 
# weight as 

# weight from iptw * weight from ipcw

sim_data <- sim_data %>% 
  dplyr::mutate(
    num_ipcw = 1 - predict(num_censor_model, type = "response"),
    denom_ipcw = 1 - predict(denom_censor_model, type = "response"),
    ipcw = num_ipcw/denom_ipcw
  )

#... Fitting Model with Weights ----

fit_ipcw <- survfit(Surv(sim_data$eventtime, sim_data$status) ~ treatment, 
                    data = sim_data, 
                    weights = sim_data$ipcw)

#... Weighted KM Curve ----

# Plot the IPCW-adjusted KM curve
ipcw_plot <- ggsurvplot(
  fit_ipcw,
  data = sim_data,
  pval = FALSE, 
  conf.int = FALSE,
  risk.table = FALSE,
  risk.table.col = "strata",
  ggtheme = custom_theme(),
  xlab = "Time (days)",
  ylab = "Survival probability (IPCW adjusted)",
  legend.labs = c("Control", "Treatment"),
  palette = c("#E7B800", "#2E9FDF"),
  title = "Weighted Using IPCW"
)

# Both Plots ----

km_plot
ipcw_plot

# Bonus! Difference in Effect Esimates ----

# If you're unsure about whether IPCW does anything, let's try estimating
# the treatment effect using weights vs no weights. 

cox_ipcw <- coxph(Surv(eventtime, status) ~ treatment, 
                  data = sim_data, 
                  weights = ipcw)

broom::tidy(cox_ipcw)

cox_fit <- coxph(Surv(eventtime, status) ~ treatment, 
                 data = sim_data)

broom::tidy(cox_fit)

# This shows the difference and why we should use IPCW!