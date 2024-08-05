# Title: IPCW for KM Curves 

# Description: Demonstrating using inverse probability of censoring weighting for 
# Kaplan-Meier Curves. 

# Setup ----

#... Libraries ----

library(tidyverse) # ol faithful
library(simsurv) # simulating survival data 
library(survival) # package for survival analysis
library(survminer) # useful for plotting results from survival analysis 

# Simulating Data ----

set.seed(456) # seed for reproducibility

n = 1000 # setting the sample size. Arbitrarily chose 1000

# Defining the baseline hazard function parameters
baseline <- list(shape = 1.5, scale = 0.001) 

#... Covariates ----

# Keeping covariates to a minimum for this example: 
# ID - patient ID
# age - age of the patient
# treatment - binary 

covariates <- data.frame(
  id = 1:n,
  age = rnorm(n, mean = 50, sd = 10), # mean of 50 for age with SD of 10
  treatment = rbinom(n = n, size = 1, prob = 0.5),
  sex = rbinom(n = n, size = 1, prob = 0.65)
)

#... Simulating Survival Data ----

# Using the simsurv function to simulate the data based on the above 
# baseline values and the covariates. 

# Simulate survival data
sim_data <- simsurv(
  lambdas = baseline$scale, # scale 
  gammas = baseline$shape, # shape
  x = covariates, # covariates from earlier 
  maxt = 365, # 1 year so 365 days
  idvar = "id",
  ids = covariates$id, # subject ID
  betas = c(treatment = -0.5, age = -0.005) # the treatment effect 
)

# Generating Censoring ----

# There are two ways to do this. One is to use censoring times and if the 
# event happens before the censoring time then the patient isn't censored. Another way 
# is to determine the probability of being censored (this is what's being done)

censoring <- rbinom(n = n, size = 1, prob = 0.2*covariates$treatment + 0.3*covariates$sex)

sim_data$censoring <- censoring

sim_data <- sim_data %>% 
  mutate(
    status = ifelse(censoring == 1, 1, 0)
  )

# Merging Censoring with Simulated Data ----

# Combining the data.
# Note: this isn't great practice to use the same object on both sides. Ideally
# should be named something new/different. 

sim_data <- sim_data %>% 
  left_join(covariates, by = "id")

# Unweighted KM Curve ----

# Fit the survival model
fit <- survfit(Surv(eventtime, status) ~ treatment, data = sim_data)
cox_fit <- coxph(Surv(eventtime, status) ~ treatment, data = sim_data)

# Custom theme for KM curve

custom_theme <- function() {
  theme_survminer() %+replace%
    theme(
      plot.title=element_text(hjust=0.5, face = "bold", size = 16)
    )
}

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
  ggtheme = custom_theme() # from above
) 

print(km_plot)

# Weighted (IPCW) KM Curve ----

#... Estimating Weights ----

# Fit a logistic regression model to estimate the probability of censoring
# For this analysis assuming it's based on treatment and age. 

censoring_model <- glm(status ~ treatment + sex, 
                       family = binomial, 
                       data = sim_data)

# Calculating the IPCW. These are unstabilized here. Stabilized weights should be 
# used in practice (won't make a difference if a saturated model but good to use stabilized weights)

# Calculate inverse probability of censoring weights (IPCW)

sim_data <- sim_data %>% 
  dplyr::mutate(
    ipcw = 1 - predict(censoring_model, type = "response")
  )

#... Fitting Model with Weights ----

fit_ipcw <- survfit(Surv(sim_data$eventtime, sim_data$status) ~ treatment, 
                    data = sim_data, 
                    weights = sim_data$ipcw)
cox_ipcw <- coxph(Surv(sim_data$eventtime, sim_data$status) ~ treatment, 
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

# Model Outputs ----

broom::tidy(cox_fit) 
broom::tidy(cox_ipcw) # note don't use this for the standard error. That would need to be calculated
# since these weights are being used 