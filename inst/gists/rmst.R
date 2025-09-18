# Title: Restricted Mean Survival Time 

# Description: When using time-to-event outcomes, a common effect measure is 
# the hazard ratio. For causal inference, there are several problems with this measure
# including selection bias, and a strange interpretability. 

# Luckily there are alternatives! 

# One such alternative is the restricted mean survival time. This code demonstrates 
# the RMST. 

# Setup ----

#... Libraries ----

library(tidyverse) # ol' faithful
library(simsurv) # for simulating survival data
library(simtrial) # in this case, to calculate the RMST 
library(surminer) # for plots 
library(survival) # fitting survival models (i.e., the Surv() function)
library(patchwork) # combining plots 

# Simulating Data ----

set.seed(456) # setting seed for reproducibility 

n = 250 # setting sample size, arbitrarily 

#... Covariates ----

# Adding two covariates z1 & z2, plus a binary treatment 
# This is used in the simsurv function from the simsurv package

covars <- data.frame(
  id = 1:n,
  z1 = rnorm(n = n, mean = 5, sd = 2),
  z2 = rbinom(n = n, size = 1, prob = 0.5),
  trt = rbinom(n = n, size = 1, prob = 0.6)
)

#... Simulating Survival Data ----

# Using a Weibull distribution 

# Generate data with a covariate and censoring
sim_data <- simsurv(
  n = n,
  dist = "weibull", # Weibull distribution for survival times
  lambdas = 0.05, # Scale parameter for Weibull distribution
  gammas = 1.5, # Shape parameter for Weibull distribution 
  x = covars, # the covariates from earlier (object named covars)
  beta = c(trt = -0.5, z1 = -0.2, z2 = 0.3), # effects of each 
  censor = 0.2, # Censoring rate (20% censored)
  maxt = 24 # maximum time of 24
) 

#... Joining Together ----

# Combining the covariates and the survival dataframes 

df <- sim_data %>% 
  full_join(covars, by = "id")

# Survival Plot -----

# Fitting a model to create survival curves 

fit <- survfit(Surv(eventtime, status) ~ trt, 
               data = df)

# Creating curves and formatting 

survminer::ggsurvplot(
  fit,
  data = df, 
  risk.table = FALSE, 
  palette = c("pink", "purple"),
  conf.int = FALSE,
  font.main = c(24, "bold"), 
  linetype = ,
  font.x = c(24, "bold"), 
  font.y = c(24, "bold"),
  legend.title = "Group",
  legend.labs = c("No Coffee", "Coffee"),
  xlab = "Hours", 
  ylab = "Happiness Probability",
  ggtheme = theme(
    panel.background = element_blank(),     # Remove panel background
    plot.background = element_blank(),      # Remove full plot background
    panel.grid.major = element_blank(),     # Remove major grid lines
    panel.grid.minor = element_blank(),     # Remove minor grid lines
    axis.line = element_line(color = "black"),  # Keep axis lines
    axis.ticks = element_line(color = "black"),  # Keep axis ticks
    legend.text = element_text(size = 24, face = "bold"),
    legend.title = element_text(size = 24, face = "bold"),
    axis.text = element_text(size = 24)
  )
)


# Calculating the Difference in Restricted Mean Survival Time ----

rmst_result <- simtrial::rmst(data = df, 
               tau = 16, # this is the restricted time (in this case 16 hours)
               formula = Surv(eventtime, status) ~ trt, 
               reference = "0")

rmst_result$estimate
rmst_result$se




