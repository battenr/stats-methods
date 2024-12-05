# Title: Checking the Proportional Hazards Assumption using Log-Log Plot

# Description: Showing how to check the PH assumption using the log-log plot. 

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
  gammas = 1,  
  betas = beta,
  x = covariates,
  maxt = 5 # maximum time of 5 years
)

# Combine simulated survival data with covariates
data_ph <- left_join(data_ph, 
                     covariates, 
                     by = "id")

#... Test the proportional hazards assumption ----

# Estimating the survival function

km_ph <- survival::survfit(survival::Surv(time = data_ph$eventtime, event = data_ph$status) ~ trt, 
               data = data_ph)

# Checking the plot 

survminer::ggsurvplot(km_ph, fun = "cloglog")

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
  gammas = 1,
  betas = beta,
  x = covariates,
  tde = c(trt = 0.5), # specifying the time dependent function (aka PH not met)
  tdefunction = "log", # making time dependent
  maxt = 5 # maximum time of 5 years
)

# Combine simulated survival data with covariates
data_nonph <- left_join(data_nonph, covariates, by = "id")

#... Test the proportional hazards assumption ----

# Estimating the survival function

km_nonph = survfit(survival::Surv(time = data_nonph$eventtime, event = data_nonph$status) ~ trt, 
                data = data_nonph)

survminer::ggsurvplot(km_nonph, fun = "cloglog") # checking how the plot looks.

# Plots! ----

#... PH Assumption Met ----

# Tidying results from earlier

km_data <- broom::tidy(km_ph)

# Adding the log-log transformation for survival and log of time

km_data <- km_data %>%
  mutate(log_neg_log_surv = log(-log(estimate)),
         log_time = log(time))

# Plotting! 

ph_plot <- ggplot(km_data, aes(x = log_time, y = log_neg_log_surv, color = strata)) +
  geom_step(size = 1.5) +
  labs(x = "Log(Time)", y = "Log(-Log(Survival))",
       title = "Log-Log Plot for Proportional Hazards Assumption",
       subtitle = "Assumption Met") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
    plot.subtitle = element_text(hjust = 0.5, face = "bold"),
    text = element_text(size = 16)
  ) +
  scale_color_manual(values = c("darkblue", "hotpink"))


#... PH Assumption Violated ----

# Tidying results from earlier. 

km_data_nonph <- broom::tidy(km_nonph)

# Adding the log-log transformation for survival and log of time

km_data_nonph <- km_data_nonph %>%
  mutate(log_neg_log_surv = log(-log(estimate)),
         log_time = log(time))

# Plot using ggplot2
nonph_plot <- ggplot(km_data_nonph, aes(x = log_time, y = log_neg_log_surv, color = strata)) +
  geom_step(size = 1.5) +
  labs(x = "Log(Time)", y = "Log(-Log(Survival))",
       title = "Log-Log Plot for Proportional Hazards Assumption",
       subtitle = "Assumption Violated") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
    plot.subtitle = element_text(hjust = 0.5, face = "bold"),
    text = element_text(size = 16)
  ) +
  scale_color_manual(values = c("darkblue", "hotpink"))

#... Combining into one plot ----

# This where the patchwork package is extremely useful. 

ph_plot + nonph_plot
