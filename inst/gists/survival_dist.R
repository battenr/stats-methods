# Title: Different Methods of Modelling Survival Data 

# Description: I used to think Cox PH was the only way to model survival data. 
# There are a wide variety of methods than can be used, however 
# we need to be careful about what assumptions we are making. 

# The goal of this code is to demonstrate how survival curves can look 
# different depending on different methods. 

# This code simulates data based on a Weibull distribution, then 
# attempts to fit different models to the data, highlighting how 
# different conclusions can be drawn (varying degrees of incorrectness)

# Note: AI (ChatGPT) assisted in generating this code. I had idea, reviewed and 
# edited resulting code. 

# Setup ----

#... Packages ----

library(tidyverse) # ol' faithful
library(survival) # for survival analysis
library(flexsurv) # for flexible parametric survival modelling 

# Simulating Data ----

set.seed(456)

n <- 250 # sample size of 250, arbitrarily chosen

# Binary treatment. For purposes of this code, focusing on different distributions 
# so did not include any confounders in simulating the data . 

treatment <- rbinom(n = n, size = 1, prob = 0.5) 

#... Specifics of the Distribution ----

# Using a Weibull distribution to simulate the data. Both groups will have the 
# same shape parameter, however different scale parameters. This should make their 
# survival curves look different. 

# Note: these were chosen arbitrarily. 

shape <- 1.5
scale_control <- 2
scale_treatment <- 3  # Longer survival for treatment group

# Simulating the surival time

surv_time <- ifelse(treatment == 0,
                    rweibull(n, shape = shape, scale = scale_control),
                    rweibull(n, shape = shape, scale = scale_treatment))

#... Censoring ----

# Time-to-event data typically has censoring. While there is no confounding in 
# this example, it seemed odd to not have censoring. Here the censoring is random
# (so no need to account for it through IPCW, etc)

# Random censoring times
censor_time <- runif(n, min = 0, max = 5)

# Determining if a patient is censored or not. Taking the smaller of the 
# two values: survival time & censor time (i.e., if patient is censored BEFORE 
# the survival time)

time <- pmin(surv_time, censor_time)
status <- as.numeric(surv_time <= censor_time)

#... Combining into a dataframe ----

surv_data <- data.frame(time = time, status = status, treatment = factor(treatment))

# Creating a Survival Object ----

# Formatting the data as a survival object using the Surv function

surv_obj <- survival::Surv(time = surv_data$time, event = surv_data$status)

# Fitting Parametric Models ----

# These were created in a list, each using flexsurvreg from the flexsurv package. 
# This allows for fitting based on different distributions. The list will make it 
# useful later when predicting survival times. 

fit_models <- list(
  Exponential = flexsurvreg(surv_obj ~ treatment, data = surv_data, dist = "exp"),
  Weibull = flexsurvreg(surv_obj ~ treatment, data = surv_data, dist = "weibull"),
  LogLogistic = flexsurvreg(surv_obj ~ treatment, data = surv_data, dist = "llogis"),
  LogNormal = flexsurvreg(surv_obj ~ treatment, data = surv_data, dist = "lnorm"),
  Gamma = flexsurvreg(surv_obj ~ treatment, data = surv_data, dist = "gamma")
)

# Survival Curves - Estimating Survival Curves ----

# To estimate the survival curve, we'll setup aa dataset for each of the possible times. 

time_seq <- seq(0, max(time), length.out = 200)

# This function predicts survival times using each of the models. 

surv_curves <- lapply(names(fit_models), function(model_name) {
  fit <- fit_models[[model_name]]
  do.call(rbind, lapply(c(0, 1), function(g) {
    preds <- summary(fit, t = time_seq, newdata = data.frame(treatment = as.factor(g)))
    data.frame(
      time = time_seq,
      surv = preds[[1]]$est,
      type = model_name,
      treatment = as.factor(g)
    )
  }))
}) %>% bind_rows()

# Kaplan-Meier Estimate ----

# KM is different so have to use a different approach, since it is a non-parametric
# estimator. 

km_fit <- survfit(Surv(time, status) ~ treatment, data = surv_data)

km_df <- data.frame(
  time = km_fit$time,
  surv = km_fit$surv,
  treatment = rep(levels(surv_data$treatment), km_fit$strata),
  type = "None (Kaplan-Meier)"
)

# Combining Results from Parametric Methods and Kaplan-Meier ----

all_surv_curves <- bind_rows(surv_curves, km_df)

# Plotting Curves ----

# Plotting all the survival curves. The point of this plot is to highlight
# how if we make different assumptions about the distributions, it could 
# lead to substanitally different conclusions. 

treatment_labeller <- function(variable, value) {
  paste("Treatment Status:", value)
}

ggplot(all_surv_curves, aes(x = time, y = surv, color = type, linetype = type)) +
  geom_line(size = 1.5) +
  facet_wrap(~ treatment, labeller = treatment_labeller) +
  labs(title = "Survival Curves by Treatment Status",
       subtitle = "True Distribution: Weibull",
       x = "Time", y = "Survival Probability",
       color = "Assumed Distribution", 
       linetype = "Assumed Distribution") +
  theme_minimal() +
  scale_color_manual(values = c(
    "None (Kaplan-Meier)" = "black",
    "Exponential" = "red",
    "Weibull" = "green",
    "LogLogistic" = "darkblue",
    "LogNormal" = "purple",
    "Gamma" = "orange"
  )) +
  theme(legend.text = element_text(size = 16),
        text = element_text(size = 24),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5)
  )


