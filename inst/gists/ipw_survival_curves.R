# Title: Estimating Survival Curves Using IP Weights

# Description: Demonstrating how we can create survival curves with inverse 
# probability weights for causal inference. 

# Notes: Used the book Causal Inference: What If by Hernan & Robins and accompanying 
# R code for guidance. Thanks to everyone involved for making it freely available!

# Setup ----

#... Libraries ----

library(tidyverse) # ol faithful
library(simsurv) # for simulating survival data
library(splitstackshape)
library(patchwork) # for combining plots

#... Functions ----

# For simulating data with: 
# - Two confounders
# - A binary exposure 
# - A continuous outcome (not used in this example)

sim_data <- function(n = 250, # sample size 
                     beta_trt = 1.5, # treatment effect
                     # Parameters for Z1 
                     z1_mean = 5, z1_sd = 2, 
                     # Parameters for Z2
                     z2_size = 1, z2_prob = 0.5, 
                     # Confounder - Effect on X
                     z1_on_x = 0.05, z2_on_x = 0.2,
                     # Confounder - Effect on Y
                     z1_on_y = 0.5, z2_on_y = 0.3){
  
  # Creating the Dataframe
  
  df <- data.frame(
    z1 = rnorm(n = n, mean = z1_mean, sd = z1_sd), 
    z2 = rbinom(n = n, size = z2_size, prob = z2_prob)
  ) %>% 
    dplyr::mutate(
      prob = plogis(z1_on_x*z1 + z2_on_x*z2), 
      x = rbinom(n = n, size = 1, prob = prob), 
      y = beta_trt*x + z1_on_y*z1 + z2_on_y*z2 + rnorm(n = n, mean = 0, sd = 1),
      id = 1:n
    )
  
  # Return the dataframe
  
  return(df)
}


# Simulating Data ----

set.seed(456) # setting seed for reproducibility 

n = 250 # arbitrary sample size of 250 

#... Covariates and Treatment ----

sim_data <- sim_data() # function from earlier 

#... Survival Times ----

# Simulating survival times using the simsurv R package

sim_surv <- simsurv(
  dist = "exponential", # using an exponential distribution
  lambdas = 0.05, 
  x = sim_data, # our dataframe from earlier 
  betas = c(x = -0.5, z1 = 0.03, z2 = 0.2),
  maxt = 10 # 10 days in this case 
)

#... Combining Survival Times & Covariates ----

sim_data <- left_join(sim_data, sim_surv, by = "id")

#... Dependent Censoring ----

# For a lot of common survival analysis methods, it's assumed that censoring is 
# independent. For this simulation, we will include information so that it is not. 

sim_data <- sim_data %>%
  mutate(
    # Generate event times
    eventtime = rexp(n, rate = 0.05),
    
    # Simulate censoring times based on the censoring probabilities
    censor_time = rexp(n, rate = exp(-0.2 * x - 0.3 * z1 - 0.5 * z2)),
    
    # The observed time will be the minimum of the eventtime and censor_time
    time = pmin(eventtime, censor_time),
    
    # If the event time is less than or equal to the censor time, status = 1 (event observed), else 0 (censored)
    status = as.integer(eventtime <= censor_time),
    
    # Generate the censor variable (1 = censored, 0 = not censored)
    censor = ifelse(status == 0, 1, 0)
  )

#... Estimate IPTWs ----

# For this, estimating the average treatment effect (ATE)

model_denom <- glm(x ~ z1 + z2, family = binomial(), data = sim_data) # model for denominator
model_numer <- glm(x ~ 1, family = binomial(), data = sim_data) # model for numerator

# Using PS models to calculate the weights

sim_data <- sim_data %>%
  mutate(
    p_denom = predict(model_denom, type = "response"),
    p_numer = predict(model_numer, type = "response"),
    iptw = if_else(x == 1, p_numer / p_denom, (1 - p_numer) / (1 - p_denom))
  )

#... Estimate IPCWs ----

# Following similar process as for IPTW, however this time the outcome is censored 
# (1 = censored, 0 = not censored)

model_censor_denom <- glm(censor ~ z1 + z2 + x, family = binomial(), data = sim_data) # model for denominator
model_censor_numer <- glm(censor ~ 1, family = binomial(), data = sim_data) # model for numerator

# Calculating IPCWs based on above models

sim_data <- sim_data %>%
  mutate(
    pC_denom = 1 - predict(model_censor_denom, type = "response"), # 1- because it's 1 - probability of being censored
    pC_numer = 1 - predict(model_censor_numer, type = "response"),
    ipcw = pC_numer/pC_denom
  )

#... Calculate Combined Weight ----

# sw for stabilized weights
# SW = IPTW * IPCW

sim_data <- sim_data %>%
  mutate(sw = iptw * ipcw)

# Expanding Data to Person-Time ----

# For this we are creating a larger dataframe for each person-time

sim_data_long <- splitstackshape::expandRows(sim_data, "time", drop = FALSE) %>% 
  mutate(
    timesq = time^2,
    event = ifelse(eventtime <= censor_time, 1, 0)
  )

# Fit Weighted Logistic Regression Model (discrete-time hazard model) ----

# This is based on Ch 12 & 17 of Hernan & Robins What If 

ipw_model <- glm(event == 0 ~ x + I(x*time) + I(x*timesq) + time + timesq,
                 family = binomial(),
                 weights = sw,
                 data = sim_data_long)

# Predict Survival Probabilities ----

#... Data for when x = 0 ----

new_data_treat0 <- tibble(
  time = seq(0, 10),
  timesq = time^2,
  x = 0
)

#... Data for when x = 1 ----

new_data_treat1 <- tibble(
  time = seq(0, 10),
  timesq = time^2,
  x = 1
)

#... Predicting Survival Probabilites ----

new_data_treat0 <- new_data_treat0 %>%
  mutate(p_noevent = predict(ipw_model, newdata = ., type = "response"),
         surv = case_when(
           time == 0 ~ 1, # when time is 0, the probability should be 1 since no events have happened yet
           time != 0 ~ cumprod(p_noevent)
         ))

new_data_treat1 <- new_data_treat1 %>%
  mutate(p_noevent = predict(ipw_model, newdata = ., type = "response"),
         surv = case_when(
           time == 0 ~ 1, 
           time != 0 ~ cumprod(p_noevent)
         ))

# Combining Data ----

surv_plot_data <- tibble(
  time = 0:10,
  surv0 = new_data_treat0$surv,
  surv1 = new_data_treat1$surv
)

# Plotting Time! ----

p.weights <- ggplot(surv_plot_data, aes(x = time)) +
  geom_line(aes(y = surv0, color = "Coffee"), linewidth = 1) +
  geom_line(aes(y = surv1, color = "No Coffee"), linewidth = 1) +
  labs(title = "Adjusted Survival Curves",
       subtitle = "Using IPCW & IPTW",
       x = "Time (hours)",
       y = "Alertness Probability",
       color = "Group") +
  theme_minimal() + 
  ylim(c(0, 1)) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    text = element_text(size = 20)
  ) 

# Now, What Happens if We Don't Use Weights? ----

# Applying the exact same methodology, but with no adjustment for weights 

#... Fit weighted logistic regression (discrete-time hazard model) ----

ipw_model <- glm(event == 0 ~ x + I(x*time) + I(x*timesq) + time + timesq,
                 family = binomial(),
                 data = sim_data_long)

#... Creating Dataframe Needed for Prediction ----

new_data_treat0 <- tibble(
  time = seq(0, 10),
  timesq = time^2,
  x = 0
)

new_data_treat1 <- tibble(
  time = seq(0, 10),
  timesq = time^2,
  x = 1
)

#... Predict survival probabilites ----

new_data_treat0 <- new_data_treat0 %>%
  mutate(p_noevent = predict(ipw_model, newdata = ., type = "response"),
         surv = case_when(
           time == 0 ~ 1, 
           time != 0 ~ cumprod(p_noevent)
         ))

new_data_treat1 <- new_data_treat1 %>%
  mutate(p_noevent = predict(ipw_model, newdata = ., type = "response"),
         surv = case_when(
           time == 0 ~ 1, 
           time != 0 ~ cumprod(p_noevent)
         ))

#... Combining Data ----

surv_plot_data <- tibble(
  time = 0:10,
  surv0 = new_data_treat0$surv,
  surv1 = new_data_treat1$surv
)

#... Plotting Time x 2! ----

p.noweights <- ggplot(surv_plot_data, aes(x = time)) +
  geom_line(aes(y = surv0, color = "Coffee"), linewidth = 1) +
  geom_line(aes(y = surv1, color = "No Coffee"), linewidth = 1) +
  labs(title = "Unadjusted Survival Curves",
       subtitle = "",
       x = "Time (hours)",
       y = "Alertness Probability",
       color = "Group") +
  theme_minimal() + 
  ylim(c(0, 1)) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    text = element_text(size = 20)
  ) 

# Combine Plots Together ----

p.noweights + p.weights

