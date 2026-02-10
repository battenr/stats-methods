# Title: Clone-Censor-Weight Method 

# Description: Immortal time bias can be a problem for causal inference. 
# The typical approach is to deal with this through study design. However, sometimes
# that's not possible. In particular when using secondary data. 

# One method that can be useful is the clone-censor-weight method. The process of doing that is 
# demonstrated below. 

# Notes: 
# - This code was written with the help of Gemini
# - First time coding this method. If you find an error, please let me know

# Setup ----

#... Libraries ----

library(tidyverse) # ol faithful
library(survival) # for survival analysis
library(survminer) # for survival analysis
library(gridExtra) # for combining plots 

# Simulating Data ----

#... Setup ----

set.seed(456) # setting seed for reproducibility

n <- 250 # sample size 

#... Simulating Time-to-Event Data ----

# For this example it's time to happiness after drinking coffee. 
# For simplicity, we are going to include an additional variable sleep that 
# impacts whether coffee is drank

data <- data.frame(
  id = 1:n, # ID for the individual
  sleep_hours = rnorm(n, 7, 1.5) # Confounder: Sleep affects both coffee & happiness
) %>%
  mutate(
    # Less sleep = higher probability/sooner coffee intake
    coffee_time = rexp(n, rate = 0.5 / (sleep_hours/7)), 
    # More sleep = faster time to happiness
    happiness_time = rexp(n, rate = 0.1 * (sleep_hours/7)),
    censor_time = 10, # adding a time of censoring 
    
    # Baseline variables for the Naive approach
    status = if_else(happiness_time < censor_time, 1, 0),
    obs_time = pmin(happiness_time, censor_time),
    drank_coffee = rbinom(n = n, size = 1, prob = plogis(0.5 + 0.05*sleep_hours))
  )

# Cloning, Censoring, Weighting ----

#... Model for Patients ----

# Probability of Drinking Coffee 

# We model the probability of DRINKING coffee based on sleep
prob_model <- glm(drank_coffee ~ sleep_hours, 
                  family = binomial(link = "logit"), 
                  data = data) 

# Adding the probability of drinking coffee 

data$p_coffee <- predict(prob_model, type = "response")

#... Cloning & Censoring Patients ----

# No Coffee 

clone_no_coffee <- data %>% 
  mutate(arm = "No Coffee", 
         # If they drank coffee, they are censored at coffee_time
         ccw_status = if_else(coffee_time < obs_time, 0, status),
         ccw_time = if_else(coffee_time < obs_time, coffee_time, obs_time),
         ccw_time = ifelse(ccw_time < 0, 0, ccw_time),
         weight = 1/(1-p_coffee) # so 1/probability of not receiving coffee
         # Note: these are unstabilized weights for purpose of demonstration. Consider 
         # using stabilized weights and/or truncating weights for "real" use. 
  ) %>% 
  select(id, arm, ccw_status, ccw_time, obs_time, weight)

# Coffee 

summary(clone_coffee$ccw_time)

clone_coffee <- data %>% 
  mutate(
    arm = "Coffee",
    ccw_status = status,
    # Remove immortal time (the time they spent NOT drinking coffee)
    ccw_time = obs_time - coffee_time,
    ccw_time = ifelse(ccw_time < 0, 0, ccw_time),
    # Weight = 1 / Prob(Entering the 'Always' arm)
    weight = 1 / p_coffee
  ) %>% 
  select(id, arm, ccw_status, ccw_time, obs_time, weight)

#... Combined Datasets ----

ccw_final <- bind_rows(clone_no_coffee, clone_coffee) %>%  #, data_final) %>%
  mutate(
    weight = pmin(weight, quantile(weight, 0.95))
    ) # Truncate extreme weights

# Plots ----

#... Plot 1: Unadjusted (Biased) ----

# Model with no adjustment

fit_naive <- survfit(Surv(obs_time, status) ~ drank_coffee, 
                     data = data)

# Plotting model 

plot_unadjusted <- ggsurvplot(
  fit_naive, 
  data = data, 
  title = "Unadjusted: Coffee & Happiness",
  legend.labs = c("Coffee", "No Coffee"),
  palette = c("#FFB6C1", "#800080"),  # Light Pink and Purple
  ggtheme = theme_minimal() # Clean base theme
)$plot + 
  labs(y = "Probability of Being Unhappy") +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"), # Size 20 & Centered
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    legend.text = element_text(size = 20),
    text = element_text(size = 20)
  )

#... Plot 2: CCW Adjusted (Corrected) ----

# Model with adjustment and weights from CCW method

fit_ccw <- survfit(Surv(ccw_time, ccw_status) ~ arm, 
                   data = ccw_final, 
                   weights = weight)

# Plotting CCW method

plot_ccw <- ggsurvplot(
  fit_ccw, 
  data = ccw_final, 
  title = "CCW Adjusted: Coffee & Happiness",
  legend.labs = c("Coffee", "No Coffee"),
  palette = c("#FFB6C1", "#800080"), # Matching palette order to arms
  ggtheme = theme_minimal()
)$plot + 
  labs(y = "Probability of Being Unhappy") +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"), # Size 20 & Centered
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    legend.text = element_text(size = 20),
    text = element_text(size = 20)
  )

#... Combine Plots ----

grid.arrange(plot_unadjusted, plot_ccw, ncol = 2)

