# Title: Demonstrating Nominal Coverage

# Description: Learning about nominal coverage helped me better understand 
# confidence intervals. The code below is meant to help illustrate that. 

# Setup ----

#... Libraries ----

library(tidyverse) # ol faithful

#... Seed ----

set.seed(456) # setting seed for reproducibility

# Simulating Data ----

n <- 250 # sample size 
beta_trt <- 1.5 # treatment effect (aka the "true" effect)

# Simulating data. We will use the following: 
# - Two confounders: one binary and one continuous
# - Binary treatment
# - Continuous outcome 

df <- data.frame(
  z1 = rbinom(n = n, size = 1, prob = 0.5), 
  z2 = rnorm(n = n, mean = 1, sd = 0.25)
) %>% 
  dplyr::mutate(
    x = rbinom(n = n, size = 1, prob = plogis(0.2*z1 + 0.2*z2)),
    y = beta_trt*x + 2*z1 + 1.5*z2 + rnorm(n = n, mean = 0, sd = 1)
  )

# Fitting a GLM! ----

# For this exercise, we're going to use a GLM. Based on the GLM, we will 
# get an estimate and SEs. From this, we can calculate our 95% CIs. 

mod <- glm(y ~ x + z1 + z2, 
           family = gaussian(), 
           data = df)

# Check for Effect ----

# We want to check to see if the effect from earlier (beta_trt) is 
# within the 95% CIs that were estimated. 

check_for_effect <- broom::tidy(mod) %>% # tidying model from earlier to make easier to read/work with
  filter(term == "x") %>% # only keeping term of interest
  select(term, estimate, std.error) %>% # selecting variables that we need
  dplyr::mutate(
    lower_95ci = estimate - 1.96*std.error, # calculating lower end of 95% CI
    upper_95ci = estimate + 1.96*std.error, # calculating upper end of 95% CI 
    contain_true_effect = isTRUE(beta_trt >= lower_95ci & beta_trt <= upper_95ci) # Is the "true" effect (beta_trt) within the 95% CI?
  ) 

# Make It a Formula! ----

# This is the same as before. The only difference is that we are going to functionalize it. 
# This is so we can run it over...and over....and OVER again!

# For comment on what's happening, see the above code. 

sim_and_check <- function(n = 250, beta_trt = 1.5){
  
  df <- data.frame(
    z1 = rbinom(n = n, size = 1, prob = 0.5), 
    z2 = rnorm(n = n, mean = 1, sd = 0.25)
  ) %>% 
    dplyr::mutate(
      x = rbinom(n = n, size = 1, prob = plogis(0.2*z1 + 0.2*z2)),
      y = beta_trt*x + 2*z1 + 1.5*z2 + rnorm(n = n, mean = 0, sd = 1)
    )
  
  mod <- glm(y ~ x + z1 + z2, 
             family = gaussian(), 
             data = df)
  
  check_for_effect <- broom::tidy(mod) %>% 
    filter(term == "x") %>% 
    select(term, estimate, std.error) %>% 
    dplyr::mutate(
      lower_95ci = estimate - 1.96*std.error, 
      upper_95ci = estimate + 1.96*std.error,
      contain_true_effect = isTRUE(beta_trt >= lower_95ci & beta_trt <= upper_95ci)
    ) %>% 
    select(
      lower_95ci, upper_95ci, contain_true_effect
    )
  
  return(check_for_effect)
  

}

# Repeat...Repeat....Repeat! ----

# Using the function we will repeat it 1000 times. 

check_1000 = replicate(1000, sim_and_check(), simplify = FALSE)

output <- do.call(rbind, check_1000) # formatting it 

# Calculate Nominal Coverage! ----

coverage <- mean(output$contain_true_effect)  # 93.2% (this isn't 95%!) but that's okay 

# Monte Carlo SE of Estimate (This comes from Morris et al. 2019)
sqrt(coverage*(1-coverage) / 1000) 

# Let's Plot a Couple! ----

# Great so we know the nominal coverage! However, we want to plot a couple. After all 
# plots are easier to comprehend. 

first_15 <- output %>% 
  slice_sample(n = 15) %>%  # randomly taking 15 of the 1000 from above
  dplyr::mutate(
    iteration_number = row_number()
  )

# Now we can finally plot! 

ggplot(first_15, mapping = aes(y = as.factor(iteration_number))) +
  geom_segment(
    aes(x = lower_95ci,xend = upper_95ci),
    color = "purple"
    ) +
  geom_vline(
    xintercept = 1.5, # this is the beta_trt (aka the "true" effect)
    linetype = "dashed", 
    color = "hotpink"
  ) +
  labs(title = "95% Confidence Intervals",
       subtitle = "15 Randomly Sampled CIs from Total of 1000",
       y = "Sample Number",
       x = "95% Confidence Intervals") +
  theme_minimal() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(size = 16)
  ) +
  lims(x = c(0.8, 2)) +
  geom_text(
    x = 1.5, 
    y = 0.6, 
    label = "True Effect"
  )
