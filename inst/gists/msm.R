# Title: Marginal Structural Models for Time-Varying Confounding

# Description: Time-varying confounding can be tricky. If it's not properly accounted 
# for the results can be misleading. 

# This code demonstrates how to fit a marginal structural model, using 
# simulated data and the WeightIt package. It also demonstrates how 
# we can get an estimate at each time point but also an overall 
# marginal effect. 

# Note: this code was written with the help of Google Gemini & ChatGPT

# Setup ----

#... Libraries ----

library(tidyverse) # ol' faithful
library(WeightIt) # for marginal structural model weights 
library(marginaleffects) # used to calculate the overall effect 
library(glmtoolbox) # to fit a GEE later for comparison/bonus fun! 
library(broom) # to tidy outputs from the model 

# Simulating Data ----

#... Pre-Sim Parameters ----

set.seed(456) # setting seed for reproducibility

n <- 250 # sample size (arbitrary)
t_max <- 3 # keeping it to three time points for simplicity

#... Setting up initial data frame

# This will change in the next portion, but here we are adding the 
# IDs (one per person), and the time. Staring with all values set to 0 
# (coffee, sleep, happiness)

df <- expand.grid(id = 1:n, 
                  time = 0:(t_max-1)) %>% 
  mutate(
    coffee = 0, 
    sleep = 0, 
    happiness = 0) %>% 
  arrange(id, time)

#... Simulate Time-Varying Confounding ----

# The idea here is to go over each time point. 

# At baseline, coffee depends on sleep (i.e., it's a confounder) since it 
# also affects the outcome (happiness). 

# Then sleep the next day is impacted by whether the person had coffee or not. 
# This creates a feedback loop, where sleep impacts whether the person has coffee
# which affects sleep. This is time-varying confounding. 

for (t in 0:(t_max-1)) {
  curr <- which(df$time == t)
  if (t == 0) {
    # 1. Baseline Sleep
    df$sleep[curr] <- rnorm(n, 7, 1)
  } else {
    prev <- which(df$time == t - 1)
    # 1. Sleep today is influenced by yesterdays coffee. Note: sleep today means 
    # the measurement of the night sleep. So say 8 hours sleep on Monday is really 
    # from Sunday night - Monday morning sleep. 
    
    df$sleep[curr] <- rnorm(n, 0.5 * df$coffee[prev] + 4, 0.8)
  }
  
  # 2. Coffee is influenced by current Sleep (Confounding)
  prob <- plogis(2 - 0.4 * df$sleep[curr])
  
  df$coffee[curr] <- rbinom(n, 1, prob)
  
  # 3. Happiness is caused by Coffee and Sleep
  # Goal coefficient for coffee: 1.5
  # Note: the effect of coffee doesn't change over time. 1.5 is just 1.5 irregardless, but as 
  # you will see when we fit the model, this can be tricky. 
  
  df$happiness[curr] <- 2 + 1.5 * df$coffee[curr] + 0.8 * df$sleep[curr] + rnorm(n, 0, 0.5)
}


# Reshaping the Data ----

# Converting the format from long to wide. This is done so that it can be used 
# in the weightitMSM() function from the WeightIt R package. 

df_wide <- df %>%
  pivot_wider(
    id_cols = id,
    names_from = time,
    values_from = c(coffee, sleep, happiness),
    names_sep = "_"
  )

# Fitting a Marginal Structural Model (MSM) ----

WMSM <- weightitMSM(
  formula = list(
    coffee_0 ~ sleep_0, # for baseline
    coffee_1 ~ coffee_0 + sleep_0 + sleep_1, # for future timepoints
    coffee_2 ~ coffee_1 + sleep_1 + coffee_0 + sleep_0 + sleep_2
  ),
  data = df_wide,
  method = "ps",
  stabilize = TRUE
)

# Checking for balance

# Here the same rules apply as for IPW: we should expect there to be 
# balance in the covariates 

cobalt::love.plot(WMSM)

# Can use plots as well, for example, sleep at baseline

cobalt::bal.plot(WMSM, which = "both", var.name = "sleep_0")

# Overall Summary of the Weights 

# if we get really extreme weights, then something is off. Here you will notice 
# they are reasonable (0.5 min to ~1.8)

summary(WMSM) # overall summary.

# Fitting the Outcome Model ----

# Here: we are only including happinness at the last timepoint in our outcome model. 
# Let's see what the result is 

fit_msm <- glm_weightit(
  happiness_2 ~ coffee_0 + coffee_1 + coffee_2,
  data = df_wide,
  weightit = WMSM
)

# Here is where we need to carefully interpret the result. 

# So we are using happiness at the end of the study (timepoint 2). 
# We would expect coffee_2 to be the value when we generated our data 
# (reminder: that is 1.5) 

# Spoiler: it is. 

# However, if you want to check whether this is true for other timepoints, 
# try it yourself! Change happiness_2 to happiness_1 see how coffee_1 changes. 
# Is it 1.5? 

fit_msm %>% summary()

# Overall Effect 

# This is great to have the value at each timepoint, but what we want is 
# an overall effect. To do this we can use the marginaleffects package 

marginaleffects::avg_comparisons(fit_msm, 
                variables = list(coffee_0 = 0:1, 
                                 coffee_1 = 0:1, 
                                 coffee_2 = 0:1),
                cross = TRUE) 

# Look at the estimate - this is different from 1.5! 
# This is expected because it's not just the effect of coffee at a point in time. 
# It's the cumulative effect of coffee on happiness

# Weight at Baseline Only ----

# So we know what's right. What about a method that we know is wrong? 

# Let's try using just weights at baseline. 

# Filtering to baseline only 

df_bl <- df %>% 
  filter(time == 0) 

# Using same causal estimand. Estimating weights 

iptw <- WeightIt::weightit(coffee ~ sleep, 
                           data = df_bl, 
                           method = "glm", 
                           estimand = "ATE")

# Adding weights to the data 

df_bl$weights <- iptw$weights

# Making an analysis dataset, where the weights are only accounting for baseline. 

df_analysis <- df %>% 
  left_join(df_bl %>% select(id, weights), 
            by = "id")

# Fit a model. Here using the glm_weightit() function since we did earlier as well. 

mod <- glm_weightit(
  happiness ~ coffee, # research question is effect of coffee on happiness after 3 days
  data = df_analysis,
  weights = weights
)

mod %>% broom::tidy(conf.int = TRUE) # as you can see this isn't the same result! 

# Conclusion: need to adjust for time-varying confounding accordingly. 

# Bonus! ----

# If you're unsure about the glm_weightit() function for longitudinal data, then 
# here's an alternative: 
# use the glmtoolbox package with geeglm() and the same weights. 
# The result will be the same 

# Note: this isn't surprising but helpful to double check! (I had to) 

glmtoolbox::glmgee(
  happiness ~ coffee, 
  data = df_analysis, 
  weights = weights
) %>% summary()

# What the "Truth" Is ----

# Here we need to simulate counterfactuals to see if our estimate is actually correct

# set.seed(456) as earlier 

simulate_truth <- function(n, always_coffee = 0) {
  
  t_max <- 3
  df_cf <- data.frame(
    sleep_0 = rnorm(n, 7, 1)
  )
  
  # Force treatment regime
  coffee_0 <- rep(always_coffee, n)
  sleep_1  <- rnorm(n, 0.5 * coffee_0 + 4, 0.8)
  coffee_1 <- rep(always_coffee, n)
  sleep_2  <- rnorm(n, 0.5 * coffee_1 + 4, 0.8)
  coffee_2 <- rep(always_coffee, n)
  
  happiness_2 <- 2 +
    1.5 * coffee_2 +
    0.8 * sleep_2 +
    rnorm(n, 0, 0.5)
  
  mean(happiness_2)
}

mean1 <- simulate_truth(100000, always_coffee = 1) # average effect when coffee is 1 
mean0 <- simulate_truth(100000, always_coffee = 0) # average effect when coffee is 0

true_effect <- mean1 - mean0 # average treatment effect
true_effect
