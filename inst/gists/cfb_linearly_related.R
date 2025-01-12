# Title: Change from Baseline 

# Description: Analyzing change from baseline requires some additional assumptions. 
# There are seven laid out in the link below from Frank Harrell
# Highly recommend reading it

# https://www.fharrell.com/post/errmed/#change-from-baseline

# This post aims to demonstrate one of those issues: 
# To use change from baseline the pre-post measurements need to be linearly related. 

# Setup ----

#... Packages ----

library(tidyverse) # ol' faithful
library(broom) # for tidying the outcome from models
library(modelsummary) # for summarizing the models and plotting them

#... Colors ----

# These colors will be used for the plots 

clrs <- c(
  "#FFBE00",  
  "#54191B"
)

#... Seed (for reproducibility) ----

set.seed(456)

# When Pre-Post Values Are Linearly Related ----

# - Binary Treatment 
# - Continuous Outcome
# - Two timepoints (baseline and followup)
# - Outcome measurement at both timepoints is linearly related 

#... Simulating Data ----

df <- data.frame(
  x = rbinom(n = 250, size = 1, prob = 0.5),
  bl = rnorm(n = 250, mean = 10, sd = 2)
) %>% 
  dplyr::mutate(
    fu = 0.75*bl + 1.5*x + rnorm(n = 250, mean = 0, sd = 1),
    chg = fu-bl
  )

#... Fitting Models ----

models <- list(
  "Change from Baseline" = glm(chg ~ x, 
                               family = gaussian(), 
                               data = df),
  "Follow-up (Adjusted for Baseline)" = glm(fu ~ x + bl, 
                                      family = gaussian(), 
                                      data = df)
)

#... Plotting Results ----

modelsummary::modelplot(models, 
                        coef_omit = c(1,3),
                        size = 0.75,
                        linewidth = 0.75) +
  scale_color_manual(values = clrs) +
  ggtitle("Two Timepoints: Baseline and Follow-up",
          "Linearly Related") + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 24),
    plot.subtitle = element_text(hjust = 0.5, size = 22),
    text = element_text(size = 22),
    axis.text.y = element_blank()
  ) + 
  lims(x = c(1, 2)) +
  labs(x = "Effect Estimate and 95% Confidence Interval",
       y = "") +
  geom_vline(xintercept = 1.5,
             linetype = "dotted"
  )


# When Not Linearly Related ----

# - Binary Treatment 
# - Continuous Outcome
# - Two timepoints (baseline and followup)
# - Outcome measurement at both timepoints are not linearly related 
#   (interaction between z1 & baseline)

#... Simulating Data ----

df <- data.frame(
  x = rbinom(n = 250, size = 1, prob = 0.5),
  z1 = rnorm(n = 250, mean = 5, sd = 2)
) %>% 
  dplyr::mutate(
    bl = rnorm(n = 250, mean = 10, sd = 2),
    fu = 2*bl + 1.5*x + z1*bl + rnorm(n = 250, mean = 0, sd = 1), # adding z1*bl as a non-linear term
    chg = fu-bl
  )

#... Fitting Models ----

models <- list(
  "Change from Baseline" = glm(chg ~ x, 
                               family = gaussian(), 
                               data = df),
  "Follow-up (Adjusted for Baseline)" = glm(fu ~ x + bl + z1*bl, 
                                            family = gaussian(), 
                                            data = df)
)

#... Plotting Results ----

# modelsummary::modelsummary(models) # can be used to check the results

modelsummary::modelplot(models, 
                        coef_omit = c(1,3, 4, 5),
                        size = 0.75,
                        linewidth = 0.75) +
  scale_color_manual(values = clrs) +
  ggtitle("Two Timepoints: Baseline and Follow-up",
          "Not Linearly Related") + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 24),
    plot.subtitle = element_text(hjust = 0.5, size = 22),
    text = element_text(size = 22),
    axis.text.y = element_blank()
  ) + 
  #lims(x = c(1, 2)) +
  labs(x = "Effect Estimate and 95% Confidence Interval",
       y = "") +
  geom_vline(xintercept = 1.5,
             linetype = "dotted"
  )

