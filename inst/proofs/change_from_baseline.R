# Title: Change from Baseline 

# Description: Analyzing change from baseline requires some assumptions. 
# There are seven laid out in this post from Frank Harrell below. 

# https://www.fharrell.com/post/errmed/#change-from-baseline

# Points from the link: 

# 1. the variable is not used as an inclusion/exclusion criterion for the study, 
# otherwise regression to the mean will be strong

# 2. if the variable is used to select patients for the study, a second post-enrollment baseline
# is measured and this baseline is the one used for all subsequent analysis

# 3. the post value must be linearly related to the pre value

# 4. the variable must be perfectly transformed so that subtraction “works” and 
# the result is not baseline-dependent

# 5. the variable must not have floor and ceiling effects

# 6. the variable must have a smooth distribution

# 7. the slope of the pre value vs. the follow-up measurement must be close to 1.0 

# This post will focus on points 3 and 5 by simulating data to prove why this is 
# problematic. 

# Setup ----

#... Packages ----

library(tidyverse) # ol' faithful
library(broom) # for tidying the outcome from models

# 3. Post-Value Must be Linearly Related to the Pre-Value ----

# the post value must be linearly related to the pre value

# For this example, including one prognostic factor

# Testing out point number 3 ----

# Only including 

#... When Pre-Post Values Are Linearly Related ----

df <- data.frame(
  x = rbinom(n = 250, size = 1, prob = 0.5),
  bl = rnorm(n = 250, mean = 10, sd = 2)
) %>% 
  dplyr::mutate(
    fu = 0.75*bl + 1.5*x + rnorm(n = 250, mean = 0, sd = 1),
    chg = fu-bl
  )

glm(chg ~ x, 
    family = gaussian(), 
    data = df) %>% 
  broom::tidy()

glm(fu ~ x + bl, 
    family = gaussian(), 
    data = df) %>% 
  broom::tidy()


#... When Not Linearly Related ----


df <- data.frame(
  x = rbinom(n = 250, size = 1, prob = 0.5),
  z1 = rnorm(n = 250, mean = 5, sd = 2)
) %>% 
  dplyr::mutate(
    #bl = runif(n = 250, min = 10, max = 50),
    bl = rnorm(n = 250, mean = 10, sd = 2),
    #fu = sin(bl) + 1.5*x + 10 + rnorm(n = 250, mean = 0, sd = 1),
    fu = 2*bl + 1.5*x + z1*bl + rnorm(n = 250, mean = 0, sd = 1), # adding z1*bl as a non-linear term
    chg = fu-bl
  )

# mean(df$bl)
# mean(df$fu)
# 
# t.test(chg ~ x, 
#        data = df)

glm(chg ~ x, 
    family = gaussian(), 
    data = df) %>% 
  broom::tidy()

glm(fu ~ x + bl + z1*bl, 
    family = gaussian(), 
    data = df) %>% 
  broom::tidy()

# 5. Floor and Ceiling Effects ----

# What if there is a floor and ceiling effect? 
# Essentially, what if there is a minimum value that can occur 
# and/or a maximum value that can occur? 

# A great example of this is a scale. Imagine there is a scale that means 
# happiness from 0 to 10. There is a minimum of 0 and maximum of 10. The
# people with a baseline of 10 can't increase any more, and the people
# with a minimum of 0 can't go down any more. 

# This can create problems when analyzing the data as a change from baseline.

df <- data.frame(
  x = rbinom(n = 250, size = 1, prob = 0.5),
  z1 = rnorm(n = 250, mean = 5, sd = 2)
) %>% 
  dplyr::mutate(
    bl = runif(n = 250, min = 10, max = 50),
    #bl = rnorm(n = 250, mean = 10, sd = 2),
    #fu = sin(bl) + 1.5*x + 10 + rnorm(n = 250, mean = 0, sd = 1),
    fu = 2*bl + 1.5*x + rnorm(n = 250, mean = 0, sd = 1), # adding z1*bl as a non-linear term
    chg = fu-bl
  )

# mean(df$bl)
# mean(df$fu)
# 
# t.test(chg ~ x, 
#        data = df)

glm(chg ~ x, 
    family = gaussian(), 
    data = df) %>% 
  broom::tidy()

glm(fu ~ x + bl, 
    family = gaussian(), 
    data = df) %>% 
  broom::tidy()

