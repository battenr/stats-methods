# Title: Conditioning on Descendant of a Collider Causes Bias

# Description: This script is intended to show that conditioning/adjusting
# for a descendant of a collider can induce bias. 

# Setup ----

library(tidyverse) # ol' faithful
library(ggdag) # for creating and showing DAG 
library(broom) # for tidying result of the model

# DAG ----
 
theme_set(ggdag::theme_dag()) # setting the theme for the DAG

# Let's create a DAG. We want: 
# - Exposure (Treatment)
# - Outcome
# - Collider
# - Second Variable
# - A descendant of a collider

dag <- ggdag::dagify(
  y ~ x + v1, 
  c ~ v1 + x, 
  dc ~ c,
  exposure = "x",
  outcome = "y"
) 

# Which variables do we need to adjust for? 

dag %>% 
  ggdag::ggdag_adjustment_set()

# What happens if we adjust for the descendant of a collider? 

dag %>% 
  ggdag::ggdag_adjust(c("dc")) # showing that adjusting for both opens pathways    

# Simulating Data - Trial Run ----

# This is used as a test run, to see if our code works. 

set.seed(456) # for reproducibility

n.sim = 500 # sample size of 500
beta_trt = 1.5 # true treatment effect

# Simulating the data as a dataframe

df <- data.frame(
  x = rbinom(n = n.sim, 
             size = 1, 
             prob = 0.45),
  v1 = rnorm(n = n.sim, 
             mean = 10, 
             sd = 2)
) %>% 
  dplyr::mutate(
    c = 1.5*v1 + 2*x,
    dc = 1.5*c, 
    y = beta_trt*x + 0.75*v1
  )

#... Fitting GLM ----

# Let's fit two models: one not adjusting for anything, 
# another one adjusting for the descendant of a collider

mod1 <- stats::glm(
  y ~ x, # not adjusting for anything
  family = gaussian(), 
  data = df
)

# Viewing results

broom::tidy(mod1) %>% 
  filter(term == "x")

# Adjusting for descendant of a collider

mod2 <- stats::glm(
  y ~ x + dc, 
  family = gaussian(), 
  data = df
)

# Viewing Results

broom::tidy(mod2) %>% 
  filter(term == "x") %>% 
  mutate()


# Replicate 1000 times ----

# We could use one run as an example, however that is a single run. We need to do it  many more times than that. 
# Let's run it 1000 times. (Note: there are ways to calculate the number of runs required. For simplicity, we'll pick 1000 for now)

#... No Adjustment ----

# Using same code from before, but now it's in a function so we can run it multiple times

simulate_no_adjust <- function(n.sim = 500, 
                               beta_trt = 1.5){
  
  df <- data.frame(
    x = rbinom(n = n.sim, 
               size = 1, 
               prob = 0.45),
    v1 = rnorm(n = n.sim, 
               mean = 10, 
               sd = 2)
  ) %>% 
    dplyr::mutate(
      c = 1.5*v1 + 2*x,
      dc = 1.5*c, 
      y = beta_trt*x + 0.75*v1
    )
  
  mod <- stats::glm(
    y ~ x, 
    family = gaussian(), 
    data = df
  )
  
  broom::tidy(mod) %>% 
    dplyr::filter(term == "x") %>% 
    dplyr::mutate(
      bias = estimate - beta_trt
    ) %>% 
    dplyr::select(
      estimate, 
      bias
    )
  
}

# Repeating the process 1000 times

results <- replicate(1000, 
                     simulate_no_adjust(n.sim = 500, beta_trt = 1.5), simplify = FALSE)

results_df <- do.call(rbind, results) # putting results into a dataframe

bias_no_adjust <- mean(results_df$bias) # calculating the bias

# Monte Carlo SE of Bias (from Morris et al. 2019)

mean_no_adjust = mean(results_df$estimate)

estimate_se = results_df %>% 
  dplyr::mutate(
    squared = (estimate - mean_no_adjust)^2 
  ) 

se_bias_no_adjust = sqrt(sum(estimate_se$squared) * (1 / (500*(500-1)))) # 500 is the number of simulations


#... Adjusting for Descendant of Collider ----

# This code does the same as the previous section but it adjusts for the 
# variable dc (a descendant of a collider)

simulate_adjust_dc <- function(n.sim = 500, 
                               beta_trt = 1.5){
  
  df <- data.frame(
    x = rbinom(n = n.sim, 
               size = 1, 
               prob = 0.45),
    v1 = rnorm(n = n.sim, 
               mean = 10, 
               sd = 2)
  ) %>% 
    dplyr::mutate(
      c = 1.5*v1 + 2*x,
      dc = 1.5*c, 
      y = beta_trt*x + 0.75*v1
    )
  
  mod <- stats::glm(
    y ~ x + dc, 
    family = gaussian(), 
    data = df
  )
  
  broom::tidy(mod) %>% 
    dplyr::filter(term == "x") %>% 
    dplyr::mutate(
      bias = estimate - beta_trt
    ) %>% 
    dplyr::select(
      estimate,
      bias
    )
  
}

results <- replicate(1000, 
                     simulate_adjust_dc(n.sim = 500, beta_trt = 1.5), simplify = FALSE)

results_df <- do.call(rbind, results)

bias_adjust_dc <- round(mean(results_df$bias), 2) # 1

mean_adjust_dc = mean(results_df$estimate)

estimate_se = results_df %>% 
  dplyr::mutate(
    squared = (estimate - mean_adjust_dc)^2 
  ) 

se_bias_adjust_dc = round(sqrt(sum(estimate_se$squared) * (1 / (500*(500-1)))), 3)

# Results! ----

# No Adjustment

bias_no_adjust
se_bias_no_adjust

# Adjustment 

bias_adjust_dc
se_bias_adjust_dc


