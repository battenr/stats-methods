# Title: Conditioning on a Collider

# Description: Directed acyclic graphs (DAGs) are a great tool that can be 
# used to guide variable selection. Colliders, a variable where two arrowheads meet, 
# should not be adjusted for since they can induce bias. This code aims to prove
# and quantify the bias using a simple example. 

# Setup ----

#... Libraries ----

library(tidyverse) # ol faithful
library(ggdag) # to create the DAG
library(broom) # handy for cleaning regression outputs

# DAG ----


theme_set(theme_dag())

# Creating the DAG to guide the scenario

dag = ggdag::dagify(
  happy ~ coffee + sleep + lifting + donut, 
  lifting ~ donut + sleep + coffee,
  coffee ~ sleep, 
  exposure = "coffee",
  outcome = "happy"
)

# Viewing the DAG

dag |> 
  ggdag::ggdag(
    layout = "nicely")

# Check what we need to adjust for. This is a handy function, although we 
# already know the answer 

ggdag::ggdag_adjustment_set(dag) 

# What would happy if we adjusted for lifting, a collider

ggdag::ggdag_dseparated(dag, controlling_for = c("lifting", "sleep")) # but if we did adjust for a collider then we'd be opening a path

# Simulated Data ----

# Using the DAG below as a guide, simulating data to show this bias. 

# Simulating data based on dag above: 

# happy ~ coffee + sleep + lifting 
# lifting ~ donut + sleep
# coffee ~ sleep

set.seed(123) # for reproducibility

sample.size = 250 # sample size for our simulation

# Simulating the variables 

donut_eaten = rbinom(n = sample.size, size = 1, prob = 0.3)
hours_sleep = rnorm(n = sample.size, mean = 8, sd = 2)
lifting = rbinom(n = sample.size, size = 1, prob = plogis(0.1*hours_sleep + 0.3*donut_eaten + 0.2*coffee))
coffee = rbinom(n = sample.size, size = 1, prob = plogis(0.2*hours_sleep))
happy = rnorm(n = sample.size, mean = 10 + 0.5 * hours_sleep + 2 * coffee + 3 * lifting)

trt_effect <- 0.3 # what we want the effect to be in the outcome. Basically what effect coffee
# has 

# Putting together as a dataframe

df <- data.frame(
  donut_eaten = rbinom(n = sample.size, size = 1, prob = 0.3),
  hours_sleep = rnorm(n = sample.size, mean = 8, sd = 2),
  lifting = rbinom(n = sample.size, size = 1, prob = plogis(0.1*hours_sleep + 0.3*donut_eaten + 0.3*coffee)),
  coffee = rbinom(n = sample.size, size = 1, prob = plogis(0.2*hours_sleep + 0.1*donut_eaten)),
  happy = rbinom(n = sample.size, size = 1, prob = plogis(0.1*lifting + 0.2*hours_sleep + trt_effect*coffee + 
                                                            0.1*donut_eaten))
)

# Test Run ----

# We can test our data by running a logistic regression using a generalized linear model 

mod <- glm(happy ~ coffee + hours_sleep, 
           family = binomial(link = "logit"), 
           data = df)

expected_value <- broom::tidy(mod)$estimate[2]

expected_value - trt_effect

# Repeat, Repeat, Repeat ----

# For this, let's just repeat it 1000 times. Note: there are much better ways to 
# choose the number of simulations required. 

#... Not adjusting for collider ----

no_collider_adjust <- function(sample.size){
  
  # See above for notes on detail of variables
  
  #sample.size = 250
  
  trt_effect <- 0.1 # what we want the effect to be in the outcome. Basically what effect coffee
  # has 
  
  df <- data.frame(
    donut_eaten = rbinom(n = sample.size, size = 1, prob = 0.3),
    hours_sleep = rnorm(n = sample.size, mean = 8, sd = 2)
    ) %>% 
    dplyr::mutate(
      coffee = rbinom(n = sample.size, size = 1, prob = plogis(0.2*hours_sleep + 0.1*donut_eaten)),
      lifting = rbinom(n = sample.size, size = 1, prob = plogis(0.1*hours_sleep + 0.3*donut_eaten + 0.3*coffee)),
      happy = rbinom(n = sample.size, size = 1, prob = plogis(0.3*lifting + 0.2*hours_sleep + trt_effect*coffee + 
                                                                0.1*donut_eaten))
  )
  
  mod <- glm(happy ~ coffee + hours_sleep, 
             family = binomial(link = "logit"), 
             data = df)
  
  expected_value <- broom::tidy(mod)$estimate[2]
  
  bias_for_one = expected_value - trt_effect
  
  df_bias = data.frame(
    bias_for_one
  )
  
  return(df_bias)
  
}

# Repeating 1000 times using a sample size of 1000

output_list <- replicate(1000, no_collider_adjust(sample.size = 1000), simplify = FALSE)

df.out <- do.call(rbind, output_list) %>% # reformatting
  
  # Adding a new column that will be used to estimate the Monte Carlo SE of the estimate
  mutate(
    squared = (bias_for_one - mean(bias_for_one))^2
  )


# Calculating the mean bias and Monte Carlo SE of estimate

# See Morris et al. (2019) for details on calculating these

mean(df.out$bias_for_one) # bias  
sqrt(sum(df.out$squared)*(1 / (1000*999))) # Monte Carlo SE of bias (1000 is number of simulations, 999 is n - 1)

#... Adjusting for collider -----

collider_adjust <- function(sample.size){
  
  # See above for notes on detail of variables
  
  #sample.size = 250
  
  trt_effect <- 0.1 # what we want the effect to be in the outcome. Basically what effect coffee
  # has 
  
  df <- data.frame(
    donut_eaten = rbinom(n = sample.size, size = 1, prob = 0.3),
    hours_sleep = rnorm(n = sample.size, mean = 8, sd = 2)
  ) %>% 
    dplyr::mutate(
      coffee = rbinom(n = sample.size, size = 1, prob = plogis(0.2*hours_sleep + 0.1*donut_eaten)),
      lifting = rbinom(n = sample.size, size = 1, prob = plogis(0.1*hours_sleep + 0.3*donut_eaten + 0.3*coffee)),
      happy = rbinom(n = sample.size, size = 1, prob = plogis(0.3*lifting + 0.2*hours_sleep + trt_effect*coffee + 
                                                                0.1*donut_eaten))
    )
  
  mod <- glm(happy ~ coffee + hours_sleep + lifting, 
             family = binomial(link = "logit"), 
             data = df)
  
  expected_value <- broom::tidy(mod)$estimate[2]
  
  bias_for_one = expected_value - trt_effect
  
  df_bias = data.frame(
    bias_for_one
  )
  
  return(df_bias)
  
}

# Repeating 1000 times using a sample size of 250

output_list <- replicate(1000, collider_adjust(sample.size = 1000), simplify = FALSE)

df.out <- do.call(rbind, output_list) %>% # reformatting
  
  # Adding a new column that will be used to estimate the Monte Carlo SE of the estimate. 
  
  mutate(
    squared = (bias_for_one - mean(bias_for_one))^2
  )

# Calculating the mean bias and Monte Carlo SE of estimate

# See Morris et al. (2019) for details on calculating these

mean(df.out$bias_for_one) # bias  
sqrt(sum(df.out$squared)*(1 / (1000*999))) # Monte Carlo SE of bias (1000 is number of simulations, 999 is n - 1)
