# Title: Table 2 Fallacy ----

# Description: Demonstrating the table 2 fallacy using a simple example.  
# This is based on the DAG that is shown in a corresponding LinkedIn post. 

# Setup ----

#... Libraries ----

library(tidyverse) # ol faithful 
library(ggdag) # for drawing DAG 
library(broom) # for tidying results

# DAG ----

theme_set(theme_dag()) # setting theme for the DAG 

dag = dagify(
  coffee ~ sleep + sunshine, 
  happy ~ coffee + sleep + sunshine, 
  sleep ~ sunshine, 
  exposure = "coffee",
  outcome = "happy"
)

dag |> 
  ggdag(
    layout = "nicely")

ggdag_adjustment_set(dag) # Need to adjust for sleep & sunshine since a confounder

# Simulating Data ----

set.seed(456) # setting the seed

n = 500 # arbitrary sample size

df <- data.frame(
  sunshine = rbinom(n = n, size = 1, prob = 0.6) # binary sunshine (yes/no)
) %>% 
  dplyr::mutate(
    sleep = runif(n = n, min = 2, max = 10) + 2*sunshine, # hours of sleep
    coffee = rbinom(n = n, size = 1, prob = plogis(0.5*sunshine - 0.1*sleep)), # coffee (yes/no)
    happy = 2 + 1.5*coffee + 2.5*sunshine + 1*sleep + rnorm(n = n, mean = 0, sd = 1) # happy (yes/no)
  )

# Outcome Model ----

# For this example, using an outcome model only 

mod <- glm(happy ~ coffee + sleep + sunshine, 
               family = gaussian(link = "identity"), 
               data = df)

# Tidying Results

broom::tidy(mod) %>% 
  dplyr::mutate(
    estimate = round(estimate, 2),
    lower_ci = round(estimate - 1.96*std.error, 2),
    upper_ci = round(estimate + 1.96*std.error,2), 
    result = paste0(estimate, " (", lower_ci, " to ", upper_ci, ")")
  ) %>% 
  dplyr::select(term, result)

# Model for Sunshine on Happiness ----

mod2 <- glm(happy ~ sunshine, 
           family = gaussian(link = "identity"), 
           data = df)

# Tidying Results

broom::tidy(mod2) %>% 
  dplyr::mutate(
    estimate = round(estimate, 2),
    lower_ci = round(estimate - 1.96*std.error, 2),
    upper_ci = round(estimate + 1.96*std.error,2), 
    result = paste0(estimate, " (", lower_ci, " to ", upper_ci, ")")
  ) %>% 
  dplyr::select(term, result)

