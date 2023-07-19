# Title: Conditioning on a Collider

# Description: Simulating conditioning on a collder (by including in a regression model 
# to understand )

# Setup ----

#... Libraries ----

library(tidyverse)
library(ggdag)
theme_set(theme_dag())

#... Functions ----

# Load all functions

# lapply(list.files("R/"), function(x) source(paste0("R/", x)))

#... Dependencies ----

# DAG ----

# This would result in m being a 

dag = dagify(
  x ~ a, 
  y ~ b + x,
  c ~ a + b,
  exposure = "x",
  outcome = "y"
)

dag |> 
  ggdag(
    layout = "nicely")

dag |> ggdag()

dag |> 
  ggdag(
    layout = "star")

ggdag_adjustment_set(dag) # No need to adjust for anything

ggdag_dseparated(dag, controlling_for = "c") # but if we did adjust for a collider then we'd be opening a path

# Simulated Data ----

# Simulating data based on dag above: 
# 
# x ~ a, 
# y ~ b + x,
# c ~ a + b

set.seed(123)

n.sim = 120 
a = rnorm(n = n.sim, mean = 0, sd = 1)
b = rnorm(n = n.sim, mean = 5, sd = 1)
c = a + b
x = rbinom(n = n.sim, size = 1, prob = plogis(a))
y = 0.4*b + 0.5*x


df <- data.frame(
  a, b, x, y, c
)

# Models ----

#... Model 1 ----

mod <- glm(y ~ x, 
             family = gaussian(link = "identity"),
             data = df)

summary(mod)

0.5 - mod$coefficients[2]

#... Model 2 ----

mod <- glm(y ~ x + c, 
           family = gaussian(link = "identity"),
           data = df)

0.5 - mod$coefficients[2]

# Repeating 1000 times ----

collider_bias <- function(n){
  
  n.sim = n
  a = rnorm(n = n.sim, mean = 0, sd = 1)
  b = rnorm(n = n.sim, mean = 5, sd = 1)
  c = a + b
  x = rbinom(n = n.sim, size = 1, prob = plogis(a))
  y = 0.4*b + 0.5*x
  
  
  df <- data.frame(
    a, b, x, y, c
  )
  
  # Marginal model 
  
  mod.m <- glm(y ~ x, 
             family = gaussian(link = "identity"),
             data = df)
  
  diff_m = mod.m$coefficients[2] - 0.5
  
  # Adjusted for collider
  
  mod.c <- glm(y ~ x + c, 
               family = gaussian(link = "identity"),
               data = df)
  
  diff_c = mod.c$coefficients[2] - 0.5
  
  df_diff = data.frame(
    diff_m, diff_c
  )
  
  return(df_diff)
  
  
}

output_list <- replicate(1000, collider_bias(n = 120), simplify = FALSE)

# Bind all data frames in the list into a single data frame
df.out <- do.call(rbind, output_list)

df.se <- df.out |> 
  mutate(across(everything(), ~ .x^2, .names = "squared_{.col}"))

mean(df.out$diff_m) # bias from unadjusted is 0.00367. 
sqrt(sum(df.se$squared_diff_m)*(1 / (1000*999))) # se bias is 0.00223


mean(df.out$diff_c) # bias from collider is -0.1795
sqrt(sum(df.se$squared_diff_c)*(1 / (1000*999))) # se bias is 0.00589


