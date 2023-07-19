# Title: Bias from not adjusting for a confounder

# Description: Simulating bias from not adjusting for a confounder 

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

dag = dagify(
  x ~ c, 
  y ~ x + c,
  exposure = "x",
  outcome = "y"
)

dag |> 
  ggdag(
    layout = "nicely")

ggdag_adjustment_set(dag) # Need to adjust for c since a confounder

# Simulated Data ----

# x ~ c, 
# y ~ x + c,

plogis(0.3*5)

c = rnorm(n = 100, mean = 5, sd = 2)
x = rbinom(n = 100, size = 1 , prob = plogis(0.3*c))
y = 0.2*x + 0.3*c

df = data.frame(
  c, x, y
)

mod <- glm(y ~ x, 
           family = gaussian(link = "identity"), 
           data = df)

summary(mod)

mod$coefficients[2] - 0.2

mod <- glm(y ~ x + c, 
           family = gaussian(link = "identity"), 
           data = df)

summary(mod)

mod$coefficients[2] - 0.2

# Repeating 1000 times ----

confounding_bias <- function(n, coeff_x = 0.2){
  c = rnorm(n = n, mean = 5, sd = 2)
  x = rbinom(n = n, size = 1 , prob = plogis(0.3*c))
  y = coeff_x*x + 0.3*c
  
  df <- data.frame(
    x, y, c
  )
  
  # Marginal model 
  
  mod.m <- glm(y ~ x, 
               family = gaussian(link = "identity"),
               data = df)
  
  diff_m = mod.m$coefficients[2] - coeff_x
  
  # Adjusted for confounding
  
  mod.a <- glm(y ~ x + c, 
               family = gaussian(link = "identity"),
               data = df)
  
  diff_a = mod.a$coefficients[2] - coeff_x
  
  df_diff = data.frame(
    diff_m, diff_a
  )
  
  return(df_diff)
  
}

output_list <- replicate(1000, confounding_bias(n = 120), simplify = FALSE)

# Bind all data frames in the list into a single data frame
df.out <- do.call(rbind, output_list)

df.se <- df.out |> 
  mutate(across(everything(), ~ .x^2, .names = "squared_{.col}"))

mean(df.out$diff_m) # bias from unadjusted
sqrt(sum(df.se$squared_diff_m)*(1 / (1000*999))) # se of bias 


mean(df.out$diff_a) # bias from adjusting for confounder
sqrt(sum(df.se$squared_diff_a)*(1 / (1000*999))) # se of bias

# Conclusion: as expected, it is substantially lower bias if you adjust for the confounder

