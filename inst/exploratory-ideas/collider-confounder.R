# Adusting for variable that is both a collider and a confounder

# Title:

# Description: 

# Setup ----

#... Libraries ----

library(tidyverse)
library(ggdag)
library(broom)

#... Functions ----

# Load all functions

# lapply(list.files("R/"), function(x) source(paste0("R/", x)))

#... Dependencies ----

# DAG ----

theme_set(theme_dag()) # setting the theme 

dag = dagify(
  x ~ c, 
  y ~ x + c, 
  c ~ a + b,
  exposure = "x", 
  outcome = "y"
)

dag |> 
  ggdag(
    layout = "nicely")

# Simulated Data ----

# Notes: starting with a binary treatment and continuous outcome. 

# Comparing Adjusted vs Unadjusted ----

n = 430

runif(1)

a = rbinom(n = n, size = 1, prob = 0.46)
b = rnorm(n = n, mean = 5, sd = 2)
c = 0.7*a + 0.3*b

x = rbinom(n = n, size = 1, prob = plogis(0.1*c))
y = 3 + 0.2*c + 0.4*x

df = data.frame(
  a, 
  b, 
  c, 
  x, 
  y
)

mod.adj <- glm(y ~ x + c, 
               family = gaussian(link = "identity"), 
               data = df)

broom::tidy(mod.adj)[2,2]

mod.unadj <- glm(y ~ x, 
               family = gaussian(link = "identity"), 
               data = df)

broom::tidy(mod.unadj)[2, 2]

# Writing as a function ----

sim_col_con <- function(trt_effect, n){
  
  n = 430
  
  runif(1)
  
  a = rbinom(n = n, size = 1, prob = 0.46)
  b = rnorm(n = n, mean = 5, sd = 2)
  c = 0.7*a + 0.3*b
  
  x = rbinom(n = n, size = 1, prob = plogis(0.1*c))
  y = 3 + 0.2*c + 0.4*x
  
  df = data.frame(
    a, 
    b, 
    c, 
    x, 
    y
  )
  
  mod.adj <- glm(y ~ x + c, 
                 family = gaussian(link = "identity"), 
                 data = df)
  
  broom::tidy(mod.adj)[2,2]
  
  mod.unadj <- glm(y ~ x, 
                   family = gaussian(link = "identity"), 
                   data = df)
  
  broom::tidy(mod.unadj)[2, 2]n = 430
  
  runif(1)
  
  a = rbinom(n = n, size = 1, prob = 0.46)
  b = rnorm(n = n, mean = 5, sd = 2)
  c = 0.7*a + 0.3*b
  
  x = rbinom(n = n, size = 1, prob = plogis(0.1*c))
  y = 3 + 0.2*c + 0.4*x
  
  df = data.frame(
    a, 
    b, 
    c, 
    x, 
    y
  )
  
  mod.adj <- glm(y ~ x + c, 
                 family = gaussian(link = "identity"), 
                 data = df)
  
  broom::tidy(mod.adj)[2,2]
  
  mod.unadj <- glm(y ~ x, 
                   family = gaussian(link = "identity"), 
                   data = df)
  
  broom::tidy(mod.unadj)[2, 2]
}





# Repeating 1000 times ----

output_list <- replicate(1000, generate_data(n = 100), simplify = FALSE)

# Bind all data frames in the list into a single data frame
df.out <- do.call(rbind, output_list)
