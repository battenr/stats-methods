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
  x ~ cc, 
  y ~ x + cc, 
  cc ~ a + b,
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

sim_col_con <- function(trt_effect, 
                        con_effect,
                        n.sample){
  
  # 
  
  a = rbinom(n = n.sample, size = 1, prob = 0.46)
  b = rnorm(n = n.sample, mean = 5, sd = 2)
  cc = 5 + 0.7*a + 0.3*b
  
  x = rbinom(n = n.sample, size = 1, prob = plogis(con_effect*cc))
  y = 3 + con_effect*cc + trt_effect*x
  
  df = data.frame(
    a, 
    b, 
    cc, 
    x, 
    y
  )
  
  # Adjusting for variable
  
  mod.adj <- glm(y ~ x + cc, 
                 family = gaussian(link = "identity"), 
                 data = df)
  
  adj.trt.effect <- broom::tidy(mod.adj)$estimate[2]
  
  # Not adjusting for variable 
  
  mod.unadj <- glm(y ~ x, 
                   family = gaussian(link = "identity"), 
                   data = df)
  
  unadj.trt.effect <- broom::tidy(mod.unadj)$estimate[2]
  
  df.trt.effects <- data.frame(
    adj.trt.effect,
    unadj.trt.effect
  )
  
  return(
    df.trt.effects
  )
  
}

sim_col_con(0.4, 0.3, n.sample = 430)



# Repeating 1000 times ----

output_list <- replicate(1000, sim_col_con(0.4, 0.3, n.sample = 430), simplify = FALSE)

# Bind all data frames in the list into a single data frame
df.out <- do.call(rbind, output_list)

unique(df.out$unadj.trt.effect)
# Bias 

(1/430)*sum(df.out$adj.trt.effect - 0.4)

(1/430)*sum(df.out$unadj.trt.effect - 0.4)

# SE of the bias 

(1/(429*430)) * sum((df.out$adj.trt.effect - mean(df.out$adj.trt.effect))^2)

(1/(429*430)) * sum((df.out$unadj.trt.effect - mean(df.out$unadj.trt.effect))^2)


