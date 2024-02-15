# Description: Variables to Add to PSM

# Ideas for code: 
# - What variables should be added to the propensity score? My initial thought before testing 
# is that it should only be confounders/based on a DAG. Does this include effect modifiers? 

# - How do adding effect modifiers affect: 
# 1. Bias
# 2. Precision (SE)
# 3. Power (calculate based on)
# 4. What metric to use to check PS? 
# 5. Is there a difference between this for IPTW (stabilized) and PSM? 


# Libraries ----

library(tidyverse)
library(ggdag)
theme_set(theme_dag())

# Functions

lapply(list.files("R/"), function(x) source(paste0("R/", x)))


# DAG ----

# Four variables: 
# 1. Binary trt
# 2. Continuous Outcome
# 3. 1 Confounder
# 4. 1 Effect modifier (E)

dag = dagify(
  x ~ c, 
  y ~ x + c + e,
  exposure = "x",
  outcome = "y"
)

dag |> 
  ggdag(
    layout = "nicely")

ggdag_adjustment_set(dag) # No need to adjust for anything

# Simulating Data ----

# y - continuous outcome
# x - binary trt 
# c - continuous
# e - binary

n = 250 # making sample size 1000 so when we match then there will be some patients left over after matching

theta = 0.3 # theta (true effect of x on y)

c = rnorm(n = n, mean = 5, sd = 2)
e = rbinom(n = n , size = 1, prob = 0.3)
x = rbinom(n = n, size = 1, prob = plogis(0.3*c)) 
y = theta*x + 0.3*c + 0.5*e

df = data.frame(
  x, c, e, y
)

# With only the confounder 

ipw.c = WeightIt::weightit(x ~ c, data = df, method = "glm")

glm.c <- glm(y ~ x, 
    family = gaussian(),
    data = df, 
    weights = ipw.c$weights)

glm.c$coefficients[2]

bias.c = glm.c$coefficients[2] - theta

library(sandwich)

?sandwich::sandwich()

sandwich::sandwich(glm.c, type = "HC")
sqrt(vcovHC(glm.c)[2,2]) # 2,2 is the position of the treatment

# With the effect modifier as well 

ipw.e <- WeightIt::weightit(x ~ c + e, data = df, method = "glm")
  
glm.e <- glm(y ~ x, # not using doubly robust estimation
             family = gaussian(),
             data = df, 
             weights = ipw.e$weights)

glm.e$coefficients[2]

bias.e = glm.e$coefficients[2] - theta

library(sandwich)

sandwich::sandwich(glm.e, type = "HC")
sqrt(vcovHC(glm.e)[2,2]) # 2,2 is the position of the treatment

ipw_c_d <- function(){
  
  bias_
  
  
  
  n = 250 # making sample size 1000 so when we match then there will be some patients left over after matching
  
  theta = 0.3 # theta (true effect of x on y)
  
  c = rnorm(n = n, mean = 5, sd = 2)
  e = rbinom(n = n , size = 1, prob = 0.3)
  x = rbinom(n = n, size = 1, prob = plogis(0.3*c)) 
  y = theta*x + 0.3*c + 0.5*e
  
  df = data.frame(
    x, c, e, y
  )
  
  # With only the confounder 
  
  ipw.c = WeightIt::weightit(x ~ c, data = df, method = "glm")
  
  glm.c <- glm(y ~ x, 
               family = gaussian(),
               data = df, 
               weights = ipw.c$weights)
  
  glm.c$coefficients[2]
  
  bias.c = glm.c$coefficients[2] - theta
  
  library(sandwich)
  
  ?sandwich::sandwich()
  
  sandwich::sandwich(glm.c, type = "HC")
  sqrt(vcovHC(glm.c)[2,2]) # 2,2 is the position of the treatment
  
  # With the effect modifier as well 
  
  ipw.e <- WeightIt::weightit(x ~ c + e, data = df, method = "glm")
  
  glm.e <- glm(y ~ x, # not using doubly robust estimation
               family = gaussian(),
               data = df, 
               weights = ipw.e$weights)
  
  glm.e$coefficients[2]
  
  bias.e = glm.e$coefficients[2] - theta
  
  library(sandwich)
  
  sandwich::sandwich(glm.e, type = "HC")
  sqrt(vcovHC(glm.e)[2,2]) # 2,2 is the position of the treatment
  
}


# Next Steps -----

# See if any different when using doubly robust estimation

# See if this makes a difference for matching (comparing matching to matching)
# Could compare vs ipw but need to make sure same causal estimand

library(MatchIt)

MatchIt::matchit(
  x~c, 
  data = df
)

?matchit











