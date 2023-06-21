# Title: Playing around with Power 

# Description: 
# Using simulations to play around with power and answer my own questions

# To do: 

# - How does adding covariates increase/decrease power
# - 

# Setup ----

#... Libraries ----

library(tidyverse)

#... Functions ----

# Load all functions

lapply(list.files("R/"), function(x) source(paste0("R/", x)))

#... Dependencies ----

# Simulated Data ----

# Look into how power increases or decreases based on adding covariates

qlogis(0.05)
qlogis(0.95)
plogis(1)


exp(qlogis(0.3))

0.3/0.7

plogis(-0.847)

log(0.3/0.7)

exp(-0.8473)

0.3/0.7

library(pwr)


qlogis(0.3)


# Pearson Ch-Squared Test for two proportions

?do.call


qnorm(0.95)
qnorm(0.975)


power_sim <- function(n.arm = 2, n.per.arm, p.trt, p.control){
  n.arm = n.arm # number of arms
  n.per.arm = n.per.arm # patients per arm
  p.trt = p.trt
  p.control = p.control
  
  df <- data.frame(
    trt = rep(c(1,0), n.arm*n.per.arm),
    outcome = rbinom(n = n.arm*n.per.arm, size = 1, prob = c(p.trt, p.control))
  ) 
  
  binary.fit <- stats::glm(
    formula = trt ~ outcome, 
    family = stats::binomial(link = "logit"),
    data = df
  )
  
  fit.sum <- summary(binary.fit)
  
  test.stat = fit.sum$coefficients[2,1] / fit.sum$coefficients[2,2]
  
  result = 2*pnorm(abs(test.stat), lower.tail = FALSE) < 0.05
  
  return(result)
}

power.results <- replicate(10000, power_sim(n.per.arm = 34, p.trt = 0.8, p.control = 0.5), simplify = TRUE)

# power.results <- do.call("rbind", replicate(10000, power_sim(), simplify = FALSE))

power.results[power.results == TRUE] |> as.data.frame() |> nrow() / 10000

mean(power.results == TRUE)

mean(power.results)



