# Title: Probabilistic Bias Analysis Example

# Description: example for probabilistic bias analysis

# Setup ----

#... Libraries ----

library(tidyverse)
library(episensr)

# Simulated Data ----



df <- data.frame(
  id = sample(1:434),
  trt = rep(0:1, times = c(330, 104)),
  response = rbinom(n = 434, size = 1, prob = c(0.322, 0.60)),
  age = runif(n = 434, min = 33.0, max = 78), # from Karmma-RW
  sex = rbinom(n = 434, size = 1, prob = 0.594) # from Karmma-RW, 1 = male
)

df %>% group_by(trt, response) %>% summarize(n = n())

probsens.conf(matrix(c(46, 155, 58, 175),
                     dimnames = list(c("Response", "No: Response"), c("Trt", "Control")), nrow = 2, byrow = TRUE),
              reps = 20000, 
              prev.exp = list("triangular", c(.7, .9, .8)),
              prev.nexp = list("trapezoidal", c(.03, .04, .05, .06)),
              risk = list("triangular", c(.6, .7, .63)),
              corr.p = .8)
