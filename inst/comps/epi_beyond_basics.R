# Epidemiology - Beyond the Basics ----

# Ch 6 ----

# interaction means effect of exposure differs depending on another variable

library(tidyverse)
library(broom)

n = 300

x1 = rnorm(n, mean = 5, sd = 2)
x2 = rbinom(n, size = 1, prob = 0.3)
x3 = rnorm(n, mean = 0, sd = 1)

df = data.frame(
  x1, x2, x3
) %>% 
  mutate(
    trt = rbinom(n, size = 1, prob = 0.5), 
    outcome = rbinom(n, size = 1, prob = plogis(0.05*x1 + 0.1*x2 + 0.03*x3 + 0.1*trt + 0.05*x2*trt))
    #outcome = trt + x1 + x2 + x3 + x1*trt
  ) 

df_test = df %>% 
  group_by(trt, x2, outcome) %>% 
  summarise(
    n = n()/300,
    log = log(n)
  )
  
  

glm(formula = outcome ~ trt + x1 + x2 + x3 + x1*trt, 
    family = gaussian(), 
    data = df) %>% 
  broom::tidy()

ggplot(data = df_test, 
       mapping = aes(x = trt, y = log, group = x2)) + 
  geom_line()
