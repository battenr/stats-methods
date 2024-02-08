# Title: Dichomotizing a continuous variable into a binary variable

# Description: Goal is to show how changing a continuous variable into a binary variable
# impacts power and precision. I had heard about this vaguely but wanted to test 
# for myself. 


# Notes: 
# - This could be used for a blog post. 

# Definition of Power; 

# Power: 1-type two error. Probability (or proportion) that a test will produce a significant 
# difference (is this true for proportions?) at a given significance level. 

# So at 0.05 level, proportion of times that is met. Aligns with Morris et al
# paper showing Pr[p < 0.05].

# Deffini

# Setup ----

#... Libraries ----

library(tidyverse)

# Simple Example ----

# Staring with a simple example: 
# - binary trt
# - binary outcome
# - continuous confounder 

# Comparing Power ----

power_sim <- function(n = 250, n.sim = 100){
  
  # Initializing the power values for continuous and categorical as 0. 
  
  power_count_cat <- 0
  power_count_cont <- 0
  
  # For loop, so that it will do the following for each iteration of the simulation. From 
  # 1 until the number of simulations 
  
  for (i in 1:n.sim){
  
  c = rnorm(n = n, mean = 5, sd = 2)
  x = rbinom(n = n, size = 1, prob = plogis(0.5*c))
  y = rbinom(n = n, size = 1, prob = plogis(0.3*c + 0.7*x))
  
  df = data.frame(
    x, y, c
  ) |> 
    mutate(
      c_binary = ifelse(c > 5, 1, 0),
      x_binary = rbinom(n = n, size = 1, prob = plogis(0.5*c_binary)),
      y_binary = rbinom(n = n, size = 1, prob = plogis(0.3*c_binary + 0.7*x))
      
    )
  
  
  binary.fit.cont <- stats::glm(
    formula = y ~ x + c, 
    family = stats::binomial(link = "logit"),
    data = df
  )
  
  fit.sum.cont <- summary(binary.fit.cont)
  
  test.stat.cont = fit.sum.cont$coefficients[2,1] / fit.sum.cont$coefficients[2,2]
  
  p_value_cont = 2*pnorm(abs(test.stat.cont), lower.tail = FALSE)
  
  # Categorical 
  
  # Same variable as in above but continuous
  
  binary.fit.cat <- stats::glm(
    formula = y_binary ~ x_binary + c_binary, 
    family = stats::binomial(link = "logit"),
    data = df
  )
  
  fit.sum.cat <- summary(binary.fit.cat)
  
  test.stat.cat = fit.sum.cat$coefficients[2,1] / fit.sum.cat$coefficients[2,2]
  
  # Seeing if result is significant
  
  p_value_cat = 2*pnorm(abs(test.stat.cat), lower.tail = FALSE) 
  
  if (p_value_cont < 0.05) {
    power_count_cont <- power_count_cont + 1
  }
  
  if (p_value_cat < 0.05){
    power_count_cat <- power_count_cat + 1
  }
  
  }
  
  power_cont <- power_count_cont/n.sim
  
  power_cat <- power_count_cat/n.sim
  
  return(data.frame(
    power_cont, 
    power_cat
  ))
}

result = power_sim(n = 250, n.sim = 1000)

# So making a continuous variable categorical does reduce power. 

# SE of estimates are 

result |> 
  mutate(
    n.sim = 1000, # from code above. number of simulations chosen
    se_power_cont = sqrt((power_cont*(1-power_cont))/n.sim) ,
    se_power_cat = sqrt((power_cat*(1-power_cat))/n.sim)
  )

# Comparing Precision ----

# Using formula from Morris et al. to look at 
# relative increase in precision (B vs A). The formula of the estimate is 
# 100*([(SE of A) / (SE of B) ]^2 - 1)

precision_sim <- function(n = 250, n.sim = 1){

  for (i in 1:n.sim){
    
    c = rnorm(n = n, mean = 5, sd = 2)
    x = rbinom(n = n, size = 1, prob = plogis(0.5*c))
    y = rbinom(n = n, size = 1, prob = plogis(0.3*c + 0.7*x))
    
    df = data.frame(
      x, y, c
    ) |> 
      mutate(
        c_binary = ifelse(c > 5, 1, 0),
        x_binary = rbinom(n = n, size = 1, prob = plogis(0.5*c_binary)),
        y_binary = rbinom(n = n, size = 1, prob = plogis(0.3*c_binary + 0.7*x))
        
      )
    
    # Continuous Fit
    
    binary.fit.cont <- stats::glm(
      formula = y ~ x + c, 
      family = stats::binomial(link = "logit"),
      data = df
    )
    
    fit.sum.cont <- summary(binary.fit.cont)
    
    estimate.cont = fit.sum.cont$coefficients[2,1]
    
    # Categorical 
    
    # Same variable as in above but continuous
    
    binary.fit.cat <- stats::glm(
      formula = y_binary ~ x_binary + c_binary, 
      family = stats::binomial(link = "logit"),
      data = df
    )
    
    fit.sum.cat <- summary(binary.fit.cat)
    
    estimate.cat = fit.sum.cat$coefficients[2,1]
    
    # Seeing if result is significant
    
  }
  
  return(
    data.frame(
      estimate.cont, 
      estimate.cat
    )
  )
  
}

estimates = do.call("rbind", replicate(1000, precision_sim(), simplify = FALSE))

lapply(estimates, mean) 

df.empse <- estimates |> 
  mutate(
    diff_squared_cont = (estimate.cont - 0.648)^2,
    diff_squared_cat = (estimate.cat - (-0.0188743) )^2,
    sum_diff_cont = sum(diff_squared_cont),
    sum_diff_cat = sum(diff_squared_cat),
   empse_cont = sqrt((1/(1000-1)) *sum_diff_cont) ,
   empse_cat = sqrt((1/(1000-1)) *sum_diff_cat) 
  )

empse.cont = unique(df.empse$empse_cont)

empse.cat = unique(df.empse$empse_cat)

# Relative precision: cont vs cat 

100*((empse.cat / empse.cont)^2 - 1)

# This makes sense! Because it's a decrease in precision (actually of a lot of -89%!)

# Further Ideas ----

# Does this apply to making a categorical variable (of say 5 categories 2? or what about 9 to 5?)

  


