# Title: Statistical Efficency of Stabilized IPTW vs unstabilized IPTW

# Description: Comparing the use of stabilized vs unstabilized IPTW for ATE

# Setup ----

#... Libraries ----

library(tidyverse)
library(ggdag)
# theme_set(theme_dag())

#... Functions ----

# Get the list of .R files in the "R/" directory
files <- list.files(path = "R/", pattern = "*.R", full.names = TRUE)

# Source all .R files
lapply(files, source)

#... Dependencies ----

# Simulated Data ----

set.seed(123)

# Note: 
# 1. Assuming a binary treatment 
# 2. Comparing these two weights across MSE, bias and coverage 

# To do/ideas: 
# 1. Are stabilized weights more efficient vs non-stabilized? 
# 2. Under what conditions are they pretty similar? Under what conditions are they different? 
# 3. W

# Formulas for measures from page 13 of morris et al. 2019

# Assuming X, Y and Z ----

#... DAG ----

dag <- dagify(
  y ~ x + z, 
  x ~ z,
  exposure = "x", 
  outcome = "y"
)

ggdag::ggdag(dag, layout = "circle")

ggdag_paths(dag)

ggdag_adjustment_set(dag)

#.... Data ----

#... Alternative for Binary Outcome

# Comparing Stabilized vs Unstabilized by Metric ----

library(boot)

#... Bias ----

# Using a continuous outcome and one continuous confounder
# "true" equation is: y = 0.3*x + 0.1*z + rnorm(n, 3,)

generate_data <- function(n){
  
  library(tidyverse)
  library(boot)
  
  n = 100
  z = rnorm(n, 3, 2)
  x = rbinom(n, size = 1, prob = plogis(0.5*z)) # does the strength of the association between Z & X/Y matter? What if it's different between x*z and x*y
  y = 0.3*x + 0.1*z + rnorm(n, 3, 2)
  
  df = data.frame(
    x, y, z
  ) 
  
  df$w_unstab = ipw_unstab(df, x, z)$weights
  df$w_stab = ipw_stab(df, x, z)$weights
  
  glm.unstab <- glm(
    as.formula(y ~ x + z), 
    family = gaussian(link = "identity"), 
    data = df,
    weights = w_unstab
  )
  
  glm.stab <- glm(
    as.formula(y ~ x + z), 
    family = gaussian(link = "identity"), 
    data = df,
    weights = w_stab
  )
  
  coeff_unstab = glm.unstab$coefficients[2] |> as.numeric()
  coeff_stab = glm.stab$coefficients[2] |> as.numeric()
  
  df.coeff <- data.frame(
    coeff_unstab, 
    coeff_stab
  )
  
  # bias_unstab = coeff.unstab - 0.3
  # bias_stab = coeff.stab - 0.3
  # 
  # df.bias <- data.frame(
  #   bias_unstab,
  #   bias_stab
  # )
  
  return(df.coeff)
}

output_list <- replicate(1000, generate_data(n = 100), simplify = FALSE)

# Bind all data frames in the list into a single data frame
df.out <- do.call(rbind, output_list)

# Calculating overall bias

mean(df.out$coeff_unstab - 0.3)
mean(df.out$coeff_stab - 0.3)

sd(df.out$coeff_stab) # I think thi sis the 
sd(df.out$coeff_unstab)/(sqrt(2*999))

mean(df.out$coeff_stab)

# SE of the bias

calculate_se <- function(df, 
                        coeff_name, 
                         beta, 
                         n.sims){
  
  bias = df[[coeff_name]] - mean(df[[coeff_name]])
  diff = bias - mean(bias)
  squared_diff = diff^2
  sum_sd = sum(squared_diff)
  variance = sum_sd / (n.sims * (n.sims - 1))
  se = sqrt(variance)
  
  return(se)
}

se.unstab = calculate_se(df.out, "coeff_unstab", 0.3, 1000)
se.stab = calculate_se(df.out, "coeff_stab", 0.3, 1000)

#... MSE ----

calculate_mse <- function(df, 
                          coeff_name, 
                          beta, 
                          nsims){
  diff = df[[coeff_name]] - beta
  diff_squared = diff^2
  sum_squared = sum(diff_squared)
  result = (1/nsims)
  
  return(result)
}

calculate_mse(df.out, "coeff_unstab", beta = 0.3, nsims = 1000)
calculate_mse(df.out, "coeff_stab", beta = 0.3, nsims = 1000)

#... Coverage ----







#... Statistical Efficiency ----

#... 

