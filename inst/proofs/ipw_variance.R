# Title: Using wrong variance estimator for matching

# Description: Want to see what happens if we don't use robust variance or bootstrapping to 
# estimate variance when we have matching

# Ideas I can build off of: 
# 1. Including prognostic variables in PS model (how does it affect bias?)
# 2. Is there a different between using robust variance estimator and bootstrapping when 
#    comparing matching vs IPTW?
# 3. How about a difference when using stabilized vs unstabilized IPTW? 
# 4. What about when doubly robust methods are used? (i.e., including confounders in model)
# 5. same as 4 but include PS instead

# Setup ----

#... Libraries ----

library(tidyverse)
library(ggdag)
theme_set(theme_dag())
library(sandwich)
library(boot)

#... Functions ----

# Load all functions

lapply(list.files("R/"), function(x) source(paste0("R/", x)))

#... Dependencies ----

# DAG ----

# Situations: 
# 1. One confounder
# 2. Two confounders
# 3. A prognostic factor that doesn't impact treatment (X)
# 4. 

#... 1 Confounder ----

# Starting with one confounder now to make it simple to start

dag = dagify(
  x ~ c, 
  y ~ x + c, 
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



# Simulated Data ----

#... 1 Confounder ----

# using continuous outcome for starters

set.seed(123)

n.sim = 216 # from runif(1, min = 100, max = 500) then rounded
c = rnorm(n.sim, mean = 3, sd = 1)
x = rbinom(n.sim, size = 1, prob = plogis(c))
y = 0.3*x + c + rnorm(n.sim, mean = 0, sd = 1)

df = data.frame(
  x, y, c
)


# Metrics ----

# Thinking about what metrics to use. Looking at options from Morris et al paper

# 1. Relative % increase in precision
# 2. Reduced power maybe? It would affect precision (look at 2014 Kahan paper)
# 3. Coverage 
# 4. Bias-eliminated coverage (maybe)
# 5. Relative percent error in ModSE


# Models ----

#... IPTW

df.ipw <- ipw_stab(df, x, c)

#... Outcome Model 

# assuming no doubly robust method 

mod <- glm(y ~ x, 
           data = df.ipw, 
           weights = weights)

#... Comparing SE estimations

sqrt(diag(sandwich::vcovHC(mod, type = "HC3")))

# Define function to obtain coefficient of interest from glm model
get_coef <- function(formula, data, indices) {
  fit <- glm(formula, data = data[indices, ])
  return(coef(fit)[2])
}

# Bootstrap
set.seed(123)  # for reproducibility
results <- boot(data = df.ipw, statistic = get_coef, R = 1000,
                formula = y ~ x)

# Estimating SE
boot_se <- sd(results$t)


robust_se <- sqrt(diag(sandwich::vcovHC(mod, type = "HC3")))[2]

mod$coefficients

var(mod)

mod_se <- sqrt(diag(vcov(mod)))[2]

# Metrics ----

# Starting with coverage 

# Coverage 

robust_lower_ci <- coef(mod)[2] - 1.96*robust_se
robust_upper_ci <- coef(mod)[2] + 1.96*robust_se

boot_lower_ci <- coef(mod)[2] - 1.96*boot_se
boot_upper_ci <- coef(mod)[2] + 1.96*boot_se

as.logical(robust_lower_ci < 0.3 < robust_upper_ci)

robust_result <- as.logical(robust_lower_ci < 0.3) & as.logical(0.3 < robust_upper_ci)
boot_result <- as.logical(boot_lower_ci < 0.3) & as.logical(0.3 < boot_upper_ci)

# Replicating Multiple Times ----

ipw_var <- function(beta, # beta for x 
                    n.sim # sample size
                        ){
  
  # Data 
  
  c = rnorm(n.sim, mean = 3, sd = 1)
  x = rbinom(n.sim, size = 1, prob = plogis(c))
  y = beta*x + c + rnorm(n.sim, mean = 0, sd = 1) # adding error term with rnorm
  
  df = data.frame(
    x, y, c
  )
  
  # IPW using PS
  
  df.ipw <- ipw_stab(df, x, c) # stabilized weights 
  
  # Outcome model, fit with weights and x only (i.e., not doubly robust)
  
  mod <- glm(y ~ x, 
             data = df.ipw, 
             weights = weights)
  
  # Robust Variance Estimator 
  
  robust_se <- sqrt(diag(sandwich::vcovHC(mod, type = "HC3")))[2]
  
  # Bootstrap
  
  #... Define function to obtain coefficient of interest from glm model
  get_coef <- function(formula, data, indices) {
    fit <- glm(formula, data = data[indices, ])
    return(coef(fit)[2])
  }
  
  #... Get Bootstrapped Estimates
  
  results <- boot(data = df.ipw, statistic = get_coef, R = 100, # would use 1000 but takes too long in this case
                  formula = y ~ x)
  
  #... Estimating SE
  
  boot_se <- sd(results$t)
  
  # Coverage 
  
  #... Robust 95% CIs
  
  robust_lower_ci <- coef(mod)[2] - 1.96*robust_se
  robust_upper_ci <- coef(mod)[2] + 1.96*robust_se
  
  #... Bootstrap 95% CIs
  
  boot_lower_ci <- coef(mod)[2] - 1.96*boot_se
  boot_upper_ci <- coef(mod)[2] + 1.96*boot_se
  
  robust_result <- as.logical(robust_lower_ci <= beta) & as.logical(beta <= robust_upper_ci)
  boot_result <- as.logical(boot_lower_ci <= beta) & as.logical(beta <= boot_upper_ci)
  
  robust_boot = data.frame(
    robust_result, 
    boot_result
  )
  
  return(robust_boot)
  
}

output_list <- replicate(1000, ipw_var(beta = 0.3, n.sim = 216), simplify = FALSE)

# Bind all data frames in the list into a single data frame
df.out <- do.call(rbind, output_list)

mean(df.out$robust_result) # 0.884 
mean(df.out$boot_result, na.rm = TRUE) # 0.8186

# So there is less coverage with bootstrapping when using IPTW with a single confounder 

# Doubly Robust ----

ipw_var <- function(beta, # beta for x 
                    n.sim # sample size
){
  
  # Data 
  
  c = rnorm(n.sim, mean = 3, sd = 1)
  x = rbinom(n.sim, size = 1, prob = plogis(c))
  y = beta*x + c + rnorm(n.sim, mean = 0, sd = 1) # adding error term with rnorm
  
  df = data.frame(
    x, y, c
  )
  
  # IPW using PS
  
  df.ipw <- ipw_stab(df, x, c) # stabilized weights 
  
  # Outcome model, fit with weights and x only (i.e., not doubly robust)
  
  mod <- glm(y ~ x + c, 
             data = df.ipw, 
             weights = weights)
  
  # Robust Variance Estimator 
  
  robust_se <- sqrt(diag(sandwich::vcovHC(mod, type = "HC3")))[2]
  
  # Bootstrap
  
  #... Define function to obtain coefficient of interest from glm model
  get_coef <- function(formula, data, indices) {
    fit <- glm(formula, data = data[indices, ])
    return(coef(fit)[2])
  }
  
  #... Get Bootstrapped Estimates
  
  results <- boot(data = df.ipw, statistic = get_coef, R = 100, # would use 1000 but takes too long in this case
                  formula = y ~ x)
  
  #... Estimating SE
  
  boot_se <- sd(results$t)
  
  # Coverage 
  
  #... Robust 95% CIs
  
  robust_lower_ci <- coef(mod)[2] - 1.96*robust_se
  robust_upper_ci <- coef(mod)[2] + 1.96*robust_se
  
  #... Bootstrap 95% CIs
  
  boot_lower_ci <- coef(mod)[2] - 1.96*boot_se
  boot_upper_ci <- coef(mod)[2] + 1.96*boot_se
  
  robust_result <- as.logical(robust_lower_ci <= beta) & as.logical(beta <= robust_upper_ci)
  boot_result <- as.logical(boot_lower_ci <= beta) & as.logical(beta <= boot_upper_ci)
  
  robust_boot = data.frame(
    robust_result, 
    boot_result
  )
  
  return(robust_boot)
  
}

output_list <- replicate(1000, ipw_var(beta = 0.3, n.sim = 216), simplify = FALSE)

# Bind all data frames in the list into a single data frame
df.out <- do.call(rbind, output_list)

mean(df.out$robust_result) # 0.884 
mean(df.out$boot_result, na.rm = TRUE) # 0.8186

# 1 Confounder & 1 Prongostic Factor ----

# 2 Confounders 
