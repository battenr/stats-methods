# Title: Bootstrapping vs Sandwich Errors

# Description: Comparing bootstrap vs sandwich estimators for estimating 
# variance of treatment effect after IPTW

# Ideas to compare: 

# Basic Starting Case: Continuous outcome and continuous confounders

# 1. Comparing under the basic case
# 2. One measured and one unmeasured confounder 
# 3. When one measured confounder using doubly robust estimation
# 4. When one measured and one unmeasured confounder using doubly robust estimation



# 2. When there is unmeasured confounding
#
# 3. When data is skewed (perhaps even BCa vs robust sandwich)
# 4. When we can only adjust for one confounder but there is two (perhaps the second isn't measured)
# 5.


# Setup ----

#... Libraries ----

library(tidyverse)
library(ggdag)

source("R/ipw_stab.R") # loading my own function 
source("R/n_sample.R")

#... Functions ----

# Load all functions

# lapply(list.files("R/"), function(x) source(paste0("R/", x)))

#... Dependencies ----

# DAG ----

theme_set(theme_dag()) # Set theme for DAGs

# Assuming a continuous outcome for ease for now 

# Drawing a dag using ggdag package with the following assumptions:
# 1. Treatment is assigned randomly
# 2. There is no unmeasured confounding
# 3. There is no measurement error

# Each DAG# is a different scenario, the number corresponds to the number of the scenario above

#... DAG1 ----

dag <- dagify(
  x ~ c, 
  y ~ x + c,
  exposure = "x",
  outcome = "y"
) 

dag |> 
  ggdag(
    layout = "nicely")

ggdag_adjustment_set(dag) # Need to adjust for c since a confounder


#... DAG2 ----



#... 

# Simulated Data ----

set.seed(123) 

# Scenario 1 ----

# All continuous variables. One confounder

# Using function to get the number of samples

# n_sample()

n.sim = 560

#  # Picking number of observations using runif: 

c1 = rnorm(n = n.sim, mean = 10, sd = 2)
trt = rbinom(n = n.sim, size = 1, prob = plogis(0.2 + 0.05*c1))
y = 2*trt + 3*c1 + rnorm(n = n.sim, mean = 0, sd = 2)

df = data.frame(
  c1 = c1,
  trt = trt,
  y = y
)

# Weights 

df.w <- ipw_stab(df, trt, c1)

# Outcome Model 

mod.outcome <- glm(
  formula = y ~ trt,
  data = df.w,
  family = gaussian(link = "identity"),
  weights = df.w$weights
)

#... Bootstrapping and Cluster Robuust Variance Estimator ----


# Define a function to calculate the statistic of interest
bootstrap_statistic_ipw <- function(data, indices) {
  
  library(tidyverse)
  library(boot)
  source("R/ipw_stab.R")
  
  # Subset the data using the bootstrap indices
  boot_data <- data[indices, ]
  
  # Fit the outcome model on the bootstrap sample
  df.w_bootstrap <- ipw_stab(boot_data, boot_data$trt, boot_data$c1)
  
  mod_outcome_bootstrap <- glm(
    formula = y ~ trt,
    data = df.w_bootstrap,
    family = gaussian(link = "identity"),
    weights = df.w_bootstrap$weights
  )
  
  # Extract and return the parameter of interest, e.g., coefficient of 'trt'
  coef(mod_outcome_bootstrap)['trt']
}

# Define the replicate_bootstrap function
replicate_bootstrap <- function() {
  
  # Treatment effect
  
  trt_effect <- 2
  
  # Simulate data as you did before
  c1 <- rnorm(n = n.sim, mean = 10, sd = 2)
  trt <- rbinom(n = n.sim, size = 1, prob = plogis(0.2 + 0.05 * c1))
  y <- trt_effect * trt + 3 * c1 + rnorm(n = n.sim, mean = 0, sd = 2)
  
  
  
  df <- data.frame(c1 = c1, trt = trt, y = y)
  
  # Perform bootstrapping
  boot_results <- boot(data = df, statistic = bootstrap_statistic_ipw, R = 100)
  
  # Access the bootstrap results
  bootstrap_coefs <- boot_results$t
  bootstrap_ci <- boot::boot.ci(boot_results, type = "perc")
  
  # Extract and format the confidence intervals
  ci_lower <- bootstrap_ci$percent[,4]
  ci_upper <- bootstrap_ci$percent[,5]
  
  boot_coverage <- all(ci_lower <= trt_effect & ci_upper >= trt_effect)
  
  # Cluster Robust 
  
  mod_outcome <- glm(
    formula = y ~ trt,
    data = df,
    family = gaussian(link = "identity"),
    weights = df.w$weights
  )
  
  library(sandwich)
  
  #cluster_se <- sqrt(diag(vcovCL(mod_outcome, cluster = df.w$c1)))
  
  cluster_se <- sqrt(diag(vcovHC(mod_outcome, type = "HC3")))
  
  # View cluster-robust standard errors
  
  cluster_lower_ci <- mod_outcome$coefficients[2] - 1.96 * cluster_se
  cluster_upper_ci <- mod_outcome$coefficients[2] + 1.96 *cluster_se
  
  #cluster_coverage <- between(trt_effect, cluster_lower_ci, cluster_upper_ci)
  
  cluster_coverage <- all(cluster_lower_ci <= trt_effect & cluster_upper_ci >= trt_effect)
  
  # Return the confidence intervals as a data frame
  return(data.frame(
    boot_coverage = boot_coverage,
    cluster_coverage = cluster_coverage)
  )
}

# Perform simulations using replicate()
bootstrap_results <- replicate(n = 1000, expr = replicate_bootstrap(), simplify = FALSE)

output <- do.call(rbind, bootstrap_results)

# Seems like they have a better coverageg

mean(output$boot_coverage) # 0.947
mean(output$cluster_coverage) # 0.715

# Scenario 2 ----

# One measured confounder (c1) and one unmeasured confounder (c2)

# Define a function to calculate the statistic of interest
bootstrap_statistic_ipw <- function(data, indices) {
  
  library(tidyverse)
  library(boot)
  source("R/ipw_stab.R")
  
  # Subset the data using the bootstrap indices
  boot_data <- data[indices, ]
  
  # Fit the outcome model on the bootstrap sample
  df.w_bootstrap <- ipw_stab(boot_data, boot_data$trt, boot_data$c1)
  
  mod_outcome_bootstrap <- glm(
    formula = y ~ trt,
    data = df.w_bootstrap,
    family = gaussian(link = "identity"),
    weights = df.w_bootstrap$weights
  )
  
  # Extract and return the parameter of interest, e.g., coefficient of 'trt'
  coef(mod_outcome_bootstrap)['trt']
}

# Define the replicate_bootstrap function
replicate_bootstrap <- function() {
  
  # Treatment effect
  
  trt_effect <- 2
  
  # Simulate data as you did before
  c1 <- rnorm(n = n.sim, mean = 10, sd = 2)
  u1 <- rnorm(n = n.sim, mean = 5, sd = 2)
  trt <- rbinom(n = n.sim, size = 1, prob = plogis(0.2 + 0.05 * c1 + 0.05*u1))
  y <- trt_effect * trt + 3 * c1 + 2*u1 + rnorm(n = n.sim, mean = 0, sd = 2)
  
  df <- data.frame(c1 = c1, trt = trt, y = y)
  
  # Perform bootstrapping
  boot_results <- boot(data = df, statistic = bootstrap_statistic_ipw, R = 100)
  
  # Access the bootstrap results
  bootstrap_coefs <- boot_results$t
  bootstrap_ci <- boot::boot.ci(boot_results, type = "perc")
  
  # Extract and format the confidence intervals
  ci_lower <- bootstrap_ci$percent[,4]
  ci_upper <- bootstrap_ci$percent[,5]
  
  boot_coverage <- all(ci_lower <= trt_effect & ci_upper >= trt_effect)
  
  # Cluster Robust 
  
  mod_outcome <- glm(
    formula = y ~ trt,
    data = df,
    family = gaussian(link = "identity"),
    weights = df.w$weights
  )
  
  library(sandwich)
  
  #cluster_se <- sqrt(diag(vcovCL(mod_outcome, cluster = df.w$c1)))
  
  # Using Sandwich (Robust) Variance Estimator instead of Cluster Robust
  
  cluster_se <- sqrt(diag(vcovHC(mod_outcome, type = "HC3")))
  
  # View cluster-robust standard errors
  
  cluster_lower_ci <- mod_outcome$coefficients[2] - 1.96 * cluster_se
  cluster_upper_ci <- mod_outcome$coefficients[2] + 1.96 *cluster_se
  
  #cluster_coverage <- between(trt_effect, cluster_lower_ci, cluster_upper_ci)
  
  cluster_coverage <- all(cluster_lower_ci <= trt_effect & cluster_upper_ci >= trt_effect)
  
  # Return the confidence intervals as a data frame
  return(data.frame(
    boot_coverage = boot_coverage,
    cluster_coverage = cluster_coverage)
  )
}

# Perform simulations using replicate()
bootstrap_results <- replicate(n = 1000, expr = replicate_bootstrap(), simplify = FALSE)

output <- do.call(rbind, bootstrap_results)

# Seems like they have a better coverageg

mean(output$boot_coverage) # 0.871
mean(output$cluster_coverage) # 0.591

# Scenario 3 ----

# One measured confounder (c1) and one unmeasured confounder (c2)

# Define a function to calculate the statistic of interest
bootstrap_statistic_ipw <- function(data, indices) {
  
  library(tidyverse)
  library(boot)
  source("R/ipw_stab.R")
  
  # Subset the data using the bootstrap indices
  boot_data <- data[indices, ]
  
  # Fit the outcome model on the bootstrap sample
  df.w_bootstrap <- ipw_stab(boot_data, boot_data$trt, boot_data$c1)
  
  mod_outcome_bootstrap <- glm(
    formula = y ~ trt + c1,
    data = df.w_bootstrap,
    family = gaussian(link = "identity"),
    weights = df.w_bootstrap$weights
  )
  
  # Extract and return the parameter of interest, e.g., coefficient of 'trt'
  coef(mod_outcome_bootstrap)['trt']
}

# Define the replicate_bootstrap function
replicate_bootstrap <- function() {
  
  # Treatment effect
  
  trt_effect <- 2
  
  # Simulate data as you did before
  c1 <- rnorm(n = n.sim, mean = 10, sd = 2)
  trt <- rbinom(n = n.sim, size = 1, prob = plogis(0.2 + 0.05 * c1 + 0.05*u1))
  y <- trt_effect * trt + 3 * c1 + rnorm(n = n.sim, mean = 0, sd = 2)
  
  df <- data.frame(c1 = c1, trt = trt, y = y)
  
  # Perform bootstrapping
  boot_results <- boot(data = df, statistic = bootstrap_statistic_ipw, R = 100)
  
  # Access the bootstrap results
  bootstrap_coefs <- boot_results$t
  bootstrap_ci <- boot::boot.ci(boot_results, type = "perc")
  
  # Extract and format the confidence intervals
  ci_lower <- bootstrap_ci$percent[,4]
  ci_upper <- bootstrap_ci$percent[,5]
  
  boot_coverage <- all(ci_lower <= trt_effect & ci_upper >= trt_effect)
  
  # Cluster Robust 
  
  mod_outcome <- glm(
    formula = y ~ trt + c1,
    data = df,
    family = gaussian(link = "identity"),
    weights = df.w$weights
  )
  
  library(sandwich)
  
  #cluster_se <- sqrt(diag(vcovCL(mod_outcome, cluster = df.w$c1)))
  
  # Using Sandwich (Robust) Variance Estimator instead of Cluster Robust
  
  cluster_se <- sqrt(diag(vcovHC(mod_outcome, type = "HC3")))
  
  # View cluster-robust standard errors
  
  cluster_lower_ci <- mod_outcome$coefficients[2] - 1.96 * cluster_se
  cluster_upper_ci <- mod_outcome$coefficients[2] + 1.96 *cluster_se
  
  #cluster_coverage <- between(trt_effect, cluster_lower_ci, cluster_upper_ci)
  
  cluster_coverage <- all(cluster_lower_ci <= trt_effect & cluster_upper_ci >= trt_effect)
  
  # Return the confidence intervals as a data frame
  return(data.frame(
    boot_coverage = boot_coverage,
    cluster_coverage = cluster_coverage)
  )
}

# Perform simulations using replicate()
bootstrap_results <- replicate(n = 1000, expr = replicate_bootstrap(), simplify = FALSE)

output <- do.call(rbind, bootstrap_results)

# Seems like they have a better coverageg

mean(output$boot_coverage) # 0.952
mean(output$cluster_coverage) # 0.356

# Scenario 4 ----




