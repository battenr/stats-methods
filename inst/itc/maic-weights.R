# Title: MAIC Weights

# Description: Working through code provided by Phillipo et al. 2020 articles
# that shows that method of moments weights and entropy balancing are equivalent for MAICs

# Script is here: ebal-mom-maic

# Setup ----

#... Libraries ----

library(tidyverse)
library(sandwich)
library(matrixStats)

#... Functions ----

# Load all functions

lapply(list.files("R/"), function(x) source(paste0("R/", x)))

#... Dependencies ----

# Functions for Deriving Weights ----

# Need two functions: an objective function and a gradient function

#... MAIC Weights ----

# using the method of moments 


#... Entropy Balancing ----


###############################################################################
# Equivalence of entropy balancing and the method of moments for 
# matching-adjusted indirect comparison
#    David M. Phillippo, Sofia Dias, A.E. Ades, Nicky J. Welton
#
# -----------------------------------------------------------------------------
#
# This code implements standard MAIC (method of moments), entropy balancing 
# MAIC with uniform base weights, and entropy balancing MAIC with non-uniform 
# base weights. These are then applied to a simple example (taken from NICE 
# TSD 18) to demonstrate.
# 
###############################################################################


# MAIC code ---------------------------------------------------------------

if (!require(sandwich)) {install.packages("sandwich"); library(sandwich)}
if (!require(matrixStats)) {install.packages("matrixStats"); library(matrixStats)}

# The following functions implement the objective and gradient functions for
# method of moments and entropy balancing forms of MAIC. They can be passed
# to a suitable numerical optimisation routine to minimise over the first 
# parameter a1. The matrix X should be a matrix of IPD covariates *centered* at
# the covariate moments in the aggregate population.

## Standard (method of moments) MAIC --------------------------------------

# Objective function
objfn_mm <- function(a1, X){
  return(sum(exp(X %*% a1)))
}

# Gradient function
gradfn_mm <- function(a1, X){
  return(colSums(sweep(X, 1, exp(X %*% a1), "*")))
}


## Entropy balancing MAIC (uniform base weights) --------------------------

objfn_eb <- function(a1, X){
  # Use numerically stable log sum exp, equivalent to
  # log(mean(exp(X %*% a1)))
  return(logSumExp(X %*% a1) - log(nrow(X)))
}

gradfn_eb <- function(a1, X){
  eXa1 <- exp(X %*% a1)
  return(crossprod(X, eXa1 / sum(eXa1)))
}


## Entropy balancing MAIC (non-uniform base weights) ----------------------

# The argument w0 is a vector of prior weights (of length equal to nrow(X)).

objfn_eb0 <- function(a1, X, w0){
  # Use numerically stable log sum exp, equivalent to
  # log(sum(w0 * exp(X %*% a1)))
  return(logSumExp(X %*% a1 + log(w0)))
}

gradfn_eb0 <- function(a1, X, w0){
  eXa1 <- w0 * exp(X %*% a1)
  return(crossprod(X, eXa1 / sum(eXa1)))
}


# Example -----------------------------------------------------------------

# We apply the methods to a simulated example from NICE TSD 18.

## Simulated data sets ----------------------------------------------------

if (!require(dplyr)) {install.packages("dplyr"); library(dplyr)}
if (!require(tidyr)) {install.packages("tidyr"); library(tidyr)}
if (!require(wakefield)) {install.packages("wakefield"); library(wakefield)}
if (!require(ggplot2)) {install.packages("ggplot2"); library(ggplot2)}

set.seed(61374988)

# Study characteristics
N_AB <- 500
N_AC <- 300
agerange_AB <- 45:75
agerange_AC <- 45:55
femalepc_AB <- 0.64
femalepc_AC <- 0.8

# Outcome model
b_0 <- 0.85
b_gender <- 0.12
b_age <- 0.05
b_age_trt <- -0.08
b_trt_B <- -2.1
b_trt_C <- -2.5


### Generate AB trial
AB_IPD <- 
  rbind(
    
    # Generate A arm
    r_data_frame(n = N_AB/2,     # Number of individuals in arm A
                 id,             # Unique ID
                 age = age(x = agerange_AB),   # Generate ages
                 gender = gender(prob = c(1 - femalepc_AB, femalepc_AB)), # Generate genders
                 trt = "A"       # Assign treatment A
    ),
    
    # Generate B arm
    r_data_frame(n = N_AB/2,     # Number of individuals in arm B
                 id,             # Unique ID
                 age = age(x = agerange_AB),   # Generate ages
                 gender = gender(prob = c(1 - femalepc_AB, femalepc_AB)), # Generate genders
                 trt = "B"       # Assign treatment B
    )
  ) %>%
  
  # Generate outcomes using logistic model
  mutate(
    yprob = 1 / (1 + exp(-(
      b_0 + b_gender * (gender == "Male") + b_age * (age - 40) + 
        if_else(trt == "B", b_trt_B + b_age_trt * (age - 40), 0)
    ))),
    y = rbinom(N_AB, 1, yprob)
  ) %>%
  select(-yprob)   # Drop the yprob column

# Tabulate
AB_IPD %>% group_by(trt) %>%
  summarise(n(), mean(age), sd(age), `n(male)` = sum(gender == "Male"), 
            `%(male)` = mean(gender == "Male"), sum(y), mean(y))


### Generate AC trial
AC_IPD <- 
  rbind(
    
    # Generate A arm
    r_data_frame(n = N_AC/2,     # Number of individuals in arm A
                 id,             # Unique ID
                 age = age(x = agerange_AC),   # Generate ages
                 gender = gender(prob = c(1 - femalepc_AC, femalepc_AC)), # Generate genders
                 trt = "A"       # Assign treatment A
    ),
    
    # Generate C arm
    r_data_frame(n = N_AC/2,     # Number of individuals in arm C
                 id,             # Unique ID
                 age = age(x = agerange_AC),   # Generate ages
                 gender = gender(prob = c(1 - femalepc_AC, femalepc_AC)), # Generate genders
                 trt = "C"       # Assign treatment C
    )
  ) %>%
  
  # Generate outcomes using logistic model
  mutate(
    yprob = 1 / (1 + exp(-(
      b_0 + b_gender * (gender == "Male") + b_age * (age - 40) + 
        if_else(trt == "C", b_trt_C + b_age_trt * (age - 40), 0)
    ))),
    y = rbinom(N_AC, 1, yprob)
  ) %>%
  select(-yprob)   # Drop the yprob column

# Tabulate
AC_IPD %>% group_by(trt) %>%
  summarise(n(), mean(age), sd(age), `n(male)` = sum(gender == "Male"), 
            `%(male)` = mean(gender == "Male"), sum(y), mean(y))

# Create aggregate data
AC_AgD <- 
  cbind(
    # Trial level stats: mean and sd of age, number and proportion of males
    summarise(AC_IPD, age_mean = mean(age), age_sd = sd(age), 
              N_male = sum(gender == "Male"), prop_male = mean(gender == "Male")),
    
    # Summary outcomes for A arm
    filter(AC_IPD, trt == "A") %>% 
      summarise(y_A_sum = sum(y), y_A_bar = mean(y), N_A = n()),
    
    # Summary outcomes for C arm
    filter(AC_IPD, trt == "C") %>% 
      summarise(y_C_sum = sum(y), y_C_bar = mean(y), N_C = n())
  )

AC_AgD


## Calculate weights ------------------------------------------------------

# Centred EMs
X_EM_0 <- sweep(with(AB_IPD, cbind(age, age^2)), 2, 
                with(AC_AgD, c(age_mean, age_mean^2 + age_sd^2)), '-')

# Estimate weights

# Method of moments
(opt_mm <- optim(par = c(0,0), fn = objfn_mm, gr = gradfn_mm, X = X_EM_0, method = "L-BFGS-B"))
a1_mm <- opt_mm$par
wt_mm <- exp(X_EM_0 %*% a1_mm)
wt_mm_rs <- (wt_mm / sum(wt_mm)) * N_AB   # rescaled weights
sum(wt_mm)^2 / sum(wt_mm^2)   # effective sample size

# Entropy balancing (uniform base weights)
(opt_eb <- optim(par = c(0,0), fn = objfn_eb, gr = gradfn_eb, X = X_EM_0, method = "L-BFGS-B"))
a1_eb <- opt_eb$par
wt_eb <- exp(X_EM_0 %*% a1_eb)
wt_eb_rs <- (wt_eb / sum(wt_eb)) * N_AB   # rescaled weights
sum(wt_eb)^2 / sum(wt_eb^2)   # effective sample size

# Compare MM and EB weights
ggplot(aes(x = wt_mm, y = wt_eb),
       data = tibble(wt_mm, wt_eb)) + 
  geom_abline(slope = 1, colour = "grey60") +
  geom_point()

all.equal(wt_mm, wt_eb)
# Equal to within tolerance of numerical optimisation routine

# Summary of weights, histogram
summary(wt_mm_rs)
qplot(wt_mm_rs, geom = "histogram", 
      xlab = "Rescaled weight (multiple of original unit weight)", 
      binwidth = function(x) diff(range(x)) / nclass.Sturges(x),
      boundary = 0)

summary(wt_eb_rs)
qplot(wt_eb_rs, geom = "histogram", 
      xlab = "Rescaled weight (multiple of original unit weight)", 
      binwidth = function(x) diff(range(x)) / nclass.Sturges(x),
      boundary = 0)

# Entropy balancing (non-uniform base weights)

# We randomly assign a vector of base weights w0 - in reality, these would be
# provided by a prior weighting method
w0 <- rbeta(N_AB, 2, 5)
w0_rs <- w0 / sum(w0) * N_AB

(opt_eb0 <- optim(par = c(0,0), fn = objfn_eb0, gr = gradfn_eb0, X = X_EM_0, w0 = w0, method = "L-BFGS-B"))
a1_eb0 <- opt_eb0$par
wt_eb0 <- w0 * exp(X_EM_0 %*% a1_eb0)
wt_eb0_rs <- (wt_eb0 / sum(wt_eb0)) * N_AB   # rescaled weights
sum(wt_eb0)^2 / sum(wt_eb0^2)   # effective sample size

# Compare base weights with final weights
ggplot(aes(x = w0, y = wt_eb0),
       data = tibble(w0, wt_eb0)) +
  geom_abline(slope = 1, colour = "grey60") +
  geom_point()

ggplot(aes(x = wt, fill = type),
       data = bind_rows(tibble(wt = w0_rs, type = "Initial"),
                        tibble(wt = wt_eb0_rs, type = "Final"))) +
  geom_histogram(alpha = 0.5, position = "identity", boundary = 0,
                 binwidth = function(x) diff(range(x)) / nclass.Sturges(x))

## Calculate indirect comparison ------------------------------------------

# Create the weighting estimator using a simple linear model, use sandwich
# estimator for standard error. Here we just use the EB weights for
# demonstration.

# Binomial GLM
fit1 <- glm(cbind(y, 1 - y) ~ trt + gender, data = AB_IPD, family = binomial, weights = wt_eb)

# Sandwich estimator of variance matrix
V_sw <- vcovHC(fit1)

# The log OR of B vs. A is just the trtB parameter estimate
print(d_AB_MAIC <- unname(coef(fit1)["trtB"]))
print(var_d_AB_MAIC <- V_sw["trtB", "trtB"])

# Estimated log OR of C vs. A from the AC trial
d_AC <- with(AC_AgD, log(y_C_sum * (N_A - y_A_sum) / (y_A_sum * (N_C - y_C_sum))))
var_d_AC <- with(AC_AgD, 1/y_A_sum + 1/(N_A - y_A_sum) + 1/y_C_sum + 1/(N_C - y_C_sum))

# Indirect comparison of C vs. B in AC trial
print(d_BC_MAIC <- d_AC - d_AB_MAIC)
print(var_d_BC_MAIC <- var_d_AC + var_d_AB_MAIC)

