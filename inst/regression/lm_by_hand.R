# Title:

# Setup ----

#... Libraries ----

library(tidyverse)

#... Dependencies ----

#... Pins ----

# Simulating Data ----

set.seed(2222)

df <- data.frame(
  age = runif(n = 748, min = c(5,50), max = c(40, 80)),
  sex = rbinom(n = 748, size = 1, prob = c(0.729, 0.569)),
  # Beta Coefficients
  beta_age = runif(n = 748, min = 0, max = 0.20),
  beta_sex = runif(n = 748, min = 0, max = 0.30)
) %>% 
  dplyr::mutate(
    prob_ice_cream_eater = (beta_age*age + beta_sex*sex)/20,
    ice_cream_eater = rbinom(n = 748, size = 1, prob = prob_ice_cream_eater)
  )

# Using LM Function in R ----

lm.fit <- lm(ice_cream_eater ~ age + sex, 
             data = df)

summary(lm.fit)

# Rewriting Code Myself -----

# Following:
# https://economictheoryblog.com/2016/02/20/rebuild-ols-estimator-manually-in-r/

#... 1.0 Need the matrices ----

# in this case, we need to add an intercept (the value 1), age and sex (the two 
# predictor variables)

X <- as.matrix(cbind(1, df$age, df$sex))

# matrix for the outcome variable, basically a vector

y <- as.matrix(df$ice_cream_eater)

#... 2.0 Estimate the coefficients (beta) ----

# We can solve for the coefficients using the following formula 
# from wikipedia (https://en.wikipedia.org/wiki/Ordinary_least_squares#Linear_model)
# Beta = (t(X)*X)^(-1)*t(X)*y
# note: t is the transpose
# %/% is matrix multiplcation

# Doing it step by step:

step.1 <- t(X)%*%X
step.2 <- solve(step.1) # solve will return the inverse of a
step.3 <- step.2%*%t(X)
step.4 <- step.3%*%y
beta <- step.4

# Alternatively, can do in one messy looking code: 

beta <- solve(t(X)%*%X)%*%t(X)%*%y

#... Residuals ----

# Calculating residuals as y-intercept-beta_1*X1-beta_2*X2

res <- as.matrix(y-beta[1]-beta[2]*X[,2]-beta[3]*X[,3])

# Note the above is really:
# res <- as.matrix(y-beta[1]-beta[n]*X[,n]) for as many n's as there are 

#... Number of Observations and Parameters ----

n <- nrow(df) # number of observations (in this case 748)
k <- ncol(X) # how many parameters? (aka how many variables, incl. intercept)

#... Variance-Covariance Matrix (VCV) ----

# Formula for VCV: (1/df)*t(res)*res*[t(X)(X)]^-1

VCV <- (1/(n-k))*as.numeric(t(res)%*%res)*solve(t(X)%*%X)

#... Calculate the Standard Error (SE) of the Coefficients ----

se <- sqrt(diag(VCV))

se

#... Calculate the P-Values ----

df = n-k

p_value <- rbind(
  2*pnorm(abs(beta[1]/se[1]), lower.tail = FALSE),
  2*pnorm(abs(beta[2]/se[2]), lower.tail = FALSE),
  2*pnorm(abs(beta[3]/se[3]), lower.tail = FALSE)
)

#... Output ----

output <- as.data.frame(
  cbind(
    c("Intercept", "Age", "Sex"),
    round(beta,7),
    round(se, 7),
    round(p_value, 3)
)
)

names(output) <- c(
  "Coefficients",
  "Estimate",
  "Std. Error",
  "Pr(>{Z}"
)

output

# Comparing By Hand to lm() function ----

summary(lm.fit)
output
