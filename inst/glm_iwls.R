# Title: GLM using IWLS

# Following this 
# https://rstudio-pubs-static.s3.amazonaws.com/440467_2d88892e381b45e4bff327fcaab98b61.html

# Setup ----

#... Libraries ----

library(tidyverse)

#... Dependencies ----

# Simulated Data ----

df <- data.frame(
  ae = rbinom(1000, size = 1, prob = 0.66),
  bzd = rbinom(1000, size = 1, prob = 0.72),
  sex = rbinom(1000, size = 1, prob = 0.81),
  age = runif(1000, min = 18, max = 90),
  ed = sample(x = c("BSc", "HS", "MSc", "PhD"), size = 1000, replace = TRUE,
              prob = c(0.40, 0.30, 0.20, 0.10))
)

# Broken down by using functions ----

#... Weighted Linear Model Function ----

weighted_lm <- function(X,y,obs_var){
  #obs_var is a vector of variances for each observation
  
  X <- cbind(1,X)       #Add intercept to the X matrix
  D <- diag(1/obs_var)  #create the D matrix
  lhs <- t(X)%*%D%*%X           #CHANGE THIS
  rhs <- t(X)%*%D%*%y           #CHANGE THIS
  return(solve(lhs,rhs)) #compute inverse(X^T D X)*t(X)%*%D%*% y
}

beta <- c(0,0)

#... Calculate Proportion ----

calculate_prop <- function(x,beta){
  X <- cbind(1,x)
  p <- exp(X%*%beta)/(1+exp(X%*%beta)) # CHANGE THIS so that this returns p_i (for step 2)
  return(p)
}

#... Calculate Variance ----

calculate_var <- function(p){
  V <- p*(1-p) # CHANGE THIS so that this returns V_i given p_i (for step 3)
  return(V)
}

#... Calculate Expected Response ----

calculate_expected_resp <- function(x,beta,y,p,V){
  X <- cbind(1,x)
  z <- X%*%beta+(y-p)/V # CHANGE THIS so that this returns the expected responses (step 4)
  return(z)
}

#... IRLS ----

did_we_converge <- function(beta0,beta1,conv.eps){
  sum((beta0-beta1)^2)<=conv.eps
}

IRLS <- function(X,y,max.iter=100,conv.eps=1e-12){
  beta <- rep(0,ncol(cbind(1,X))) #initialize beta
  beta_prev <- beta               #beta_{t-1} (for comparisons)
  
  for(iter in 1:max.iter){
    p <- calculate_prop(X,beta) %>% as.numeric
    V <- calculate_var(p) %>% as.numeric
    z <- calculate_expected_resp(X,beta,y,p,V) %>% as.numeric
    beta <- weighted_lm(X,z,1/V)
    if(did_we_converge(beta_prev,beta,conv.eps)){
      break
    } else {
      beta_prev <- beta
    }
  }
  return(beta)
}
  

# Compare glm function to IRLS ----

#... Using GLM function ----

mod <- glm(
  ae ~ bzd + sex + age + ed,
  family = binomial(link = "logit"),
  data = df
)

summary(mod)

#... Using functions created ----

IRLS(df[,2], df[,1]) # bzd = 0.005007 using glm, 0.03033948 using our functions
IRLS(df[,3], df[,1]) # sex = -0.232271 using glm, -0.2390645 using our functions
IRLS(df[,4], df[,1]) # age = 0.004788 using glm, 0.004588011 using our functions


head(df)

