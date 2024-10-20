# Title: Bayesian Parametric G-Formula as a Sensitivity Analysis

# Description: This code is for a presentation that I gave at Bayes Pharma 2024. 
# It discusses using the Bayesian parametric g-formula as a sensitivity analysis. 

# This particular script focuses on frequentist methods. Two commonly used ones are
# IPTW and the G-formula. 


# Setup ----

source("inst/bayes2024/00_simulating_data.R") # only need to run this once. 
library(boot)

# IPTW  ----

# Inverse probability of treatment weighting. This section uses IPTW to estimate the 
# treatment effect. Stabilized weights are used to estimate the ATE 

#... Propensity Score Model 

ipw <- WeightIt::weightit(x ~ l1 + l2, # including both confounders. Assuming we select the correct variables
                          data = df, 
                          method = "glm", 
                          estimand = "ATE", 
                          stabilize = TRUE) # using stabilized weights 

#... Outcome Model 

# Note, this uses M-estimation (just for ease/time). Alternatives to estimate the variance 
# are bootstrapping and sandwich estimators. Doubly robust methods weren't used for this. 

out.mod <- WeightIt::glm_weightit(y ~ x, 
                                  family = gaussian(), 
                                  weights = ipw$weights,
                                  data = df
)

broom::tidy(out.mod) # results from IPW 

# 0.160 (0.157)

# Parametric G-Formula ----

#... Fitting a model with the data 

mod <- glm(y ~ x + l1 + l2, 
           family = gaussian(),
           data = df
)

#... Using Model to Predict Outcomes 

# This code doesn't need to be ran but gives an example of how the g-formula works. 
# It is currently commented out so it's not run accidentally. 

# # When x = 1 
# 
# df1 <- df %>% 
#   dplyr::select(l1, l2) %>% 
#   mutate(x = 1)
# 
# df1$y1 <- predict(mod, newdata = df1) # predicting outcome 
# 
# # When x = 0 
# 
# df0 <- df %>% 
#   dplyr::select(l1, l2) %>% 
#   mutate(x = 0)
# 
# 
# df0$y0 <- predict(mod, newdata = df0) # predicting outcome

#.... Predicting the Average Treatment Effect  ----

# Function, altered from the above link for our use 

standardization <- function(data, indices) {
  
  # We need to first make the datasets that we need
  
  # 1st copy: our original
  
  d <- data[indices, ] 
  
  # 2nd copy: Same as original but with group = 0 and outcome as missing
  
  d0 <- d 
  d0$x <- 0 # setting group to 0
  d0$y <- NA # setting plant length (our outcome) to missing 
  
  # 3rd copy: Same as original but with group = 1 and outcome as missing
  
  d1 <- d 
  d1$x <- 1 # setting group to 1
  d1$y <- NA # setting plant length (our outcome) to missing 
  
  # Making one sample
  
  d.onesample <- rbind(d, d0, d1) # combining datasets
  
  # Fitting a model for each iteration
  
  fit <- glm(
    y ~ x + l1 + l2, 
    data = d.onesample
  )
  
  # Using model to predict the outcome
  
  d.onesample$predicted_meanY <- predict(fit, d.onesample)
  
  # Calculate the mean for each of the groups. The third calculation is for the difference in group O (0) vs group A (1)
  
  return(c(
    mean(d.onesample$predicted_meanY[d.onesample$x == 0]),
    mean(d.onesample$predicted_meanY[d.onesample$x == 1]),
    # Treatment - No Treatment
    
    mean(d.onesample$predicted_meanY[d.onesample$x == 1]) -
      mean(d.onesample$predicted_meanY[d.onesample$x == 0])
  ))
}


# Now we can bootstrap this statistic using our dataset

results <- boot(data = df,
                statistic = standardization,
                R = 1000)

# Using the bootstrapped sample (titled result), we can calculate the confidence intervals. We take the standard deviation of the sampling distribution. This will give us our standard error

se <- c(sd(results$t[, 1]),
        sd(results$t[, 2]),
        sd(results$t[,3]))




mean <- results$t0 # calculate mean 
ll <- mean - qnorm(0.975) * se # lower limits
ul <- mean + qnorm(0.975) * se # upper limits

finalresults <- data.frame(
  result_title = c("X = O", "X = 1", "Difference"),
  mean = round(mean,3),
  ci = paste0("95% CI: ", round(ll, 3), " - ", round(ul, 3))
)

finalresults 

tidy(out.mod)

mod


