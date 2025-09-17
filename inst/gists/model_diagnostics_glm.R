# Title: Model Diagnostics - GLM

# Description: Demonstrating how we can explore model diagnostics 
# using simulated data. For this specific use case, we are going 
# to explore diagnostics for a generalized linear model (glm). 

# Setup ----

#... Libraries ----

library(tidyverse) # ol' faithful
library(performance) # model diagnostics 
library(patchwork) # combining plots

#... Functions ----

# Simulating Data 
# - Two confounders: one binary, one continuous
# - Binary exposure
# - Continuous outcome 

sim_data <- function(n = 250, # sample size 
                     beta_trt = 1.5, # treatment effect
                     # Parameters for Z1 
                     z1_mean = 5, z1_sd = 2, 
                     # Parameters for Z2
                     z2_size = 1, z2_prob = 0.5, 
                     # Confounder - Effect on X
                     z1_on_x = 0.05, z2_on_x = 0.2,
                     # Confounder - Effect on Y
                     z1_on_y = 0.5, z2_on_y = 0.3){
  
  # Creating the Dataframe
  
  df <- data.frame(
    z1 = rnorm(n = n, mean = z1_mean, sd = z1_sd), 
    z2 = rbinom(n = n, size = z2_size, prob = z2_prob),
    z3 = rnorm(n = n, mean = 10, sd = 5)
  ) %>% 
    dplyr::mutate(
      prob = plogis(z1_on_x*z1 + z2_on_x*z2), 
      x = rbinom(n = n, size = 1, prob = prob), 
      y = beta_trt*x + z1_on_y*z1 + z2_on_y*z2 + rnorm(n = n, mean = 0, sd = 1)
    )
  
  # Return the dataframe
  
  return(df)
}

# Simulating Data ----

set.seed(456) # for reproducibility 

df <- sim_data()

# Fitting Models ----

#... Model 1 - Correct Model ----

mod1 <- glm(y ~ x + z1 + z2 ,
            family = gaussian(link = "identity"), 
            #family = Gamma(),
            data = df)


#... Model 2 - Incorrect Model ----

mod2 <- glm(y ~ x + z2^3 + x*z3,
           family = gaussian(link = "inverse"), 
           #family = Gamma(),
           data = df)

# Model Diagnostics ----

# Using the performance package for:
# - Posterior Predictive Check
# - Influential Observations
# - Collinearity

# Additionally will check the residuals vs predicted value

#... Model 1 ----

# PP Check, Outliers & VIF

check_model(mod1,
            check = c("pp_check", 
                      "outliers", 
                      "vif"),
            #residual_type = "deviance" # not needed for this example
            title_size = 24, 
            base_size = 22
)

# Residuals vs Predicted 

# Checking residuals as Well. Here you could use the "linearity" 
# function in check_model() and ignore the horizontal line (looking for
# if the residuals are dispersed or not). Instead, here going to calculate and plot
# ourselves 

# Couple of things to note from this: 
# 1. The points are scattered around seemingly random
# 2. Look at the x axis. That range is closer to our data 
# summary(df$y) and check min max. In the misspecified version, 
# the linear predictor is way off 

df_resid <- data.frame(
  resid = stats::residuals(mod1, "deviance"), # using deviance residuals
  predicted = stats::predict(mod1)
)

ggplot(data = df_resid, 
       mapping = aes(x = predicted, y = resid)) + 
  geom_point(size = 2) + 
  theme_minimal() + 
  labs(x = "Linear Predictor", y = "Residuals (deviance residuals)") + 
  ggtitle("Predicted vs Residual Values") + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 24),
    text = element_text(size = 22)
  )

#... Model 2 ----

# PP Check, Outliers & VIF

check_model(mod2,
            check = c("pp_check", 
                      "outliers", 
                      "vif"),
            #residual_type = "deviance" # not needed for this example
            title_size = 24, 
            base_size = 22
)

# Residuals vs Predicted 

# Checking residuals as Well. Here you could use the "linearity" 
# function in check_model() and ignore the horizontal line (looking for
# if the residuals are dispersed or not). Instead, here going to calculate and plot
# ourselves 

# Couple of things to note from this: 
# 1. The points are scattered around seemingly random
# 2. Look at the x axis. That range is closer to our data 
# summary(df$y) and check min max. In the misspecified version, 
# the linear predictor is way off 

df_resid <- data.frame(
  resid = stats::residuals(mod2, "deviance"), # using deviance residuals
  predicted = stats::predict(mod2)
)

ggplot(data = df_resid, 
       mapping = aes(x = predicted, y = resid)) + 
  geom_point(size = 2) + 
  theme_minimal() + 
  labs(x = "Linear Predictor", y = "Residuals (deviance residuals)") + 
  ggtitle("Predicted vs Residual Values") + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 24),
    text = element_text(size = 22)
  )

