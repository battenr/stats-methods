# Title: Estimating variance with PS-based methods

# Description: Demonstrating two different ways to estimate variance 
# after using PS-based methods. 

# Setup ----

library(tidyverse) # ol faithful 
library(broom) # tidying the model results
library(sandwich) # for robust variance estimator 
library(WeightIt) # for IPTW
library(tidymodels) # using for bootstrapping

# Generate Data ----

set.seed(456) # setting seed for reproducibility

n <- 500 # sample size (arbitrary)

df <- data.frame(
  c1 = rnorm(n = n, mean = 5, sd = 2), # a continuous confounder
  c2 = rbinom(n = n, size = 1, prob = 0.4) # a binary confounder
) %>% 
  dplyr::mutate(
    x = rbinom(n = n, size = 1, prob = plogis(0.05*c1 + 0.01*c1)), # binary treatment
    y = 2 + 3*x + 0.5*c1 + 1.5*c2 + rnorm(n) # continuous outcome
  )

# Inverse Probability of Treatment Weighting ----

# Fitting the propensity score model. Including both of the confounders here. 

ps_model <- WeightIt::weightit(x ~ c1 + c2, 
                               data = df, 
                               method = "ps")

# Outcome Model ----

# Fitting the outcome model 

mod <- stats::glm(
  y ~ x, 
  family = gaussian(link = "identity"),
  data = df,
  weights = ps_model$weights # weights from the previous model 
)


# Estimating Variance ----

#... Bootstrapping ----

# Function for calculating the propensity score model and then the outcome model. 
# Personally, I fit the PS model again in each replicate (similar to replicate 
# weights in surveys) but open to discussion on why/why not. 

ps_glm <- function(split) {
  df <- rsample::analysis(split) # need to do based on how the bootstraps are 
  # formatted from the bootstraps function
  
  # Fitting PS model 
  
  ps_model <- WeightIt::weightit(x ~ c1 + c2, 
                                 data = df, 
                                 method = "ps")
  
  # Fitting outcome model and tidying. 
  
  stats::glm(y ~ x, 
      data = df, 
      weights = ps_model$weights) %>% 
    tidy()
}

# Bootstrapping

boot_results <- rsample::bootstraps(df, 1000, apparent = TRUE) %>% # 1000 bootstraps (arbitrary for this example)
  # if using bootstrapping suggest reviewing references on how to choose the number of replicates required. 
 
  # Applying our function above to the data. 
  
   mutate(results = map(splits, 
                       ps_glm))

# Calculate the CIs (using 95% CIs)

boot_cis <- int_pctl(boot_results, results)[2,] %>% # calculating the CIs based on percentiles. 
  # note: this inherently has some assumptions. 
  
  # Reformatting and adding the type of CI (for later)
  
  rename(lower = .lower, 
         estimate = .estimate, 
         upper = .upper) %>% 
  select(lower, 
         #estimate, 
         upper) %>% 
  mutate(
    type = "Bootstrapping",
    estimate = tidy(mod)[2, 2] %>% as.numeric() # taking estimate from the GLM model. 
  )

#... Robust Variance Estimation ----

# Estimating variance using the robust variance estimator (aka Huber "Sandwich" Estimator)

?sandwich::vcovCL()

?sandwich::vcovHC

robust_var <- sandwich::vcovHC(mod, type = "HC1") # estimating the variance-covariance matrix
robust_se <- sqrt(diag(robust_var)) # if we take the square root of the diagonal of the matrix, we 
# will get our standard error 

trt_robust_se <- robust_se[2] %>% as.numeric() # getting the SE for the treatment

trt_estimate <- tidy(mod)[2, 2] %>% as.numeric() # getting estimate from the model previously

# Robust Confidence Intervals 

# Calculating the CIs as 95% CIs (hence the 1.96) 

robust_ci <- data.frame(
  lower = trt_estimate - 1.96*trt_robust_se, 
  estimate = trt_estimate, 
  upper = trt_estimate + 1.96*trt_robust_se,
  type = "Robust Variance\n Estimator"
)

#... Combining into one dataframe ----

df_ci <- rbind(
  boot_cis, 
  robust_ci
)

# Plotting! ----

# Creating a plot like a mini forest plot to show the different CIs for the different
# methods. 

ggplot(df_ci,
       aes(x = type, y = estimate)) +
  
  # Adding the estimates as points. 
  
  geom_point(
    size = 4, 
    shape = 21, 
    color = "hotpink", 
    fill = "hotpink") + 
  
  # Adding standard error bars. The width is set to 0 since I don't want it to 
  # look like a star wars tie fighter. 
  
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    linewidth = 2.5,
    width = 0, 
    color = "hotpink") +  
  
  
  coord_flip() +  # Flip coordinates for horizontal plot
  
  # Minimal theme
  
  theme_minimal() + 
  
  # Adding labels for axes. 
  
  labs(
    title = "Estimating Variance After IPTW",
    subtitle = "Robust Variance Estimator and Bootstrapping (N = 500)",
    y = "Estimate (with 95% CI)",
    x = "Method to Estimate Variance") +
  
  # Adding limits. However here this is really based on this particular example. 
  # If you re-run you may need to adjust accordingly. 
  
  lims(y = c(2.5, 3.5)) + 
  
  # 
  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    text = element_text(size = 20),
    axis.text = element_text(color = "black")) 

