# Title: Parametric G-Formula vs IPTW 

# Description: Comparing results from parametric g-formula to results from IPTW. 
# These methods model different things. The parametric g-formula models the outcome 
# while IPTW models the treatment. The goal of this code is to show how they differ, 
# but are not drastically different (in this example). 

# Setup ----

library(tidyverse) # ol faithful 
library(WeightIt) # for IPTW
library(boot) # for estimating SE using bootstrapping for standardization
library(tidymodels) # bootstrapping for IPTW 
library(broom) # tidying the model results

# Simulating Data  ----

set.seed(456) # setting seed for reproducibility

n = 500 # sample size. Arbitrarily chosen as 500. Note the sample size will 
# impact the standard error. 

# Simulating data
# - Two confounders: one binary, one continuous
# - Binary treatment
# - Continuous outcome 

df <- data.frame(
  c1 = rbinom(n = n, size = 1, prob = 0.45), # arbitrary probability of 0.45 
  c2 = rnorm(n = n, mean = 5, sd = 2) # continuous confounder
) %>% 
  dplyr::mutate(
    x = rbinom(n = n, size = 1, prob = plogis(c1 + 0.1*c2)), # binary treatment
    y = 3 + 0.5*c1 + 0.75*c2 + 1.5*x + rnorm(n = n) # continuous outcome
  )

# Inverse Probability of Treatment Weighting (IPTW) ----

# Fitting the propensity score model 

ps_mod <- WeightIt::weightit(x ~ c1 + c2, # including both confounders. 
                           data = df,
                           method = "glm", 
                           estimand = "ATE") # using the same estimand as the 
# parametric g-formula (ATE)

# Outcome model

# This outcome model uses the weights from the PS model. 

out_mod <- glm(y ~ x, 
               family = gaussian(link = "identity"), # continuous outcome
               data = df, 
               weights = ps_mod$weights)

#... Bootstrapping ----

#..... Setup for Bootstrapping ----

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

#..... Actual Bootstrapping ----

boot_results <- rsample::bootstraps(df, 1000, apparent = TRUE) %>% # 1000 bootstraps (arbitrary for this example)
  # if using bootstrapping suggest reviewing references on how to choose the number of replicates required. 
  
  # Applying our function above to the data. 
  
  mutate(results = map(splits, 
                       ps_glm))

# Calculate the CIs (using 95% CIs)

ipw_cis <- int_pctl(boot_results, results)[2,] %>% # calculating the CIs based on percentiles. 
  # note: this inherently has some assumptions. 
  
  # Reformatting and adding the type of CI (for later)
  
  rename(lower = .lower, 
         estimate = .estimate, 
         upper = .upper) %>% 
  select(lower, 
         #estimate, - don't want to select the estimate in this case 
         upper) %>% 
  mutate(
    type = "Bootstrapping",
    estimate = tidy(out_mod)[2, 2] %>% as.numeric() # taking estimate from the GLM model. 
  )

# Parametric G-Formula ----

# The basic idea of the parametric g-formula is to fit a model with the outcome. 
# This model is then used to predict values for the outcomes when the treatment is 0 
# for all individuals and 1 for all individuals. 

# Fitting model

mod <- glm(y ~ x + c1 + c2, 
           data = df)

# Predicting outcome when treatment status is 0 

df_x0 <- df %>% 
  mutate(x = 0, 
         ypred = predict(mod) # predicting the outcome based on x being 0 
  )

# Predicting outcome when treatment is 1 

df_x1 <- df %>% 
  mutate(x = 1, 
         ypred = predict(mod))

# To calculate the estimate, we'd take the difference when treatment is 1 and treatment is 0. 
# We could then take the average of these values. 

#... Bootstrapping ----

#..... Setup for Bootstrapping ----

# To estimate the CIs we need to use bootstrapping. Credit to Joy Shi, 
# Sean McGrath and Tom Palmer for making this R code freely available 
# from "Causal Inference: What If" by Hernan & Robins

standardization <- function(data, indices) {
  
  # We need to first make the datasets that we need
  
  # 1st copy: our original
  
  d <- data[indices, ] 
  
  # 2nd copy: Same as original but with group = 0 and outcome as missing
  
  d0 <- d 
  d0$x <- 0 # setting treatment to 0
  d0$y <- NA # setting our outcome to missing 
  
  # 3rd copy: Same as original but with group = 1 and outcome as missing
  
  d1 <- d 
  d1$x <- 1 # setting treatment to 1
  d1$y <- NA # setting our outcome to missing 
  
  # Making one sample
  
  d.onesample <- rbind(d, d0, d1) # combining datasets
  
  # Fitting a model for each iteration
  
  fit <- glm(
    y ~ x + c1 + c2,
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

#..... Actual Bootstrapping ----

# Now we can bootstrap this statistic using our dataset

results <- boot(data = df,
                statistic = standardization,
                R = 5)

# Using the bootstrapped sample (titled result), we can calculate the confidence intervals. 
# We take the standard deviation of the sampling distribution. 
# This will give us our standard error. 

se <- c(sd(results$t[, 1]),
        sd(results$t[, 2]),
        sd(results$t[,3]))


mean <- results$t0 # calculate mean 
ll <- mean - qnorm(0.975) * se # lower limits
ul <- mean + qnorm(0.975) * se # upper limits

g_formula_results <- data.frame(
  result_title = c("Group 0", "Group 1", "Difference"),
  mean = round(mean,3),
  ci = paste0("95% CI: ", round(ll, 3), " - ", round(ul, 3))
)

# Plotting! ----

#... Getting Necessary Data ----

# Data for Parametric G-Formula

lower_g <- ll[3]
est_g <- mean[3]
upper_g <- ul[3]

# Data for IPTW 

lower_ipw <- ipw_cis$lower
est_ipw <- ipw_cis$estimate
upper_ipw <- ipw_cis$upper

# Combining into one dataframe. 

df_ci <- data.frame(
  method = c("Parametric G-Formula", "IPTW"), 
  lower = c(lower_g, lower_ipw),
  estimate = c(est_g, est_ipw),
  upper = c(upper_g, upper_ipw)
)

#... Plot Time! ----

# Creating a plot like a mini forest plot to show the different CIs for the different
# methods. 

ggplot(df_ci,
       aes(x = method, y = estimate)) +
  
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
    title = "Estimating Treatment Effect",
    subtitle = "Parametric G-Formula and IPTW with Bootstrapping (N = 500)",
    y = "Estimate (with 95% CI)",
    x = "Method") +
  
  # Adding limits. However here this is really based on this particular example. 
  # If you re-run you may need to adjust accordingly. 
  
  lims(y = c(1, 2)) + 
  
  # 
  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    text = element_text(size = 20),
    axis.text = element_text(color = "black")) 
