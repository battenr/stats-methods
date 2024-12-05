# Title: Multiple Imputation 

# Description: Showing very, very briefly how multiple imputation works. This was inspired by the 
# section on multiple imputation in Flexible Imputation of Missing Data by Stef van Buuren. 

# Setup ----

#... Libraries ---- 

library(tidyverse) # ol' faithful
library(mice) # for multiple imputation
library(broom) # for tidying results 

# Simulating Data ----

set.seed (456) # setting seed for reproducbility 

n = 250 # arbitrary sample size

# Simulating a dataset with a sample size of 250 patients. There are two covariates
# and a treatment variable (trt) with a continuous outcome. 

df <- data.frame(
  x1 = rnorm(n, mean = 50, sd = 10),
  x2 = rnorm(n, mean = 100, sd = 15),
  trt = rbinom(n, size = 1, prob = 0.5)
) %>% 
  dplyr::mutate(
  y = 0.5 * x1 - 0.2 * x2 + 1.5*trt + rnorm(n, mean = 0, sd = 5)
)

# Introduce Missingness ----

# Making missingness in x1 depend on the values of x2 (MAR)
df$x1[sample(1:n, size = 40, prob = ifelse(df$x2 > 100, 0.9, 0.1))] <- NA

# Perform Multiple Imputation ----

# Using the mice package with five iterations. In reality more thought should be 
# given to this. Additionally, using pmm but there are other options as well. 

imp <- mice::mice(df, m = 5, method = 'pmm', maxit = 10, seed = 123)

# Regression Model ---- 

# Fitting a GLM to each imputed dataset. 

imputed_models <- lapply(1:5, function(i) {
  stats::glm(y ~ trt + x1 + x2, 
             data = complete(imp, action = i))
})

#... Extract the estimates for trt from each model

estimates <- lapply(imputed_models, function(model) {
  tidy(model) %>% filter(term == "trt")
}) %>%
  bind_rows(.id = "imputation")

# Combining estimates ----

# We have estimates for the different imputations. To get our treatment effect we need to 
# estimate the overall effect. To do this, we combine the estimates using Rubin's rule. 

models <- with(imp, glm(y ~ trt + x1 + x2)) # fitting GLM using each dataset 

pooled_model <- mice::pool(models) # pooling the models together

combined_estimate <- summary(pooled_model) %>%
  
  # Taking only the trt term (aka the effect estimate)
  
  filter(term == "trt")

# Reformatting Data for Plotting ----

# Adding the combined estimate to the data including the standard errors. 


# First, creating a dataframe with the combined values. 

combined_row <- data.frame(
  imputation = "Overall",
  estimate = combined_estimate$estimate,
  std.error = combined_estimate$std.error,
  conf.low = combined_estimate$estimate - 1.96 * combined_estimate$std.error,
  conf.high = combined_estimate$estimate + 1.96 * combined_estimate$std.error
)

# Next, actually combine the results so that we have the results of the GLM for each 
# imputation and the "overall" result. 

# Combine the individual estimates with the overall estimate
plot_data <- bind_rows(estimates, combined_row)

# Plot It! ----

# Plotting the data from above. The idea is to make it sort of like a forest plot, showing 
# the result for each imputed dataset, then the combined overall version. 

ggplot(plot_data, 
       aes(x = estimate, 
           y = factor(imputation))) +
  
  # Adding points to show the effect size
  
  geom_point(size = 3,
             color = "hotpink") +
  
  # Using error bars to show the confidence interval for each result.  
  
  geom_errorbarh(aes(xmin = estimate - 1.96 * std.error, 
                     xmax = estimate + 1.96 * std.error), 
                 height = 0.2,
                 color = "hotpink") +
  
  # Adding a vertical line for the overall effect. This is similar to a forest plot but what I'm trying to show 
  # is how far away a particular individual effect estimate may be from the combined version. 
  
  geom_vline(xintercept = combined_estimate$estimate, linetype = "dashed", 
             color = "darkblue", 
             size = 1) +
  
  # Adding labels and titles. 
  
  labs(title = "Treatment Effect Estimate for Each Imputation", 
  subtitle = "N = 250. Data is Missing at Random",
       x = "Effect Estimate",
       y = "Imputation") +
  
  # Starting with minimal theme as a baseline
  
  theme_minimal() +
  
  # Formatting the plot by increasing font size, centering titles and making title bold. 
  
  theme(
    text = element_text(size = 16),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )


