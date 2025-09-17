# Title: Adjusting for Variables for Precision ----

# Description: In causal inference, we know to adjust for confounding and avoid 
# adjusting for colliders. However, what if a variable is not a confounder, collider 
# or a mediator. Should we adjust? 

# Note: The variable must be included in a directed acyclic graph (DAG)
# because it's a common cause of a pair of variables. 

# For this example we are going to look at: 
# - bias
# - increase in precision (or decrease)

# Reference: Morris et al. (2019) - Using simulation studies to evaluate statistical methods
# This reference is used for calculating the performance measures that are used 

# Setup ----

#... Libraries ----

library(tidyverse) # ol' faithful
library(ggdag) # for drawing DAG 
library(WeightIt) # for IP weighting
library(broom) # tidying output from model

#... Functions ----

# Description: Skeleton for Simulating Data 

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
    v1 = rnorm(n = n, mean = 3, sd = 1) # this variable causes z1 & y
    ) %>% 
    dplyr::mutate(
      z1 = rnorm(n = n, mean = z1_mean + 1.5*v1, sd = z1_sd), 
      z2 = rbinom(n = n, size = z2_size, prob = z2_prob),
      prob = plogis(z1_on_x*z1 + z2_on_x*z2), 
      x = rbinom(n = n, size = 1, prob = prob), 
      y = beta_trt*x + z1_on_y*z1 + z2_on_y*z2 + 2*v1 + rnorm(n = n, mean = 0, sd = 1)
    )
  
  # Return the dataframe
  
  return(df)
}



# DAG ----

theme_set(theme_dag()) # setting the theme for the DAG

# Creating the DAG to guide the scenario

dag = ggdag::dagify(
  sleep ~ lifting, 
  happy ~ lifting + sunshine + sleep, 
  coffee ~ sleep + sunshine, 
  exposure = "coffee",
  outcome = "happy"
)

# Viewing the DAG

dag |> 
  ggdag::ggdag(
    layout = "nicely")

dag %>% 
  ggdag::ggdag_adjustment_set() # what we'd need to adjust for. Note: this is 
# based on only for reducing bias / rules of DAGs

# Simulating Data ----

df <- sim_data() # function from above. This is to test the function works 
  
# Not Adjusting for Variable ----

no_adjust <- function(sample.size){
  
  trt_effect <- 1.5 # same as in sim_data() function 
  
  # Simulating data
  
  df <- sim_data()
  
  # Fitting PS model 
  
  ps.mod <- WeightIt::weightit(x ~ z1 + z2, 
                               data = df, 
                               method = "glm", 
                               estimand = "ATE", 
                               stabilize = TRUE)
  
  mod <- glm(y ~ x + z1 + z2, # doubly robust 
             family = gaussian(link = "identity"), 
             data = df,
             weights = ps.mod$weights)
  
  estimate <- broom::tidy(mod)$estimate[2] # this is the estimate
  
  bias_for_one = estimate - trt_effect # comparing the estimate to the "true" effect 
  
  df_bias = data.frame(
    bias_for_one,
    estimate # need for estimating relative precision later 
  )
  
  return(df_bias)
  
}

# Repeating 1000 times using a sample size of 250

output_list <- replicate(1000, no_adjust(sample.size = 250), simplify = FALSE)

df.out.no.adjust <- do.call(rbind, output_list) %>% # reformatting
  
  # Adding a new column that will be used to estimate the Monte Carlo SE of the estimate
  mutate(
    squared = (bias_for_one - mean(bias_for_one))^2,
    residuals_squared = (estimate - mean(estimate))^2
  )


# Calculating the mean bias and Monte Carlo SE of estimate

# See Morris et al. (2019) for details on calculating these

mean(df.out.no.adjust$bias_for_one) # bias  
sqrt(sum(df.out.no.adjust$squared)*(1 / (1000*999))) # Monte Carlo SE of bias (1000 is number of simulations, 999 is n - 1)

# Emperical SE 

emp.se.no.adjust <- sqrt(sum(df.out.no.adjust$residuals_squared)/999)

emp.se.no.adjust / sqrt(2*999)

# Adjusting for Variable ----

adjust <- function(sample.size){
  
  trt_effect <- 1.5 # same as in sim_data() function 
  
  # Simulating data
  
  df <- sim_data()
  
  # Fitting PS model 
  
  ps.mod <- WeightIt::weightit(x ~ z1 + z2, 
                               data = df, 
                               method = "glm", 
                               estimand = "ATE", 
                               stabilize = TRUE)
  
  mod <- glm(y ~ x + z1 + z2 + v1, # doubly robust & including variable v1 
             family = gaussian(link = "identity"), 
             data = df,
             weights = ps.mod$weights)
  
  estimate <- broom::tidy(mod)$estimate[2] # this is the estimate
  
  bias_for_one = estimate - trt_effect # comparing the estimate to the "true" effect 
  
  df_bias = data.frame(
    bias_for_one,
    estimate
  )
  
  return(df_bias)
  
}

# Repeating 1000 times using a sample size of 1000

output_list <- replicate(1000, adjust(sample.size = 250), simplify = FALSE)

df.out.adjust <- do.call(rbind, output_list) %>% # reformatting
  
  # Adding a new column that will be used to estimate the Monte Carlo SE of the estimate
  mutate(
    squared = (bias_for_one - mean(bias_for_one))^2,
    residuals_squared = (estimate - mean(estimate))^2
  )


# Calculating the mean bias and Monte Carlo SE of estimate

# See Morris et al. (2019) for details on calculating these

mean(df.out.adjust$bias_for_one) # bias  
sqrt(sum(df.out.adjust$squared)*(1 / (1000*999))) # Monte Carlo SE of bias (1000 is number of simulations, 999 is n - 1)

# Emperical SE 

emp.se.adjust <- sqrt(sum(df.out.adjust$residuals_squared)/999)

emp.se.adjust / sqrt(2*999)

# Percent Increase in Precision ----

# Adjusting vs Not Adjusting 

100*(((emp.se.no.adjust/emp.se.adjust)^2) - 1)

# Monte Carlo SE of Estimate

corr.a.b <- cor(df.out.no.adjust$estimate,
                 df.out.adjust$estimate)

200*((emp.se.no.adjust/emp.se.adjust)^2)*sqrt((1 - (corr.a.b^2)) / (1000-1))


