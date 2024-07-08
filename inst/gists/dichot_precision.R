# Title: Dichotomization Can Reduce Precision (SE)

# Description: Demonstrating how dichotomization of a continuous covariate
# can reduce precision (showing this based on the standard error)

# Setup ----

library(tidyverse) # ol' faithful

# Parameters for the Simulation ----

n <- 200  # Sample size (completely arbitrary)
effect_size <- 5 # Effect size
n_simulations <- 25  # Number of simulations (important note: in reality 
# would use many more, but for the plot limiting to 25)

# Simulate Data and Fit Models (Function) ----

# The purpose of this model is to simulate data. It takes a sample size and 
# effect size. Later it will be used multiple times, so the i variable
# keeps track of which simulation it is (i.e., 1:25)

simulate_estimates <- function(n, effect_size, i) {
  
  # Creating the data first. 
  
  df = data.frame(
    trt = rbinom(n = n, size = 1, prob = 0.5), # binary treatment
    continuous_covariate = rnorm(n, mean = 40, sd = 10) # continuous covariate
    # note: not a confounder but for this purpose just a covariate associated with 
    # the outcome (avoiding variable selection for this demo)
  ) %>% 
    dplyr::mutate(
      # Making covariate two categories. Completely arbitrary cutoff
      dichotomized_covariate = ifelse(continuous_covariate > 32.5, "cat1", "cat2"), 
      outcome = effect_size * trt + 0.15*continuous_covariate + rnorm(n)
    )
  
  # Model with Continuous Covariate
  
  model_continuous <- glm(outcome ~ trt + continuous_covariate, # Fitting model
                          data = df, 
                          family = gaussian(link = "identity"))
  summary_continuous <- summary(model_continuous) # formatting using summary function
  estimate_continuous <- coef(summary_continuous)[2, "Estimate"] # extracting the point estimate
  se_continuous <- coef(summary_continuous)[2, "Std. Error"] # extracting the SE
  
  # Model with Binary Covariate (that is actually continuous)
  
  model_dichotomized <- glm(outcome ~ trt + dichotomized_covariate,
                            data = df, 
                            family = gaussian(link = "identity"))
  summary_dichotomized <- summary(model_dichotomized)
  estimate_dichotomized <- coef(summary_dichotomized)[2, "Estimate"]
  se_dichotomized <- coef(summary_dichotomized)[2, "Std. Error"]
  
  # Outputs for Function: 
  # - Whether the covariate is continuous or dichotomized
  # - The point estimate for each model
  # - The SE for each model
  # - Which simulation number it was
  
  data.frame(
    Covariate = c("Continuous", "Dichotomized"),
    Estimate = c(estimate_continuous, estimate_dichotomized),
    StdError = c(se_continuous, se_dichotomized),
    Simulation = i
  )
}

# Repeat Repeat and Repeat Again! ----

#  Using the function above and running it n_simulation number of times. 
# This can also be done a different way using the replicate function. 

simulation_results <- do.call(rbind, lapply(1:n_simulations, function(i) {
  simulate_estimates(n, effect_size, i)
}))

# Plots! ----


ggplot(simulation_results, # Setting up the plot
       aes(x = Estimate, 
           y = as.factor(Simulation), 
           color = Covariate)) +
  geom_point(position = position_dodge(width = 0.8), size = 2) + # adding point for the point estimate
  geom_errorbar(
    aes(xmin = Estimate - 1.96 * StdError, 
        xmax = Estimate + 1.96 * StdError), # using 95% CI
    position = position_dodge(width = 0.8), width = 0.5) +
  geom_vline(
    xintercept = effect_size, 
    linetype = "dashed", 
    color = "red") + # this shows the actual effect size
  
  # Adding Labels 
  
  labs(title = "Impact of Dichotomizing a Covariate on Precision",
       subtitle = "Continuous Outcome, Sample Size of 1000",
       y = "Simulation",
       x = "Estimate (with 95% CI)",
       color = "Covariate Type") +
  
  # Formatting the graph: color, text size, theme, etc. 
  
  theme_minimal() +
  lims(x = c(4,6)) +
  
  coord_flip() + # decided later to flip the x and y axis so bars are vertical
  scale_color_manual(values = c("darkred", "darkblue")) +
  theme_minimal() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5, face = "bold"), # centering plot title
        plot.subtitle = element_text(hjust = 0.5), # centering subtitle
        text = element_text(size = 26) # increasing text size
        ) 

