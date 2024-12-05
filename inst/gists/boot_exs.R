# Title: Bootstrapping 

# Description: Showing what bootstrapping is and how it can be useful. 

# Setup ----

#... Libraries ----

library(tidyverse) # ol' faithful
library(tidymodels)

#... Seed ----

set.seed(456) # setting seed for reproducibility

# Simple Example ----

# Using the mean for a simple example

#... Simulating Data ----

# Generate a simple dataset

n = 250 # sample size 

data <- tibble(
  x = rnorm(n = n, mean = 50, sd = 10)
  )

#... Perform bootstrapping ----

# Actually performing the bootstrapping

bootstrap_samples <- rsample::bootstraps(data, times = 1000) 
# Resampling n times. 1000 chosen arbitrarily for this example

# Calculate the mean for each bootstrap sample

bootstrap_means <- bootstrap_samples %>%
  dplyr::mutate(
    mean = purrr::map_dbl(splits, ~ mean(analysis(.x)$x))
    )

#... Organizing for Plot ----

# Convert results to a tibble for plotting

bootstrap_results <- tibble(
  mean = bootstrap_means$mean
  )

#... Plotting Result ----

# Plot the distribution of bootstrap means

ggplot(bootstrap_results, 
       aes(x = mean)) +
  
  # Using a histogram
  
  geom_histogram(binwidth = 0.5, 
                 fill = 'hotpink', 
                 color = 'hotpink', 
                 alpha = 0.7) +
  
  # Adding labels
  
  labs(title = 'Distribution of Bootstrap Sample Means',
       subtitle = "N = 250 with 1000 Bootstrap Replicates",
       x = 'Mean Change',
       y = 'Frequency') +
  
  # Making theme minimal
  
  theme_minimal() +
  
  # Reformatting like centering title, etc. 
  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    text = element_text(size = 16)
  ) +
  scale_y_continuous(limits = c(0, 350), 
                     breaks = c(0, 50, 100, 150, 200, 250, 300, 350))

# Calculating 95% CIs from GLM ----

#... Simulating Data ----

n = 250 

data <- tibble(
  c1 = rnorm(n = n, mean = 50, sd = 10), 
  trt = rbinom(n =n, size = 1, prob = plogis((1/100)*c1)),
  y = 5 + (1/20)*c1 + 3*trt + rnorm(n = n)
)

#... Perform bootstrapping ----

# Resampling n times. 1000 chosen arbitrarily for this example

bootstrap_samples <- rsample::bootstraps(data, times = 1000)

# Fitting a GLM and extract the coefficients for each bootstrap sample

# Fit a GLM and extract coefficients for each bootstrap sample
bootstrap_coefs <- bootstrap_samples %>%
  mutate(model = map(splits, ~ glm(y ~ trt + c1, 
                                   data = analysis(.x), 
                                   family = gaussian(link = "identity"))),
         coefs = map(model, broom::tidy)) %>%
  unnest(coefs)

#... Organizing for the plot ----

# Keeping only the coefficient of interest (in this case trt)

bootstrap_coefs_x <- bootstrap_coefs %>%
  filter(term == "trt")

#... Plotting Result ----

# Plot the distribution of the bootstrap coefficients for trt. 
# See code in the simple example section for comments pertaining to the code for the plot

ggplot(bootstrap_coefs_x, aes(x = estimate)) +
  geom_histogram(binwidth = 0.05, fill = 'hotpink', color = 'hotpink', alpha = 0.7) +
  labs(title = 'Distribution of Bootstrap Sample Effect Estimates',
       subtitle = "N = 250 with 1000 Bootstrap Replicates",
       x = 'Effect Estimate',
       y = 'Frequency') +
  theme_minimal() +
  # Reformatting like centering title, etc. 
  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    text = element_text(size = 16)
  ) +
  lims(x = c(2.5, 3.5))

