# Title: Bias-corrected and Accelerated Bootstrap Interval for Causal Inference 

# Description: Sometimes the confidence interval can be tricky to estimate for 
# causal inference. In particular, there may be situations where we can't simply estimate
# it with a formula. 

# In these cases, bootstrapping can help. However, when the sampling distribution 
# is skewed or biased this can cause problems. The BCa interval can help by accounting 
# for this bias and skewness. This code demonstrates how the BCa bootstrap 
# interval works with an example. 

# Note: this code was generated with the help of Claude but edited and reviewed 
# by me (Ryan). 

# Setup ----

#... Packages ----

library(tidyverse) # ol' faithful
library(WeightIt) # IP weighting
library(boot) # for boostrapping 

#... Functions ----

# Simulating data

sim_data <- function(n = 150, # sample size (arbitrary)
                     z1_mean = 5, z1_sd = 2,
                     z2_prob = 0.5,
                     z1_on_x = 0.05, z2_on_x = 0.2,
                     intercept = -6) {  # making the outcome rare
  
  data.frame(
    z1 = rnorm(n, z1_mean, z1_sd),
    z2 = rbinom(n, 1, z2_prob)
  ) %>%
    dplyr::mutate(
      prob = plogis(z1_on_x * z1 + z2_on_x * z2),
      x    = rbinom(n, 1, prob),
      y    = rbinom(n, 1, plogis(intercept + 0.5 * z1 + 0.3 * z2))
    )
}

# Simulate Data ----

set.seed(456) # setting seed for reproducibility 

df <- sim_data() # simulating data 

# Calculating the prevalence of the outcome, in this case it's rare

cat("Overall outcome prevalence:", round(mean(df$y), 3), "\n")

# Preparing for Bootstrapping ----

# The ATE compares the weighted mean outcome in the treated group
# vs the weighted mean outcome in the control group, across the
# full sample. 

# In this case we are going to use the ATE weights, but we are 
# actually going to use a causal risk ratio (instead of a risk difference)

#... Function for Bootstrapping ----

# We are going to use estimate our statistic of interest. In this 
# case it's the causal risk ratio. In each replicate (i.e., bootstrapped sample)
# we are going to re-estimate the weights 

ipw_ate <- function(data, indices) {
  
  d <- data[indices, ]
  
  # ATE weighting
  
  ps_mod <- WeightIt::weightit(
    x ~ z1 + z2,
    data     = d,
    estimand = "ATE",
    stabilize = TRUE
  )
  
  # Add weights to dataset
  
  d$w <- ps_mod$weights
  
  # Treated group
  
  treated  <- d %>% filter(x == 1)
  
  # Control group
  
  control  <- d %>% filter(x == 0)
  
  # Calculating the risk in each group
  
  y1 <- weighted.mean(treated$y, w = treated$w)
  y0 <- weighted.mean(control$y, w = control$w)
  
  # Causal Risk Ratio = E[Y(1)] / E[Y(0)]
  
  y1 / y0
  
}

# BCa and Percentile Bootstrap CIs ----

#... Bootstrapping ----

# This creates bootstrap samples (the R indicates how many times we are creating a 
# replicate. For example, we are making 1000 version by resampling). This means 
# 1000 samples in this case 

bo <- boot(data = df, statistic = ipw_ate, R = 1000)

ci_bca  <- boot.ci(bo, conf = 0.95, type = "bca") # this is the BCa interval
ci_perc <- boot.ci(bo, conf = 0.95, type = "basic") # the basic bootstrap (for comparison)

# Results ----

cat("RR Estimate:\n")
cat("  Estimate    :", round(bo$t0, 3), "\n")
cat("  BCa  95% CI :", round(ci_bca$bca[4],    3), "to", round(ci_bca$bca[5],    3), "\n")
cat("  Perc 95% CI :", round(ci_perc$basic[4], 3), "to", round(ci_perc$basic[5], 3), "\n")

# Sampling Distribution ----

# Plotting the sampling distribution and both bootstrap intervals

tibble(estimate = as.numeric(bo$t)) %>%
  ggplot(aes(x = estimate)) +
  geom_histogram(bins = 40, fill = "steelblue", colour = "white") +
  geom_vline(xintercept = ci_bca$bca[4:5],     linetype = "dashed", colour = "lightpink",  linewidth = 1.2) +
  geom_vline(xintercept = ci_perc$basic[4:5],  linetype = "dotted", colour = "purple",     linewidth = 1.2) +
  geom_vline(xintercept = bo$t0,                linetype = "solid",  colour = "#FFB81C",      linewidth = 1.2) +
  labs(
    title    = "BCa vs Percentile Bootstrap Intervals: Risk Ratio using ATE Weights",
    subtitle = "Pink = BCa 95% Interval, Purple = Percentile 95% Interval, Buffalo Sabres Yellow = Estimate",
    x        = "Risk Ratio",
    y        = "Count"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 26),
    plot.subtitle = element_text(hjust = 0.5, size = 20),
    text = element_text(size = 20)
  ) + 
  lims(x = c(-5, 10), y = c(0, 200))

# Bonus: Density plot to inspect skewness ----

# If you want to see how skewed the sampling distribution looks

tibble(estimate = as.numeric(bo$t)) %>%
  ggplot(aes(x = estimate)) +
  geom_density()

