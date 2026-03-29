# Title: Bias-corrected and accelerated Bootstrap for Causal Inference 

# Description: Sometimes the confidence interval can be tricky to estimate for 
# causal inference. In particular, there may be situations where we can't simply estimate
# it with a formula. 

# In these cases, bootstrapping can help. However, when the sampling distribution 
# isn't normally distributed, it can lead to misleading results. This code 
# demonstrates how the BCa bootstrap can help here. 

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
                     intercept = -6) {  # push outcome rare
  
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

# Overall prevalence of the outcome, in this case it's rare 

cat("Overall outcome prevalence:", round(mean(df$y), 3), "\n")

# Really we are curious about comparing the two groups after we do some weighting
# that targets the average treatment effect in the treated. 

# Pre-weighting it looks like this: 

df %>% 
  group_by(x) %>% 
  summarise(y = mean(y))

# Preparing for Bootstrapping ----

# To prepare for bootstrapping we first need to write a function that we could use. 
# We are going to target the average treatment effect in the treated (ATT). 

ipw_prop <- function(data, indices) {
  d <- data[indices, ]
  
  ps_mod <- WeightIt::weightit(
    x ~ z1 + z2,
    data      = d,
    estimand  = "ATE"
  )
  
  d$w <- ps_mod$weights
  
  # Weighted proportion across the whole sample (ATT-standardised)
  
  x0_only <- d %>% 
    filter(x == 0)
  
  weighted.mean(x0_only$y, w = x0_only$w)
  
}

# BCa and Percentile Bootstrap CIs ----

set.seed(123) # setting the seed 

bo <- boot(data = df, statistic = ipw_prop, R = 1000) # bootstrap 

ci_bca  <- boot.ci(bo, conf = 0.95, type = "bca") # BCa bootstrap
ci_perc <- boot.ci(bo, conf = 0.95, type = "basic") # just the basic bootstrap

# Calculating the Values ----

# Calculating the values, specifically the weighted proportion in the control group 

cat("ATT-Weighted Proportion:\n")
cat("  Estimate    :", round(bo$t0, 3), "\n") 
cat("  BCa  95% CI :", round(ci_bca$bca[4],      3), "to", round(ci_bca$bca[5],      3), "\n")
cat("  Perc 95% CI :", round(ci_perc$basic[4],  3), "to", round(ci_perc$basic[5],  3), "\n")

# Sampling distribution ----

# We are now going to plot the sampling distribution to show how they look different 

tibble(estimate = as.numeric(bo$t)) %>%
  ggplot(aes(x = estimate)) +
  geom_histogram(bins = 40, fill = "steelblue", colour = "white") +
  geom_vline(xintercept = ci_bca$bca[4:5],     linetype = "dashed", colour = "lightpink",  linewidth = 0.8) +
  geom_vline(xintercept = ci_perc$basic[4:5], linetype = "dotted", colour = "purple", linewidth = 0.8) +
  geom_vline(xintercept = bo$t0,                linetype = "solid",  colour = "black",      linewidth = 0.8) +
  labs(
    title    = "BCa vs Percentile Bootstrap: ATT-Weighted Proportion",
    subtitle = "Pink = BCa 95% CI, Purple = Percentile 95% CI, Solid = estimate",
    x        = "Weighted Proportion",
    y        = "Count"
  )

# Bonus: Skewed Sampling Distribution 

# If like me, you wondered if this was "really" skewed then use a density plot 

tibble(estimate = as.numeric(bo$t)) %>%
  ggplot(aes(x = estimate)) +
  geom_density()





