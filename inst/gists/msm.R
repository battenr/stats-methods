# Title: Marginal Structural Models for Time-Varying Confounding ----

# Description: Time-varying confounding can be problematic for causal studies. 
# There are several 

# To do: 
# - Try with parametric g-formula as well
# - Use a gee to show how that'd be a problem



set.seed(42)
n <- 500
t_max <- 3 # Keeping it to 3 for the formula example

df_binary <- expand.grid(id = 1:n, time = 0:(t_max-1)) %>%
  mutate(coffee = 0, sleep = 0, happiness = 0) %>%
  arrange(id, time)

for (t in 0:(t_max-1)) {
  curr <- which(df_binary$time == t)
  if (t == 0) {
    df_binary$sleep[curr] <- rnorm(n, 7, 1)
    # Probability of coffee based on baseline sleep
    prob <- plogis(1 - 0.2 * df_binary$sleep[curr])
    df_binary$coffee[curr] <- rbinom(n, 1, prob)
  } else {
    prev <- which(df_binary$time == t - 1)
    # Coffee today depends on yesterday's sleep
    prob <- plogis(1 - 0.5 * df_binary$sleep[prev])
    df_binary$coffee[curr] <- rbinom(n, 1, prob)
    
    # Sleep today depends on today's coffee
    df_binary$sleep[curr] <- rnorm(n, 8 - 1.5 * df_binary$coffee[curr], 0.8)
  }
  df_binary$happiness[curr] <- 2 + 0.5 * df_binary$coffee[curr] + 0.8 * df_binary$sleep[curr] + rnorm(n, 0, 0.5)
}








df_wide_bin <- df_binary %>%
  pivot_wider(
    id_cols = id, 
    names_from = time, 
    values_from = c(coffee, sleep),
    names_sep = "_"
  )

library(WeightIt)

WMSM_bin <- weightitMSM(
  formula = list(
    coffee_0 ~ sleep_0,
    coffee_1 ~ sleep_1 + coffee_0 + sleep_0,
    coffee_2 ~ sleep_2 + coffee_1 + sleep_1
  ),
  data = df_wide_bin,
  method = "ps",        # Propensity score for binary treatment
  link = "logit",       # Standard for binary
  stabilize = TRUE
)

# Map weights back to long data
df_binary$final_weight <- rep(WMSM_bin$weights, each = t_max)

library(survey)
design <- svydesign(ids = ~id, weights = ~final_weight, data = df_binary)
summary(svyglm(happiness ~ coffee, design = design))

# Check the weights - are they balanced?
summary(WMSM_bin)

cobalt::bal.tab(WMSM_bin)
cobalt::love.plot(WMSM_bin)
cobalt::bal.plot(WMSM_bin, which = "both")

?WeightIt::weightitMSM()
