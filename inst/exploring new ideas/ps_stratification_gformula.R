# Propensity Score Stratificaiton with Parametric g-formula

# Use PS to stratify
# g-formula in each group
# combine estimates



set.seed(123)
n <- 1000
X1 <- rnorm(n)
X2 <- rbinom(n, 1, 0.5)
A <- rbinom(n, 1, plogis(0.5*X1 - 0.5*X2))
Y <- 2*A + X1 + X2 + rnorm(n)

dat <- data.frame(X1, X2, A, Y)


ps_model <- glm(A ~ X1 + X2, family = binomial(), data = dat)
dat$ps <- predict(ps_model, type = "response")

# Create strata based on quantiles of the PS
K <- 5
dat$stratum <- cut(dat$ps, breaks = quantile(dat$ps, probs = seq(0, 1, length.out = K + 1)), 
                   include.lowest = TRUE, labels = FALSE)



library(dplyr)

# Function to estimate ATE in a stratum using g-formula
estimate_gformula <- function(df) {
  model <- lm(Y ~ A + X1 + X2, data = df)
  
  df0 <- df; df0$A <- 0
  df1 <- df; df1$A <- 1
  
  y0_hat <- predict(model, newdata = df0)
  y1_hat <- predict(model, newdata = df1)
  
  ate <- mean(y1_hat - y0_hat)
  return(ate)
}

stratum_ates <- dat %>%
  group_by(stratum) %>%
  summarise(n = n(),
            ate = estimate_gformula(cur_data()))

# Weight by stratum size
overall_ate <- with(stratum_ates, sum(n * ate) / sum(n))
overall_ate

# Repeated ----

set.seed(123)

library(dplyr)

# -----------------------------
# 1. Simulation Parameters
# -----------------------------
nsim <- 1000
n <- 1000
true_ate <- 2  # We simulate Y = 2*A + ...

# -----------------------------
# 2. Single Simulation Function
# -----------------------------

simulate_once <- function(n, K = 5) {
  # Step 1: Simulate data
  X1 <- rnorm(n)
  X2 <- rbinom(n, 1, 0.5)
  A <- rbinom(n, 1, plogis(0.5*X1 - 0.5*X2))
  Y <- 2*A + X1 + X2 + rnorm(n)  # true ATE = 2
  
  dat <- data.frame(X1, X2, A, Y)
  
  # Step 2: Estimate PS and Stratify
  ps_model <- glm(A ~ X1 + X2, family = binomial(), data = dat)
  dat$ps <- predict(ps_model, type = "response")
  dat$stratum <- cut(dat$ps, 
                     breaks = quantile(dat$ps, probs = seq(0, 1, length.out = K + 1)), 
                     include.lowest = TRUE, labels = FALSE)
  
  # Step 3: G-formula within strata
  estimate_gformula <- function(df) {
    model <- lm(Y ~ A + X1 + X2, data = df)
    df0 <- df; df0$A <- 0
    df1 <- df; df1$A <- 1
    y0_hat <- predict(model, newdata = df0)
    y1_hat <- predict(model, newdata = df1)
    return(mean(y1_hat - y0_hat))
  }
  
  stratum_ates <- dat %>%
    group_by(stratum) %>%
    summarise(n = n(),
              ate = estimate_gformula(cur_data()),
              .groups = 'drop')
  
  # Combine estimates (weighted average)
  overall_ate <- sum(stratum_ates$n * stratum_ates$ate) / sum(stratum_ates$n)
  
  return(overall_ate)
}

# -----------------------------
# 3. Run Simulations
# -----------------------------
results <- replicate(nsim, simulate_once(n))

# -----------------------------
# 4. Evaluate Performance
# -----------------------------
bias <- mean(results - true_ate)
mse <- mean((results - true_ate)^2)
variance <- var(results)

gform_summary_stats <- data.frame(
  Bias = bias,
  Variance = variance,
  MSE = mse,
  Mean_Estimate = mean(results)
)

print(gform_summary_stats)

# IP Weighting 

# Bias was 0.001429 from g-formula & stratum


simulate_once <- function(n, K = 5) {
  # Step 1: Simulate data
  X1 <- rnorm(n)
  X2 <- rbinom(n, 1, 0.5)
  A <- rbinom(n, 1, plogis(0.5*X1 - 0.5*X2))
  Y <- 2*A + X1 + X2 + rnorm(n)  # true ATE = 2
  
  dat <- data.frame(X1, X2, A, Y)
  
  # Step 2: Estimate PS and Stratify
  
  ps_mod <- WeightIt::weightit(A ~ X1 + X2, 
                               estimand = "ATE",
                               stabilize = TRUE
                               )
  
  mod <- WeightIt::glm_weightit(Y ~ A, 
                                data = dat,
                                weights = ps_mod$weights)
  
  overall_ate <- mod %>% 
    broom::tidy() %>% 
    filter(term == "A") %>% 
    select(estimate) %>% 
    as.numeric()
  
  return(overall_ate)
}

# -----------------------------
# 3. Run Simulations
# -----------------------------
results <- replicate(nsim, simulate_once(n))

# -----------------------------
# 4. Evaluate Performance
# -----------------------------
bias <- mean(results - true_ate)
mse <- mean((results - true_ate)^2)
variance <- var(results)

ipw_summary_stats <- data.frame(
  Bias = bias,
  Variance = variance,
  MSE = mse,
  Mean_Estimate = mean(results)
)

print(ipw_summary_stats)

