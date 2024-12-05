# Load necessary library
library(tidyverse)

# Set sample size
n <- 1000

# Set seed for reproducibility
set.seed(12345)

# Simulate data
data <- tibble(
  L = rbinom(n, 1, 0.5),  # Confounder (binary)
  X = rbinom(n, 1, 0.5),  # Exposure (binary)
  # Outcome Y depends on L and X, with some random noise
  Y = 5 + 2 * X - 3 * L + rnorm(n, 0, 1)  # Continuous outcome
)

# View the first few rows of the data
head(data)

# Parametric G-formula ----

# Fit a linear regression model
model <- lm(Y ~ X + L, data = data)

# Predict potential outcomes under X = 1 and X = 0
data <- data %>%
  mutate(
    Y_X1 = predict(model, newdata = data.frame(X = 1, L = L)),  # Y under X = 1
    Y_X0 = predict(model, newdata = data.frame(X = 0, L = L))   # Y under X = 0
  )

# Calculate the risk difference (Average Treatment Effect)
parametric_RD <- mean(data$Y_X1 - data$Y_X0)

# Print the estimated risk difference
parametric_RD

# Bayesian Parametric G-Formula ----

# Load the necessary package
library(brms)

# Fit a Bayesian linear model using brms
bayesian_model <- brm(Y ~ X + L, data = data, family = gaussian(), 
                      prior = c(prior(normal(0, 10), class = "b"),
                                prior(normal(0, 10), class = "Intercept")),
                      iter = 4000, chains = 4)

# Posterior predictive samples for Y under X = 1 and X = 0
Y_X1_samples <- posterior_predict(bayesian_model, newdata = data.frame(X = 1, L = data$L))
Y_X0_samples <- posterior_predict(bayesian_model, newdata = data.frame(X = 0, L = data$L))

# Estimate the risk difference (Average Treatment Effect) by averaging posterior draws
bayesian_RD <- mean(rowMeans(Y_X1_samples - Y_X0_samples))

# Print the estimated risk difference
bayesian_RD

# Compare Results ----

# Print the risk differences from both approaches
cat("Parametric g-formula RD:", parametric_RD, "\n")
cat("Bayesian g-formula RD:", bayesian_RD, "\n")
