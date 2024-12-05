
# Model from the Weak Priors ----

# This is the same code found in 02 script

# Fit a Bayesian linear model with weak priors

bayes_mod # object from 02_bayes_gformula.R


# Plots! ----

# Plotting the Posterior Predictive Distribution

draws <- bayes_mod %>% 
  epred_draws(newdata = df)

draws_y1 <- data.frame(
  y1 = rowMeans(Y_X1_samples)
)

draws_y0 <- data.frame(
  y0 = rowMeans(Y_X0_samples)
)

draws <- cbind(draws_y1, draws_y0) %>% 
  mutate(effect = y1-y0) %>% 
  tidyr::pivot_longer(
    everything(), 
    names_to = "outcome"
  ) 

draws %>% 
  filter(outcome != "effect") %>% 
  ggplot(aes(x = value, y = outcome, fill = as.factor(outcome))) +
  stat_halfeye(color = "#C32048", fill = "lightpink", point_interval = "mean_qi", size = 12) + 
  labs(x = "Y", y = "Treatment Group") +
  scale_y_discrete(labels = c(y0 = "X = 0", y1 = "X = 1")) +
  theme_minimal() + 
  theme(text = element_text(size = 20),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5)
  ) +
  ggtitle("Potential Outcomes", 
          subtitle = "Draws of Expected Value from the Posterior Predictive Distribution")  +
  lims(x = c(2.5, 4.5))

draws %>% 
  filter(outcome == "effect") %>% 
  summarise(
    mean = mean(value),
    median = median(value),
    q1 = quantile(value, 0.025), 
    q3 = quantile(value, 0.0975)
  ) %>% 
  mutate(
    IQR = q3-q1
  )



draws %>% 
  filter(outcome == "effect") %>% 
  ggplot(aes(x = value, y = outcome, fill = as.factor(outcome))) +
  stat_halfeye(color = "#C32048", fill = "lightpink", point_interval = "mean_qi", size = 12) + 
  labs(x = "Y", y = "") +
  scale_y_discrete(labels = c(effect = "")) +
  theme_minimal() + 
  theme(text = element_text(size = 20),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5)
  ) +
  ggtitle("Treatment Effect", 
          subtitle = "Draws of Expected Value from the Posterior Predictive Distribution") +
  lims(x = c(-1, 1))

