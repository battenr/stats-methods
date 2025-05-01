# Title: Propensity Score Plots Using IP Weighting ----

# Description: This code shows how different patients will receive different weights
# depending upon the causal estimand when using IP weighting. This code focuses on 
# three estimands: ATE, ATT and ATU. 

# Setup ----

#... Libraries ----

library(tidyverse) # ol' faithful
library(patchwork) # for combining plots

# Simulating Data ----

set.seed(456) # setting seed for reproducibility

n = 250 # arbitrary sample size
                       
df <- data.frame(
  z1 = rnorm(n = n, mean = 5, sd = 2), 
  z2 = rbinom(n = n, size = 1, prob = 0.5)
) %>% 
  dplyr::mutate(
    prob = plogis(0.05*z1 + 0.2*z2), 
    x = rbinom(n = n, size = 1, prob = prob), 
    trteffect = rnorm(n = n, mean = 2, sd = 1),
    y = trteffect*x + 0.5*z1 + 0.3*z2
  )

# Propensity Score Model ----

# Fitting the propensity score model 

psmod <- glm(x ~ z1 + z2, 
             family = binomial(link = "logit"),
             data = df)

# Calculating Weights ----

# Calculating the weights based on the propensity score model. This is based on 
# the ATE, ATT and ATU for this example. 

df.w <- df %>% 
  dplyr::mutate(
    prop_score = predict(psmod, type = "response"),
    w_ate = dplyr::case_when(
      x == 1 ~ 1/prop_score, 
      x == 0 ~ 1/(1-prop_score)
    ),
    w_att = dplyr::case_when(
      x == 1 ~ 1,
      x == 0 ~ prop_score/(1-prop_score)
    ),
    w_atu = dplyr::case_when(
      x == 1 ~ (1-prop_score)/prop_score,
      x == 0 ~ 1
    )
  )

# Plots ----

#... Setting Colors ----

clrs <- c(
  "magenta", 
  "purple"
)

#... ATE ----

plot_data_weights <- tibble(
  propensity = df.w$prop_score,
  weight = df.w$w_ate,
  treatment = df.w$x
)

p1 <- ggplot() + 
  geom_histogram(data = filter(plot_data_weights, treatment == 1), 
                 bins = 50, aes(x = propensity, weight = weight, fill = "Treated pseudo-population")) + 
  geom_histogram(data = filter(plot_data_weights, treatment == 0), 
                 bins = 50, aes(x = propensity, weight = weight, y = -after_stat(count), fill = "Untreated pseudo-population")) +
  geom_histogram(data = filter(plot_data_weights, treatment == 1), 
                 bins = 50, aes(x = propensity, fill = "Treated")) + 
  geom_histogram(data = filter(plot_data_weights, treatment == 0), 
                 bins = 50, aes(x = propensity, y = -after_stat(count), fill = "Untreated")) +
  geom_hline(yintercept = 0, color = "white", linewidth = 0.25) +
  scale_y_continuous(label = c(20, 10, 0, 10, 20), limits = c(-20, 20), breaks = seq(-20, 20, by = 10)) +
  scale_fill_manual(
    values = c(clrs[1], colorspace::lighten(clrs[1], 0.5), clrs[2], colorspace::lighten(clrs[2], 0.5)), 
    guide = guide_legend(reverse = FALSE, nrow = 2)) +
  labs(x = "Propensity Score", y = "Count", fill = NULL) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.key.size = unit(0.65, "lines"),
    text = element_text(size = 24),
    #panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.background = element_rect(fill = "white", color = "white"),
    panel.background = element_blank(),
    plot.subtitle = element_text(hjust = 0.5)
    ) +
  ggtitle("", 
  subtitle = "Estimating ATE")

#... ATT ----

plot_data_weights <- tibble(
  propensity = df.w$prop_score,
  weight = df.w$w_att,
  treatment = df.w$x
)

p2 <- ggplot() + 
  geom_histogram(data = filter(plot_data_weights, treatment == 0), 
                 bins = 50, aes(x = propensity, weight = weight, y = -after_stat(count), fill = "Untreated pseudo-population")) +
  geom_histogram(data = filter(plot_data_weights, treatment == 1), 
                 bins = 50, aes(x = propensity, fill = "Treated")) + 
  geom_histogram(data = filter(plot_data_weights, treatment == 0), 
                 bins = 50, alpha = 0.5, 
                 aes(x = propensity, y = -after_stat(count), fill = "Untreated")) +
  geom_hline(yintercept = 0, color = "white", linewidth = 0.25) +
  scale_y_continuous(label = c(20, 10, 0, 10, 20), limits = c(-20, 20), breaks = seq(-20, 20, by = 10)) +
  scale_fill_manual(
    values = c(clrs[1], clrs[2], colorspace::lighten(clrs[2], 0.5))
  ) + 
  labs(x = "Propensity Score", y = "Count", fill = NULL) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.key.size = unit(0.65, "lines"),
    text = element_text(size = 24),
    panel.grid.major = element_blank(),
    plot.background = element_rect(fill = "white", color = "white"),
    panel.background = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
  ) + 
  ggtitle("", 
  subtitle = "Estimating ATT")


#... ATU ----

plot_data_weights <- tibble(
  propensity = df.w$prop_score,
  weight = df.w$w_atu,
  treatment = df.w$x
)


p3 <- ggplot() + 
  geom_histogram(data = filter(plot_data_weights, treatment == 1), 
                 bins = 50, aes(x = propensity, weight = weight, fill = "Treated pseudo-population")) + 
  geom_histogram(data = filter(plot_data_weights, treatment == 1), 
                 bins = 50, alpha = 0.5, aes(x = propensity, fill = "Treated")) + 
  geom_histogram(data = filter(plot_data_weights, treatment == 0), 
                 bins = 50, aes(x = propensity, y = -after_stat(count), fill = "Untreated")) +
  geom_hline(yintercept = 0, color = "white", linewidth = 0.25) +
  # scale_x_continuous(limits = c(0.3, 0.8)) + 
  #scale_x_continuous(labels = scales::label_percent()) +
  scale_y_continuous(label = c(20, 10, 0, 10, 20), limits = c(-20, 20), breaks = seq(-20, 20, by = 10)) +
  #scale_y_continuous(label = abs, limits = c(0, 20)) +
  scale_fill_manual(
    values = c(colorspace::lighten(clrs[1],  0.5), clrs[1], clrs[2])) +
  labs(x = "Propensity Score", y = "", fill = NULL) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.key.size = unit(0.65, "lines"),
    text = element_text(size = 24),
    #panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.background = element_rect(fill = "white", color = "white"),
    panel.background = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) + 
  ggtitle( "",
    subtitle = "Estimating ATU") 

# Combining Plots ----

# Combining all the plots together from above using the patchwork R package

p1 / (p2 + p3) + plot_annotation("Inverse Probability Weighting",
                                 theme = theme(
                                   text = element_text(size = 24), 
                                   plot.title = element_text(hjust = 0.5, face = "bold")
                                 )
)
                                 size = 24)



