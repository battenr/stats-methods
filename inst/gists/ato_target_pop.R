# Title: Average Treatment Effect in the Overlap 

# Description: The average treatment effect in the overlap (ATO) can be a tremendously helpful 
# causal estimand. However, it can be tricky to define because it's hard to identify
# before an analysis. 

# This code demonstrates what the target population is for the ATO using code 

# Note: Claude was used to help with parts of this code, in particular the formatting of the graphic.  

# Setup ----

#... Packages ----

library(tidyverse) # ol' faithful
library(WeightIt) # for IP weighting 

# Simulate Data ----

set.seed(456) # setting seed for reproducibility

# Simulating data with two confounders, binary treatment and continuous outcome

df <- data.frame(
  z1 = rnorm(n = 250, mean = 5, sd = 2), 
  z2 = rnorm(n = 250, mean = 3, sd = 2)
) %>% 
  mutate(
    x = rbinom(n = 250, size = 1, prob = plogis(-0.5 + 0.5 * z1 - 0.8 * z2)),
    y = 1 + 2 * x + 0.5 * z1 + 1.5*z2 + rnorm(n = 250)
  )

# Estimate the ATO ----

ps.mod <- WeightIt::weightit(x ~ z1 + z2, 
                             data = df, 
                             method = "glm", 
                             estimand = "ATO")

# Note: in reality we should always check overlap, and weights summary. 
# For this example, we are not going to. 

# Adding Weights and Propensity Score to Data ----

df$weight   <- ps.mod$weights
df$ps_est   <- ps.mod$ps     # estimated propensity scores

# Estimate the ATO ----

# True - ATO, from code is 2 (the number in front of x in the code y = )

WeightIt::glm_weightit(
  y ~ x, 
  data = df, 
  family = gaussian(link = "identity"),
  weightit = ps.mod
)

# Visualize the Propensity Scores ----

#... PS distribution by group ----

# Note: This is really where Claude helped a lot: formatting the figure 

ggplot(df, aes(x = ps_est, fill = factor(x))) +
  geom_density(alpha = 0.40) +
  geom_vline(xintercept = 0.5, linetype = "dashed", linewidth = 0.8,
             colour = "#534AB7") +
  annotate("rect", xmin = 0.3, xmax = 0.7,
           ymin = -Inf, ymax = Inf,
           fill = "#7F77DD", alpha = 0.12) +
  annotate("text", x = 0.5, y = Inf, vjust = 1.5,
           label = "Overlap region\n(ATO target population)",
           colour = "#534AB7", size = 7, fontface = "bold",
           hjust = 0.5) +
  
  # Arrows: show that tails get down-weighted
  annotate("segment",
           x = 0.15, xend = 0.15, y = 2.8, yend = 0.3,
           arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
           colour = "#185FA5", linewidth = 0.8) +
  annotate("text", x = 0.15, y = 3.1, label = "down-\nweighted",
           colour = "#185FA5", size = 7, hjust = 0.5) +
  annotate("segment",
           x = 0.85, xend = 0.85, y = 2.8, yend = 0.3,
           arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
           colour = "#993C1D", linewidth = 0.8) +
  annotate("text", x = 0.85, y = 3.1, label = "down-\nweighted",
           colour = "#993C1D", size = 7, hjust = 0.5) +
  
  scale_fill_manual(values  = c("0" = "#3B8BD4", "1" = "#D85A30"),
                    labels  = c("0" = "Control", "1" = "Treated"),
                    name    = "Group") +
  labs(
    title    = "Average Treatment Effect in the Overlap",
    subtitle = "Distribution of Propensity Scores by Group",
    x = "Estimated Propensity Score",
    y = "Density"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 18, hjust = 0.5, colour = "grey40"))

