# Title: Entropy Balancing

# Description: Demonstrating how entropy balancing can be useful for causal inference. 
# In particular, it can be useful when there is some overlap between the groups. 

# For further reading, I highligh recommend the paper by Jans Hainmuller 
# "Entropy Balancing for Causal Effects"

# Setup ----

#... Libraries ----

library(tidyverse) # ol' faithful
library(WeightIt) # for weighting
library(cobalt) # for checking balance
library(patchwork) # combining plots 

#... Functions ----

# Simulating Data

sim_data <- function(n = 250, # sample size 
                     beta_trt = 1.5, # treatment effect
                     # Parameters for Z1 (sleep)
                     z1_mean = 5, z1_sd = 2, 
                     # Parameters for Z2 (weightlifting)
                     z2_size = 1, z2_prob = 0.3, 
                     # Confounder - Effect on X
                     z1_on_x = 0.05, z2_on_x = 0.2,
                     # Confounder - Effect on Y
                     z1_on_y = 0.5, z2_on_y = 0.3){
  
  # Creating the Dataframe
  
  df <- data.frame(
    sleep = rnorm(n = n, mean = z1_mean, sd = z1_sd), 
    weightlifting = rbinom(n = n, size = z2_size, prob = z2_prob)
  ) %>% 
    dplyr::mutate(
      prob = plogis(z1_on_x*sleep + z2_on_x*weightlifting), 
      coffee = rbinom(n = n, size = 1, prob = prob), 
      happiness = beta_trt*coffee + z1_on_y*sleep + z2_on_y*weightlifting + rnorm(n = n, mean = 0, sd = 1)
    )
  
  # Return the dataframe
  
  return(df)
}

# Simulate Data ----

set.seed(123) # for reproducibility

df <- sim_data() # simulating data 

# Entropy Balancing ----

eb <- WeightIt::weightit(coffee ~ sleep + weightlifting,
                         data = df, 
                         method = "ebal", 
                         moments = 2, # aligning on first two moments for variables
                         estimand = "ATE")

# Checking Weights ----

#... Effective Sample Size ----

ESS(eb$weights) 

#... Plot ----

ggplot(data = df, 
       mapping = aes(x = eb$weights)) + 
  geom_density() + 
  lims(x = c(0, 2)) + 
  labs(x = "Weights", y = "Density") + 
  ggtitle("Distribution of Weights") +
  theme_minimal() + 
  theme(
    text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5)
  ) 

# Checking Balance ----

#... Love Plot ----

p1 <- cobalt::love.plot(eb, 
                  abs = TRUE, 
                  stars = "std", # sleep is standardized mean difference but weightlifting is not 
                  size = 5,
                  labels = TRUE,
                  title = "Covariate Balance\n (Entropy Balancing)",
                  themes = theme(
                      text = element_text(size = 22),
                      plot.title = element_text(hjust = 0.5, face = "bold")
                    )
) + 
  labs(x = "Mean Differences")

#... Distributions (Pre-Post Weighting) ----

p2 <- cobalt::bal.plot(eb,
                 var.name = "sleep", 
                 which = "both"
                 ) + 
  ggtitle("Balance for Sleep", 
          subtitle = "Before and After Weighting") + 
  labs(x = "Sleep", fill = "Coffee") + 
  theme(
    text = element_text(size = 22),
    plot.title = element_text(hjust = 0.5, 
                              face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

p3 <- cobalt::bal.plot(eb, var.name = "weightlifting", 
                       which = "both") + 
  ggtitle("Balance for Weightlifting", 
          subtitle = "Before and After Weighting") + 
  labs(x = "Weighlifting", fill = "Coffee") + 
  theme(
    text = element_text(size = 22),
    plot.title = element_text(hjust = 0.5, 
                              face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

#... Combining Plots ----

(p1 / (p2 + p3)) + 
  plot_annotation(
  caption = "*Standardized mean difference for sleep",
  theme = theme(plot.caption = element_text(size = 16, hjust = 0, face = "bold"))
)

# Fitting Outcome Model ----

#... Without Weights ----

# naive model, without weights 

glm(happiness ~ coffee, 
    data = df)

#... With Weights ----

glm_weightit(happiness ~ coffee, 
    data = df, 
    weights =  eb$weights)

