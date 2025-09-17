# Title: Outliers in Causal Inference

# Description: Outliers can cause problems. This code shows how 
# we can add some data that we know are outliers (since we added them). 

# Setup ----

#... Libraries ----

library(tidyverse) # ol' faithful

# Simulating Data ----

set.seed(456) # set.seed for reproducibility

n = 250 # arbitrary sample size 

df <- data.frame(
  id = 1:n,
  y = rnorm(n = n, mean = 5, sd = 2), # continuous outcome
  value = rep("normal", n = n) # for getting colors later
) %>% 
  # Adding values that we know are outliers
  dplyr::add_row(id = c(251:253), y = c(23, 51, 44), value = "outlier") 

# Plotting Data ----

# Plotting the data to demonstrate which values are outliers. Since we 
# added the outliers, we know if they are anomlies, just unlikely, or 
# an error. 

ggplot(data = df, 
       mapping = aes(x = id, y = y, color = value)) + 
  geom_point(size = 3) +
  scale_color_manual(values = c("pink", "purple")) + 
theme_minimal() + 
  labs(x = "Subject ID", y = "Effect of Coffee on Happiness") + 
  ggtitle("Effect of Coffee on Happiness by Subject ID") + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 34, face = "bold"),
    text = element_text(size = 30),
    legend.position = "none"
  ) + 
  lims(x = c(0, 255)) +
  scale_y_continuous(breaks = c(seq(from = -20, to = 60, by = 5)))