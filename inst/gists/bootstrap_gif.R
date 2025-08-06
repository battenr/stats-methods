# Title: Demonstrating Bootstrapping Using a Visual (gif)

# Description: Demonstrating how bootstrapping works, repeatedly resampling then 
# calculating the statistic of interest. 

# Note: ChatGPT was used to assist with creating this code. I reviewed it and made changes to it, as needed. 

# Setup ----

#... Packages ----

library(tidyverse) # ol' faithful
library(camcorder) # for creating the gif

#... Functions ----

# Description: Skeleton for Simulating Data 

sim_data <- function(n = 250, # sample size 
                     beta_trt = 1.5, # treatment effect
                     # Parameters for Z1 
                     z1_mean = 5, z1_sd = 2, 
                     # Parameters for Z2
                     z2_size = 1, z2_prob = 0.5, 
                     # Confounder - Effect on X
                     z1_on_x = 0.05, z2_on_x = 0.2,
                     # Confounder - Effect on Y
                     z1_on_y = 0.5, z2_on_y = 0.3){
  
  # Creating the Dataframe
  
  df <- data.frame(
    z1 = rnorm(n = n, mean = z1_mean, sd = z1_sd), 
    z2 = rbinom(n = n, size = z2_size, prob = z2_prob)
  ) %>% 
    dplyr::mutate(
      prob = plogis(z1_on_x*z1 + z2_on_x*z2), 
      x = rbinom(n = n, size = 1, prob = prob), 
      y = beta_trt*x + z1_on_y*z1 + z2_on_y*z2 + rnorm(n = n, mean = 0, sd = 1)
    )
  
  # Return the dataframe
  
  return(df)
}

# Simulating Data ---

set.seed(456) # seed for reproducibilty 

df <- sim_data() # simulating data 

# Bootstrap Setup ----

# Function for the bootstrap estimate. In this case, taking the differece of the 
# means for treated (group 1) & untreated (group 0)

boot_estimate <- function(data, indices) {
  d <- data[indices, ]
  mean(d$y[d$x == 1]) - mean(d$y[d$x == 0])
}

B <- 200 # number of replicates 
boot_estimates <- numeric(B) # reformatting

# Setup for Recording ----

# This code is used to setup the recording (will be used later)

gg_record(
  dir = "bootstrap_histogram",
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 150
)

# Iteratively Sample & Calculate Statistic (Bootstrap) then Plot Result ----

# Creating loop because each time we resample and calculate the statistic, 
# we will want to plot the result. 

for (i in 1:B) {
  indices <- sample(1:nrow(df), replace = TRUE) # sampling the same number (sample size is 250)
  boot_estimates[i] <- boot_estimate(df, indices) # calculating the bootstrap estimate
  
  ci_lower <- quantile(boot_estimates[1:i], probs = 0.025) # lower CI (lower of 95% would be 0.025)
  ci_upper <- quantile(boot_estimates[1:i], probs = 0.975) # upper CI (0.975)
  
  # Creating the plot
  
  p <- ggplot(data.frame(est = boot_estimates[1:i]), aes(x = est)) +
    geom_histogram(bins = 30, fill = "skyblue", color = "black") +
    # Adding vertical lines for lower and upper limit CI
    geom_segment(x = ci_lower, xend = ci_lower, y = 0, yend = 19, color = "purple", size = 2) + 
    geom_segment(x = ci_upper, xend = ci_upper, y = 0, yend = 19, color = "purple", size = 2) + 
    # Adding text for what the lines are 
    geom_text(
      aes(x = ci_lower, y = 20, label = "Lower Limit"),
      color = "purple",
      vjust = 0,
      hjust = 0.5,
      size = 5
    ) +
    geom_text(
      aes(x = ci_upper, y = 20, label = "Upper Limit"),
      color = "purple",
      vjust = 0,
      hjust = 0.5,
      size = 5
    ) +
    labs(
      title = "Bootstrap Estimates",
      subtitle = "95% Confidence Interval",
      x = "Bootstrap Estimate (Mean Difference)",
      y = "Count"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      plot.subtitle = element_text(hjust = 0.5),
      panel.grid = element_blank(),   # removes grid lines
      axis.ticks = element_line(color = "black")  # keep axis ticks visible
    ) +
    ylim(0, 20)
  
  print(p)
}

# Creating GIF ----

camcorder::gg_playback(
  name = "bootstrap_histogram.gif",
  frame_duration = 0.1, # how long each frame
  image_resize = 800,
  loop = TRUE # want the gif to run on a loop
)




