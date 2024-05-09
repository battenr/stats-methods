# Title: Demonstration of How Power Can Vary

# Description: The purpose of this script is to show how power can vary
# based on effect size and sample size. Power is calculated for several scenarios
# then plotted to demonstrate the importance of power/sample size. 

# Setup ----

library(tidyverse) # ol faithful

# Simulating Power ----

# Creating a function to simulate the data and then calculate power. 

simulate_power <- function(sample_size, # sample size
                           effect_size, # effect size on the log scale (since logit)
                           sig_level = 0.05, # significance level: set as 0.05 by default
                           num_simulations = 1000) { # number of simulations, arbitrarily 1000
  
  # Calculating the p-value
 
   p_values <- replicate(num_simulations, { # repeating the number of simulation times
    
    # X variable, in this situation the treatment (binary and split evenly
     # in the distribution)
     
    x <- rbinom(sample_size, size = 1, prob = 0.5)
    
    # Simulating the binary outcome. 
    # plogis() to use the logit scale (odds/1+odds)
    
    y <- rbinom(sample_size, size = 1, prob = plogis(effect_size*x)) 
    
    # Making dataframe 
    
    df <- data.frame(
      y, x
    )
    
    # Fit logistic GLM
    
    fit <- glm(formula = y ~ x, 
               family = "binomial",
               data = df)
    
    # Get the p-value from the output. Alternatively, could calculate 
    # manually. Note: this is the Wald test 
    
    summary(fit)$coefficients['x', 'Pr(>|z|)']
    
  })
   
  # Previoulsy we calculated the p-values. Now what we want is how many times it was 
  # rejected. Here, we are using a significance level of 0.05. We could count or as a 
  # shortcut, using the evaluation (<) it will return TRUE/FALSE so we can just take mean
  
   # Remember: Power is probability of rejecting null.  
   
  mean(p_values < sig_level)
  
  
}

# Calcuating Power Across Scenarios ----

#... Parameters ----

sample_sizes <-  seq(100, 1000, by = 100) # sample sizes from 100 to 1000 by 100
odds_ratios <- c(1.1, 1.25, 1.5, 1.75, 2) # various odds ratios. Wanted to show 
# different scenarios while also using realistic ORs. 
effect_sizes <- log(odds_ratios) # using log of the odds ratios. This is wrong terminology 
# for "effect size". It's really the coefficients from the logistic regression

#... Calculating Power ----

# Calculate power for each combination of sample size and effect. 

results <- expand.grid(sample_size = sample_sizes, # using expand.grid to get all combinations
                       effect_size = effect_sizes) %>%
  
  # Calculating power by applying the function above for each scenario. 
  
  mutate(power = mapply(simulate_power, sample_size, effect_size))

# Plotting Results! ----

# Time to plot results (yay!)

ggplot(data = results, 
       mapping = aes(x = sample_size, y = power, color = as.factor(effect_size))) +
  
  geom_line(linewidth = 1) + # increasing line width for visibility
  
  labs(x = "Sample Size", y = "Power", color = "Odds Ratio") + # labelling graph 
  
  # Adding title
  
  ggtitle("Power Analysis for Logistic Regression", subtitle = "Unadjusted Model (Significance Level: 0.05)") + 
  
  # Formatting theme and colors of the graph
  
  theme_minimal() + 
  scale_color_manual(
    values = c("darkgreen", "orange", "darkblue", "darkred", "purple"),
    labels = as.character(odds_ratios)
  ) + 
  theme(
    text = element_text(size = 16),
    plot.title = element_text(hjust = 0.5), 
    plot.subtitle = element_text(hjust = 0.5)
  ) + 
  
  scale_x_continuous(breaks = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000), 
                     limits = c(100, 1000)) +
  
  # Adding horizontal line at 0.80 power (what is often chosen, based on type II error of 0.20)
  
  geom_hline(yintercept = 0.80, color = "black", linewidth = 1) + 
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0))
  
  
