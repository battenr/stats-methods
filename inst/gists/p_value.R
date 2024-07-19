# Title: Demonstration of P-Value

# Description: The purpose of this script is to demonstrate what a 
# p-value is. A logistic regression was used and the difference in deviances for 
# including the main effect in the model vs not (aka Likelihood Ratio Test).

# Setup ----

library(tidyverse) # ol faithful

# Generating Data ----

sample.size = 1000 # setting sample size. 1000 was chosen arbitrarily 

z = rnorm(n = sample.size, mean = 5, sd = 2) # continuous confounder
x = rbinom(n = sample.size, size = 1, prob = 0.3 + 0.05*z) # binary treatment
y = rbinom(n = sample.size, size = 1, plogis(0.41*x + 0.05*z)) # binary outcome

# Making as a dataframe for later

df = data.frame(
  x,
  y,
  z
)

# Fitting Logistic Regression Model ----

# Fit the full logistic regression model (i.e., treatment and confounder)

full_model <- glm(formula = y ~ x + z, 
                  family = binomial(link = "logit"), 
                  data = df)

# Fit the reduced logistic regression model (excluding z)

reduced_model <- glm(formula = y ~ z, 
                     family = binomial(), 
                     data = df)

#... Test Statistic ----

# The Likelihood Ratio Test compares the deviance of these two models. 

test_statistic = summary(reduced_model)$deviance - summary(full_model)$deviance 

#... Calculating the p-value ----

# Note there is one degree of freedom in this case because it's the difference 
# in the number of parameters. 

p_value <- pchisq(test_statistic, df = 1, lower.tail = FALSE) 


#... Checking result ----

# Perform Likelihood Ratio Test

lrt <- anova(reduced_model, full_model, test = "Chisq")

# Generating Data: Pt 2. - Null Distribution ----

# Generating data from the null distribution (Chi-Squared with df = 1 in this case)

null_dist <- rchisq(sample.size, df = 1) %>% 
  as.data.frame()

null_dist$value <- null_dist$. # renaming the variable (prefer to use rename() but wasn't working)

density_data <- density(null_dist$value) # creating this for later (edit: may not actually be needed)
df_density <- data.frame(x = density_data$x, y = density_data$y)

# Graph Time! ----

# Creating a plot that is the null distribution, with a vertical line showing the 
# test statistic and the corresponding p-value. 

ggplot(data = null_dist, 
       mapping = aes(x = value)) + 
  
  # Plotting Density
  
  geom_density(size = 1) + 
  
  # Adding the test-statistic as a dark green vertical line
  
  geom_vline(xintercept = test_statistic, 
             color = "darkgreen", 
             linewidth = 1) +
  
  # Using geom_ribbon to shade the area to the right of the test statistic on the 
  # plot. 
  
  geom_ribbon(data = subset(df_density, x >= test_statistic), 
                aes(x = x, 
                    ymin = 0, 
                    ymax = y),
                fill = "red", 
                alpha = 1) +
  
  # Adding text for both the p-value and the test-statistic. 
  
  geom_text(
    aes(x = obs_value + 1, y = 0.02, label = paste("p-value:", round(p_value, 4))),
    color = "red",
    size = 8, 
    vjust = -2, 
    hjust = -0.5) +
  geom_text(
    aes(x = obs_value + 1, y = 0.4), 
    color = "darkgreen", 
    label = "test statistic",
    size = 8) +
  
  # Starting with minimal theme (i.e., )
  
  theme_minimal() + 
  
  # Adding Title and Subtitle. 
  # With subtitle, trying to clarify where the test statistic came from. 
  
  
  ggtitle("Chi-Squared Distribution (df = 1)", subtitle = "Test statistic is difference in deviances for main effect after logistic regression") +
  
  # Aligning plot titles. 
  
  theme(
    plot.title = element_text(hjust = 0.5),
    text = element_text(size = 16),
    plot.subtitle = element_text(hjust = 0.5)
  ) + 
  
  # Adding labels for the x-axis and y-axis. 
  
  labs(x = expression(chi^2), y = "Density")



