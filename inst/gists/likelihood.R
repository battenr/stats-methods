# Title: Likelihood 

# Description: This code is to give a toy example of what likelihood is. 


# Setup ----

#... Libraries ----

library(tidyverse) # ol faithful 

# Example Data ----

# Lets assume we have some data on happiness. It's a scale that measures from 
# 0 to 100 how happy you are. 

# Let's simulate some data! There are 100 participants

set.seed(456) # for reproducibility 

df <- rnorm(n = 100, mean = 100, sd = 20) # mean of 100, sd of 20. 

# Important to remember! In reality, we don't actually know the mean and sd 
# of our population. Only that of the sample we have 

mean(df) # actual mean that we have based on our dataset

# Let's Test Some Options! ----

# The likelihood is how likely we are to see the data 

# Probability(Distribution | Data)

# So let's test an option. Say we think the mean is 30 and the sd is 10. 
# We can calculate this for each data point. 

# Let's guess that the distribution has a mean of 50 and sd of 10 

likelihood <- dnorm(df, mean = 50, sd = 10)

likelihood # this gives us a value for each data point. 

# We can take the average to get an overall value 

mean (likelihood)

# Perfect! This means we have some number 0.0026156 

# Let's try another guess. How about a mean of 120 and sd of 10 

likelihood_guess2 <- mean(dnorm(df, mean = 120, sd = 10))

likelihood_guess2 # 0.00996171 

# Okay so this one seems more likely than 50, 10

# Narrow it Down ----

# We can narrow this down even more 

# Code for Linkedin ----


mean_75_sd_10 = data.frame(
  value = rnorm(n = 10000, mean = 85, sd = 10)
)

ggplot(data = mean_75_sd_10, 
       mapping = aes(x = value))+ 
  geom_density(size = 1.5, color = "turquoise") +
  theme_minimal() + 
  theme(
    text = element_text(size = 20),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
  ) +
  labs(y = "Density", x = "Value") +
  ggtitle("Normal Distribution",
          subtitle = "Mean of 85, SD of 10")

dnorm(x = 75, mean = 85, sd = 10)

test_values <- expand.grid(
  mean = c(75, 85), 
  sd = c(5, 10)
)



test_values %>% 
  mutate(
    likelihood = dnorm(x = 75, mean = mean, sd = sd)
  ) %>% 
  arrange(likelihood)


dnorm(x = 75, mean = 75)




The population-level has a 
# mean of 100, and a standard deviation 




df <- rnorm(n = 100, mean = 10, sd = 2)

distribution based on data - likelihood

data based on distribution - probability

dnorm(x = c(10, 50, 100, 1000), mean = 5, sd = 1)

dnorm(0.5, mean = 3, sd = 1)

?dnorm

?dnorm

df <- rnorm(n = 10, mean = 100, sd = 25)

df

dnorm(df, mean = 50, sd = 2)

# Grid Approximation ----

?dbinom

# define grid
p_grid <- seq( from=0 , to=1 , length.out=20 )
# define prior
prior <- rep( 1 , 20 )
# compute likelihood at each value in grid
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

# a distribution assigned to an observed  variable - likelilhood

plot(posterior)

# likelihood - the number of ways that a parameter can produce the data

P(data | distribution) # probability

 #Likelihood - average probability of the data

mean(likelihood)

P(distribution | data) # likelihood of distribution given the data 

# Posterior 

likelihood

# • The relative number of ways that a value p can produce the data is usually called a likelihood. 
# It is derived by enumerating all the possible data sequences that could have happened and then 
# eliminating those sequences inconsistent with the data.


# Say we have an outcome that is happiness ----

# Working through example on youtube here: 
# https://www.youtube.com/watch?v=pYxNSUDSFH4

happiness <- rnorm(n = 100, mean = 10, sd = 2)

mean_values <- seq(from = 5, to = 15, by = 0.5)
sd_values <- seq(from = 1, to = 3, by = 0.5)

df <- expand.grid(
  mean_test = mean_values, 
  sd_test = sd_values
) %>% 
  mutate(average_density = map2_dbl(mean_test, sd_test, 
                                    ~ mean(dnorm(happiness, mean = .x, sd = .y))))

# This obviously not exact. There a different methods we can use to find the 
# maximum likelihood. 






max(df$average_density)

ggplot(data = df, mapping = aes(x = average_density)) + 
  geom_density()

mean(dnorm(happiness, mean = 20, sd = 5)) # 0.0139

plot(density(rnorm(n = 10000, mean = 32, sd = 2.5)))

test = rnorm(n = 100, mean = 100, sd = 10) # this is out test data

mean_values <- seq(from = 90, to = 110, by = 5)
sd_values <- seq(from = 7, to = 12, by = 1)

df <- expand.grid(
  mean_test = mean_values, 
  sd_test = sd_values
)






df <- df %>%
  mutate(average_density = map2_dbl(mean_value, sd_value, 
                                    ~ mean(dnorm(test, mean = .x, sd = .y))))

df <- df %>%
  mutate(average_density = map2_dbl(mean, sd, ~ mean(dnorm(test, mean = .x, sd = .y))))

# View the results
print(df)



df %>% 
  mutate(
    likelihood = dnorm(x =  mean = Var1, sd = Var2)
  )

mean(dnorm(test, mean = 50, sd = 5))

mean(dnorm(test, mean = 100, sd = 5))


mean(dnorm(test, mean = 98, sd = 10))


dnorm(x = 34, mean = 32, sd = 2.5, log = FALSE) # 0.12 - 

dnorm(x = 34, mean = 34, sd = 2.5) # 

?dnorm





?dnorm




