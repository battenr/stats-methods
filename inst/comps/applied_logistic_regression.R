# Applied Logistic Regression ----

# Description: Using this script to test/learn things for my comprehensive exam
# while reading Hosmer & Lemeshow's Applied Logistic Regression 

# Setup ----

library(tidyverse)


# Interaction terms ----

age = rnorm(n = 100, mean = 50, sd = 5)
sex = rbinom(n = 100, size = 1, prob = 0.5)

outcome = rbinom(n = 100, size = 1, prob = plogis(0.5*sex))

df = data.frame(
  age, 
  sex, 
  outcome
) %>% 
  dplyr::mutate(
    pi = plogis(0.01*age + 0.2*sex + 0.3*age*sex), # note plogis is the same as exp(formula) / [1 + exp(formula)]
    step1 = pi/ (1-pi),
    logit = log(step1, base = 2)
  )

ggplot(data = df, 
       mapping = aes(x = age, y = logit, group = sex, color = sex)
       ) + 
  geom_line()


mod <- glm(outcome ~ age + sex, 
           family = binomial, 
           data = df)

library(broom)

augment(mod)

?augment

df.updated = df %>% 
  dplyr::mutate(
    pi_pred = predict(mod, type = "response"),
    mj = 1, # there will only be one person in each covariate pattern 
    ri = -sqrt(mj)*sqrt(pi_pred/(1-pi_pred)),
    residuals = residuals(mod),
    change_in_chi = ri^2
  )

residuals(mod)



ggplot(data = df.updated, 
       mapping = aes(x = pi_pred, y = change_in_chi)) + 
  geom_point()



unique(df) # so literally every person is unique. Now let's fit some Pearson residuals to 
# this bad boy

# Playing with Plots ----

n = 500

df = data.frame(
  x1 = rbinom(n = n, size = 1, prob = 0.5), 
  x2 = rbinom(n = n, size = 1, prob = 0.3), 
  x3 = sample(x = c("cat1", "cat2", "cat3", "cat4"),
              size = 100, 
              replace = TRUE)
) %>% 
  dplyr::mutate(
    trt = rbinom(n = n, size = 1, prob = plogis(0.3*x1)),
    outcome = rbinom(n = n, size = 1, prob = plogis(0.3*x1))
  )

mod <- glm(outcome ~ x1 + x2 + x3, 
           family = binomial(),
           data = df)

library(dplyr)
library(ggplot2)

library(dplyr)
library(ggplot2)


#... Trying to calculate delta D ----




pattern_probs <- df %>% 
  mutate(
    predicted = predict(mod, type = "response")
  ) %>% 
  dplyr::group_by(
    x1, x2, x3
  ) %>% 
  summarise(
    AvgPredictedProb = mean(predicted)
  )

# Step 1: Group by covariate patterns and calculate average predicted probabilities
pattern_probs <- df %>%
  group_by(x1, x2, x3) %>%
  summarise(AvgPredictedProb = mean(predict(mod, newdata = ., type = "response")), .groups = 'drop')

# Initialize a vector to store Delta D for each pattern
pattern_probs$DeltaD <- NA

# Step 3: Calculate Delta D for Each Covariate Pattern
for(i in 1:nrow(pattern_probs)) {
  pattern <- pattern_probs[i, ]
  
  # Create a reduced dataset excluding the current covariate pattern
  reduced_df <- df %>% 
    filter(!(x1 == pattern$x1 & x2 == pattern$x2 & x3 == pattern$x3))
  
  # Fit the model to the reduced dataset
  reduced_mod <- glm(outcome ~ x1 + x2 + x3, family = binomial(), data = reduced_df)
  
  # Calculate Delta D
  pattern_probs$DeltaD[i] <- deviance(mod) - deviance(reduced_mod)
}

# Step 4: Plot Delta D vs. Average Predicted Probabilities for Covariate Patterns
ggplot(pattern_probs, aes(x = AvgPredictedProb, y = DeltaD)) +
  geom_point() +
  labs(x = "Average Predicted Probabilities", y = "Delta D",
       title = "Delta D vs. Average Predicted Probabilities for Covariate Patterns") +
  theme_minimal() + 
  lims(x = c(0, 1)) # need to set scale from 0 to 1 since predicted probabilites






?deviance



mod.partial <- glm(
  
)

df %>% count(x3)

deviance(mod)

summary(mod)

leverage(mod)





#... Calculating change in chi-squared statistic to plot against predicted probabilty ----




df = data.frame(
  x1 = rbinom(n = n, size = 1, prob = 0.5), 
  x2 = rbinom(n = n, size = 1, prob = 0.3), 
  x3 = sample(x = c("cat1", "cat2", "cat3", "cat4"),
              size = 100, 
              replace = TRUE)
) %>% 
  dplyr::mutate(
    trt = rbinom(n = n, size = 1, prob = plogis(0.3*x1)),
    outcome = rbinom(n = n, size = 1, prob = plogis(0.3*x1))
  ) %>%
  dplyr::group_by(x1, x2, x3) %>% 
  mutate(pattern_number = cur_group_id()) %>% 
  ungroup()

mod <- glm(outcome ~ trt + x1 + x2 + x3, 
           family = binomial(),
           data = df)

# Need to break up into each covariate pattern (that's what's recommended by Hosmer & Lemeshow)

# Calculating statistic per covariate pattern

new_df = df %>% 
  distinct(pattern_number, .keep_all = TRUE) %>% 
  select(x1, x2, x3, trt) %>% 
  arrange(x2, x2, x3, trt)

predicted_probs <- predict(mod, type = "response", newdata = new_df) %>% 
  as.data.frame() 

predicted_probs$pred_pi <- predicted_probs$. # renaming 

predicted_probs = predicted_probs %>% 
  mutate(
    pattern_number = seq(1:16)
  )

df_mj <- df %>% group_by(pattern_number) %>% count() %>% 
  rename(mj = n)

cov_pattern_stats <- df %>% 
  mutate(
    hj = hatvalues(mod)
  ) %>% 
  distinct(pattern_number, .keep_all = TRUE) %>% 
  select(x1, x2, x3, pattern_number, hj)  %>%
  left_join(df_mj, 
            by = "pattern_number") %>% 
  left_join(predicted_probs, 
            by = "pattern_number") %>% 
  
  # mj is just n for j covariate 
  mutate( # it's response because it's E(Y|x). 
    rj = -sqrt(mj)*sqrt(pred_pi / (1-pred_pi)),
    # rsj = rj / sqrt(1-hj)
    rsj = rj/sqrt(1-hj),
    change_chi = rsj^2,
    change_beta = (rsj^2 * hj)/((1-hj)^2)
  ) 

# Delta Chi-Squared vs Predicted Pi ----

delta_chi_vs_pi <- ggplot(data = cov_pattern_stats, 
       mapping = aes(x = pred_pi, y = change_chi)) + 
  geom_point() +
  lims(x = c(0, 1)) + 
  labs(x = "Predicted Pi", y = "Change in Chi-Squared Statistic")


#... Delta D vs Predicted Pi ----

df_di <- df %>% 
  mutate(
    dj = residuals(mod, type = "deviance"),
    hj = hatvalues(mod)
  ) %>% 
  dplyr::group_by(
    pattern_number, 
    outcome
  ) %>% 
  slice_head() %>% 
  dplyr::ungroup() %>% 
  left_join(predicted_probs, 
            by = "pattern_number")  %>% 
  mutate(
    delta_d = (dj^2)/(1-hj)
  )
  
  # dplyr::select(
  #   x1, 
  #   x2, 
  #   x3, 
  #   trt,
  #   pattern_number,
  #   outcome,
  #   deviance 
  # ) %>% 

delta_d_vs_pi <- ggplot(data = df_di, 
       mapping = aes(x = pred_pi, y = delta_d)) + 
  geom_point() +
  lims(x = c(0, 1)) + 
  labs(x = "Predicted Pi", y = "Change in Delta")

#... Delta B vs Predicted Pi ----

delta_b_vs_pi <- ggplot(data = cov_pattern_stats, 
       mapping = aes(x = pred_pi, y = change_beta)) + 
  geom_point() +
  lims(x = c(0, 1)) + 
  labs(x = "Predicted Pi", y = "Change in Beta")

# Piecing Together ----

library(patchwork)

delta_d_vs_pi + delta_chi_vs_pi + delta_b_vs_pi

# Delta Chi-Squared vs Leverage

delta_chi_vs_hj <- ggplot(data = cov_pattern_stats, 
                          mapping = aes(x = hj, y = change_chi)) + 
  geom_point() +
  lims(x = c(0, 1)) + 
  labs(x = "Leverage", y = "Change in Chi-Squared Statistic")

# Delta D vs Leverage


delta_d_vs_hj <- ggplot(data = df_di, 
                        mapping = aes(x = pred_pi, y = delta_d)) + 
  geom_point() +
  lims(x = c(0, 1)) + 
  labs(x = "Leverage", y = "Change in Delta")

# Delta B vs Leverage


delta_b_vs_hj <- ggplot(data = cov_pattern_stats, 
                        mapping = aes(x = pred_pi, y = change_beta)) + 
  geom_point() +
  lims(x = c(0, 1)) + 
  labs(x = "Leverage", y = "Change in Beta")

delta_chi_vs_hj + delta_d_vs_hj + delta_b_vs_hj
