# Comparing Bayesian Parametric G-Formula to Other Methods 

# Idea would be that we often know what a confounder is, or at least can guess
# what the effect/association would be in terms of magnitude. Due to this
# if say we have an external control arm, we could introduce this information 
# in the form of a prior. 

# Could we use Bayesian PS using a prior for the confounder 

library(tidyverse)
library(brms)

n = 250

df <- data.frame(
  c1 = rnorm(n = n, 
            mean = 10, 
            sd = 2)
) %>% 
  mutate(
    trt = rbinom(n = n,
                 size = 1, 
                 prob = plogis(0.02*c1)),
    y = 1.5*trt + 0.05*c1 + rnorm(n = n)
  )

mod <- brms::brm(y ~ trt + c1, 
                 data = df, 
          family = gaussian(),
          set_prior("normal(0, 1)", # using random prior (especially since we know the "truth") 
                    # This is important for confounder adjustment in causal inference. 
                    class = "b",
                    coef = "c1"))

mod %>% brms::posterior_predict() 

# Predicting ----

trt_1 <- df %>% 
  mutate(trt = 1)

predict_trt1 <- posterior_predict(mod, 
                                  newdata = trt_1, 
                                  ndraws = 10) %>% 
  as.data.frame() %>% 
  pivot_longer(
    cols = everything(),
    names_to = "subjid",
    values_to = "y1"
  ) %>% 
  group_by(subjid) %>% 
  mutate(
    obs = row_number()
  ) %>% 
  ungroup()

trt_0 <- df %>% 
  mutate(trt = 0)

predict_trt0 <- posterior_predict(mod, 
                                  newdata = trt_0, 
                                  ndraws = 10) %>% 
  as.data.frame() %>% 
  pivot_longer(
    cols = everything(),
    names_to = "subjid",
    values_to = "y0"
  ) %>% 
  group_by(subjid) %>% 
  mutate(
    obs = row_number()
  ) %>% 
  ungroup()

predictions <- predict_trt1 %>% 
  left_join(
    predict_trt0
  ) %>% 
  mutate(
    effect = y1 - y0
  )

mean(predictions$effect)

# Rudimentary Plot of Densities ----

ggplot(data = predictions) + 
  geom_density(mapping = aes(x = y1), color = "red", fill = "red") + 
  geom_density(mapping = aes(x = y0), color = "blue", fill = "blue") + 
  theme_minimal()

library(tidybayes)

?add_predicted_draws

df %>%
  add_predicted_draws(mod, ndraws = 10) %>%  # adding the posterior distribution
  ggplot(aes(x = France %>%
  group_by(Location.of.population) %>%
  add_predicted_draws(france3_mbrms) %>%
  ggplot(aes(x = year, y = pop, color = ordered(Location.of.population), fill = ordered(Location.of.population))) +
  stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50), alpha = 1/4) +
  geom_point(data = France) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw() +
  ylab("Calidris canutus abundance\n") +
  xlab("\nYear") +
  theme_bw() +
  theme(legend.title = element_blank())), y = trt)) +  
  stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50),  # regression line and CI
                  alpha = 0.5, colour = "black") +
  geom_point(data = df, colour = "darkseagreen4", size = 3) +   # raw data
  scale_fill_brewer(palette = "Greys") +
  ylab("Calidris canutus abundance\n") +  # latin name for red knot
  xlab("\nYear") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = c(0.15, 0.85)))

mean(predictions$effect)

# Now let's compare to frequentist approach ----

freq_fit <- glm(y ~ trt + c1,
                data = df)

freq1 <- df %>% 
  mutate(trt = 1)

predict1 <- predict(freq_fit, 
                    newdata = freq1) %>% 
  as.data.frame() %>% 
  mutate(
    id = row_number()
  )

predict1$y1 <- predict1$.

freq0 <- df %>% 
  mutate(trt = 0)

predict0 <- predict(freq_fit, 
                    newdata = freq0) %>% 
  as.data.frame() %>% 
  mutate(
    id = row_number()
  )

predict0$y0 <- predict0$.

freq_result <- predict1 %>% 
  select(id, y1) %>% 
  left_join(predict0 %>% select(id, y0)
            ) %>% 
  mutate(
    effect = y1 - y0
      )



broom::tidy(freq_fit)

# Now let's use previous information about the confounders (from another study, we can 
# get the strength of the confounder) 
  

# PS with Priors ----

# Is predicting the propensity score with Bayesian models any better? What are some drawbacks? 

# Using BART when we know some prior information about the confounders. Is that any 
# better than using frequentist approach

# Basic idea is: is it worth it? 

library(WeightIt)

w1 <- weightit()

data("lalonde")

w1 <- weightit(treat ~ age + educ + married +
                  nodegree + re74, data = lalonde,
                method = "bart", estimand = "ATT")



