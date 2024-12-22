# Setup ----

library(tidyverse)
library(brms)
library(tidybayes)

#... Simulation Parameters ----

n = 250 # sample size 
beta_trt = 1.5

priors <- c(
  prior(normal(0, 5), class = "b", coef = "x"),    # Prior for the regression coefficients (b)
  prior(normal(0, 5), class = "Intercept"),  # Prior for the intercept
  prior(normal(0, 0.5), class = "b", coef = "z1"),
  prior(normal(0, 0.5), class = "b", coef = "z2")
)

bform <- bf(y_mar | mi() ~ x + z2)

fit <- brms::brm(bform,
                 data = df, 
                 family = gaussian(link = "identity")
)



# With Imputation ----

df <- data.frame(
  z1 = rnorm(n = n, mean = 5, sd = 1), 
  z2 = rbinom(n = n, size = 1, prob = 0.5)
) %>%
  dplyr::mutate(
    #prob = plogis(0.05*z1 + 0.2*z2), # this is an intermediate variable
    prob = plogis(2*z2),
    x = rbinom(n = n, size = 1, prob = prob), 
    y = beta_trt*x + 4*z2 + rnorm(n = n, mean = 0 , sd = 1),
    id = row_number()
  ) %>% 
  dplyr::mutate(
    # Is variable missing 
    is_mcar = rbinom(n = n, size = 1, prob = 0.5), 
    is_mar = rbinom(n = n, size = 1, prob = plogis(0.1*z1)),
    is_mnar = ifelse(y > 16, 1, 0),
    
    # Outcomes for y_mcar, y_mar and y_mnar
    y_mcar = ifelse(is_mcar == 1, NA, y), 
    y_mar = ifelse(is_mar == 1, NA, y),
    y_mnar = ifelse(is_mnar == 1, NA, y)
  )

# Check what percent of each is missing 

lapply(df %>% select(contains("y")), \(x)is.na(x) %>% mean()) # 


#... Fitting Model ----

bform <- bf(y_mar | mi() ~ x + z2)

fit <- brms::brm(bform,
                 data = df, 
                 family = gaussian(link = "identity")
)


#... Repeating ----

update_model <- function(){
  
  # New Data Frame. 
  # New in this sense means that it's just been rerun 
  # (aka new values were created for each )
  
  newdf <- data.frame(
    z1 = rnorm(n = n, mean = 5, sd = 1), 
    z2 = rbinom(n = n, size = 1, prob = 0.5)
  ) %>%
    dplyr::mutate(
      #prob = plogis(0.05*z1 + 0.2*z2), # this is an intermediate variable
      prob = plogis(2*z2),
      x = rbinom(n = n, size = 1, prob = prob), 
      y = beta_trt*x + 4*z2 + rnorm(n = n, mean = 0 , sd = 1),
      id = row_number()
    ) %>% 
    dplyr::mutate(
      # Is variable missing 
      is_mcar = rbinom(n = n, size = 1, prob = 0.5), 
      is_mar = rbinom(n = n, size = 1, prob = plogis(0.1*z1)),
      is_mnar = ifelse(y > 16, 1, 0),
      
      # Outcomes for y_mcar, y_mar and y_mnar
      y_mcar = ifelse(is_mcar == 1, NA, y), 
      y_mar = ifelse(is_mar == 1, NA, y),
      y_mnar = ifelse(is_mnar == 1, NA, y)
    )
  
  #
  
  mod2 <- update(fit, newdata = newdf)
  
  # Updating the posterior 
  
  dfp2 <- mod2 %>% 
    spread_draws(b_x)
  
  results <- median_hdci(dfp2$b_x) %>% 
    select(y, ymin, ymax) %>% 
    lapply(\(x)round(x, 2)) %>% 
    as.data.frame()
  
  return(results)
  
}

update_model()

n.sim = 100 

repeat_samples <- replicate(n = n.sim, 
                            expr = update_model(), 
                            simplify = FALSE)

results <- do.call(rbind, repeat_samples)

#... Bias ----

dfbias <- results %>% 
  mutate(
    bias = y - beta_trt # 1.5 is "true (from earlier in the code)
  )

mean(dfbias$bias)

# Monte Carlo SE of Estimate 

sqrt(1/(n.sim*(n.sim -1)) * sum((dfbias$bias - mean(dfbias$bias))^2))

# Using No Multiple Imputation ----

#... Fitting Model ----

bform <- bf(y_mar ~ x + z2)

fit2 <- brms::brm(bform,
                 data = df, 
                 family = gaussian(link = "identity")
)


#... Repeating ----

update_model2 <- function(){
  
  # New Data Frame. 
  # New in this sense means that it's just been rerun 
  # (aka new values were created for each )
  
  newdf <- data.frame(
    z1 = rnorm(n = n, mean = 5, sd = 1), 
    z2 = rbinom(n = n, size = 1, prob = 0.5)
  ) %>%
    dplyr::mutate(
      #prob = plogis(0.05*z1 + 0.2*z2), # this is an intermediate variable
      prob = plogis(2*z2),
      x = rbinom(n = n, size = 1, prob = prob), 
      y = beta_trt*x + 4*z2 + rnorm(n = n, mean = 0 , sd = 1),
      id = row_number()
    ) %>% 
    dplyr::mutate(
      # Is variable missing 
      is_mcar = rbinom(n = n, size = 1, prob = 0.5), 
      is_mar = rbinom(n = n, size = 1, prob = plogis(0.1*z1)),
      is_mnar = ifelse(y > 16, 1, 0),
      
      # Outcomes for y_mcar, y_mar and y_mnar
      y_mcar = ifelse(is_mcar == 1, NA, y), 
      y_mar = ifelse(is_mar == 1, NA, y),
      y_mnar = ifelse(is_mnar == 1, NA, y)
    )
  
  #
  
  mod2 <- update(fit2, newdata = newdf)
  
  # Updating the posterior 
  
  dfp2 <- mod2 %>% 
    spread_draws(b_x)
  
  results <- median_hdci(dfp2$b_x) %>% 
    select(y, ymin, ymax) %>% 
    lapply(\(x)round(x, 2)) %>% 
    as.data.frame()
  
  return(results)
  
}

update_model2()

n.sim = 100 

repeat_samples <- replicate(n = n.sim, 
                            expr = update_model2(), 
                            simplify = FALSE)

results <- do.call(rbind, repeat_samples)

#... Bias ----

dfbias <- results %>% 
  mutate(
    bias = y - beta_trt # 1.5 is "true (from earlier in the code)
  )

mean(dfbias$bias)

# Monte Carlo SE of Estimate 

sqrt(1/(n.sim*(n.sim -1)) * sum((dfbias$bias - mean(dfbias$bias))^2))

# Comparing Results ----

# 0.009 (0.002) for using MI 

# Not using MI: 0.0203 (0.0207)

# So it is in fact more biased if we don't impute the values 




