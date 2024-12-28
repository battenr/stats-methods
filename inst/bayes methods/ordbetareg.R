# Title: Ordered Beta Regression 

library(tidyverse)
library(brms)
library(tidybayes)
library(bayesplot)
library(ordbetareg)

# Replicating OrdBetaReg ----

# Fitting Model First 

ss = 250

df <- data.frame(
  z = rnorm(n = ss, mean = 5, sd = 2)
) %>% 
  dplyr::mutate(
    x = rbinom(n = ss, size = 1, prob = plogis(0.03*z)),
    y = rordbeta(n = ss, mu = plogis(0.1*x + 0.03*z))
  )

ggplot(data = df, 
       mapping = aes(x = y)) + 
  stat_halfeye() 
  

ordmod <- ordbetareg::ordbetareg(formula = y~x + z, 
                                 data = df)

update_model <- function(mod = ordmod){
  
  # New Data Frame. 
  # New in this sense means that it's just been rerun 
  # (aka new values were created for each )
  
  newdf <- data.frame(
    z = rnorm(n = ss, mean = 5, sd = 2)
  ) %>% 
    dplyr::mutate(
      x = rbinom(n = ss, size = 1, prob = plogis(0.03*z)),
      y = rordbeta(n = ss, mu = plogis(0.1*x + 0.03*z))
    )
  
  #
  
  mod2 <- update(mod, newdata = newdf)
  
  # Updating the posterior 
  
  dfp2 <- mod2 %>% 
    spread_draws(b_x)
  
  results <- median_hdci(dfp2$b_x) %>% 
    select(y, ymin, ymax) %>% 
    lapply(\(x)round(x, 2)) %>% 
    as.data.frame()
  
  return(results)
  
}

# update_model() # test run 

n.sim = 100 # how many times to simulate 

repeated_samples <- replicate(n = n.sim, expr = update_model(), simplify = FALSE)

results <- do.call(rbind, repeated_samples)

dfbias <- results %>% 
  mutate(
    bias = y - 0.1 # 1.5 is "true (from earlier in the code)
  )

mean(dfbias$bias)

# Monte Carlo SE of Estimate 

sqrt(1/(n.sim*(n.sim -1)) * sum((dfbias$bias - mean(dfbias$bias))^2))

# 0.0377 (0.01738)


# Replicating GLM ----

glm_model <- function(){
  
  # New Data Frame. 
  # New in this sense means that it's just been rerun 
  # (aka new values were created for each )
  
  df <- data.frame(
    z = rnorm(n = ss, mean = 5, sd = 2)
  ) %>% 
    dplyr::mutate(
      x = rbinom(n = ss, size = 1, prob = plogis(0.03*z)),
      y = rordbeta(n = ss, mu = plogis(0.1*x + 0.03*z))
    )
  
  #
  
  modglm <- glm(y ~ x + z, 
                data = df,
                family = gaussian(link = "identity")
  )
  
  result <- broom::tidy(modglm) %>% 
    dplyr::filter(term == "x") %>% 
    select(estimate) %>% 
    as.data.frame()
  
  return(result)
  
}

glm_model()

n.sim = 100 # how many times to simulate 

glmrepeated_samples <- replicate(n = n.sim, expr = glm_model(), simplify = FALSE)

results <- do.call(rbind, glmrepeated_samples)

dfbias <- results %>% 
  mutate(
    bias = estimate - 0.1 # 1.5 is "true (from earlier in the code)
  )

mean(dfbias$bias)

# Monte Carlo SE of Estimate 

sqrt(1/(n.sim*(n.sim -1)) * sum((dfbias$bias - mean(dfbias$bias))^2))

