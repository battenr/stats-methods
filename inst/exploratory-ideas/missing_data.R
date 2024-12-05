



library(tidyverse)
library(smdi)

# Simulating Data ----

ss = 500

df <- data.frame(
  x = rbinom(n = ss,
             size = 1,
             prob = 0.5 
             ),
  c1 = rnorm(n = ss, mean = 10, sd = 2), 
  c2 = rnorm(n = ss, mean = 5, sd = 1), 
  c3 = rbinom(n = ss, size = 1, prob = 0.4),
  unknown1 = rnorm(n = ss, mean = 5, sd = 2)
) %>% 
  dplyr::mutate(
    y = rnorm(n = ss, mean = 12, sd = 2) + 1.5*x
  )

summary(df$y)


# Missing Data Mechanisms ----

# From Stef Van Buuren 

# MCAR - Probability of being missing is the same for all cases
# MAR - Probabiliy of being missing is the same only within groups defined by the observed data
# MNAR - Probabiliy of being missing varies for reasons that are unknown to us 

df_na = df %>% 
  dplyr::mutate(
    
    # MCAR 
    
    value_mcar = rbinom(n = ss, size = 1, prob = 0.45),
    y_mcar = case_when(
      value_mcar == 1 ~ NA, 
      value_mcar == 0 ~ y
    ),
    
    # MAR 
    
    value_mar = rbinom(n = ss, size = 1, prob = 0.05*c2 + 0.1*c3),
    y_mar = case_when(
      value_mar == 1 ~ NA, 
      value_mar == 0 ~ y
    ),
    
    # MNAR 
    
    value_mnar = rbinom(n = ss, size = 1, prob = plogis(0.1*unknown1)),
    y_mnar = case_when(
      value_mnar == 1 ~ NA, 
      value_mnar == 0 ~ y
    ) 
    
  )

#... Separating Each Dataset Out ----

truevalue <- df_na %>% 
  dplyr::select(
    starts_with("c"), 
    x, 
    y
  )

mcar <- df_na %>% 
  dplyr::select(
    starts_with("c"), 
    x, 
    y_mcar
  )
  
mar <- df_na %>% 
  dplyr::select(
    starts_with("c"), 
    x, 
    y_mar
  )

mnar <- df_na %>% 
  dplyr::select(
    starts_with("c"), 
    x, 
    y_mnar
  )

# Testing Out Some Things ----

smdi::md.pattern(df)


# MCAR ----

smdi::smdi_asmd(data = mcar, covar = "y_mcar")
smdi::smdi_asmd(data = mar, covar = "y_mar")
smdi::smdi_asmd(data = mnar, covar = "y_mnar")

smdi::smdi_asmd(data = mcar, covar = "y_mcar") # Remember, low aSMD means the groups are 
# similar 

smdi::smdi_little(data = mcar) # Fail to reject null. Null is MCAR 

smdi::smdi_little(data = mar)
smdi::smdi_hotelling(data = mar, covar = "y_mar")

# Random Forest for AUC ----

# Using a random forest for AUC

# You'll be able to predict the missingness at random, whereas you may not be able to 
# with the MCAR/MNAR (based on Weberpals paper)

smdi_rf(data = mcar)
smdi_rf(data = mar)
smdi_rf(data = mnar)

# Crude vs Adjusted ----

#... MCAR ----

# Hardly changes

glm(
  value_mcar ~ 1, 
  data = df_na,
  family = binomial(link = "logit")) %>% 
  broom::tidy()

glm(
  value_mcar ~ x + c1 + c2 + c3,
  data = df_na,
  family = binomial(link = "logit")) %>% 
  broom::tidy()

#... MAR ----

glm(
  value_mar ~ , 
  data = df_na,
  family = binomial(link = "logit")) %>% 
  broom::tidy()

glm(
  value_mar ~ y_mar + x + c1 + c2 + c3,
  data = df_na,
  family = binomial(link = "logit")) %>% 
  broom::tidy()

#... MNAR ----

# Random Code ----

glm(
  value_mcar ~ 1,
  data = df_na,
  family = binomial(link = "logit")) %>% 
  broom::tidy()

glm(
  value_mcar ~ x + c1 + c2 + c3,
  data = df_na,
  family = binomial(link = "logit")) %>% 
  broom::tidy()

glm(
  value_mar ~ x,
  data = df_na,
  family = binomial(link = "logit")) %>% 
  broom::tidy()

glm(
  value_mar ~ x + c1 + c2 + c3,
  data = df_na,
  family = binomial(link = "logit")) %>% 
  broom::tidy()




?smdi_rf

smdi_data_complete %>% view()

randomForest::randomForest(x = , 
                           data = )


smdi::smdi_diagnose(data = mnar)
 
glm(y_mcar ~ c1 + c2 + c3, 
    data = mcar, 
    family = gaussian(link = "identity")) %>% 
  broom::tidy() 

glm(y_mar ~ c1 + c2 + c3, 
    data = mar, 
    family = gaussian(link = "identity")) %>% 
  broom::tidy() 

0.858
0.715
0.196

glm(y_mnar ~ c1 + c2 + c3, 
    data = mcar, 
    family = gaussian(link = "identity")) %>% 
  broom::tidy() 
 
 ?smdi_outcome

smdi::smdi_asmd(data = mcar, covar = "y_mcar")




