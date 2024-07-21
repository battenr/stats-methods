# Title: LMS Method for Adjusted Z-scores. 

# Description: Using the LMS method proposed by Cole & Green (1990). 
# The data source for this is from the NHANES 2017-2018 survey. The goal was to 
# be able to calculate height-adjusted Z-scores based on the measure. To do that, 
# a model is fit which can then be used to get L, M and S for values. 

# I used the gamlss pacakge to do this. Instead of using the lms function, 
# I used the gamlss() function. 

# Setup ----

#... Libraries ----

library(tidyverse)
library(haven)
library(janitor)
library(gamlss) # for fitting the model 

#... Loading Data ----

demo <- haven::read_xpt("data/DEMO_J.XPT") %>% 
  janitor::clean_names()

dxx <- haven::read_xpt("data/DXX_J.XPT") %>% 
  janitor::clean_names()

bmx <- haven::read_xpt("data/BMX_J.XPT") %>% 
  janitor::clean_names()

dxx <- haven::read_xpt("data/DXXSPN_J.XPT") %>% 
  janitor::clean_names()

# Reformatting NHANES Data ----

# Select patient ID and weights from the demographics dataset 

id_wghts <- demo %>% 
  select(
    seqn,
    wtmec2yr # using this weight for now but need to confirm 
  ) %>% 
  rename(
    id = seqn, 
    wght = wtmec2yr
  )

# Selecting height (bmxht) 

height <- bmx %>% 
  select(
    seqn, 
    bmxht
  ) %>% 
  rename(
    id = seqn, 
    ht = bmxht
  )

# Joining the different components together 

df <- dxx %>% 
  rename(id = seqn) %>% 
  dplyr::left_join(
    height, 
    by = "id"
  ) %>% 
  dplyr::left_join(
    id_wghts, 
    by = "id"
  )

# Fitting a GLMSS ----

# This part required a bit of tinkering. Few notes below: 

# - Based on the LMS method proposed by Cole & Green (1990). 

# - Removed missing values. Doing this is an assumption that removing the 
# missing values won't affect the weights too much. Alternatively, 
# the weights can be inflated. For now, I just removed them 

# - The weights are inflation/expansion weights. This is an assumption based on 
# the summary statistics of the weights. These will "wreck havoc" on the 
# regression model. Due to this, analytic weights are often used by dividing the 
# inflation weights by the mean of the inflation weights. This scales them down

# Using the LMS method proposed by Cole & Green

#... Getting Data Ready for Model ----

test = df %>% 
  
  # Calculating the analytic weights 
  
  mutate(wghta = wght/mean(df$wght)) %>% 
  
  # Selecting the DXA measurement. There are limited values available. For now, 
  # I tried using the "overall spine" category. 
  
  select(id, dxxosbmd, dxxosa, ht, wghta) %>% 
  
  # Removing missing values 
  
  na.omit() %>% 
  
  # This was 
  mutate(y = (dxxosbmd / dxxosa)*100, 
         height = round(ht), # rounding the height. This seemed to make a difference. 
         # before doing this, the model wouldn't fit. 
         
         yd = round(dxxosbmd, 2),
         
         y2 = exp(dxxosbmd)) 


#... Fitting the Model ----

# This was after trying several iterations. 

mod_simple <- gamlss(
  yd ~ height,  # formula
  data = test,  # data. The name of the dataframe isn't really right
  family = BCCG,  # distribution. BCCG is "Box-Cox Cole Green"
  sigma.formula = ~height, # if you don't do this sigma will be constant
  nu.formula = ~height, # same as the comment above for sigmas
  weight = wghta # analytic weights 
  #control = gamlss.control(n.cyc = 100, optim.method = "BFGS") # this works 
)

# Diagnostic Plots ----

plot(mod_simple)

# Predicting L M & S for Height ----

#... New Data. From the minimum available to the max available. 

new_df <- data.frame(
  height = seq(from = 138, to = 196, by =0.5)
)

# Predicting the Values 

predictAll(mod_simple, 
           output = "data.frame",
           newdata = new_df
           ) %>% 
  cbind(new_df$height) %>% 
  rename(
    L = nu, 
    M = mu, 
    S = sigma
  ) %>% view()

# Test Calculation ----

# Test calculation for Z-score. 

# Height was 160 and BMD was 1.136

((1.136/0.962)^(0.525) - 1)/(0.524*0.161)

# 1.08 Z score but was originally 1.70. 



