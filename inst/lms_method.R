# Title: LMS Method for Adjusted Z-scores. 

# Description

# Setup ----

#... Libraries ----

library(tidyverse)
library(haven)
library(janitor)
library(gamlss)

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

id_wghts <- demo %>% 
  select(
    seqn,
    wtmec2yr # using this weight for now but need to confirm 
  ) %>% 
  rename(
    id = seqn, 
    wght = wtmec2yr
  )

height <- bmx %>% 
  select(
    seqn, 
    bmxht
  ) %>% 
  rename(
    id = seqn, 
    ht = bmxht
  )

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

# Using the LMS method proposed by COle & Green

dxx$dxxosbmd

df$wght

summary(df$wght)
mean(df$wght)

df$dxxos

test = df %>% 
  mutate(wghta = wght/mean(df$wght)) %>% 
  select(id, dxxosbmd, ht, wghta) %>% 
  na.omit() %>% 
  rename(y = dxxosbmd)

summary(test$wghta)

rm(mod_lms)

?lms


mod_lms <- gamlss::lms(y = dxxosbmd,
                       x = ht, 
                       data = test,
                       #weights = wghta,
                       family = BCT,
                       trans.x = TRUE
)

mod_lms <- gamlss::gamlss(dxxosbmd ~ pb(ht), 
                          weights = wghta,
                          data = test,
                          family = BCT
)






new_df <- data.frame(
  ht = seq(from = 115, 
           to = 195, 
           by = 1
           )
)

gamlss::predictAll(mod_lms, newdata = new_df, output = "data.frame")

summary(mod_lms)

lms <- gamlss::predictAll(mod_lms, newdata = new_df, output = "data.frame") %>% 
  cbind(new_df) %>% 
  rename(
    L = nu, 
    M = mu, 
    S = sigma
  )

# Measurement 


# Test calculation for Z-score. 

# BMD for spine was 0.898 

# Height was 160 and BMD was 1.136

# Calculating Z-score

lms %>% filter(ht == 160)

(((1.136/0.964)^(0.5195)) - 1)/(0.51958*0.161)

# Z score of 1.06. This is compared to the Z-socre of 1.70 not adjusting for height. 
# Seems reasonable on the surface but needs to be checked further. 


# Picking a person with the measurement for BMD for head


?predictAll()

?predictAll

unique(test$ht) %>% sort()



summary(mod_lms)

gamlss

data(db)

# Incorporating Sample Weights ----







