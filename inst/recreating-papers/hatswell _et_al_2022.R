# Title: Replicating Hatswell

# Setup ----

#... Libraries ----

library(tidyverse)
library(truncnorm)
library(survminer)
library(survival)

#... Dependencies ----

#... Pins ----

set.seed(20220918)

# Data ----

n.eca <- 500
n.intervention <- 750

#External Control Arm (ECA) ----

df.eca <- data.frame(
  id = as.character(rep(1:n.eca, each = 6))
) %>% 
  dplyr::group_by(
    id
  ) %>% 
  dplyr::mutate(
    LOT = seq(1:6),
    starting_lot = rbinom(n = 1, size = 6, prob = 1/3)
  ) %>% 
  ungroup() %>% 
  dplyr::filter(
    starting_lot >= LOT
  )

n.unique <- nrow(df.eca %>% group_by(id) %>% slice(1))

df.eca.lot1 <- df.eca %>% 
  dplyr::group_by(id) %>% 
  dplyr::filter(LOT == 1) %>% 
  ungroup() %>% 
  dplyr::mutate(
    x1 = rtruncnorm(n = n.unique, a = 100, mean = 140, sd = 20),
    x2 = rtruncnorm(n = n.unique, a = 100, mean = 140, sd = 20),
    x3 = rtruncnorm(n = n.unique, a = 100, mean = 140, sd = 20),
    x4 = rtruncnorm(n = n.unique, a = 100, mean = 140, sd = 20),
    x5 = rtruncnorm(n = n.unique, a = 100, mean = 140, sd = 20),
    x6 = rtruncnorm(n = n.unique, a = 100, mean = 140, sd = 20),
    x7 = rtruncnorm(n = n.unique, a = 100, mean = 140, sd = 20),
    x8 = rtruncnorm(n = n.unique, a = 100, mean = 140, sd = 20)
  ) %>% 
  dplyr::select(
    id, LOT, starts_with("x")
  ) 

#... Joining LOT1 values to rest of dataset ----

df.eca.char <- df.eca.lot1 %>% dplyr::full_join(df.eca, by = c("id", "LOT")) %>% 
  dplyr::group_by(id) %>% 
  dplyr::arrange(LOT) %>% 
  dplyr::mutate(
    
    #.....x1 changes ----
    
    x1 = dplyr::case_when(
      LOT < 2 ~ x1, 
      LOT == 2 ~ lag(x1) + rnorm(1, mean = -6, sd = 5)
    ), 
    x1 = dplyr::case_when(
      LOT < 3 ~ x1,
      LOT == 3 ~ lag(x1) + rnorm(1, mean = -6, sd = 5)
    ),
    x1 = dplyr::case_when(
      LOT < 4 ~ x1,
      LOT == 4 ~ lag(x1) + rnorm(1, mean = -6, sd = 5)
    ),
    x1 = dplyr::case_when(
      LOT < 5 ~ x1,
      LOT == 5 ~ lag(x1) + rnorm(1, mean = -6, sd = 5)
    ),
    x1 = dplyr::case_when(
      LOT < 6 ~ x1,
      LOT == 6 ~ lag(x1) + rnorm(1, mean = -6, sd = 5)
    ),
    
    #.....x2 changes ----
    
    x2 = dplyr::case_when(
      LOT < 2 ~ x2, 
      LOT == 2 ~ lag(x2) + rnorm(1, mean = -6, sd = 5)
    ), 
    x2 = dplyr::case_when(
      LOT < 3 ~ x2,
      LOT == 3 ~ lag(x2) + rnorm(1, mean = -6, sd = 5)
    ),
    x2 = dplyr::case_when(
      LOT < 4 ~ x2,
      LOT == 4 ~ lag(x2) + rnorm(1, mean = -6, sd = 5)
    ),
    x2 = dplyr::case_when(
      LOT < 5 ~ x2,
      LOT == 5 ~ lag(x2) + rnorm(1, mean = -6, sd = 5)
    ),
    x2 = dplyr::case_when(
      LOT < 6 ~ x2,
      LOT == 6 ~ lag(x2) + rnorm(1, mean = -6, sd = 5)
    ),
    
    #.....x3 changes ----
    
    x3 = dplyr::case_when(
      LOT < 2 ~ x3, 
      LOT == 2 ~ lag(x3) + rnorm(1, mean = -6, sd = 5)
    ), 
    x3 = dplyr::case_when(
      LOT < 3 ~ x3,
      LOT == 3 ~ lag(x3) + rnorm(1, mean = -6, sd = 5)
    ),
    x3 = dplyr::case_when(
      LOT < 4 ~ x3,
      LOT == 4 ~ lag(x3) + rnorm(1, mean = -6, sd = 5)
    ),
    x3 = dplyr::case_when(
      LOT < 5 ~ x3,
      LOT == 5 ~ lag(x3) + rnorm(1, mean = -6, sd = 5)
    ),
    x3 = dplyr::case_when(
      LOT < 6 ~ x3,
      LOT == 6 ~ lag(x3) + rnorm(1, mean = -6, sd = 5)
    ),
    
    #.....x4 changes ----
    
    x4 = dplyr::case_when(
      LOT < 2 ~ x4, 
      LOT == 2 ~ lag(x4) + rnorm(1, mean = 0, sd = 5)
    ), 
    x4 = dplyr::case_when(
      LOT < 3 ~ x4,
      LOT == 3 ~ lag(x4) + rnorm(1, mean = 0, sd = 5)
    ),
    x4 = dplyr::case_when(
      LOT < 4 ~ x4,
      LOT == 4 ~ lag(x4) + rnorm(1, mean = 0, sd = 5)
    ),
    x4 = dplyr::case_when(
      LOT < 5 ~ x4,
      LOT == 5 ~ lag(x4) + rnorm(1, mean = 0, sd = 5)
    ),
    x4 = dplyr::case_when(
      LOT < 6 ~ x4,
      LOT == 6 ~ lag(x4) + rnorm(1, mean = 0, sd = 5)
    ),
    
    #.....x5 changes ----
    
    x5 = dplyr::case_when(
      LOT < 2 ~ x5, 
      LOT == 2 ~ lag(x5) + rnorm(1, mean = 0, sd = 5)
    ), 
    x5 = dplyr::case_when(
      LOT < 3 ~ x5,
      LOT == 3 ~ lag(x5) + rnorm(1, mean = 0, sd = 5)
    ),
    x5 = dplyr::case_when(
      LOT < 4 ~ x5,
      LOT == 4 ~ lag(x5) + rnorm(1, mean = 0, sd = 5)
    ),
    x5 = dplyr::case_when(
      LOT < 5 ~ x5,
      LOT == 5 ~ lag(x5) + rnorm(1, mean = 0, sd = 5)
    ),
    x5 = dplyr::case_when(
      LOT < 6 ~ x5,
      LOT == 6 ~ lag(x5) + rnorm(1, mean = 0, sd = 5)
    ),
    
    #.....x6 changes ----
    
    x6 = dplyr::case_when(
      LOT < 2 ~ x6, 
      LOT == 2 ~ lag(x6) + rnorm(1, mean = 0, sd = 5)
    ), 
    x6 = dplyr::case_when(
      LOT < 3 ~ x6,
      LOT == 3 ~ lag(x6) + rnorm(1, mean = 0, sd = 5)
    ),
    x6 = dplyr::case_when(
      LOT < 4 ~ x6,
      LOT == 4 ~ lag(x6) + rnorm(1, mean = 0, sd = 5)
    ),
    x6 = dplyr::case_when(
      LOT < 5 ~ x6,
      LOT == 5 ~ lag(x6) + rnorm(1, mean = 0, sd = 5)
    ),
    x6 = dplyr::case_when(
      LOT < 6 ~ x6,
      LOT == 6 ~ lag(x6) + rnorm(1, mean = 0, sd = 5)
    ),
    
    #.....x7 changes ----
    
    x7 = dplyr::case_when(
      LOT < 2 ~ x7, 
      LOT == 2 ~ lag(x7) + rnorm(1, mean = 0, sd = 5)
    ), 
    x7 = dplyr::case_when(
      LOT < 3 ~ x7,
      LOT == 3 ~ lag(x7) + rnorm(1, mean = 0, sd = 5)
    ),
    x7 = dplyr::case_when(
      LOT < 4 ~ x7,
      LOT == 4 ~ lag(x7) + rnorm(1, mean = 0, sd = 5)
    ),
    x7 = dplyr::case_when(
      LOT < 5 ~ x7,
      LOT == 5 ~ lag(x7) + rnorm(1, mean = 0, sd = 5)
    ),
    x7 = dplyr::case_when(
      LOT < 6 ~ x7,
      LOT == 6 ~ lag(x7) + rnorm(1, mean = 0, sd = 5)
    ),
    
    #.....x8 changes ----
    
    x8 = dplyr::case_when(
      LOT < 2 ~ x8, 
      LOT == 2 ~ lag(x8) + rnorm(1, mean = 0, sd = 5) + rnorm(1, mean = -9, sd = 5)
    ), 
    x8 = dplyr::case_when(
      LOT < 3 ~ x8,
      LOT == 3 ~ lag(x8) + rnorm(1, mean = 0, sd = 5) + rnorm(1, mean = -9, sd = 5)
    ),
    x8 = dplyr::case_when(
      LOT < 4 ~ x8,
      LOT == 4 ~ lag(x8) + rnorm(1, mean = 0, sd = 5) + rnorm(1, mean = -9, sd = 5)
    ),
    x8 = dplyr::case_when(
      LOT < 5 ~ x8,
      LOT == 5 ~ lag(x8) + rnorm(1, mean = 0, sd = 5) + rnorm(1, mean = -9, sd = 5)
    ),
    x8 = dplyr::case_when(
      LOT < 6 ~ x8,
      LOT == 6 ~ lag(x8) + rnorm(1, mean = 0, sd = 5) + rnorm(1, mean = -9, sd = 5)
    )
    
  ) %>% 
  dplyr::ungroup()

df.eca <- df.eca.char %>% 
  dplyr::mutate(
    rate = 3 + (1/3)*(x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8),
    lamdba = 1/exp(rate),
    C = rexp(1008, rate = lambda),
    time = pmin(60, C),
    status = as.numeric(C <= 60)
  )

# KM Curves ----

survminer::ggsurvplot(
  fit = survfit(Surv(time, status) ~ LOT, 
                data = df.eca %>% dplyr::filter(starting_lot == LOT), 
  xlab = "Days", 
  ylab = "Overall survival probability")   

  
 