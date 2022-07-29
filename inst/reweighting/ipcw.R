# Inverse Probability of Censoring Weighting (IPCW)

# Setup ----

#... Libraries ----

library(tidyverse)
library(simstudy)
library(survival)

#... Dependencies ----

#... Pins ----

# Simulated Survival Data ----

#... Variable Definitions ----

survival.variable.definitions <- simstudy::defData(varname = "sex", dist = "binary", formula = 0.5) %>% 
  
  # Note: need to figure out how to make age increase with time
  
  simstudy::defData(varname = "age", dist = "normal", formula = "45 + 1.5*sex", variance = 10) %>% 
  simstudy::defData(varname = "ecog", dist = "binary", formula = 0.5) %>% 
  
  # Note: assumption is that age, sex and ecog are measured
  
  # Objective Response (used for determining  objective response rate [ORR])
  
  simstudy::defData(varname = "response", dist = "binary", formula = "0.5*ecog", link = "logit") %>%
  
  # Measured variable (can be used for different types of missingness) 
  
  simstudy::defData(varname = "m", dist = "binary", formula = 0.5) %>% 
  
  # Unmeasured variable (can be used for different types of missingness)
  
  simstudy::defData(varname = "u", dist = "binary", formula = 0.5) 

#... Survival Definitions ----

sdef <- defSurv(varname = "survTime", formula = "1.5*u", scale = "0.5*m + (1-m)*25",
                shape = "m*1 + (1-m)*1.5")
sdef <- defSurv(sdef, varname = "censorTime", scale = 80, shape = 1)


dtSurv <- genData(300, survival.variable.definitions)
dtSurv <- genSurv(dtSurv, sdef)


formula1 <- c(0.45, 0.35, 0.2)
dtSurv <- simstudy::trtObserve(dtSurv, formulas = formula1, 
                               logit.link = TRUE, grpName = "LOT")



#..... Censoring ----

cdef <- defDataAdd(varname = "obsTime", formula = "pmin(survTime, censorTime)", dist = "nonrandom")
cdef <- defDataAdd(cdef, varname = "status", formula = "I(survTime <= censorTime)",
                   dist = "nonrandom")

dtSurv <- addColumns(cdef, dtSurv)

dtSurv

# Kaplan Meier Curve ----

km.fit <- survfit(Surv(censorTime, status) ~ LOT, data=dtSurv)
km.fit

# Cox Proportional Hazard Model ----

