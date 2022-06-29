# IPMW: Inverse Probability of Missing Weights

# Idea: could use covariates that are available to inform the patients with missing data 

# Setup ----

#... Libraries ----

library(tidyverse)
library(simstudy)
library(car)

#... Dependencies ----

#... Pins ----

# Simulated Data ----

# Simulating data that would be expected from an oncology EHR

variable.definitions <- simstudy::defData(varname = "sex", dist = "binary", formula = 0.5) %>% 
  
  # Note: need to figure out how to make age increase with time
  
  simstudy::defData(varname = "age", dist = "normal", formula = "45 + 1.5*sex", variance = 10) %>% 
  simstudy::defData(varname = "ecog", dist = "binary", formula = 0.5) %>% 
  
  # Note: assumption is that age, sex and ecog are measured
  
  # Objective Response (used for determining  objective response rate [ORR])
  
  simstudy::defData(varname = "response", dist = "binary", formula = "0.5*ecog", link = "logit") %>%
  
  # Measured variable (can be used for different types of missingness) 
  
  simstudy::defData(varname = "m", dist = "binary", formula = 0.5) %>% 

  # Unmeasured variable (can be used for different types of missingness)
  
  simstudy::defData(varname = "u", dist = "binary", formula = 0.5) %>% 
  
  # Variables needed for making longitudinal
  
  simstudy::defData(varname = "nCount", dist = "noZeroPoisson", formula = 6) %>% 
  simstudy::defData(varname = "mInterval", dist = "gamma", formula = 30, variance = 0.01) %>% 
  simstudy::defData(varname = "vInterval", dist = "nonrandom", formula = 0.07)

#... Generating cross-sectional data ----

cross.sectional.data <- simstudy::genData(1000, variable.definitions)
cross.sectional.data

#... Adding Treatment ----

cross.sectional.data <- simstudy::trtObserve(cross.sectional.data, formulas = 0.5, logit.link = FALSE, 
                                             grpName = "trt")

#... Making Longitudinal ----

longitudinal.data <- simstudy::addPeriods(cross.sectional.data, n = 5)

#... Defining Missing Data ----

# Assumption is that age and sex are not missing

missing.def <- simstudy::defMiss(varname = "ecog", formula = 0.1, logit.link = TRUE,
                                 monotonic = FALSE, baseline = TRUE) %>% 
  
  # Note: assumption is that age, sex and ecog are measured
  
  # Objective Response (used for determining  objective response rate [ORR])
  
  simstudy::defMiss(varname = "response", formula = 0.4, logit.link = FALSE,
                    monotonic = FALSE, baseline = FALSE) %>% 
  
  # Unmeasured variable (can be used for different types of missingness)
  
  simstudy::defMiss(varname = "m", formula = 0.5, logit.link = FALSE,
                    monotonic = FALSE, baseline = FALSE) %>%  
  
  simstudy::defMiss(varname = "u", formula = 0.5, logit.link = FALSE,
                    monotonic = FALSE, baseline = FALSE) 

#..... Missing matrix ----

missing.matrix <- simstudy::genMiss(longitudinal.data , missing.def, idvars = "id")

#..... Generating Missing Data (MCAR) ----

data.mcar <- simstudy::genObs(longitudinal.data, missing.matrix, idvars = "id") 

# Data for Testing Missing Data Method ----

df <- data.mcar %>% # from above section
  dplyr::mutate(
    age = age + (period/5)
  )

# Checking Proportion of Missing per Patient ----

data.mcar %>% 
  dplyr::group_by(id) %>% 
  count(response)




