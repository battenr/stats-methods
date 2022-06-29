# Simulating Longitudinal Data 

# Setup ----

#... Libraries ----

library(tidyverse)
library(simstudy)

#... Dependencies ----

# No dependencies

# Simulating Data ----

#... Cross-Section Data ----

# Setting seed for reproducibility

set.seed(2022)

# First need to make cross-sectional data 

# Independent variables

tdef <- simstudy::defData(varname = "sex", dist = "binary", formula = 0.5) %>% 
  simstudy::defData(varname = "age", dist = "normal", formula = 45, variance = 10) %>% 
# tdef <- simstudy::defData(tdef, varname = "bzd_dosage", dist = "normal", "1.5*bzd+1.5*age+0.5*sex")
# parking bzd dosage for now for simplicity
  simstudy::defData(varname = "province", dist = "categorical", formula = "0.2; 0.2; 0.2; 0.2; 0.2") %>% 
  simstudy::defData(varname = "marital_status", dist = "categorical", formula = "0.3; 0.3; 0.4") %>% 
  simstudy::defData(varname = "education", dist = "categorical", formula = "0.3; 0.2; 0.5") %>% 
  simstudy::defData(varname = "urban_rural", dist = "categorical", formula = "0.7; 0.3") %>% 
  simstudy::defData(varname = "income", dist = "categorical", formula = "0.2; 0.2; 0.2; 0.2; 0.2") %>% 
  
  # Adverse Event 
  
  simstudy::defData(varname = "anxiety", dist = "binary", formula = "0+1.5*age+0.5*sex", link = "logit")  %>% 
  simstudy::defData(varname = "depression", dist = "binary", formula = 0.5) %>% 
  simstudy::defData(varname = "diabetes_mellitus", dist = "binary", formula = 0.5) %>% 
  simstudy::defData(varname = "high_blood_pressure", dist = "binary", formula = 0.5) %>% 
  simstudy::defData(varname = "insomnia", dist = "binary", formula = 0.5) %>% 
  simstudy::defData(varname = "myocardial_infarction", dist = "binary", formula = 0.5) %>% 
  simstudy::defData(varname = "stroke", dist = "binary", formula = 0.5) %>% 
  simstudy::defData(varname = "cancer", dist = "binary", formula = 0.5) %>% 
  simstudy::defData(varname = "cardiovascular_disease", dist = "binary", formula = 0.5) %>% 
  simstudy::defData(varname = "copd", dist = "binary", formula = 0.5) %>% 
  simstudy::defData(varname = "dementia", dist = "binary", formula = 0.5) %>% 
  simstudy::defData(varname = "infections", dist = "binary", formula = 0.5) %>% 
  simstudy::defData(varname = "pneumonia", dist = "binary", formula = 0.5) %>% 

# Variables needed for making longitudinal
  
  simstudy::defData(varname = "nCount", dist = "noZeroPoisson", formula = 6) %>% 
  simstudy::defData(varname = "mInterval", dist = "gamma", formula = 30, variance = 0.01) %>% 
  simstudy::defData(varname = "vInterval", dist = "nonrandom", formula = 0.07)

# Generating cross-sectional data

dtTrial <- simstudy::genData(1000, tdef)
dtTrial

#... Add variable to existing data ----

#... Making Longitudinal with Varying Time Periods ----

dtTime <- simstudy::addPeriods(dtTrial)

dtTime

# Adding Observed Treatment ----

#... If Dependent on Covariates ----

exp(0.25)

formula1 <- c("0 + .252*sex + .5*age", "0 - 2*sex + .5*age")

dtExp <- simstudy::trtObserve(dtTime, formulas = formula1, logit.link = TRUE, grpName = "bzd")

# Update Definitions ----

# Updating definition to make age increase with time

df <- dtExp %>% 
  dplyr::mutate(
    age = age+(timeID/12)
  ) %>% 
  dplyr::rename(
    patientid = id
  ) 
