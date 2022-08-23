# Learning more about the variance-covariance matrix 

# Setup ----

#... Libraries ----

library(tidyverse)
library(simstudy)

# Simulated Data ----

# Using cross-sectional data for this
?defData
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
  
  simstudy::defData(varname = "anxiety", dist = "binary", formula = "-3+0.5*sex", link = "logit")  %>% 
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
  simstudy::defData(varname = "pneumonia", dist = "binary", formula = 0.5) 

#... Generating cross-sectional data ----

df.outcomes <- simstudy::genData(1000, tdef)
df.outcomes

#... Adding BZD (Observed Treatment) ----

# If Dependent on Covariates 

formula1 <- c("0 + .252*sex + .5*age", "0 - 2*sex + .5*age")

df.bzd <- simstudy::trtObserve(df.outcomes, formulas = formula1, logit.link = TRUE, grpName = "bzd")

# Formatting Data ----

df.bzd = df.bzd %>% 
  dplyr::mutate(
    bzd = dplyr::case_when(
      bzd == 1 ~ 0, 
      bzd == 2 ~ 1
    )
  )

# Logistic Regression for Anxiety ----

df.bzd %>% count(anxiety)

mod <- stats::glm(anxiety ~ bzd + age + sex + province + marital_status,
                  family = binomial(),
                  data = df.bzd)

# Variance-Covariance Matrix 

matrix <- vcov(mod)


cov2cor(matrix)

broom::tidy(mod)
matrix

sqrt(1.734499e-01)
