# Simulating Longitudinal Data 

# Setup ----

#... Libraries ----

library(tidyverse)
library(simstudy)
library(survival)
library(survminer) # for  KM curves

#... Dependencies ----

# No dependencies

# Simulating Data ----

# Setting seed for reproducibility

set.seed(2022)

# Baseline data definitions

def <- defData(varname = "age", formula = 40, variance = 10, dist = "normal") %>% 
  defData(varname = "female", formula = "-1.5 + age*0.1", dist = "binary", link = "logit") #%>% 
  # defData(varname = "bzd", formula = "1.5 + 0.2*age - 0.5*female", dist = "binary", link = "logit")



# Survival data definitions

sdef <- defSurv(varname = "survTime", formula = "1.5*female", scale = "bzd*50 + (1-bzd)*25",
                shape = "bzd*1 + (1-bzd)*1.5") %>% 
  defSurv(varname = "censorTime", scale = 80, shape = 1)

sdef

# Baseline data definitions

dtSurv <- genData(300, def)
dtSurv <- genSurv(dtSurv, sdef)

head(dtSurv)

# A comparison of survival by group and x1

dtSurv[, round(mean(survTime), 1), keyby = .(bzd, female)]

cdef <- defDataAdd(varname = "obsTime", formula = "pmin(survTime, censorTime)", dist = "nonrandom") %>% 
  defDataAdd(varname = "status", formula = "I(survTime <= censorTime)",
                   dist = "nonrandom")

dtSurv <- addColumns(cdef, dtSurv)

head(dtSurv)

# estimate proportion of censoring by x1 and group

dtSurv[, round(1 - mean(status), 2), keyby = .(bzd, female)]

# Kaplan Meier Curve ----

survminer::ggsurvplot(
  fit = survfit(Surv(obsTime, status) ~ 1, data = dtSurv), 
  xlab = "Days", 
  ylab = "Overall survival probability")

survminer::ggsurvplot(
  fit = survfit(Surv(obsTime, status) ~ bzd, data = dtSurv), 
  xlab = "Days", 
  ylab = "Overall survival probability")

survminer::ggsurvplot(
  fit = survfit(Surv(obsTime, status) ~ female, data = dtSurv), 
  xlab = "Days", 
  ylab = "Overall survival probability")

survminer::ggsurvplot(
  fit = survfit(Surv(obsTime, status) ~ bzd + sex, data = dtSurv), 
  xlab = "Days", 
  ylab = "Overall survival probability")


# Cox Model ----

coxfit <- survival::coxph(Surv(obsTime, status) ~ bzd + female, data = dtSurv)


broom::tidy(coxfit)
exp(0.40)
