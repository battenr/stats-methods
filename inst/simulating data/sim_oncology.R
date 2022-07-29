# Simulating Multiple Lines of Therapy

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

tdef <- simstudy::defData(varname = "age", dist = "normal", formula = 45, variance = 10)
tdef <- simstudy::defData(tdef, varname = "sex", dist = "binary", formula = 0.5)
tdef <- simstudy::defData(tdef, varname = "")


tdef <- simstudy::defData(tdef, varname = "bzd", dist = "binary", formula = 0.5)
tdef <- simstudy::defData(tdef, varname = "high.blood.pressure", dist = "binary", formula = 0.5)

# Variables needed for making 

tdef <- simstudy::defData(tdef, varname = "nCount", dist = "noZeroPoisson", formula = 6)
tdef <- simstudy::defData(tdef, varname = "mInterval", dist = "gamma", formula = 30, variance = 0.01)
tdef <- simstudy::defData(tdef, varname = "vInterval", dist = "nonrandom", formula = 0.07)

# Generating cross-sectional data

dtTrial <- simstudy::genData(500, tdef)
dtTrial

#... Add variable to existing data ----

#... Making Longitudinal with Varying Time Periods ----

dtTime <- simstudy::addPeriods(dtTrial)

dtTime

# Adding Observed Treatment ----

#... If Dependent on Covariates ----

formula1 <- c("-2 + 2*sex + .5*age", "-1 - 2*sex + .5*age")
dtExp <- simstudy::trtObserve(dtTime, formulas = formula1, logit.link = TRUE, grpName = "exposure")

#... Not Dependent on any Covariates 

formula2 <- c(0.35, 0.45)

dtExp2 <- simstudy::trtObserve(dtstudy, formulas = formula2, logit.link = FALSE, grpName = "exposure")

#... Adding dosage ----

def2 <- defDataAdd(varname = "dosage", dist = "normal", formula = "15 + .1 * time + exposure", variance = 5)
dtPeriod <- addColumns(def2, dtExp)
