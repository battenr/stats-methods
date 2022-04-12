# Simulating Longitudinal Data 

# Setup ----

#... Libraries ----

library(tidyverse)
library(simstudy)

#... Dependencies ----

# No dependencies

# Simulating Data ----

#... Cross-Section Data ----

# First need to make cross-sectional data 

tdef <- defData(varname = "sex", dist = "binary", formula = 0.5)
tdef <- defData(tdef, varname = "age", dist = "normal", formula = , variance = 1)
tdef <- defData(tdef, varname = "", dist = "normal", formula = "Y0 + 5 + 5 * T",
                variance = 1)
tdef <- defData(tdef, varname = "Y2", dist = "normal", formula = "Y0 + 10 + 5 * T",
                variance = 1)

?simstudy::addColumns()
?defDataAdd()

set.seed(483726)

dtTrial <- genData(500, tdef)
dtTrial

#... Add variable to existing data ----



#... Making Longitudinal ----

dtTime <- addPeriods(dtTrial, nPeriods = 3, idvars = "id", timevars = c("Y0", "Y1",
                                                                        "Y2"), timevarName = "Y")
dtTime

#... Varying Time Periods ----

def <- defData(varname = "xbase", dist = "normal", formula = 20, variance = 3)
def <- defData(def, varname = "nCount", dist = "noZeroPoisson", formula = 6)
def <- defData(def, varname = "mInterval", dist = "gamma", formula = 30, variance = 0.01)
def <- defData(def, varname = "vInterval", dist = "nonrandom", formula = 0.07)

dt <- genData(200, def)
dt[id %in% c(8, 121)]  # View individuals 8 and 121

dtPeriod <- addPeriods(dt)
dtPeriod[id %in% c(8, 121)]  # View individuals 8 and 121 only

# Adding Observed Treatment ----

#... If Dependent on Covariates ----

formula1 <- c("-2 + 2*male - .5*over65", "-1 + 2*male + .5*over65")
dtExp <- trtObserve(dtstudy, formulas = formula1, logit.link = TRUE, grpName = "exposure")

#... Not Dependent on any Covariates 

formula2 <- c(0.35, 0.45)

dtExp2 <- trtObserve(dtstudy, formulas = formula2, logit.link = FALSE, grpName = "exposure")

