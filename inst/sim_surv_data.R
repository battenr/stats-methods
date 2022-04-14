# Simulating Longitudinal Data 

# Setup ----

#... Libraries ----

library(tidyverse)
library(simstudy)

#... Dependencies ----

# No dependencies

# Simulating Data ----

# Setting seed for reproducibility

set.seed(2022)

# Baseline data definitions

def <- defData(varname = "x1", formula = 0.5, dist = "binary")
def <- defData(def, varname = "x2", formula = 0.5, dist = "binary")
def <- defData(def, varname = "grp", formula = 0.5, dist = "binary")

# Survival data definitions

sdef <- defSurv(varname = "survTime", formula = "1.5*x1", scale = "grp*50 + (1-grp)*25",
                shape = "grp*1 + (1-grp)*1.5")
sdef <- defSurv(sdef, varname = "censorTime", scale = 80, shape = 1)

sdef

# Baseline data definitions

dtSurv <- genData(300, def)
dtSurv <- genSurv(dtSurv, sdef)

head(dtSurv)

# A comparison of survival by group and x1

dtSurv[, round(mean(survTime), 1), keyby = .(grp, x1)]

cdef <- defDataAdd(varname = "obsTime", formula = "pmin(survTime, censorTime)", dist = "nonrandom")
cdef <- defDataAdd(cdef, varname = "status", formula = "I(survTime <= censorTime)",
                   dist = "nonrandom")

dtSurv <- addColumns(cdef, dtSurv)

head(dtSurv)

# estimate proportion of censoring by x1 and group

dtSurv[, round(1 - mean(status), 2), keyby = .(grp, x1)]

# Baseline data definitions

def <- defData(varname = "x1", formula = 0.5, dist = "binary")
def <- defData(def, varname = "x2", formula = 0.5, dist = "binary")

# Survival data definitions

sdef <- defSurv(varname = "survTime", formula = "1.5*x1 - .8*x2", scale = 50, shape = 1/2)
sdef <- defSurv(sdef, varname = "censorTime", scale = 80, shape = 1)

dtSurv <- genData(300, def)
dtSurv <- genSurv(dtSurv, sdef)

cdef <- defDataAdd(varname = "obsTime", formula = "pmin(survTime, censorTime)", dist = "nonrandom")
cdef <- defDataAdd(cdef, varname = "status", formula = "I(survTime <= censorTime)",
                   dist = "nonrandom")

dtSurv <- addColumns(cdef, dtSurv)
coxfit <- survival::coxph(Surv(obsTime, status) ~ x1 + x2, data = dtSurv)