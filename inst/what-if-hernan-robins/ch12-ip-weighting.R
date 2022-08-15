# Title: Chapter 12 of What If by Hernan and Robins

# working through the R code here:

# https://remlapmot.github.io/cibookex-r/ip-weighting-and-marginal-structural-models.html

# Setup ----

#... Libraries ----

library(tidyverse)
library(readr)

#... Dependencies ----

#... Pins ----

# Data ----

nhefs <- readr::read_csv("data/nhefs.csv")

# Program 12.2: Reweighting ----

# First, fit a logisitc regression for the outcome variable. here that is smoking

fit <- glm(
  qsmk ~ sex + race + age + I(age ^ 2) +
    as.factor(education) + smokeintensity +
    I(smokeintensity ^ 2) + smokeyrs + I(smokeyrs ^ 2) +
    as.factor(exercise) + as.factor(active) + wt71 + I(wt71 ^ 2),
  family = binomial(),
  data = nhefs
)

# We want to predict the propensity score for each person. for those that are treated, 
# the probability of receiving treatment. For those that are not treated, the probability
# of not receiving treatment

prob.qsmk <- nhefs %>% 
  dplyr::mutate(
    predict.value = predict(fit, type = "response"),
    ps = dplyr::case_when(
      qsmk == 0 ~ 1-predict.value, # 1 - ps = probability of not receiving trt
      qsmk == 1 ~ predict.value
    ),
    weight = 1/ps, # unstabilized weight,
    half.weight = 0.5/ps
  ) 

prob.qsmk %>% count()

# Checking the weights:

mean(prob.qsmk$weight) # we expect it to be 2 because each person
# gets 2 copies of themselves. 
# If we didn't want to do this, we could use 0.5/ps

mean(prob.qsmk$half.weight) # if we use 0.5 then the mean is 1

# We now fit a GEE model 

library(geepack)

# Marginal Structural Model 

msm.w <- geeglm(
  wt82_71 ~ qsmk,
  data = prob.qsmk,
  weights = weight,
  id = seqn,
  corstr = "independence"
)

summary(msm.w)

3.524+1.96*0.52 # using standard error from the vcov of gee model
3.524-1.96*0.52

# Program 12.3: Stabilized Weights ----

# To create stabilized weights, we need to calculate two parts
# of the formula: f(A) & f(A|L)

# SW = f(A) / f(A|L)

denom.fit <-
  glm(
    qsmk ~ as.factor(sex) + as.factor(race) + age + I(age ^ 2) +
      as.factor(education) + smokeintensity +
      I(smokeintensity ^ 2) + smokeyrs + I(smokeyrs ^ 2) +
      as.factor(exercise) + as.factor(active) + wt71 + I(wt71 ^ 2),
    family = binomial(),
    data = nhefs
  )

summary(denom.fit)

pd.qsmk <- predict(denom.fit, type = "response")

# estimation of numerator of ip weights
numer.fit <- glm(qsmk ~ 1, family = binomial(), data = nhefs.nmv)
summary(numer.fit)

pn.qsmk <- predict(numer.fit, type = "response")

nhefs.nmv$sw <-
  ifelse(nhefs.nmv$qsmk == 0, ((1 - pn.qsmk) / (1 - pd.qsmk)),
         (pn.qsmk / pd.qsmk))

summary(nhefs.nmv$sw)

msm.sw <- geeglm(
  wt82_71 ~ qsmk,
  data = nhefs.nmv,
  weights = sw,
  id = seqn,
  corstr = "independence"
)

summary(msm.sw)

