# Title: ATT using IPTW

# Description: Trying to better understand the formula for estimating ATT using IPTW. Goal is to compare 
#              formula PS/(1-PS) from
#              https://www.bmj.com/content/bmj/367/bmj.l5657.full.pdf

# Setup ----

#... Libraries ----

library(tidyverse)

#... Dependencies ----

# Data ----

.Random.seed

set.seed(20221017)

n.obs = 234

df = data.frame(
  age = runif(n = n.obs, min = 18, max = 80), 
  sex = rbinom(n = n.obs, size = 1, prob = 0.694),
  bin = rbinom(n = n.obs, size = 1, prob = 0.3)
) %>% 
  dplyr::mutate(
    # prob_trt = 0.008*age + 0.3*sex,
    # trt = rbinom(n = n.obs, size = 1, prob = prob_trt),
    trt = rbinom(n = n.obs, size = 1, prob = 0.576),
    prob_outcome = 0.004*age + 0.2*sex + 0.4*trt,
    outcome = rbinom(n = n.obs, size = 1, prob = prob_outcome)
  )

summary(df$prob_outcome)

# Simulating data with binary outcome variable 

# Logistic Regression for Baseline Check ----

log.mod <- glm(
  outcome ~ age + sex + trt, 
  family = binomial(link = "identity"), # reminder: have to use link = identity if you want to compare to coefficients for generating data
  data = df
)


summary(log.mod)

# Using logistic regression to make sure everything seems right 

# PS Estimation ----

# Assuming that the PS score is correctly specified 

ps.mod <- glm(
  trt ~ as.factor(sex) + bin, 
  family = binomial(link = "logit"), # reminder: have to use link = identity if you want to compare to coefficients for generating data
  data = df
)

df.control = df %>% dplyr::filter(trt == 0)

df.ps <- df.control %>% 
  dplyr::mutate(
    ps = predict(ps.mod)
  )

df.ps <- predict(ps.mod, df) %>% as.data.frame() %>% rename(ps = ".")

summary(df.ps$ps)

dat = cbind(df, df.ps) %>% 
  dplyr::mutate(
    att_wt = ps / (1-ps)
  )

log.mod <- glm(
  outcome ~ age + sex + trt + bin, 
  family = quasibinomial(link = "logit"), # reminder: have to use link = identity if you want to compare to coefficients for generating data
  data = dat,
  weights = att_wt
)

summary(log.mod)

summary(dat$att)

library(WeightIt)

?WeightIt::weightit()

att_wt_package = weightit(
  trt ~ as.factor(sex) + bin,
  data = dat,
  method = "ps",
  estimand = "ATT",
  focal = 
)

mean()
att_wt_package$ps

?weightit()

?quantile()

quantile(dat$att_wt, 0.975)

summary(att_wt_package)
summary(dat$att_wt)

att_wt_package$covs

# Lalonde Dataset ----

# Using the lalonde dataset since that's what is used in the 
# WeightIt vignette

data("lalonde", package = "cobalt")

#... IPTW Using WeightIt Package ----

W.out <- weightit(treat ~ age + educ + race + married + nodegree + re74 + re75,
                  data = lalonde, 
                  estimand = "ATT", 
                  method = "ps",
                  stabilize = TRUE)
view(W.out$ps)
view(W.out$weights)

#... IPTW using BMJ Article ----

ps.mod <- glm(
  treat ~ age + educ + race + married + nodegree + re74 + re75,
  family = binomial(link = "logit"),
  data = lalonde
)


df.new = lalonde %>% 
  dplyr::select(
    age, educ, race, married, nodegree, re74, re75
  )

ps.uncon <- glm(
  treat ~ 1,
  family = binomial(link = "logit"),
  data = lalonde
)


weight.calc <- lalonde %>% 
  dplyr::mutate(
    ps = predict(ps.mod, type = "response"),
    ps.uncond = predict(ps.uncon, type = "response"),
    weight = dplyr::case_when(
      treat == 1 ~ 1*(185/(185+429)), 
      treat == 0 ~ (ps)/(1-ps)*(429/(185+429))
      # treat == 0 ~ ((1-ps.uncond)/(ps.uncond)) /((1-ps)/ps)
    ),
    weight_uncon = dplyr::case_when(
      treat == 1 ~ 1*ps.uncond, 
      treat == 0 ~ ((ps)/(1-ps))*ps.uncond
    )
  )

view(weight.calc$weight)
view(weight.calc$weight_uncon)

view(get_w_from_ps(W.out$ps,
              W.out$treat,
              estimand = "ATT"))

library(survey)

d.w <- svydesign(~1, weights = weight.calc$weight_uncon, data = lalonde)
fit <- svyglm(re78 ~ treat, design = d.w)

fit
summary(fit)
confint(fit)

test <- lalonde %>% 
  dplyr::mutate(
    ps = W.out$ps,
    weight = get_w_from_ps(W.out$ps, 
                           W.out$treat,
                           estimand = "ATT")
  )
glm.fit()
mean(W.out$ps)
mean(weight.calc$ps)

view(weight.calc$ps)

summary(weight.calc$ps)
summary(weight.calc$weight)

wt <- trim(weight.calc$ps, at = 0.75)
summary(wt)
?trim

predict(ps.fit)


summary(ll.control$weight)

weightit

process.ps

install.packages("misaem")
library(misaem)

ps.fit <- misaem::miss.glm(treat ~ age + educ + race + married + nodegree + re74 + re75, data = lalonde)
predict(ps.fit, newdata = lalonde)
summary(ps.fit)
summary(ps.mod)
logLik(ps.mod)
