library(tidyverse)
library(survival)
library(powerSurvEpi)

hr = 0.7
alpha = 0.05
n0 = 300
n1 = 104
n = n0+n1
dropr = 0.05

prop0 = 0.48
prop1 = 0.37
medsurv0 = 12
max_time = 24



surv_calc <- function(hr = 0.7, 
                      medsurv0 = 12, 
                      n0 = 330, 
                      n1 = 104,
                      prop0 = 0.48, 
                      prop1 = 0.37,
                      alpha = 0.05
                      ){
 lambda0 = 0.35
 lambda1 = 0.35/hr
 n = n0 + n1
 
 # medsurv1 = medsurv0/hr
 
 time_sim0 = rexp(n0, rate = lambda0) %>% as.data.frame()
 time_sim1 = rexp(n1, rate = lambda1) %>% as.data.frame()
 time_sim = rbind(time_sim1, time_sim0)
 t0 = runif(n)*12
 
 max_time = runif(n = n, min = 1, max = 24)
 min_time = 0
 
 time = as.numeric(abs(pmin(time_sim$., max_time-t0)))
 
 numdeaths1 = round(prop1*n1, 0)
 numdeaths0 = round(prop0*n0,0)
 
 numalive1 = n1 - numdeaths1
 numalive0 = n0 - numdeaths0
 
 status = c(rep(1, numdeaths1), rep(0, numalive1), rep(1, numdeaths0), rep(0, numalive0))
 
 arm = c(rep('trt', n1), rep('ctr', n0))
 
 dat <- bind_cols(time = time, arm = arm, status = status)
 
 fit = survival::coxph(Surv(time, status) ~ arm, data = dat) %>% summary()
 
 beta = fit$coefficients[1]
 se = fit$coefficients[3]
 test_stat = beta/se
 
 crit = abs(qnorm(alpha/2))
 
 stat_sig = abs(test_stat)>crit
 
 return(data.frame(stat_sig))
}


iter = 10

surv_power <- do.call(rbind.data.frame, lapply(1:iter, function(x){
  surv_calc()
}))

mean(surv_power$stat_sig)


powerSurvEpi::powerCT.default(
  200, 
  200,
  0.37,
  0.48,
  0.7
)
