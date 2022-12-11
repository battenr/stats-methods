library(powerSurvEpi)
library(tidyverse)
library(survival)

?powerSurvEpi::powerCT.default()



hr = 0.7
alpha = 0.05
n0 = 200
n1 = 200
dropr = 0.05
meddrop = 6 
etime = 12
atime = 24
medsurv0 = 15
prop0 = 0.489
prop1 = 0.3707

  n = n0 + n1
  
  # calculate rate for the control and treatment from median survival time
  lambda0 = log(2)/medsurv0
  medsurv1 = medsurv0/hr
  lambda1 = log(2)/medsurv1
  
  t0 = runif(n)*etime
  t1 = c(rexp(n1, lambda1), rexp(n0, lambda0))
  td = if(dropr==0){         # simulate dropout time
    rep(Inf, n)
  } else {
    rexp(n, -log(1-dropr)/meddrop)
  }
  
  arm = c(rep('trt', n1), rep('ctr', n0))
  
  # time = as.numeric(pmin(atime-t0, pmin(t1, td)))         # obtain survival time
  
  time = as.numeric(pmin(atime-t0, t1))

surv_calc <- function(n1 = 200, n0 = 200, prop1 = 0.85, prop0 = 0.489){
  status0 = rbinom(n = n0, size = 1, prob = prop0) %>% as.data.frame()
  status1 = rbinom(n = n1, size = 1, prob = prop1) %>% as.data.frame()
  status = rbind(status1, status0) %>% as.data.frame()
  
  
  dat <- bind_cols(t0=t0, td=td, t1=t1, arm=arm, status = status$.)

  fit = survival::coxph(Surv(time, status) ~ arm, data = dat) %>% summary()

  beta = fit$coefficients[1]
  se = fit$coefficients[3]
  test_stat = beta/se

  crit = abs(qnorm(alpha/2))

  stat_sig = abs(test_stat)>crit
  
  # summary = survdiff(Surv(time, status) ~ arm, dat)
  # test_stat = summary$chisq
  # df = 1
  # 
  # pvalue = pchisq(test_stat, df = 1, lower.tail = FALSE)
  # 
  # stat_sig = pvalue < alpha
  
  return(data.frame(stat_sig))
  
}



surv_calc()

iter = 10000

surv_power <- do.call(rbind.data.frame, lapply(1:iter, function(x){
  surv_calc()
}))

mean(surv_power$stat_sig)

powerCT.default(nE = 200, 
                nC = 200, 
                pE = 0.3707, 
                pC = 0.4890, 
                RR = 0.7, 
                alpha = 0.05)

powerCT.default(nE = 300, 
                nC = 200, 
                pE = 0.15, 
                pC = 0.4890, 
                RR = 0.7, 
                alpha = 0.05)


