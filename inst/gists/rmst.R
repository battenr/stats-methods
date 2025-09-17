# Title: Restricted Mean Survival Time 

# Description: When using time-to-event outcomes, a common effect measure is 
# the hazard ratio. There are several problems with this measure including selection bias, 
# and a strange interpretability. 

# Luckily there are alternatives! 

# One such alternative is the restricted mean survival time. This code demonstrates 
# the RMST. 

# Setup ----

#... Libraries ----

library(tidyverse) # ol' faithful
library

#... Functions ----

source("R/custom_theme.R")
source("R/sim_data.R")

# Code ----






# Description: 

library(tidyverse)
library(simsurv)
library(simtrial)

# Simulating Data ----

# Set seed for reproducibility
set.seed(42)
n = 250

covars <- data.frame(
  id = 1:n,
  z1 = rnorm(n = n, mean = 5, sd = 2),
  z2 = rbinom(n = n, size = 1, prob = 0.5),
  trt = rbinom(n = n, size = 1, prob = 0.6)
)


beta <- 0.5  # Effect of covariate (e.g., treatment effect)
lambda <- 0.05  # Baseline hazard rate (for exponential distribution)

# Generate data with a covariate and censoring
sim_data <- simsurv(
  n = n,
  dist = "weibull",   # Weibull distribution for survival times
  lambdas = 0.05,        # Shape parameter for Weibull distribution
  gammas = 1.5,         # Scale parameter for Weibull distribution
  x = covars,  # Include covariate (age)
  beta = c(trt = -0.5, z1 = -0.2, z2 = 0.3),   # Covariate effect
  censor = 0.2 ,       # Censoring rate (20% censored)
  maxt = 20
) 



df <- sim_data %>% 
  full_join(covars)


fit0 <- surv_fit(Surv(eventtime, status) ~ 1, 
                data = df %>% filter(trt == 0))

surv_plot_trt0 <- survminer::ggsurvplot(
  fit0,
  data = df %>% filter(trt == 0),
  risk.table = TRUE, 
  palette = "Dark2",
  conf.int = FALSE
)

fit1 <- surv_fit(Surv(eventtime, status) ~ 1, 
                 data = df %>% filter(trt == 1))

surv_plot_trt1 <- survminer::ggsurvplot(
  fit1,
  data = df %>% filter(trt == 1),
  risk.table = TRUE, 
  palette = "Dark2",
  conf.int = FALSE
)

library(patchwork)


surv_plot_trt1$plot + surv_plot_trt0$plot



surv_plot$plot + geom_ribbon(data = data.frame(time = surv_plot$plot$data$time,
                                surv = surv_plot$plot$data$surv,
                                trt = rep(0, nrow(surv_plot$plot$data))),
              aes(x = time, 
                  ymin = 0, 
                  ymax = surv,
                  color = as.factor(trt), 
                  fill = as.factor(trt)
                  ), 
                  alpha = 0.3) 
   theme_minimal() +
   labs(title = "Survival Curves with Area Under the Curve (RMST)",
        subtitle = paste("RMST for Treatment 0: ", round(rmst_trt0$RMST, 2),
                         " | RMST for Treatment 1: ", round(rmst_trt1$RMST, 2)))
   
simtrial::rmst(data = df, 
               tau = 15, 
               formula = Surv(eventtime, status) ~ trt, 
               reference = "0")




