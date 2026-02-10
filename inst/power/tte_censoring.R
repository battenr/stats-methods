# TTE sample size calc 

# Assuming: 
# - no loss ot followup

# Note: Created by Google Gemini. Basically, it first determines the 
# probability of survival 


# No 



# Function for One-Arm Survival Sample Size with Recruitment
# s0: Historical survival probability
# s1: Expected survival probability 
# t_point: Time point for the probabilities (e.g., 2 years)
# R: Recruitment (accrual) period duration
# F: Follow-up period (time from end of accrual to analysis)
# alpha: Significance level (one-sided)
# power: Statistical power

ss_survival_accrual <- function(s0, s1, t_point, R, F, alpha = 0.05, power = 0.80) {
  
  # 1. Hazards based on exponential distribution
  lambda0 <- -log(s0) / t_point
  lambda1 <- -log(s1) / t_point
  
  # 2. Z-scores
  z_alpha <- qnorm(1 - alpha)
  z_beta  <- qnorm(power)
  
  # 3. Required Events (d)
  # Based on the log-hazard ratio
  log_HR <- log(lambda1 / lambda0)
  d <- ((z_alpha + z_beta) / log_HR)^2
  
  # 4. Probability of observing an event (P_e)
  # This accounts for the fact that recruitment is spread over R
  # and followed for additional time F.
  # Formula for P_e under exponential distribution:
  Pe <- 1 - (exp(-lambda1 * F) * (1 - exp(-lambda1 * R))) / (lambda1 * R)
  
  # 5. Total Sample Size
  n_total <- d / Pe
  
  return(data.frame(
    Events_Needed = ceiling(d),
    Total_N = ceiling(n_total),
    Prob_of_Event = round(Pe, 4),
    Accrual_Period = R,
    Follow_Up_Period = F
  ))
}

# Example: 2-year survival 40% (Null) vs 60% (Alt).
# Recruiting for 1.5 years, then 1 year of additional follow-up.
ss_survival_accrual(s0=0.922, s1=0.96, t_point=24, R=24, F=24, alpha=0.05, power=0.8)


# Accounting for Censoring ----

# TTE sample size calc 

# Assuming: 
# - no loss ot followup

# Note: Created by Google Gemini. Basically, it first determines the 
# probability of survival 

# Function for One-Arm Survival Sample Size with Recruitment and Dropouts
ss_survival_accrual <- function(s0, s1, t_point, R, F, alpha = 0.05, power = 0.80, censoring_rate = 0.10) {
  
  # 1. Hazards based on exponential distribution
  lambda0 <- -log(s0) / t_point
  lambda1 <- -log(s1) / t_point
  
  # 2. Z-scores
  z_alpha <- qnorm(1 - alpha)
  z_beta  <- qnorm(power)
  
  # 3. Required Events (d)
  log_HR <- log(lambda1 / lambda0)
  d <- ((z_alpha + z_beta) / log_HR)^2
  
  # 4. Probability of observing an event (Pe)
  # This is the administrative event probability
  Pe <- 1 - (exp(-lambda1 * F) * (1 - exp(-lambda1 * R))) / (lambda1 * R)
  
  # 5. Total Sample Size (N)
  # Initial N based on events and recruitment timing
  n_initial <- d / Pe
  
  # 6. Adjust for Random Censoring (Loss to Follow-up)
  # Formula: N_final = N_initial / (1 - loss_rate)
  n_total <- n_initial / (1 - censoring_rate)
  
  return(data.frame(
    Events_Needed = ceiling(d),
    Total_N_Adjusted = ceiling(n_total),
    Initial_N = ceiling(n_initial),
    Censoring_Buffer = ceiling(n_total) - ceiling(n_initial),
    Prob_of_Event = round(Pe, 4)
  ))
}

# Example: 10% censoring rate included
ss_survival_accrual(s0=0.922, s1=0.96, t_point=24, R=24, F=24, alpha=0.05, power=0.8, censoring_rate=0.10)







# Function for One-Arm Survival Sample Size (Scientific Censoring Adjustment)
# loss_rate: Proportion of patients expected to be lost by the end of the study (R+F)
ss_survival_scientific <- function(s0, s1, t_point, R, F, alpha = 0.05, power = 0.80, loss_rate = 0.10) {
  
  # 1. Hazards based on exponential distribution
  lambda0 <- -log(s0) / t_point
  lambda1 <- -log(s1) / t_point
  
  # 2. Random Censoring Hazard (lambda_c)
  # Derived from: 1 - exp(-lambda_c * Total_Time) = loss_rate
  total_time <- R + F
  lambda_c <- -log(1 - loss_rate) / total_time
  
  # 3. Z-scores
  z_alpha <- qnorm(1 - alpha)
  z_beta  <- qnorm(power)
  
  # 4. Required Events (d)
  # This stays the same because it's driven by the effect size (HR)
  log_HR <- log(lambda1 / lambda0)
  d <- ((z_alpha + z_beta) / log_HR)^2
  
  # 5. Probability of observing an event (Pe) 
  # This formula accounts for: 
  # - Exponential event time
  # - Exponential random censoring (loss to follow up)
  # - Uniform recruitment over period R
  # - Fixed follow-up F
  k <- lambda1 + lambda_c
  term <- (exp(-k * F) - exp(-k * (R + F))) / (R * k)
  pe <- (lambda1 / k) * (1 - term)
  
  # 6. Total Sample Size
  n_total <- d / pe
  
  return(data.frame(
    Events_Needed = ceiling(d),
    Total_N = ceiling(n_total),
    Prob_of_Event = round(pe, 4),
    Loss_Rate_Applied = paste0(loss_rate * 100, "%"),
    Hazard_Event = round(lambda1, 6),
    Hazard_Censoring = round(lambda_c, 6)
  ))
}

# Run the calculation
ss_survival_scientific(
  s0 = 0.922, 
  s1 = 0.96, 
  t_point = 24, 
  R = 24, 
  F = 24, 
  alpha = 0.05, 
  power = 0.8, 
  loss_rate = 0.10
)
