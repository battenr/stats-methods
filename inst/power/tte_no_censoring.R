# TTE sample size calc 

# Assuming: 
# - no loss ot followup

# Note: Created by Google Gemini. Basically, it first determines the 
# probability of survival 






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

