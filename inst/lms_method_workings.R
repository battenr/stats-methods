library(gamlss)
library(dplyr)

# Example data
set.seed(123)
n <- 100
data <- data.frame(
  y = rnorm(n, mean = 50, sd = 10),
  x = runif(n, min = 0, max = 10),
  w = runif(n, min = 1, max = 5)  # sampling weights
)

test_df <- test %>% 
  dplyr::mutate(
    
  )
?gamlss::gamlssML

?gamlss

gamlss::lms(y, ht, 
            data = test,
            #weights = wghta,
            mu.start = -1.11, 
            nu.start = 5.3, 
            sigma.start = -1.6,
            tau.start = 1.74
            
            )

gamlss::lms(y, ht, data = test, k = 4)

?svydesign

design <- svydesign(ids= ~ id, 
                    weights = ~wghta,
                    data = test
                    )

mod <- svyglm(y ~ ht, 
              design = design)

test_new <- test %>% 
  mutate(
    yi = predict(mod),
    ht_new = ht/100
  ) %>% 
  filter(wghta <= 7)

svyglm(family = BCT)

mod_lms <- gamlss::gamlss(y ~ ht,
                          data = test, 
                          family = BCT,
                          weights = wghta)



                       ht,
                       data = test)#,
                       #weights = wghta)

predictAll(mod_lms, newdata = new_df, output = "data.frame") %>% view()



mod_lms < gamlss::lms(yi, 
                      ht_new,
                      
                      data = test_new)


predictAll(mod_lms, new_df) %>% 
  cbind(new_df)

summary(mod)

gamlss::lms



fitted(gamlss, "nu")

library(survey)

svygamlss


# Fit LMS model without weights
initial_fit <- gamlss::gamlss(y ~ pb(ht), data = test, family = BCT)

# Extract initial parameter estimates
M_initial <- predict(initial_fit, what = "mu")
S_initial <- predict(initial_fit, what = "sigma")
L_initial <- predict(initial_fit, what = "nu")

# Define weighted log-likelihood function
weighted_log_likelihood <- function(params, y, x, w) {
  M <- params[1:length(M_initial)]
  S <- params[(length(M_initial) + 1):(2 * length(M_initial))]
  L <- params[(2 * length(M_initial) + 1):(3 * length(M_initial))]
  
  # Negative log-likelihood for BCT distribution with weights
  nll <- -sum(w * dBCT(y, mu = M, sigma = S, nu = L, log = TRUE))
  return(nll)
}

# Initial parameter vector
initial_params <- c(M_initial, S_initial, L_initial)

# Optimize parameters using weighted log-likelihood
opt_results <- optim(
  par = initial_params,
  fn = weighted_log_likelihood,
  y = test$y,
  x = test$ht,
  w = test$wghta,
  method = "BFGS",
  control = list(maxit = 1000)
)

# Extract optimized parameters
optimized_params <- opt_results$par
M_optimized <- optimized_params[1:length(M_initial)]
S_optimized <- optimized_params[(length(M_initial) + 1):(2 * length(M_initial))]
L_optimized <- optimized_params[(2 * length(M_initial) + 1):(3 * length(M_initial))]

# Results
M_optimized
S_optimized
L_optimized

?gamlss

gamlss::gamlss(y ~ pb(ht), 
               mu.start = 1.4, 
               sigma.start = 1.89,
               family = BCT, 
               data = test,
               weights = wghta,
               iterations = 100
               )
