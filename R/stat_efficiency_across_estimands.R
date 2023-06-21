#' Compare ATT from IPTW, PSM, and entropy balancing
#'
#' This function estimates the Average Treatment Effects on the Treated (ATT) 
#' and their variances using Inverse Probability of Treatment Weighting (IPTW),
#' Propensity Score Matching (PSM), and entropy balancing. It also checks the 
#' covariate balance after applying each method and plots the results.
#'
#' @param data A data frame containing the variables for the analysis.
#' @param treat_var A character string indicating the name of the treatment variable. 
#'                  This should be a binary variable where 1 indicates treated 
#'                  and 0 indicates control.
#' @param outcome_var A character string indicating the name of the outcome variable.
#' @param covariates A character vector of the names of the covariate(s).
#'
#' @return It prints the ATT and their variances for each method and generates love 
#'         plots for visual examination of covariate balance. It does not explicitly return a value.
#'
#' @examples
#' \dontrun{
#' compare_methods(data, "treat", "outcome", c("covariate1", "covariate2"))
#' }
#' @export
#' @importFrom twang iptw get.weights
#' @importFrom MatchIt matchit match.data
#' @importFrom ebal ebalance
#' @importFrom cobalt love.plot
compare_methods <- function(data, treat_var, outcome_var, covariates) {
  # Load the required libraries
  library(twang)
  library(MatchIt)
  library(ebal)
  library(cobalt)
  
  # IPTW
  iptw_fit <- iptw(as.formula(paste(treat_var, "~", paste(covariates, collapse = "+"))), 
                   data = data, stop.method = "es.mean")
  data$iptw_weights <- get.weights(iptw_fit)
  ate_iptw <- with(data, weighted.mean(data[[outcome_var]][data[[treat_var]] == 1], iptw_weights[data[[treat_var]] == 1]) - 
                     weighted.mean(data[[outcome_var]][data[[treat_var]] == 0], iptw_weights[data[[treat_var]] == 0]))
  var_iptw <- with(data, var(data[[outcome_var]][data[[treat_var]] == 1] * iptw_weights[data[[treat_var]] == 1]) +
                     var(data[[outcome_var]][data[[treat_var]] == 0] * iptw_weights[data[[treat_var]] == 0]))
  
  # PSM
  psm_fit <- matchit(as.formula(paste(treat_var, "~", paste(covariates, collapse = "+"))), 
                     data = data, method = "nearest")
  matched_data <- match.data(psm_fit)
  ate_psm <- with(matched_data, mean(data[[outcome_var]][data[[treat_var]] == 1]) - 
                    mean(data[[outcome_var]][data[[treat_var]] == 0]))
  var_psm <- with(matched_data, var(data[[outcome_var]][data[[treat_var]] == 1]) + 
                    var(data[[outcome_var]][data[[treat_var]] == 0]))
  
  # Entropy Balancing
  eb_fit <- ebalance(as.formula(paste(treat_var, "~", paste(covariates, collapse = "+"))), 
                     data = data)
  data$eb_weights <- eb_fit$weights
  ate_eb <- with(data, weighted.mean(data[[outcome_var]][data[[treat_var]] == 1], eb_weights[data[[treat_var]] == 1]) - 
                   weighted.mean(data[[outcome_var]][data[[treat_var]] == 0], eb_weights[data[[treat_var]] == 0]))
  var_eb <- with(data, var(data[[outcome_var]][data[[treat_var]] == 1] * eb_weights[data[[treat_var]] == 1]) +
                   var(data[[outcome_var]][data[[treat_var]] == 0] * eb_weights[data[[treat_var]] == 0]))
  
  # Print results
  print(paste0("IPTW ATE: ", ate_iptw, " Variance: ", var_iptw))
  print(paste0("PSM ATE: ", ate_psm, " Variance: ", var_psm))
  print(paste0("EB ATE: ", ate_eb, " Variance: ", var_eb))
  
  # Check balance with cobalt package
  love.plot(iptw_fit, weights = "iptw", var.order = "unadjusted")
  love.plot(psm_fit, weights = "psm", var.order = "unadjusted")
  love.plot(eb_fit, weights = "eb", var.order = "unadjusted")
}
