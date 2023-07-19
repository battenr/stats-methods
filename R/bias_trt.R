#' Estimate the bias in treatment effect
#'
#' This function fits a generalized linear model to estimate the treatment effect, then calculates the bias
#' by comparing the estimated treatment effect with the true treatment effect.
#'
#' @param df A data frame containing the variables of interest. Default is `df`.
#' @param treat A character string specifying the treatment variable in the dataset.
#' @param confounder A character string specifying the confounder variable in the dataset.
#' @param outcome A character string specifying the outcome variable in the dataset.
#' @param beta The true treatment effect.
#'
#' @return The estimated bias in treatment effect.
#'
#' @importFrom stats glm
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(treat = c(0, 1, 1, 0, 1),
#'                    confounder = c(1, 2, 1, 2, 3),
#'                    outcome = c(2.5, 3.0, 2.7, 3.2, 2.8))
#' bias_trt(data, "treat", "confounder", "outcome", 1.5)
#' }
bias_trt <- function(df = df, 
                     treat, 
                     confounder, 
                     outcome, 
                     weights,
                     beta){
  
  mod <- glm(as.formula(outcome ~ treat + confounder), 
             family = gaussian(link = "identity"), 
             data = df, 
             weights = weights)
  
  result = mod$coefficients[2] - beta
  
  return(result)
}
