#' Calculate stabilized inverse probability weights for a given dataset
#'
#' This function uses a generalized linear model to estimate the propensity score, then it calculates the
#' inverse probability weights based on the propensity score. 
#' Marginal probabilites are used for the numerator while conditional probabilites are used 
#' for the denominator
#' The weights are used to adjust for confounding in observational studies.
#'
#' @param df A data frame containing the variables of interest. 
#' @param treat A character string specifying the treatment variable in the dataset.
#' @param confounder A character string specifying the confounder variable in the dataset.
#' 
#' @return A data frame that includes the original data and additional columns for the propensity score (ps.conditional, ps.marginal) 
#' and the calculated inverse probability weights.
#'
#' @importFrom dplyr mutate case_when
#' @importFrom stats glm
#' @importFrom tidyverse %>%
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(treat = c(0, 1, 1, 0, 1),
#'                    confounder = c(1, 2, 1, 2, 3))
#' ipw_stab(data, "treat", "confounder")
#' }
ipw_stab <- function(df, treat, confounder){
  
  library(tidyverse)
  
  mod.conditional <- glm(as.formula(treat~confounder), 
                         family = binomial(link = "logit"),
                         data = df)
  
  mod.marginal <- glm(as.formula(treat~1), 
                      family = binomial(link = "logit"),
                      data = df)
  df = df |> 
    dplyr::mutate(
      ps.conditional = predict(mod.conditional, type = "response"),
      ps.marginal = predict(mod.marginal, type = "response"),
      weights = dplyr::case_when(
        x == 1 ~ ps.marginal/ps.conditional, 
        x == 0 ~ (1-ps.marginal)/(1-ps.conditional)
      )
    )
  
  return(df)
  
}