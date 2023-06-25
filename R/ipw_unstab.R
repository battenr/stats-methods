#' Calculate the unstabilized inverse probability weights for a given dataset
#'
#' This function uses a generalized linear model to estimate the propensity score, then it calculates the
#' unstabilized inverse probability weights based on the propensity score. 
#' The weights are used to adjust for confounding in observational studies.
#'
#' @param df A data frame containing the variables of interest. 
#' @param treat A character string specifying the treatment variable in the dataset.
#' @param confounder A character string specifying the confounder variable in the dataset.
#' 
#' @return A data frame that includes the original data and additional columns for the propensity score (ps) 
#' and the calculated unstabilized inverse probability weights.
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
#' ipw_unstab(data, "treat", "confounder")
#' }
ipw_unstab <- function(df, treat, confounder){
  
  mod <- glm(as.formula(treat~confounder), 
             family = binomial(link = "logit"),
             data = df)
  
  df = df |> 
    dplyr::mutate(
      ps = predict(mod, type = "response"),
      weights = dplyr::case_when(
        x == 1 ~ 1/(ps),
        x == 0 ~ 1/(1-ps)
      )
    )
  
  return(df)
  
}
