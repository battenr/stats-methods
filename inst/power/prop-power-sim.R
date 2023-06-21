#' Power Calculation for Proportions
#'
#' @param n.arm # number of treatment arms. default is 2 
#' @param n.per.arm # number of participants per arm
#' @param p.trt # "true" proportion in the treated
#' @param p.control # "true" proportion in the controls
#'
#' @return # reurns 
#' @export
#'
#' @examples
power_sim <- function(n.arm = 2, n.per.arm, p.trt, p.control){
  n.arm = n.arm # number of arms
  n.per.arm = n.per.arm # patients per arm
  p.trt = p.trt
  p.control = p.control
  
  df <- data.frame(
    trt = rep(c(1,0), n.arm*n.per.arm),
    outcome = rbinom(n = n.arm*n.per.arm, size = 1, prob = c(p.trt, p.control))
  ) 
  
  binary.fit <- stats::glm(
    formula = trt ~ outcome, 
    family = stats::binomial(link = "logit"),
    data = df
  )
  
  fit.sum <- summary(binary.fit)
  
  test.stat = fit.sum$coefficients[2,1] / fit.sum$coefficients[2,2]
  
  result = 2*pnorm(abs(test.stat), lower.tail = FALSE) < 0.05
  
  return(result)
}