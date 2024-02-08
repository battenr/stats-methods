#' Get size of sample
#'
#' @param n n of 1, the number of values needed (we only need one number)
#' @param min minimum value. set to 100 by default
#' @param max maximum value. set to 1000 by default
#'
#' @return a number between min and max
#' @export n_sample
#'
#' @examples 
n_sample <- function(n = 1, 
                  min = 100, 
                  max = 1000){
  runif(n = n, min = min, max = max)
}