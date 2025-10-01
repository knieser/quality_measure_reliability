#' Parameters for performance calculations
#' @description
#' This function stores parameters for performance calculations
#' @param min.n minimum number of observations per entity
#' @param alpha statistical significance level to use for confidence intervals
#' @param n.boots number of bootstraps to use for confidence interval estimation for P/E ratios
#' @param n.cores number of cores to use for parallel processing
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @export

controlPerf <- function(min.n = 2, alpha = 0.05, n.boots = 1000, n.cores = 2){

  total.cores = parallel::detectCores()

  if (!is.numeric(min.n) || min.n < 2 || min.n != floor(min.n)) stop('min.n must be an integer greater than or equal to 2.')
  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1) stop('alpha must be a number between 0 and 1.')
  if (!is.numeric(n.boots) || n.boots != floor(n.boots) || n.boots < 1) stop('n.boots must be a positive integer.')
  if (!is.numeric(n.cores) || n.cores > total.cores || n.cores < 1) stop('n.cores must be a postive integer less than or equal to the total number of available cores.')

  output = list()
  output$min.n <- min.n
  output$alpha <- alpha
  output$n.boots <- n.boots
  output$n.cores <- n.cores
  return(output)
}
