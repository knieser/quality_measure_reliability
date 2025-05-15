#' Parameters for performance calculations
#' @description
#' This function stores parameters for performance calculations
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @references None
#' @examples
#' # TBD
#' @export

controlPerf <- function(min.n = 2, alpha = 0.05, n.boots = 200, n.cores = 4){

  total.cores = detectCores()

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
