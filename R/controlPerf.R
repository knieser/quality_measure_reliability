#' Parameters for performance calculations
#' @description
#' This function stores parameters for performance calculations
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @references None
#' @examples
#' # TBD
#' @export

controlPerf <- function(min.n = 2, alpha = 0.05, n.boots = 200, n.cores = 4){
  output = list()
  output$min.n <- min.n
  output$alpha <- alpha
  output$n.boots <- n.boots
  output$n.cores <- n.cores
  return(output)
}
