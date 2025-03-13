#' Parameters for reliability calculations
#' @description
#' This function stores parameters for reliability calculations
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @references None
#' @examples
#' # TBD
#' @export

controlRel <- function(n.resamples = 10, SSRmethod = 'permutation', fn = NA, d.steps = 10){
  output = list()
  output$n.resamples <- n.resamples
  output$SSRmethod <- SSRmethod
  output$fn <- fn
  output$d.steps <- d.steps
  return(output)
}

