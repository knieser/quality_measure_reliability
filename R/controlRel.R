#' Parameters for reliability calculations
#' @description
#' This function stores parameters for reliability calculations
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @export

controlRel <- function(n.resamples = 100, SSRmethod = 'permutation', fn = NA, MC.reps = 1000, d.steps = 10){

  if(!is.numeric(n.resamples) || n.resamples < 1 || n.resamples != floor(n.resamples)) stop('n.resamples must be a positive integer.')
  if(SSRmethod != 'permutation' && SSRmethod != 'bootstrap') stop('The only options for SSRmethod are permutation or bootstrap.')
  if(!is.numeric(d.steps) || d.steps < 1 || d.steps != floor(d.steps)) stop('d.steps must be a positive integer.')

  output = list()
  output$n.resamples <- n.resamples
  output$SSRmethod <- SSRmethod
  output$fn <- fn
  output$d.steps <- d.steps
  output$MC.reps <- MC.reps
  return(output)
}

