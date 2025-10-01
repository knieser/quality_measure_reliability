#' Parameters for reliability calculations
#' @description
#' This function stores parameters for reliability calculations.
#' @param n.resamples number of resamples for split-sample reliability method
#' @param SSRmethod use either the `permutation` (default) or the `bootstrap` method for the split-sample reliability calculation
#' @param fn aggregation function for observations within entities, default is `NA` and will produce entity-level means
#' @param MC.reps number of Monte Carlo simulations to produce reliability estimates for data modeled with hierarchical logistic regression
#' @param d.steps number of percentiles removed to check for misclassification probabilities
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @export

controlRel <- function(n.resamples = 100, SSRmethod = 'permutation', fn = NA, MC.reps = 1000, d.steps = 10){

  if(!is.numeric(n.resamples) || n.resamples < 1 || n.resamples != floor(n.resamples)) stop('n.resamples must be a positive integer.')
  if(SSRmethod != 'permutation' && SSRmethod != 'bootstrap') stop('The only options for SSRmethod are permutation or bootstrap.')
  if(SSRmethod == 'bootstrap') message('\tYou have selected the bootstrap method for split-sample reliability calculations, our recommendation is to use the default permutation method.')
  if(!is.numeric(d.steps) || d.steps < 1 || d.steps != floor(d.steps)) stop('d.steps must be a positive integer.')

  output = list()
  output$n.resamples <- n.resamples
  output$SSRmethod <- SSRmethod
  output$fn <- fn
  output$d.steps <- d.steps
  output$MC.reps <- MC.reps
  return(output)
}

