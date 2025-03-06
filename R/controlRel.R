controlRel <- function(n.cores = 4, n.resamples = 10, SSRmethod = 'permutation', fn = NA, rs.method = 'oe', d.steps = 10, classification.quantiles = 4){
  output = list()
  output$n.cores <- n.cores
  output$n.resamples <- n.resamples
  output$SSRmethod <- SSRmethod
  output$fn <- fn
  output$rs.method <- rs.method
  output$d.steps <- d.steps
  output$classification.quantiles <- classification.quantiles
  return(output)
}

