controlRel <- function(n.cores = 4, n.resamples = 10, SSRmethod = 'permutation', fn = NA){
  output = list()
  output$n.cores <- n.cores
  output$n.resamples <- n.resamples
  output$SSRmethod <- SSRmethod
  output$fn <- fn
  return(output)
}

