controlRel <- function(n.resamples = 10, n.cores = 4, method = 'permutation', fn = NA, n.boots = 10){
  output = list()
  output$n.resamples <- n.resamples
  output$n.cores <- n.cores
  output$method <- method
  output$fn <- fn
  output$n.boots <- n.boots
  return(output)
}
  
  