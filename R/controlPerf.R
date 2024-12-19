controlPerf <- function(min.n = 10, cl = 0.95, n.sims = 10, n.cores = 4){
  output = list()
  output$min.n <- min.n
  output$cl <- cl
  output$n.sims <- n.sims
  output$n.cores <- n.cores
  return(output)
}
