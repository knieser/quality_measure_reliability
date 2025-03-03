controlPerf <- function(min.n = 2, cl = 0.95, n.boots = 10, n.cores = 4){
  output = list()
  output$min.n <- min.n
  output$cl <- cl
  output$n.boots <- n.boots
  output$n.cores <- n.cores
  return(output)
}
