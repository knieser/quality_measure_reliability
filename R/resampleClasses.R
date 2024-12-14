resampleClasses <- function(m, method, y, provider, nsims){
  f <- function(x) {
    data <- x@frame
    data$y <- data[[y]]
    data$provider <- data[[provider]]
    res = RiskAdjPerf(m, data, y, provider)
    class <- classify(res = res, m = m, y, provider, method)
  }
  b <- bootMer(m, FUN = f, seed = 110, nsim = nsims,
               use.u = TRUE, parallel = 'multicore', ncpus = 3)
  as.data.frame(b)
}