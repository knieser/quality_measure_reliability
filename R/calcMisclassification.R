calcMisclassification <- function(m, df, y, provider, method, nsims){
  
  # calculate OE and PE ratios
  results <- RiskAdjPerf(m, df, y, provider)
  
  # classify facilities
  results$class <- classify(res = results, m = m, y, provider, method)
  
  # resample new classifications for each facility
  bootmer_res <- resampleClasses(m, method, y, provider, nsims)
  
  # compare original to distribution of classifications from the bootstraps
  misclass.df <- data.frame(
    provider = rep(results$provider, each = nsims),
    orig.class = rep(results$class, each = nsims),
    boot.class = c(as.matrix(bootmer_res))
  )
  misclass.df
}