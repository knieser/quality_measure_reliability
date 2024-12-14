cleanData <- function(df0, y, provider, ctrPerf = controlPerf()){
  min.n <- ctrPerf$min.n
  
  # fix columns
  df0$y <- df0[[y]]
  df0$provider <- df0[[provider]]
  
  # drop providers with less than the minimum number of observations
  sample.size <- aggregate(y ~ provider, data = df0, length)
  small.providers <- sample.size[sample.size[,2] < min.n, 1]
  if(length(small.providers) > 0){cat(paste0('Dropping entities with insufficient sample size...', small.providers, '\n'))}
  df <- df0[!(df0$provider %in% small.providers), ]
  
  df$provider = factor(df$provider)
  
  return(df)
}