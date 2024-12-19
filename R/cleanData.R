cleanData <- function(df, y = "y", provider = "provider", ctrPerf = controlPerf()){
  min.n <- ctrPerf$min.n

  # fix columns
  df$y <- df[[y]]
  df$provider <- df[[provider]]

  # drop providers with less than the minimum number of observations
  sample.size <- aggregate(y ~ provider, data = df, length)
  small.providers <- sample.size[sample.size[,2] < min.n, 1]
  if(length(small.providers) > 0){
    message(paste0('Dropping entities with insufficient sample size...', c(small.providers)))
    message(paste0(length(small.providers), ' entities have been dropped.'))
    }
  df <- df[!(df$provider %in% small.providers), ]

  df$provider = factor(df$provider)

  return(df)
}
