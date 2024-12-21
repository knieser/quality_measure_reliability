cleanData <- function(df, entity = "entity", y = "y", ctrPerf = controlPerf()){
  min.n <- ctrPerf$min.n

  # fix columns
  df$y <- df[[y]]
  df$entity <- df[[entity]]

  # drop entities with less than the minimum number of observations
  sample.size <- aggregate(y ~ entity, data = df, length)
  small.entities <- sample.size[sample.size[,2] < min.n, 1]
  if(length(small.entities) > 0){
    message('Dropping entities with insufficient sample size:')
    meassage(paste(as.character(small.entities), collapse = ', '))
    message(paste0(length(small.entities), ' entities were dropped.'))
    }
  df <- df[!(df$entity %in% small.entities), ]

  df$entity = factor(df$entity)

  return(df)
}
