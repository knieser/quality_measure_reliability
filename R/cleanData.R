cleanData <- function(df, entity = "entity", y = "y", ctrPerf = controlPerf()){
  min.n <- ctrPerf$min.n

  # check df is a dataframe
  if(!is.data.frame(df)) stop('Please specify an R dataframe for the first parameter (df) of this function.')

  # check that entity and y columns exist
  df.names <- names(df)
  if(!(entity %in% df.names)) stop(paste0(entity, ' is not in the dataframe. Check column name provided to identify the accountable entities.'))
  if(!(y %in% df.names)) stop(paste0(y, ' is not in the dataframe. Check column name provided to identify the outcome variable.'))

  # check for missing data
  n.missing <- sum(!complete.cases(df))
  df = na.omit(df)
  if (n.missing > 0) message(paste0(n.missing, " rows with missing values were removed."))

  # fix columns
  df$y <- df[[y]]
  df$entity <- df[[entity]]

  # check data in y column
  if(!all(is.numeric(df$y))) stop('Outcome data should contain numeric data only.')

  # drop entities with less than the minimum number of observations
  sample.size <- aggregate(y ~ entity, data = df, length)
  small.entities <- sample.size[sample.size[,2] < min.n, 1]
  if(length(small.entities) > 0){
    message('...dropping entities with insufficient sample size: ')
    message(paste(as.character(small.entities), collapse = ', '))
    message(paste0('A total of ', length(small.entities), ' out of ', nrow(sample.size), ' entities were dropped.'))
    }
  df <- df[!(df$entity %in% small.entities), ]

  df$entity = factor(df$entity)
  df[[entity]] = factor(df[[entity]])

  return(df)
}
