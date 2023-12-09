makeData <- function(n.providers, n.pts, type, p = NA, var.w = NA, lambda = NA){
  
  n = sum(n.pts)
  id = 1:n
  provider = rep(1:n.providers, times = n.pts)
  y <- vector()
  
  if (type == 'binary'){
    for (j in 1:n.providers){
      provider_y <- rbinom(n.pts[j], 1, p[j])
      ifelse(length(y)==0, y <- provider_y, y <- c(y, provider_y))
    }
  } else if (type == 'normal'){
    for (j in 1:n.providers){
      provider_y <- rnorm(n.pts[j], p[j], sqrt(var.w))
      ifelse(length(y)==0, y <- provider_y, y <- c(y, provider_y))
    }
  } else if (type == 'count'){
    for (j in 1:n.providers){
      provider_y <- rpois(n.pts[j], lambda[j])
      ifelse(length(y)==0, y <- provider_y, y <- c(y, provider_y))
    }
  }

  df <- data.frame(
    id = id, 
    provider = as.factor(provider), 
    y = y
  )
  return(df)
}