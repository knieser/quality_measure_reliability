#' Simulate data
#' @description
#' This function simulates some data.
#' @param n a vector of the number of observations for each accountable entity
#' @param type variable type of the observations (e.g., binary)
#' @returns A dataframe with the following columns:
#'  \item{entity}{the accountable entity}
#'  \item{y}{outcome}
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @references None
#' @examples
#' # TBD
#' @importFrom stats aov
#' @export

simulateData <- function(n, type = 'binary', p = NA, var.w = NA, lambda = NA){

  n.entity = length(n)
  entity = rep(1:n.entity, times = n)
  y <- vector()

  if (type == 'binary'){
    for (j in 1:n.entity){
      entity.y <- rbinom(n[j], 1, p[j])
      ifelse(length(y)==0, y <- entity.y, y <- c(y, entity.y))
    }
  } else if (type == 'normal'){
    for (j in 1:n.entity){
      entity.y <- rnorm(n[j], p[j], sqrt(var.w))
      ifelse(length(y)==0, y <- entity.y, y <- c(y, entity.y))
    }
  } else if (type == 'count'){
    for (j in 1:n.entity){
      entity.y <- rpois(n[j], lambda[j])
      ifelse(length(y)==0, y <- entity.y, y <- c(y, entity.y))
    }
  }

  df <- data.frame(
    entity = as.factor(entity),
    y = y
  )
  return(df)
}
