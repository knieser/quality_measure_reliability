#' Simulate data
#' @description
#' This function simulates some data.
#' @param n a vector of the number of observations for each accountable entity
#' @param type variable type of the observations (e.g., binary)
#' @returns A dataframe with the following columns:
#'  \item{provider}{the accountable entity}
#'  \item{y}{outcome}
#' @returns The summary function can be used to...
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @references None
#' @examples
#' # TBD
#' @importFrom stats aov
#' @export

simulateData <- function(n, type = 'binary', p = NA, var.w = NA, lambda = NA){

  n.providers = length(n)
  provider = rep(1:n.providers, times = n)
  y <- vector()

  if (type == 'binary'){
    for (j in 1:n.providers){
      provider_y <- rbinom(n[j], 1, p[j])
      ifelse(length(y)==0, y <- provider_y, y <- c(y, provider_y))
    }
  } else if (type == 'normal'){
    for (j in 1:n.providers){
      provider_y <- rnorm(n[j], p[j], sqrt(var.w))
      ifelse(length(y)==0, y <- provider_y, y <- c(y, provider_y))
    }
  } else if (type == 'count'){
    for (j in 1:n.providers){
      provider_y <- rpois(n[j], lambda[j])
      ifelse(length(y)==0, y <- provider_y, y <- c(y, provider_y))
    }
  }

  df <- data.frame(
    provider = as.factor(provider),
    y = y
  )
  return(df)
}
