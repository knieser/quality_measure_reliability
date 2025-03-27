#' Simulate data
#' @description
#' This function simulates some data.
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @references None
#' @examples
#' # TBD
#' @importFrom stats aov
#' @export

simulateData <- function(n.entity, avg.n, tau = tau, theta = theta, type = 'binary'){

  n = rpois(n.entity, avg.n)
  total.n = sum(n)
  entity = rep(1:n.entity, times = n)
  x1 = rnorm(total.n, 0, 1)

  if (type == 'binary'){
    z = rep(rnorm(n.entity, tau[1], tau[2]), times = n)
    lp = z + theta * x1
    p = exp(lp) / (1 + exp(lp))
    y = rbinom(total.n, 1, p)
    df = data.frame(
      entity = as.factor(entity),
      z = z,
      x1 = x1,
      lp = lp,
      p = p,
      y = y
    )
  }

  if (type == 'normal'){
    z = rep(rnorm(n.entity, 0, tau[1]), times = n)
    lp = z + theta * x1
    y = rnorm(total.n, mean = lp, sd = tau[2])
    df = data.frame(
      entity = as.factor(entity),
      z = z,
      x1 = x1,
      lp = lp,
      y = y
    )
  }
  return(df)
}
