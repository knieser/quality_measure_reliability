#' Simulate data
#' @description
#' This function simulates some data.
#' @param n.entity total number of entities to simulate
#' @param avg.n average number of observations per entity; entity sample sizes are simulated from a Poisson distribution with mean avg.n
#' @param tau parameters for distribution of random intercepts. For binary observations, input a vector of length 2 where the 1st entry is the overall probability of the outcome on the log-odds scale and the 2nd entry is the between-entity standard deviation.
#' For continuous observations, input a vector of length 2 where the 1st entry is the between-entity standard deviation and the 2nd entry is the within-entity (or residual) standard deviation.
#' @param theta regression coefficient for covariate added to the linear predictor; default is 0.
#' @param type type of data to simulate. Valid options include: 'binary' (default) and 'normal'.
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @examples
#' # number of accountable entities
#' n.entity = 100
#'
#' # average number of patients or cases per accountable entity
#' avg.n = 50
#'
#' # marginal probability of the outcome
#' p = 0.1
#'
#' # approximate reliability for entity with an average number of patients
#' r = 0.6
#'
#' # implied between-entity variance
#' var.btwn = r / (1 - r) * (1/(avg.n * p * (1 - p)))
#'
#' mu = log(p / (1-p))
#' tau = c(mu, sqrt(var.btwn))
#'
#' # parameter for risk-adjustment model (i.e., coefficient for x1)
#' theta = log(1.5)
#'
#' df <- simulateData(n.entity = n.entity, avg.n = avg.n, tau = tau, theta = theta)
#' head(df)
#'
#' @importFrom stats aov
#' @export

simulateData <- function(n.entity, avg.n, tau, theta = 0, type = 'binary'){

  if (!is.numeric(n.entity)){stop('n.entity must be a positive integer.')}
  if (floor(n.entity) != n.entity){stop('n.entity must be a positive integer.')}
  if (n.entity <= 0){stop('n.entity must be a positive integer.')}
  if (!is.numeric(avg.n)){stop('avg.n must be a positive integer.')}
  if (floor(avg.n) != avg.n){stop('avg.n must be a positive integer.')}
  if (avg.n <= 0){stop('avg.n must be a positive integer.')}
  if (length(tau) != 2){stop('tau must be a vector of length 2.')}
  if (type != 'binary' &  type != 'normal'){stop('Valid options for data type include binary and normal.')}

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
