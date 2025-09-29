#' Simulate data
#' @description
#' This function simulates some data.
#' @param n.entity total number of entities to simulate
#' @param n.obs average number of observations per entity; entity sample sizes are simulated from a Poisson distribution with mean given by n.obs OR a vector of length n.entity with entity sample sizes
#' @param mu average probability of the outcome for binary data OR average outcome value for Normal data
#' @param r median reliability
#' @param beta1 regression coefficient for covariate added to the linear predictor; default is 0.
#' @param data.type type of data to simulate. Valid options include: `binary` (default) and `normal`.
#' @param dist specifies the distribution family to use to simulate provider performance. Valid options include: `normal` (default) and `beta`.
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @examples
#' # number of accountable entities
#' n.entity = 100
#'
#' # average number of patients or cases per accountable entity
#' n.obs = 50
#'
#' # marginal probability of the outcome
#' mu = 0.1
#'
#' # approximate reliability for entity with a median number of patients
#' r = 0.6
#'
#' # parameter for risk-adjustment model (i.e., coefficient for x1)
#' beta1 = log(1.5)
#'
#' df <- simulateData(n.entity = n.entity, n.obs = n.obs, mu = mu, r = r, beta1 = beta1)
#' head(df)
#'
#' @importFrom stats aov rnorm rbeta rpois
#' @export

simulateData <- function(n.entity, n.obs, mu, r, beta1 = 0, data.type = 'binary', dist = 'normal'){

  if (length(n.obs)==1){
    n = stats::rpois(n.entity, n.obs)
    } else if (length(n.obs) == n.entity){
      n = n.obs
    } else {
      stop('n.obs must either be a single number indicating the average sample size or a vector with the same length as the n.entity')
    }

  total.n = sum(n)
  median.n = median(n)
  entity = rep(1:n.entity, times = n)
  x1 = stats::rnorm(total.n, 0, 1)

  if (data.type == 'binary'){
    beta0 = -log(1/mu - 1)
    var.b = r/(1 - r) * (1/(median.n*mu*(1 - mu)))

    if (dist == 'normal'){
      z = rep(stats::rnorm(n.entity, 0, sqrt(var.b)), times = n)
      lp = z + beta0 + beta1 * x1

    } else if (dist == 'beta'){
      a = median.n * mu * (1 - r) / r
      b = median.n * (1 - mu) * (1 - r) / r
      z = rep(stats::rbeta(n.entity, a, b), times = n)
      lp = stats::qlogis(z) + beta1 * x1
    }
    p = stats::plogis(lp)
    y = stats::rbinom(total.n, 1, p)
    df = data.frame(
      entity = as.factor(entity),
      z = z,
      x1 = x1,
      lp = lp,
      p = p,
      y = y
    )
  }

  if (data.type == 'normal'){
    beta0 = mu
    var.b = 1 / (1 - r) * 1 / median.n
    z = rep(stats::rnorm(n.entity, 0, sqrt(var.b)), times = n)
    lp = z + beta0 + beta1 * x1
    y = stats::rnorm(total.n, mean = lp, sd = tau[2])
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
