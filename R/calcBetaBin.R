#' Calculate reliability using a Beta-Binomial model
#' @description
#' This function estimates reliability using a Beta-Binomial model.
#' @param df dataframe; if null, will use the dataframe in the model object
#' @param model model; if null, will use an unadjusted model
#' @param entity variable to use as the accountable entity
#' @param y variable to use as the outcome
#' @param ctrPerf parameters to control performance measure calculation
#' @returns Estimated parameters from the Beta-Binomial model, estimates of between and within-entity variance, and estimates of entity-specific reliability
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @references None
#' @examples
#' # TBD
#' @importFrom stats optim median
#' @export

calcBetaBin <- function(df = NULL, model = NULL, entity = 'entity', y = 'y', ctrPerf = controlPerf()){
  if (is.null(df) & is.null(model)) stop ('Please provide either a dataframe or a model object')
  if (is.null(df)){df <- model@frame}

  data.out <- calcDataSummary(df, model, entity, y, ctrPerf)
  df <- data.out$df
  marg.p <- data.out$marg.p
  marg.p.model <- data.out$marg.p.model
  n  <- data.out$n
  x <- data.out$obs
  p <- data.out$p
  p.re <- data.out$p.re

  # negative log-likelihood
  neg.loglik <- function(v){
    -sum(lgamma(v[1] + v[2]) - lgamma(v[1]) - lgamma(v[2]) +
           lchoose(n, x) + lgamma(v[1] + x) + lgamma(v[2] + n - x) -
           lgamma(v[1] + v[2] + n))
  }

  # calculate method of moments estimates for starting values
  m1 = mean(x)
  m2 = mean(x^2)
  median.n = mean(n)
  a.mom = (median.n*m1 - m2) / (median.n *(m2/m1 - m1 - 1) + m1)
  b.mom = ((median.n - m1) * (median.n - m2/m1)) / (median.n*(m2/m1 - m1 - 1) + m1)
  a0 = max(a.mom, .5)
  b0 = max(b.mom, .5)

  # maximize negative log-likelihood
  theta = optim(c(a0, b0), neg.loglik, method = 'L-BFGS-B', lower = c(1e-6, 1e-6))$par
  a = theta[1]
  b = theta[2]

  # calculate variance ratio
  var.b.BB = a * b / (a + b + 1) / (a + b)^2
  est.BB = n / (a + b + n)

  var.w.FE <- p * (1 - p) / n
  est.BB.FE = var.b.BB / (var.b.BB + var.w.FE)

  var.w.RE <- p.re * (1 - p.re) / n
  est.BB.RE = var.b.BB / (var.b.BB + var.w.RE)

  # within variance using Jeffreys prior
  p.J = (0.5 + x) / (1 + n)
  var.w.J = p.J * (1 - p.J) / n
  est.BB.J = var.b.BB / (var.b.BB + var.w.J)

  results <- list(alpha = a,
                  beta = b,
                  var.b.BB = var.b.BB,
                  var.w.FE = var.w.FE,
                  var.w.RE = var.w.RE,
                  var.w.J = var.w.J,
                  est.BB = est.BB,
                  est.BB.FE = est.BB.FE,
                  est.BB.RE = est.BB.RE,
                  est.BB.J = est.BB.J)
  return(results)
}
