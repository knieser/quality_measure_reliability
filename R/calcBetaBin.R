#' Calculate reliability using a Beta-Binomial model
#' @description
#' This function estimates reliability using a Beta-Binomial model.
#' @param df observation-level data; if null, will use the dataframe from the model object
#' @param model model; if null, will use an unadjusted model (NOTE: currently, Beta-Binomial reliability estimates do not take risk-adjustment into account.)
#' @param entity data column containing the accountable entity identifier
#' @param y data column containing the outcome variable
#' @param df.aggregate set this to TRUE if the data have already been aggregated to include only summary data (n, x) for each entity; default is FALSE.
#' @param n if using aggregated data, data column containing the sample size by entity
#' @param x if using aggregated data, data column containing the number of observations that met measure criteria by entity
#' @param show.all logical parameter indicating whether all variations of reliability estimates should be calculated; default is FALSE.
#' @param ctrPerf parameters to control performance measure calculation
#' @returns Estimated parameters from the Beta-Binomial model, estimates of between and within-entity variance, and estimates of entity-specific reliability
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @references Adams JL. The Reliability of Provider Profiling: A Tutorial. 2009.
#' @references Nieser KJ, Harris AH. Comparing methods for assessing the reliability of health care quality measures. Statistics in Medicine. 2024 Oct 15;43(23):4575-94.
#' @references Zhou G, Lin Z. Improved beta-binomial estimation for reliability of healthcare quality measures. medRxiv. 2023 Jan 9:2023-01.
#' @importFrom stats optim median
#' @export

calcBetaBin <- function(df = NULL, model = NULL, entity = 'entity', y = 'y', df.aggregate = FALSE, n = 'n', x = 'x', show.all=FALSE, ctrPerf = controlPerf()){
  message('\tCurrently, Beta-Binomial reliability estimates do not account for risk-adjustment (even if you specified a model). Updates to this function to account for risk-adjustment are in progress.')
  if (is.null(df) & is.null(model)) stop ('Please provide either a dataframe or a model object')
  if (is.null(df)){df <- model@frame}
  if(!is.logical(show.all)) stop('show.all needs to be TRUE or FALSE')

  if (isFALSE(df.aggregate)){
  data.out <- calcDataSummary(df, model, entity, y, data.type = 'binary', ctrPerf)
  df <- data.out$df
  n  <- data.out$n
  x <- data.out$obs
  p <- data.out$p
  p.re <- data.out$p.re
  } else{
    message('Note that aggregated data are being used, so Beta-Binomial reliability estimates with random effects predictions cannot be calculated.')
    n <- df[[n]]
    x <- df[[x]]
    p <- x / n
    p.re <- NA
}

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
  var.w.BB = a * b / ((a + b + 1) * (a + b) * n)
  est.BB = n / (a + b + n)

  results <- list(alpha = a,
                  beta = b,
                  var.between = var.b.BB,
                  var.within = var.w.BB,
                  est.BB = est.BB)

  if(show.all==TRUE){
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
                    var.w.BB = var.w.BB,
                    var.w.FE = var.w.FE,
                    var.w.RE = var.w.RE,
                    var.w.J = var.w.J,
                    est.BB = est.BB,
                    est.BB.FE = est.BB.FE,
                    est.BB.RE = est.BB.RE,
                    est.BB.J = est.BB.J)
  }

  return(results)
}
