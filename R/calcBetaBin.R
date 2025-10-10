#' Calculate reliability using a Beta-Binomial model
#'
#' @description
#' This function estimates reliability using a Beta-Binomial model. **NOTE:** currently, Beta-Binomial reliability estimates do not take risk-adjustment into account.
#' @details
#' To fit the Beta-Binomial model, the function first calculates
#' method-of-moments estimates for the alpha and beta parameters which are used as starting values.
#' Then, we use the `optim()` function to calculate maximum likelihood estimates with `method = 'L-BFGS-B'`.
#' Reliability estimates are calculated used the maximum likelihood estimates of alpha and beta.
#'
#' @param df dataframe (assumed to be observation-level unless `df.aggregate` is changed below); if null, will use the dataframe from the model object
#' @param model model; if null, will use an unadjusted model (NOTE: currently, Beta-Binomial reliability estimates do not take risk-adjustment into account.)
#' @param entity data column containing the accountable entity identifier
#' @param y data column containing the outcome variable
#' @param df.aggregate set this to `TRUE` if the data have already been aggregated to include only summary data (sample sizes and numerators) for each entity; default is `FALSE`.
#' @param n if using aggregated data, data column containing the sample size by entity
#' @param x if using aggregated data, data column containing the number of observations that met measure criteria by entity
#' @param show.all logical parameter indicating whether all variations of reliability estimates should be calculated; default is `FALSE`.
#' @param ctrPerf parameters to control performance measure calculation
#' @returns A list containing:
#' * `alpha`: estimated alpha from Beta-Binomial model
#' * `beta`: estimated beta from Beta-Binomial model
#' * `entity`: list of entities
#' * `n`: sample sizes for each entity
#' * `var.b.BB`: between-entity variance
#' * `var.w.BB`: within-entity variance
#' * `est.BB`: entity-level reliability estimates
#'
#' If `show.all` is set to `TRUE`, then the outputted list will also contain:
#' * `var.w.FE`: within-entity variance using fixed effect estimates of entity-specific outcome probabilities
#' * `var.w.RE`: within-entity variance using random effect estimates of entity-specific outcome probabilities
#' * `var.w.J`: within-entity variance using Bayesian estimates of entity-specific outcome probabilities, with Jeffrey's prior
#' * `est.BB.FE`: entity-level reliability estimates using fixed effect estimates of entity-specific outcome probabilities
#' * `est.BB.RE`: entity-level reliability estimates using random effect estimates of entity-specific outcome probabilities
#' * `est.BB.J`: entity-level reliability estimates using Bayesian estimates of entity-specific outcome probabilities, with Jeffrey's prior
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @references Adams JL. The Reliability of Provider Profiling: A Tutorial. 2009.
#' @references Nieser KJ, Harris AH. Comparing methods for assessing the reliability of health care quality measures. Statistics in Medicine. 2024 Oct 15;43(23):4575-94.
#' @references Zhou G, Lin Z. Improved beta-binomial estimation for reliability of healthcare quality measures. medRxiv. 2023 Jan 9:2023-01.
#' @examples
#' # Simulate data
#' df <- simulateData(n.entity = 50, n.obs = 100, mu = .2, r = .7)
#'
#' # Calculate reliability
#' out <- calcBetaBin(df = df, entity = 'entity', y = 'y')
#' summary(out$est.BB)
#'
#' # Plot entity-level reliability by sample size
#' plot(out$n, out$est.BB)
#'
#'
#' ## Reliability can also be calculated with data aggregated by entity
#' df.agg <- data.frame(
#'           entity = aggregate(y ~ entity, data = df, length)$entity,
#'           n = aggregate(y ~ entity, data = df, length)$y,
#'           x = aggregate(y ~ entity, data = df, sum)$y
#'           )
#'
#' out2 <- calcBetaBin(df = df.agg, df.aggregate = TRUE, n = 'n', x = 'x')
#' summary(out2$est.BB)
#'
#' @importFrom stats optim median
#' @export

calcBetaBin <- function(df = NULL, model = NULL, entity = 'entity', y = 'y', df.aggregate = FALSE, n = 'n', x = 'x', show.all=FALSE, ctrPerf = controlPerf()){
  if (is.null(df) & is.null(model)) stop ('Please provide either a dataframe or a model object')
  if (is.null(df)){df <- model@frame}
  if(!is.logical(show.all)) stop('show.all needs to be TRUE or FALSE')
  message('\tCurrently, Beta-Binomial reliability estimates do not account for risk-adjustment (even if you specified a model). Updates to this function to account for risk-adjustment are in progress.')

  cl <- match.call()

  if (isFALSE(df.aggregate)){
    data.out <- calcDataSummary(df, model, entity, y, data.type = 'binary', ctrPerf)
    df <- data.out$df
    entities <- data.out$entities
    n  <- data.out$n
    x <- data.out$obs
    p <- data.out$p
    p.re <- data.out$p.re
  } else{
    message('Note that aggregated data are being used, so Beta-Binomial reliability estimates with random effects predictions cannot be calculated.')
    entities <- df[[entity]]
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

  if(show.all==TRUE){
    var.w.FE <- p * (1 - p) / n
    est.BB.FE = var.b.BB / (var.b.BB + var.w.FE)

    var.w.RE <- p.re * (1 - p.re) / n
    est.BB.RE = var.b.BB / (var.b.BB + var.w.RE)

    # within variance using Jeffreys prior
    p.J = (0.5 + x) / (1 + n)
    var.w.J = p.J * (1 - p.J) / n
    est.BB.J = var.b.BB / (var.b.BB + var.w.J)

    results <- list(call = cl,
                    alpha = a,
                    beta = b,
                    entity = as.vector(entities),
                    n = n,
                    var.b.BB = var.b.BB,
                    var.w.BB = var.w.BB,
                    var.w.FE = var.w.FE,
                    var.w.RE = var.w.RE,
                    var.w.J = var.w.J,
                    est.BB = est.BB,
                    est.BB.FE = est.BB.FE,
                    est.BB.RE = est.BB.RE,
                    est.BB.J = est.BB.J)
  } else{
    results <- list(call = cl,
                    alpha = a,
                    beta = b,
                    entity = as.vector(entities),
                    n = n,
                    var.b = var.b.BB,
                    var.w = var.w.BB,
                    est.BB = est.BB)
  }

  return(results)
}
