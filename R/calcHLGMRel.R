#' Calculate reliability using a multilevel logistic regression model
#' @description
#' This function estimates reliability using a multilevel logistic regression model.
#' @param df dataframe; if null, will use the dataframe in the model object
#' @param model model; if null, will use an unadjusted model
#' @param entity variable to use as the accountable entity
#' @param y variable to use as the outcome
#' @param ctrPerf parameters to control performance measure calculation
#' @returns Estimated parameters and reliability
#'  \item{var.b.aov}{between-entity variance}
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @references None
#' @examples
#' # TBD
#' @importFrom lme4 VarCorr
#' @export

calcHLGMRel <- function(df = NULL, model = NULL, entity = 'entity', y = 'y', ctrPerf = controlPerf()){
  if (is.null(df) & is.null(model)) stop ('Please provide either a dataframe or a model object')
  if (is.null(df)){df <- model@frame}

  data.out <- calcDataSummary(df, model, entity, y, ctrPerf)
  df <- data.out$df
  fit <- data.out$fit
  marg.p <- data.out$marg.p
  n  <- data.out$n
  p <- data.out$p
  p.re <- data.out$p.re
  var.expected = aggregate(expect ~ entity, data = df, function(x) sum(x * (1-x)))$expect

  # calculate between-variance based on model
  var.b.HLGM <- lme4::VarCorr(fit)[[entity]][1,1]
  var.b.HLGM.delta <- (var.expected / n)^2 * var.b.HLGM

  # within-variance on the latent scale
  var.w.latent = pi^2 / (3 * n)
  est.HLGM.latent <- var.b.HLGM / (var.b.HLGM + var.w.latent)

  # within-variance based on delta method approximation
  var.w.delta <- var.expected / n^2
  est.HLGM.delta <- var.b.HLGM.delta / (var.b.HLGM.delta + var.w.delta)

  # within-variance based on sample proportion estimates
  var.w.FE <- p * (1 - p) / n
  est.HLGM.FE <- var.b.HLGM.delta / (var.b.HLGM.delta + var.w.FE)

  # within-variance from random effects model
  var.w.RE <- p.re * (1 - p.re) / n
  est.HLGM.RE <- var.b.HLGM.delta / (var.b.HLGM.delta + var.w.RE)

  output = list(
    marg.p = marg.p,
    n = n,
    p = p,
    p.re = p.re,
    var.b.HLGM = var.b.HLGM,
    var.b.HLGM.delta = var.b.HLGM.delta,
    var.w.latent = var.w.latent,
    var.w.delta = var.w.delta,
    var.w.FE = var.w.FE,
    var.w.RE = var.w.RE,
    est.HLGM.latent = est.HLGM.latent,
    est.HLGM.delta = est.HLGM.delta,
    est.HLGM.FE = est.HLGM.FE,
    est.HLGM.RE = est.HLGM.RE
  )

  return(output)
}
