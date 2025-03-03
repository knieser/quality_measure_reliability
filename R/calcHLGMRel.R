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
  marg.p.model <- data.out$marg.p.model
  n  <- data.out$n
  p <- data.out$p
  p.re <- data.out$p.re

  # calculate between-variance based on model
  var.b.HLGM <- lme4::VarCorr(fit)[[entity]][1,1]
  var.b.HLGM.pscale.model <- marg.p.model^2 * (1 - marg.p.model)^2 * var.b.HLGM

  # within-variance on the logistic scale
  var.w.logistic = pi^2 / (3 * n)
  est.HLGM.logistic <- var.b.HLGM / (var.b.HLGM + var.w.logistic)
  
  # within-variance based on marginal probability
  var.w.model <- marg.p.model * (1 - marg.p.model) / n
  est.HLGM.model <- var.b.HLGM.pscale.model / (var.b.HLGM.pscale.model + var.w.model)
  
  # within-variance based on sample proportion estimates
  var.w.FE <- p * (1 - p) / n
  est.HLGM.FE.model <- var.b.HLGM.pscale.model / (var.b.HLGM.pscale.model + var.w.FE)

  # within-variance from random effects model
  var.w.RE <- p.re * (1 - p.re) / n
  est.HLGM.RE.model <- var.b.HLGM.pscale.model / (var.b.HLGM.pscale.model + var.w.RE)

  output = list(
    marg.p = marg.p,
    marg.p.model = marg.p.model,
    n = n,
    p = p,
    p.re = p.re,
    var.b.HLGM = var.b.HLGM,
    var.b.HLGM.pscale.model = var.b.HLGM.pscale.model,
    var.w.logistic = var.w.logistic,
    var.w.model = var.w.model,
    var.w.FE = var.w.FE,
    var.w.RE = var.w.RE,
    est.HLGM.logistic = est.HLGM.logistic,
    est.HLGM.model = est.HLGM.model,
    est.HLGM.FE.model = est.HLGM.FE.model,
    est.HLGM.RE.model = est.HLGM.RE.model
  )

  return(output)
}
