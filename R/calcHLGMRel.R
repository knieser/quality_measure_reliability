#' Calculate reliability using a hierarchical logistic regression model
#' @description
#' This function estimates reliability using a hierarchical logistic regression model.
#' @param df observation-level data; if null, will use the dataframe from the model object
#' @param model model; if null, will use an unadjusted model
#' @param entity data column containing the accountable entity identifier
#' @param y data column containing the outcome variable
#' @param show.all logical parameter indicating whether all variations of reliability estimates should be calculated; default is FALSE.
#' @param ctrPerf parameters to control performance measure calculation
#' @param ctrRel parameters to control reliability estimation
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @references Goldstein H, Browne W, Rasbash J. Partitioning variation in multilevel models. Understanding statistics: statistical issues in psychology, education, and the social sciences. 2002 Dec 2;1(4):223-31.
#' @references He K, Kalbfleisch JD, Yang Y, Fei Z, Kim S, Kang J, Li Y. Inter-unit reliability for quality measure testing. Journal of hospital administration. 2019 Jan 8;8(2):1.
#' @references Hwang J, Adams JL, Paddock SM. Defining and estimating the reliability of physician quality measures in hierarchical logistic regression models. Health Services and Outcomes Research Methodology. 2021 Mar;21(1):111-30.
#' @references Nieser KJ, Harris AH. Comparing methods for assessing the reliability of health care quality measures. Statistics in Medicine. 2024 Oct 15;43(23):4575-94.
#' @importFrom lme4 VarCorr
#' @export

calcHLGMRel <- function(df = NULL, model = NULL, entity = 'entity', y = 'y', show.all=FALSE, ctrPerf = controlPerf(), ctrRel = controlRel()){
  if(!is.logical(show.all)) stop('show.all needs to be TRUE or FALSE')

  cl <- match.call()

  data.out <- calcDataSummary(df, model, entity, y, data.type = 'binary', ctrPerf)
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

  # within-variance based on delta method approximation
  var.w.delta <- var.expected / n^2
  est.HLGM.delta <- var.b.HLGM.delta / (var.b.HLGM.delta + var.w.delta)

  if (show.all==TRUE){
    # Monte Carlo method
    reps = ctrRel$MC.reps
    out.HLGM.MC <- calcMCmethodHLGM(df, n, var.b.HLGM, reps)
    var.b.MC <- out.HLGM.MC$var.b.MC
    var.w.MC <- out.HLGM.MC$var.w.MC
    est.HLGM.MC <- out.HLGM.MC$est.HLGM.MC

    # within-variance on the latent scale
    var.w.latent = pi^2 / (3 * n)
    est.HLGM.latent <- var.b.HLGM / (var.b.HLGM + var.w.latent)

    # within-variance based on sample proportion estimates
    var.w.FE <- p * (1 - p) / n
    est.HLGM.FE <- var.b.HLGM.delta / (var.b.HLGM.delta + var.w.FE)

    # within-variance from random effects model
    var.w.RE <- p.re * (1 - p.re) / n
    est.HLGM.RE <- var.b.HLGM.delta / (var.b.HLGM.delta + var.w.RE)

    output = list(
      call = cl,
      fit = fit,
      marg.p = marg.p,
      n = n,
      p = p,
      p.re = p.re,
      var.b.HLGM = var.b.HLGM,
      var.b.HLGM.delta = var.b.HLGM.delta,
      var.b.MC = var.b.MC,
      var.w.MC = var.w.MC,
      var.w.latent = var.w.latent,
      var.w.delta = var.w.delta,
      var.w.FE = var.w.FE,
      var.w.RE = var.w.RE,
      est.HLGM.MC = est.HLGM.MC,
      est.HLGM.latent = est.HLGM.latent,
      est.HLGM.delta = est.HLGM.delta,
      est.HLGM.FE = est.HLGM.FE,
      est.HLGM.RE = est.HLGM.RE
    )
  } else{
    output = list(
      call = cl,
      fit = fit,
      n = n,
      var.between = var.b.HLGM.delta,
      var.within = var.w.delta,
      est.HLGM.delta = est.HLGM.delta
    )
  }

  return(output)
}
