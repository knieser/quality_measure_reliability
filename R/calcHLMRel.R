#' Calculate reliability using a hierarchical linear regression model
#' @description
#' This function estimates reliability using a hierarchical linear regression model.
#' @param df observation-level data; if null, will use the dataframe from the model object
#' @param model model; if null, will use an unadjusted model
#' @param entity data column containing the accountable entity identifier
#' @param y data column containing the outcome variable
#' @param ctrPerf parameters to control performance measure calculation
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @references Nieser KJ, Harris AH. Comparing methods for assessing the reliability of health care quality measures. Statistics in Medicine. 2024 Oct 15;43(23):4575-94.
#' @importFrom lme4 VarCorr
#' @export

calcHLMRel <- function(df = NULL, model = NULL, entity = 'entity', y = 'y', ctrPerf = controlPerf()){

  cl <- match.call()

  data.out <- calcDataSummary(df, model, entity, y, data.type = 'continuous', ctrPerf)
  df <- data.out$df
  fit <- data.out$fit
  n  <- data.out$n

  vcov <- as.data.frame(lme4::VarCorr(fit))
  var.b <- vcov[1, 'vcov']
  var.w <- vcov[2, 'vcov']
  est.HLM <- var.b / (var.b + var.w / n)

  output = list(
    call = cl,
    fit = fit,
    n = n,
    var.between = var.b,
    var.within = var.w,
    est.HLM = est.HLM
  )

  return(output)
}
