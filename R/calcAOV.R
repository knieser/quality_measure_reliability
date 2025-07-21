#' Calculate reliability using one-way ANOVA method
#' @description
#' This function estimates reliability using the one-way ANOVA method.
#' @param df observation-level data
#' @param entity data column containing the accountable entity identifier
#' @param y data column containing the outcome variable
#' @param ctrPerf parameters to control performance measure calculation
#' @returns A list with the following components:
#'  \item{var.b.aov}{between-entity variance}
#'  \item{var.w.aov}{within-entity variance}
#'  \item{est.aov}{reliability estimate}
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @references He K, Kalbfleisch JD, Yang Y, Fei Z. Inter‐unit reliability for nonlinear models. Statistics in Medicine. 2019 Feb 28;38(5):844-54.
#' @references Nieser KJ, Harris AH. Comparing methods for assessing the reliability of health care quality measures. Statistics in Medicine. 2024 Oct 15;43(23):4575-94.
#' @importFrom stats aov
#' @export

calcAOV <- function(df, entity = 'entity', y = 'y', ctrPerf = controlPerf()){
  df <- cleanData(df, entity, y, ctrPerf)
  n  <- aggregate(y ~ entity, data = df, length)$y
  n0 <- 1 / (length(n) - 1) * (sum(n) - sum(n^2) / sum(n))

  aov.out <- aov(y ~ entity, data = df)
  aov.summary <- matrix(unlist(summary(aov.out)), ncol=2, byrow = T)
  MSB = aov.summary[3,1]
  MSW = aov.summary[3,2]
  var.b.aov = (MSB - MSW) / n0
  var.w.aov = MSW / n
  est.aov   = var.b.aov / (var.b.aov + var.w.aov)

  output = list(var.b.aov = var.b.aov, var.w.aov = var.w.aov, est.aov = est.aov)

  return(output)
}
