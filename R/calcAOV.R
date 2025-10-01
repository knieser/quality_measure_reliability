#' Calculate reliability using one-way ANOVA method
#' @description
#' This function estimates reliability using the one-way ANOVA method.
#' @param df observation-level data
#' @param entity data column containing the accountable entity identifier
#' @param y data column containing the outcome variable
#' @param df.aggregate set this to `TRUE` if the data have already been aggregated to include only summary data (sample size, means, and standard deviations) for each entity; default is `FALSE`.
#' @param n if using aggregated data, data column containing the sample sizes for each entity; default is `n`.
#' @param mean if using aggregated data, data column containing the sample means for each entity; default is `mean`.
#' @param std.dev if using aggregated data, data column containing the sample standard deviations for each entity entity; default is `sd`.
#' @param ctrPerf parameters to control performance measure calculation
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @references He K, Kalbfleisch JD, Yang Y, Fei Z. Inter‐unit reliability for nonlinear models. Statistics in Medicine. 2019 Feb 28;38(5):844-54.
#' @references Nieser KJ, Harris AH. Comparing methods for assessing the reliability of health care quality measures. Statistics in Medicine. 2024 Oct 15;43(23):4575-94.
#' @importFrom stats aov
#' @export

calcAOV <- function(df, entity = 'entity', y = 'y', df.aggregate = FALSE, n = 'n', mean = 'mean', std.dev = 'sd', ctrPerf = controlPerf()){
  cl <- match.call()

  if (isFALSE(df.aggregate)){
    df <- cleanData(df, entity, y, ctrPerf)
    n  <- aggregate(y ~ entity, data = df, length)$y

    aov.out <- aov(y ~ entity, data = df)
    aov.summary <- matrix(unlist(summary(aov.out)), ncol=2, byrow = T)
    MSB = aov.summary[3,1]
    MSW = aov.summary[3,2]
    } else{
    n <- df[[n]]
    mu <- df[[mean]]
    sd <- df[[std.dev]]

    m = nrow(df) # number of entities
    n.total = sum(n)
    grand.mean = 1 / n.total * sum(n * mu)
    MSB = 1 / (m - 1) * sum(n * (mu - grand.mean)^2)
    MSW = 1 / (n.total - m) * sum((n-1)*sd^2)
  }

  n0 <- 1 / (length(n) - 1) * (sum(n) - sum(n^2) / sum(n))
  var.b.aov = (MSB - MSW) / n0
  var.w.aov = MSW / n
  est.aov   = var.b.aov / (var.b.aov + var.w.aov)

  output = list(call = cl, var.b.aov = var.b.aov, var.w.aov = var.w.aov, est.aov = est.aov)

  return(output)
}
