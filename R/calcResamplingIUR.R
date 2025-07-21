#' Calculate reliability using resampling inter-unit reliability method
#' @description
#' This function estimates reliability using the resampling inter-unit reliability method.
#' @param df observation-level data; if null, will use the dataframe from the model object
#' @param model model; if null, will use an unadjusted model
#' @param entity data column containing the accountable entity identifier
#' @param y data column containing the outcome variable
#' @param ctrPerf parameters to control performance measure calculation
#' @param ctrRel parameters to control reliability estimation
#' @returns A list with the following components:
#'  \item{var.b}{between-entity variance}
#'  \item{var.w}{within-entity variance}
#'  \item{var.total}{total variance}
#'  \item{IUR}{estimate of IUR}
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @references He K, Kalbfleisch JD, Yang Y, Fei Z. Inter‐unit reliability for nonlinear models. Statistics in Medicine. 2019 Feb 28;38(5):844-54.
#' @references Nieser KJ, Harris AH. Comparing methods for assessing the reliability of health care quality measures. Statistics in Medicine. 2024 Oct 15;43(23):4575-94.
#' @importFrom stats aggregate
#' @export

calcResamplingIUR <- function(df = NULL, model = NULL,  entity = 'entity', y = 'y', ctrPerf = controlPerf(), ctrRel = controlRel()){
  if (is.null(df) & is.null(model)) stop ('Please provide either a dataframe or a model object')
  if (is.null(df)){df <- model@frame}

  n.cores     <- ctrRel$n.cores
  n.resamples <- ctrRel$n.resamples

  data.out <- calcDataSummary(df, model, entity, y, ctrPerf)
  df <- data.out$df
  n  <- data.out$n
  n0 <- 1 / (length(n) - 1) * (sum(n) - sum(n^2) / sum(n))
  marg.p <- data.out$marg.p

  entities = unique(df$entity)
  n.entity = length(entities)

  entity.means0 = aggregate(y ~ entity, data = df, mean)$y
  var.total = 1/(n0*(n.entity - 1)) * sum(n * (entity.means0 - marg.p)^2)

  entity.means = matrix(data = NA, nrow = n.entity, ncol = n.resamples)

  for (j in 1:n.resamples){
    # take a bootstrap resample within each entity separately
    df.resample = data.frame()
    for (i in 1:n.entity){
      entity.df <- df[df$entity == entities[i], ]
      entity.df.resample <- entity.df[sample(nrow(entity.df), nrow(entity.df), replace = T), ]
      df.resample <- rbind(df.resample, entity.df.resample)
    }

    # calculate measure by entity
    entity.means[,j] = aggregate(y ~ entity, data = df.resample, mean)$y
  }

  bootstrap.means = apply(entity.means, 1, mean)
  bootstrap.sqrd.resid = apply(entity.means, 2, function(x) (x - bootstrap.means)^2)
  bootstrap.var = 1/(n.resamples - 1) * apply(bootstrap.sqrd.resid, 1, sum)
  var.w = sum((n - 1) * bootstrap.var) / (sum(n) - n.entity)

  IUR = (var.total - var.w) / var.total

  results = list(var.b = var.total - var.w,
                 var.w = var.w,
                 var.total = var.total,
                 IUR = IUR)

  return(results)
}
