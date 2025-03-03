#' Calculate reliability using ANOVA method
#' @description
#' This function estimates reliability using the ANOVA method.
#' @param df dataframe; if null, will use the dataframe in the model object
#' @param model model; if null, will use an unadjusted model
#' @param entity variable to use as the accountable entity
#' @param y variable to use as the outcome
#' @param ctrPerf parameters to control performance measure calculation
#' @returns A list with the following components:
#'  \item{var.b.aov}{between-entity variance}
#'  \item{var.w.aov}{within-entity variance}
#'  \item{est.aov}{reliability estimate}
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @references None
#' @examples
#' # TBD
#' @importFrom stats aov
#' @export

calcAOV <- function(df = NULL, model = NULL, entity = 'entity', y = 'y', ctrPerf = controlPerf()){
  if (is.null(df) & is.null(model)) stop ('Please provide either a dataframe or a model object')
  if (is.null(df)){df <- model@frame}

  data.out <- calcDataSummary(df, model, entity, y, ctrPerf = ctrPerf)
  df <- data.out$df
  n  <- data.out$n
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
