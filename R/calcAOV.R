#' Calculate reliability using ANOVA method
#' @description
#' This function estimates reliability using the ANOVA method.
#' @param df dataframe; if null, will use the dataframe in the model object
#' @param model model; if null, will use an unadjusted model
#' @param y variable to use as the outcome
#' @param provider variable to use as the accountable entity
#' @param ctrPerf parameters to control performance measure calculation
#' @returns A list with the following components:
#'  \item{var.b.aov}{between-entity variance}
#'  \item{var.w.aov}{within-entity variance}
#'  \item{est.aov}{reliability estimate}
#' @returns The plot function can be used to plot the provider-level reliability estimates.
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @references None
#' @examples
#' # TBD
#' @importFrom stats aov
#' @export

calcAOV <- function(df = NULL, model = NULL, y = 'y', provider = 'provider', ctrPerf = controlPerf()){
  if (is.null(df) & is.null(model)) stop ('Please provide either a dataframe or a model object')
  if (is.null(df)){df <- model@frame}

  data.out <- calcDataSummary(df, model, y, provider, ctrPerf = ctrPerf)
  df <- data.out$df
  n  <- data.out$n
  n0 <- 1 / (length(n) - 1) * (sum(n) - sum(n^2) / sum(n))

  aov.out <- aov(y ~ provider, data = df)
  aov.summary <- matrix(unlist(summary(aov.out)), ncol=2, byrow = T)
  MSB = aov.summary[3,1]
  MSW = aov.summary[3,2]
  var.b.aov = (MSB - MSW) / n0
  var.w.aov = MSW
  est.aov   = var.b.aov / (var.b.aov + var.w.aov / n)

  output = list(var.b.aov = var.b.aov, var.w.aov = var.w.aov, est.aov = est.aov)

  return(output)
}
