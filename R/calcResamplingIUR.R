#' Calculate reliability using resampling inter-unit reliability method
#' @description
#' This function estimates reliability using the resampling inter-unit reliability method.
#' @param df dataframe; if null, will use the dataframe in the model object
#' @param model model; if null, will use an unadjusted model
#' @param y variable to use as the outcome
#' @param provider variable to use as the accountable entity
#' @param ctrPerf parameters to control performance measure calculation
#' @param ctrRel parameters to control reliability estimation
#' @returns A list with the following components:
#'  \item{var.b}{between-entity variance}
#'  \item{var.w}{within-entity variance}
#'  \item{var.total}{total variance}
#'  \item{IUR}{estimate of IUR}
#' @returns The plot function can be used to plot the provider-level reliability estimates.
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @references None
#' @examples
#' # TBD
#' @importFrom stats aggregate
#' @export

calcResamplingIUR <- function(df = NULL, model = NULL, y = 'y', provider = 'provider', ctrPerf = controlPerf(), ctrRel = controlRel()){
  if (is.null(df) & is.null(model)) stop ('Please provide either a dataframe or a model object')
  if (is.null(df)){df <- model@frame}

  n.cores     <- ctrRel$n.cores
  n.resamples <- ctrRel$n.resamples

  data.out <- calcDataSummary(df, model, y, provider, ctrPerf)
  df <- data.out$df
  n  <- data.out$n
  n0 <- 1 / (length(n) - 1) * (sum(n) - sum(n^2) / sum(n))
  marg.p <- data.out$marg.p

  providers = unique(df$provider)
  n.providers = length(providers)

  provider.means0 = aggregate(y ~ provider, data = df, mean)$y
  var.total = 1/(n0*(n.providers - 1)) * sum(n * (provider.means0 - marg.p)^2)

  provider.means = matrix(data = NA, nrow = n.providers, ncol = n.resamples)

  for (j in 1:n.resamples){
    # take a bootstrap resample within each provider separately
    df.resample = data.frame()
    for (i in 1:n.providers){
      provider.df <- df[df$provider == providers[i], ]
      provider.df.resample <- provider.df[sample(nrow(provider.df), nrow(provider.df), replace = T), ]
      df.resample <- rbind(df.resample, provider.df.resample)
    }

    # calculate measure by provider
    #obs <- aggregate(y ~ provider, data = df.resample, sum)$y
    #exp <- aggregate(expect ~ provider, data = df.resample, sum)$expect
    #provider.means[,j] = obs / exp
    provider.means[,j] = aggregate(y ~ provider, data = df.resample, mean)$y
  }

  bootstrap.means = apply(provider.means, 1, mean)
  bootstrap.sqrd.resid = apply(provider.means, 2, function(x) (x - bootstrap.means)^2)
  bootstrap.var = 1/(n.resamples - 1) * apply(bootstrap.sqrd.resid, 1, sum)
  var.w = sum((n - 1) * bootstrap.var) / (sum(n) - n.providers)

  IUR = (var.total - var.w) / var.total

  results = list(var.b = var.total - var.w,
                 var.w = var.w,
                 var.total = var.total,
                 IUR = IUR)

  return(results)
}
