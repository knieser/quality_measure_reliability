#' Calculate reliability of quality measure performance
#' @description
#' This function calculates several estimates of quality measure performance.
#' @param df dataframe; if null, will use the dataframe in the model object
#' @param model model; if null, will use an unadjusted model
#' @param entity variable to use as the accountable entity; default = "entity"
#' @param y variable to use as the outcome; default = "y"
#' @param ctrPerf parameters to control performance measure calculation
#' @param ctrRel parameters to control reliability estimation
#' @returns A list with the following components:
#'  \item{perf.res}{list of measure performance results}
#'  \item{rel.results}{table of reliability estimates by method}
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @references None
#' @examples
#' # TBD
#' @export

calcReliability <- function(df = NULL, model = NULL, entity = "entity", y = "y", ctrPerf = controlPerf(), ctrRel = controlRel()){

  #### calculate measure performance ####
  message('calculating measure performance...')
  res <- calcPerformance(df, model, entity, y, ctrPerf)
  df = res$df
  model = res$model
  message('...done')

  #### resampling methods ####
  # split-sample reliability
  message('calculating reliability based on split-sample method...')
  SSR.out <- calcSSR(df, model, entity, y, ctrPerf, ctrRel)
  est.SSR.oe  <- SSR.out$est.SSR.oe
  est.PSSR.oe <- SSR.out$est.PSSR.oe
  est.SSR.pe  <- SSR.out$est.SSR.pe
  est.PSSR.pe <- SSR.out$est.PSSR.pe
  message('...done')

  #### SNR methods ####
  # anova
  message('calculating reliability based on anova method...')
  AOV.out <- calcAOV(df, model, entity, y, ctrPerf)
  est.aov <- AOV.out$est.aov
  message('...done')

  # multilevel logistic regression model methods
  message('calculating reliability based on multilevel logistic regression model...')
  HLGM.out <- calcHLGMRel(df, model, entity, y, ctrPerf)
  est.HLGM.FE.model <- HLGM.out$est.HLGM.FE.model
  est.HLGM.RE.model <- HLGM.out$est.HLGM.RE.model
  message('...done')

  # Beta-Binomial method
  message('calculating reliability based on Beta-Binomial method...')
  betabin.out <- calcBetaBin(df, model, entity, y, ctrPerf)
  est.BB <- betabin.out$est.BB
  est.BB.FE <- betabin.out$est.BB.FE
  est.BB.RE <- betabin.out$est.BB.RE
  est.BB.J <- betabin.out$est.BB.J
  message('...done')

  # resampling IUR method from He et al 2019
  message('calculating reliability based on resampling IUR method...')
  RIUR.results <- calcResamplingIUR(df, model, entity, y, ctrPerf, ctrRel)
  est.RIUR <- RIUR.results$IUR
  message('...done')

  ##### compile output ####
  rel.results <- data.frame(
    method = c('SSR.OE', 'SSR.PE', 'PSSR.OE', 'PSSR.PE', 'ANOVA', 'Logit w/ FE', 'Logit w/ RE', 'BetaBinomial', 'BetaBinomial w/ FE', 'BetaBinomial w/ RE', 'BetaBinomial w/ Jeffreys', 'Resampling IUR'),
    reliability = c(
      est.SSR.oe,
      est.SSR.pe,
      est.PSSR.oe,
      est.PSSR.pe,
      median(est.aov),
      median(est.HLGM.FE.model),
      median(est.HLGM.RE.model),
      median(est.BB),
      median(est.BB.FE),
      median(est.BB.RE),
      median(est.BB.J),
      est.RIUR = est.RIUR
    )
  )

  output = list(perf.res = res, rel.results = rel.results)

  return(output)
}
