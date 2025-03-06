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

  #### resampling methods ####
  # split-sample reliability
  message('calculating reliability based on split-sample method...')
  SSR.out <- calcSSR(df, model, entity, y, ctrPerf, ctrRel)
  est.SSR  <- SSR.out$est.SSR
  est.PSSR <- SSR.out$est.PSSR
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
  est.HLGM.latent <- HLGM.out$est.HLGM.latent
  est.HLGM.model <- HLGM.out$est.HLGM.model
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
  #RIUR.results <- calcResamplingIUR(df, model, entity, y, ctrPerf, ctrRel)
  #est.RIUR <- RIUR.results$IUR
  est.RIUR <- NA
  message('...done')

  ##### compile output ####
  results <- data.frame(
    method = c('SSR', 'PSSR', 'SSR.OE', 'SSR.PE', 'PSSR.OE', 'PSSR.PE', 'ANOVA', 'Logit on latent scale', 'Logit w/ delta approx', 'Logit w/ FE', 'Logit w/ RE', 'BetaBinomial', 'BetaBinomial w/ FE', 'BetaBinomial w/ RE', 'BetaBinomial w/ Jeffreys', 'Resampling IUR'),
    between_var = c(NA, NA, NA, NA, NA, NA,
                    AOV.out$var.b.aov,
                    HLGM.out$var.b.HLGM,
                    HLGM.out$var.b.HLGM.pscale.model,
                    HLGM.out$var.b.HLGM.pscale.model,
                    HLGM.out$var.b.HLGM.pscale.model,
                    betabin.out$var.b.BB,
                    betabin.out$var.b.BB,
                    betabin.out$var.b.BB,
                    betabin.out$var.b.BB,
                    NA
                    ),
    within_var = c(NA, NA, NA, NA, NA, NA,
                   median(AOV.out$var.w.aov),
                   median(HLGM.out$var.w.latent),
                   median(HLGM.out$var.w.model),
                   median(HLGM.out$var.w.FE),
                   median(HLGM.out$var.w.RE),
                   median(betabin.out$var.w.BB),
                   median(betabin.out$var.w.FE),
                   median(betabin.out$var.w.RE),
                   median(betabin.out$var.w.J),
                   NA
                   ),
    within_var_min = c(NA, NA, NA, NA, NA, NA,
                              min(AOV.out$var.w.aov),
                              min(HLGM.out$var.w.latent),
                              min(HLGM.out$var.w.model),
                              min(HLGM.out$var.w.FE),
                              min(HLGM.out$var.w.RE),
                              min(betabin.out$var.w.BB),
                       min(betabin.out$var.w.FE),
                       min(betabin.out$var.w.RE),
                       min(betabin.out$var.w.J),
                              NA
    ),
    within_var_max = c(NA, NA, NA, NA, NA, NA,
                       max(AOV.out$var.w.aov),
                       max(HLGM.out$var.w.latent),
                       max(HLGM.out$var.w.model),
                       max(HLGM.out$var.w.FE),
                       max(HLGM.out$var.w.RE),
                       max(betabin.out$var.w.BB),
                       max(betabin.out$var.w.FE),
                       max(betabin.out$var.w.RE),
                       max(betabin.out$var.w.J),
                              NA
    ),
    reliability = c(
      est.SSR,
      est.PSSR,
      est.SSR.oe,
      est.SSR.pe,
      est.PSSR.oe,
      est.PSSR.pe,
      median(est.aov),
      median(est.HLGM.latent),
      median(est.HLGM.model),
      median(est.HLGM.FE.model),
      median(est.HLGM.RE.model),
      median(est.BB),
      median(est.BB.FE),
      median(est.BB.RE),
      median(est.BB.J),
      est.RIUR = est.RIUR
    ),
    reliability_mean = c(
      NA,
	  NA,
      NA,
      NA,
      NA,
      NA,
      mean(est.aov),
      mean(est.HLGM.latent),
      mean(est.HLGM.model),
      mean(est.HLGM.FE.model),
      mean(est.HLGM.RE.model),
      mean(est.BB),
      mean(est.BB.FE),
      mean(est.BB.RE),
      mean(est.BB.J),
      est.RIUR = NA
    ),
    reliability_min = c(
      NA,
	  NA,
      NA,
      NA,
      NA,
      NA,
      min(est.aov),
      min(est.HLGM.latent),
      min(est.HLGM.model),
      min(est.HLGM.FE.model),
      min(est.HLGM.RE.model),
      min(est.BB),
      min(est.BB.FE),
      min(est.BB.RE),
      min(est.BB.J),
      est.RIUR = NA
    ),
    reliability_25p = c(
      NA,
	  NA,
      NA,
      NA,
      NA,
      NA,
      quantile(est.aov, 0.25),
      quantile(est.HLGM.latent, 0.25),
      quantile(est.HLGM.model, 0.25),
      quantile(est.HLGM.FE.model, 0.25),
      quantile(est.HLGM.RE.model, 0.25),
      quantile(est.BB, 0.25),
      quantile(est.BB.FE, 0.25),
      quantile(est.BB.RE, 0.25),
      quantile(est.BB.J, 0.25),
      est.RIUR = NA
    ),
    reliability_75p = c(
      NA,
	  NA,
      NA,
      NA,
      NA,
      NA,
      quantile(est.aov, 0.75),
      quantile(est.HLGM.latent, 0.75),
      quantile(est.HLGM.model, 0.75),
      quantile(est.HLGM.FE.model, 0.75),
      quantile(est.HLGM.RE.model, 0.75),
      quantile(est.BB, 0.75),
      quantile(est.BB.FE, 0.75),
      quantile(est.BB.RE, 0.75),
      quantile(est.BB.J, 0.75),
      est.RIUR = NA
    ),
    reliability_max = c(
      NA,
	  NA,
      NA,
      NA,
      NA,
      NA,
      max(est.aov),
      max(est.HLGM.latent),
      max(est.HLGM.model),
      max(est.HLGM.FE.model),
      max(est.HLGM.RE.model),
      max(est.BB),
      max(est.BB.FE),
      max(est.BB.RE),
      max(est.BB.J),
      est.RIUR = NA
    )
  )
  return(results)
}
