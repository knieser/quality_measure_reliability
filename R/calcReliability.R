#' Calculate reliability of quality measure performance
#' @description
#' This function calculates several estimates of quality measure performance.
#' @param df observation-level data; if null, will use the dataframe from the model object
#' @param model model; if null, will use an unadjusted model
#' @param entity data column containing the accountable entity identifier
#' @param y data column containing the outcome variable
#' @param data.type acceptable values are "binary" for 0/1 data and "continuous" for continuous data (default: 'binary')
#' @param show.all logical indicator for whether full list of reliability method estimates should be calculated (default: FALSE)
#' @param ctrPerf parameters to control performance measure calculation
#' @param ctrRel parameters to control reliability estimation
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @references Nieser KJ, Harris AH. Comparing methods for assessing the reliability of health care quality measures. Statistics in Medicine. 2024 Oct 15;43(23):4575-94.
#' @export

calcReliability <- function(df = NULL, model = NULL, entity = "entity", y = "y", data.type = 'binary', show.all = FALSE, ctrPerf = controlPerf(), ctrRel = controlRel()){

  if (is.null(df) & is.null(model)) stop ('Please provide either a dataframe or a model object')
  if (is.null(df)){df <- model@frame}
  if(!is.logical(show.all)) stop('show.all needs to be TRUE or FALSE')

  cl <- match.call()

  df <- cleanData(df, entity, y, ctrPerf)

  #### resampling methods ####
  # split-sample reliability
  message('calculating reliability based on split-sample method...')
  SSR.out <- suppressMessages(calcSSR(df, model, entity, y, data.type, ctrPerf, ctrRel))
  est.SSR  <- SSR.out$est.SSR
  est.PSSR <- SSR.out$est.PSSR
  if (data.type == 'binary'){
    est.SSR.oe  <- SSR.out$est.SSR.oe
    est.PSSR.oe <- SSR.out$est.PSSR.oe
    est.SSR.pe  <- SSR.out$est.SSR.pe
    est.PSSR.pe <- SSR.out$est.PSSR.pe
  }
  message('...done')

  if (data.type == 'binary'){
    # hierarchical logistic regression model methods
    message('calculating reliability based on hierarchial logistic regression model...')
    HLGM.out <- suppressMessages(calcHLGMRel(df, model, entity, y, show.all, ctrPerf))
    est.HLGM.delta <- HLGM.out$est.HLGM.delta
    message('...done')

    # Beta-Binomial method
    message('calculating reliability based on Beta-Binomial method...')
    BB.out <- suppressMessages(calcBetaBin(df, model, entity, y, df.aggregate = FALSE, show.all=show.all, ctrPerf = ctrPerf))
    est.BB <- BB.out$est.BB
    message('...done')

    if(show.all == TRUE){
      est.HLGM.MC <- HLGM.out$est.HLGM.MC
      est.HLGM.latent <- HLGM.out$est.HLGM.latent
      est.HLGM.FE <- HLGM.out$est.HLGM.FE
      est.HLGM.RE <- HLGM.out$est.HLGM.RE
      est.BB.FE <- BB.out$est.BB.FE
      est.BB.RE <- BB.out$est.BB.RE
      est.BB.J <- BB.out$est.BB.J

      # anova
      message('calculating reliability based on one-way ANOVA method...')
      AOV.out <- suppressMessages(calcAOV(df, entity, y, ctrPerf))
      est.aov <- AOV.out$est.aov
      message('...done')

      # resampling IUR method from He et al 2019
      # message('calculating reliability based on resampling IUR method...')
      # RIUR.results <- calcResamplingIUR(df, model, entity, y, ctrPerf, ctrRel)
      # est.RIUR <- RIUR.results$IUR
      # est.RIUR <- NA
      # message('...done')
    }

    ##### compile output ####
    if (show.all == FALSE){
      rel.results <- data.frame(
        method = c('Permutation split-sample (O/E)', 'Hierarchical logit regression', 'Beta-Binomial'),
        between_var = c(NA,
                        median(HLGM.out$var.between),
                        median(BB.out$var.between)),
        within_var = c(NA,
                       median(HLGM.out$var.within),
                       median(BB.out$var.within)
        ),
        within_var_min = c(NA,
                           min(HLGM.out$var.within),
                           min(BB.out$var.within)
        ),
        within_var_max = c(NA,
                           max(HLGM.out$var.within),
                           max(BB.out$var.within)
        ),
        reliability = c(
          est.PSSR.oe,
          median(est.HLGM.delta),
          median(est.BB)
        ),
        reliability_mean = c(
          NA,
          mean(est.HLGM.delta),
          mean(est.BB)
        ),
        reliability_min = c(
          NA,
          min(est.HLGM.delta),
          min(est.BB)
        ),
        reliability_25p = c(
          NA,
          quantile(est.HLGM.delta, 0.25),
          quantile(est.BB, 0.25)
        ),
        reliability_75p = c(
          NA,
          quantile(est.HLGM.delta, 0.75),
          quantile(est.BB, 0.75)
        ),
        reliability_max = c(
          NA,
          max(est.HLGM.delta),
          max(est.BB)
        )
      )

      output <- list(
        call = cl,
        data.type = data.type,
        show.all = show.all,
        rel.results = rel.results,
        SSR.out = SSR.out,
        HLGM.out = HLGM.out,
        BB.out = BB.out
      )
    } else{
      rel.results <- data.frame(
        method = c('SSR', 'PSSR', 'SSR (O/E)', 'SSR (P/E)', 'PSSR (O/E)', 'PSSR (P/E)', 'ANOVA', 'Hier logit on latent scale', 'Hier logit w/ delta approx', 'Hier logit w/ Monte Carlo method', 'Hier logit w/ FE', 'Hier logit w/ RE', 'Beta-Binomial', 'Beta-Binomial w/ FE', 'Beta-Binomial w/ RE', 'Beta-Binomial w/ Jeffreys'),
        between_var = c(NA, NA, NA, NA, NA, NA,
                        AOV.out$var.b.aov,
                        HLGM.out$var.b.HLGM,
                        median(HLGM.out$var.b.HLGM.delta),
                        median(HLGM.out$var.b.MC),
                        median(HLGM.out$var.b.HLGM.delta),
                        median(HLGM.out$var.b.HLGM.delta),
                        BB.out$var.b.BB,
                        BB.out$var.b.BB,
                        BB.out$var.b.BB,
                        BB.out$var.b.BB
        ),
        within_var = c(NA, NA, NA, NA, NA, NA,
                       median(AOV.out$var.w.aov),
                       median(HLGM.out$var.w.latent),
                       median(HLGM.out$var.w.delta),
                       median(HLGM.out$var.w.MC),
                       median(HLGM.out$var.w.FE),
                       median(HLGM.out$var.w.RE),
                       median(BB.out$var.w.BB),
                       median(BB.out$var.w.FE),
                       median(BB.out$var.w.RE),
                       median(BB.out$var.w.J)
        ),
        within_var_min = c(NA, NA, NA, NA, NA, NA,
                           min(AOV.out$var.w.aov),
                           min(HLGM.out$var.w.latent),
                           min(HLGM.out$var.w.delta),
                           min(HLGM.out$var.w.MC),
                           min(HLGM.out$var.w.FE),
                           min(HLGM.out$var.w.RE),
                           min(BB.out$var.w.BB),
                           min(BB.out$var.w.FE),
                           min(BB.out$var.w.RE),
                           min(BB.out$var.w.J)
        ),
        within_var_max = c(NA, NA, NA, NA, NA, NA,
                           max(AOV.out$var.w.aov),
                           max(HLGM.out$var.w.latent),
                           max(HLGM.out$var.w.delta),
                           max(HLGM.out$var.w.MC),
                           max(HLGM.out$var.w.FE),
                           max(HLGM.out$var.w.RE),
                           max(BB.out$var.w.BB),
                           max(BB.out$var.w.FE),
                           max(BB.out$var.w.RE),
                           max(BB.out$var.w.J)
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
          median(est.HLGM.delta),
          median(est.HLGM.MC),
          median(est.HLGM.FE),
          median(est.HLGM.RE),
          median(est.BB),
          median(est.BB.FE),
          median(est.BB.RE),
          median(est.BB.J)
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
          mean(est.HLGM.delta),
          mean(est.HLGM.MC),
          mean(est.HLGM.FE),
          mean(est.HLGM.RE),
          mean(est.BB),
          mean(est.BB.FE),
          mean(est.BB.RE),
          mean(est.BB.J)
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
          min(est.HLGM.delta),
          min(est.HLGM.MC),
          min(est.HLGM.FE),
          min(est.HLGM.RE),
          min(est.BB),
          min(est.BB.FE),
          min(est.BB.RE),
          min(est.BB.J)
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
          quantile(est.HLGM.delta, 0.25),
          quantile(est.HLGM.MC, 0.25),
          quantile(est.HLGM.FE, 0.25),
          quantile(est.HLGM.RE, 0.25),
          quantile(est.BB, 0.25),
          quantile(est.BB.FE, 0.25),
          quantile(est.BB.RE, 0.25),
          quantile(est.BB.J, 0.25)
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
          quantile(est.HLGM.delta, 0.75),
          quantile(est.HLGM.MC, 0.75),
          quantile(est.HLGM.FE, 0.75),
          quantile(est.HLGM.RE, 0.75),
          quantile(est.BB, 0.75),
          quantile(est.BB.FE, 0.75),
          quantile(est.BB.RE, 0.75),
          quantile(est.BB.J, 0.75)
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
          max(est.HLGM.delta),
          max(est.HLGM.MC),
          max(est.HLGM.FE),
          max(est.HLGM.RE),
          max(est.BB),
          max(est.BB.FE),
          max(est.BB.RE),
          max(est.BB.J)
        )
      )

      output <- list(
        call = cl,
        data.type = data.type,
        show.all = show.all,
        rel.results = rel.results,
        SSR.out = SSR.out,
        AOV.out = AOV.out,
        HLGM.out = HLGM.out,
        BB.out = BB.out
      )
    }
  }

  if (data.type == 'continuous'){
    # anova
    message('calculating reliability based on one-way ANOVA method...')
    AOV.out <- calcAOV(df, entity, y, ctrPerf)
    est.aov <- AOV.out$est.aov
    message('...done')

    # hierarchical linear regression model method
    message('calculating reliability based on hierarchial linear regression model...')
    HLM.out <- calcHLMRel(df, model, entity, y, ctrPerf)
    est.HLM <- HLM.out$est.HLM
    message('...done')

    rel.results <- data.frame(
      method = c('Permutation split-sample', 'ANOVA', 'Hierarchical linear regression'),
      between_var = c(NA,
                      AOV.out$var.b.aov,
                      median(HLM.out$var.between)),
      within_var = c(NA,
                     median(AOV.out$var.w.aov),
                     median(HLM.out$var.within)
      ),
      within_var_min = c(NA,
                         min(AOV.out$var.w.aov),
                         min(HLM.out$var.within)
      ),
      within_var_max = c(NA,
                         max(AOV.out$var.w.aov),
                         max(HLM.out$var.within)
      ),
      reliability = c(
        est.PSSR,
        median(est.aov),
        median(est.HLM)
      ),
      reliability_mean = c(
        NA,
        mean(est.aov),
        mean(est.HLM)
      ),
      reliability_min = c(
        NA,
        min(est.aov),
        min(est.HLM)
      ),
      reliability_25p = c(
        NA,
        quantile(est.aov, 0.25),
        quantile(est.HLM, 0.25)
      ),
      reliability_75p = c(
        NA,
        quantile(est.aov, 0.75),
        quantile(est.HLM, 0.75)
      ),
      reliability_max = c(
        NA,
        max(est.aov),
        max(est.HLM)
      )
    )

    output <- list(
      call = cl,
      data.type = data.type,
      show.all = show.all,
      rel.results = rel.results,
      SSR.out = SSR.out,
      AOV.out = AOV.out,
      HLM.out = HLM.out
    )

  }

  return(output)
}
