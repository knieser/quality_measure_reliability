#' Calculate reliability of quality measure performance
#' @description
#' This function calculates several estimates of quality measure performance.
#' @param df observation-level data; if null, will use the dataframe from the model object
#' @param model model; if null, will use an unadjusted model
#' @param entity data column containing the accountable entity identifier
#' @param y data column containing the outcome variable
#' @param data.type acceptable values are `binary` for 0/1 data and `continuous` for continuous data (default: `binary`)
#' @param show.all logical indicator for whether full list of reliability method estimates should be calculated (default: `FALSE`)
#' @param ctrPerf parameters to control performance measure calculation
#' @param ctrRel parameters to control reliability estimation
#' @return A list with reliability estimates. `rel.results` is a dataframe summarizing estimates from the various methods. Output from each method's respective function is also included.
#' More details on output from each method can be found within the help documentation for the respective function for that method. For example, see [calcSSR()] for more detail on `SSR.out`.
#' @seealso [calcAOV()], [calcBetaBin()], [calcHLGMRel()], [calcHLMRel()], [calcResamplingIUR()], [calcSSR()]
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @references Nieser KJ, Harris AH. Comparing methods for assessing the reliability of health care quality measures. Statistics in Medicine. 2024 Oct 15;43(23):4575-94.
#' @examples
#' ### Simulate data with binary outcome
#' df <- simulateData(n.entity = 50, n.obs = 100, mu = .2, r = .7)
#'
#' # Calculate reliability
#' out <- calcReliability(df = df, entity = 'entity', y = 'y')
#'
#' # Plot estimates
#' plotReliability(out)
#'
#'
#' # Calculate reliability with expanded set of methods
#' out1 <- calcReliability(df = df, entity = 'entity', y = 'y', show.all = T)
#'
#' # Plot estimates for expanded set of methods
#' plotReliability(out1)
#'
#'
#' ### This function also works with continuous outcome data
#' # simulate data from multilevel normal distribution
#' df.c <- simulateData(n.entity = 50, n.obs = 100, mu = 25, r = .6, data.type = 'normal')
#'
#' # calculate reliability
#' out.c <- calcReliability(df = df, entity = 'entity', y = 'y', data.type = 'continuous')
#' plotReliability(out.c)
#'
#'
#' @importFrom stats quantile
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
  SSR.out <- suppressMessages(calcSSR(df = df, model = model, entity = entity, y = y, data.type = data.type, ctrPerf = ctrPerf, ctrRel = ctrRel))

  if (data.type == 'binary' && !is.null(model)){
    est.SSR  <- SSR.out$est.SSR.oe
    est.PSSR <- SSR.out$est.PSSR.oe
  }

  if (data.type == 'binary' && is.null(model)){
    est.SSR  <- SSR.out$est.SSR
    est.PSSR <- SSR.out$est.PSSR
  }

  if (data.type == 'continuous'){
    est.SSR <- SSR.out$est.SSR
    est.PSSR <- SSR.out$est.PSSR
  }
  message('...done')

  if (data.type == 'binary'){
    # hierarchical logistic regression model methods
    message('calculating reliability based on hierarchial logistic regression model...')
    HLGM.out <- suppressMessages(calcHLGMRel(df = df, model = model, entity = entity, y = y, show.all = show.all, ctrPerf = ctrPerf, ctrRel = ctrRel))
    est.HLGM.delta <- HLGM.out$est.HLGM.delta
    message('...done')

    # Beta-Binomial method
    message('calculating reliability based on Beta-Binomial method...')
    BB.out <- suppressMessages(calcBetaBin(df = df, model = model, entity = entity, y = y, df.aggregate = FALSE, show.all = show.all, ctrPerf = ctrPerf))
    est.BB <- BB.out$est.BB
    message('...done')

    if(show.all == TRUE){
      est.HLGM.latent <- HLGM.out$est.HLGM.latent
      est.HLGM.MC <- HLGM.out$est.HLGM.MC
      est.HLGM.FE <- HLGM.out$est.HLGM.FE
      est.HLGM.RE <- HLGM.out$est.HLGM.RE
      est.BB.FE <- BB.out$est.BB.FE
      est.BB.RE <- BB.out$est.BB.RE
      est.BB.J <- BB.out$est.BB.J

      # anova
      message('calculating reliability based on one-way ANOVA method...')
      AOV.out <- suppressMessages(calcAOV(df = df, entity = entity, y = y, ctrPerf = ctrPerf))
      est.aov <- AOV.out$est.aov
      message('...done')

      # resampling IUR method from He et al 2019
      message('calculating reliability based on resampling IUR method...')
      RIUR.out <- calcResamplingIUR(df = df, model = model, entity = entity, y = y, ctrPerf = ctrPerf, ctrRel = ctrRel)
      est.RIUR <- RIUR.out$IUR
      message('...done')
    }

    ##### compile output ####
    if (show.all == FALSE){
      rel.results <- data.frame(
        method = c('Permutation split-sample', 'Hierarchical logit regression', 'Beta-Binomial'),
        between_var = c(NA,
                        median(HLGM.out$var.b),
                        median(BB.out$var.b)),
        within_var = c(NA,
                       median(HLGM.out$var.w),
                       median(BB.out$var.w)
        ),
        within_var_min = c(NA,
                           min(HLGM.out$var.w),
                           min(BB.out$var.w)
        ),
        within_var_max = c(NA,
                           max(HLGM.out$var.w),
                           max(BB.out$var.w)
        ),
        reliability = c(
          est.PSSR,
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
          stats::quantile(est.HLGM.delta, 0.25),
          stats::quantile(est.BB, 0.25)
        ),
        reliability_75p = c(
          NA,
          stats::quantile(est.HLGM.delta, 0.75),
          stats::quantile(est.BB, 0.75)
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
        method = c('SSR:Single', 'SSR:Permutation', 'ANOVA', 'HLGM:latent scale', 'HLGM:delta approx', 'HLGM:Monte Carlo approx', 'HLGM:use FE', 'HLGM:use RE', 'Beta-Binomial', 'Beta-Binomial:use FE', 'Beta-Binomial:use RE', 'Beta-Binomial:use Jeffreys', 'Resampling IUR'),
        between_var = c(NA, NA,
                        AOV.out$var.b.aov,
                        HLGM.out$var.b.HLGM.latent,
                        median(HLGM.out$var.b.HLGM.delta),
                        median(HLGM.out$var.b.MC),
                        median(HLGM.out$var.b.HLGM.delta),
                        median(HLGM.out$var.b.HLGM.delta),
                        BB.out$var.b.BB,
                        BB.out$var.b.BB,
                        BB.out$var.b.BB,
                        BB.out$var.b.BB,
                        NA
        ),
        within_var = c(NA, NA,
                       median(AOV.out$var.w.aov),
                       median(HLGM.out$var.w.latent),
                       median(HLGM.out$var.w.delta),
                       median(HLGM.out$var.w.MC),
                       median(HLGM.out$var.w.FE),
                       median(HLGM.out$var.w.RE),
                       median(BB.out$var.w.BB),
                       median(BB.out$var.w.FE),
                       median(BB.out$var.w.RE),
                       median(BB.out$var.w.J),
                       NA
        ),
        within_var_min = c(NA, NA,
                           min(AOV.out$var.w.aov),
                           min(HLGM.out$var.w.latent),
                           min(HLGM.out$var.w.delta),
                           min(HLGM.out$var.w.MC),
                           min(HLGM.out$var.w.FE),
                           min(HLGM.out$var.w.RE),
                           min(BB.out$var.w.BB),
                           min(BB.out$var.w.FE),
                           min(BB.out$var.w.RE),
                           min(BB.out$var.w.J),
                           NA
        ),
        within_var_max = c(NA, NA,
                           max(AOV.out$var.w.aov),
                           max(HLGM.out$var.w.latent),
                           max(HLGM.out$var.w.delta),
                           max(HLGM.out$var.w.MC),
                           max(HLGM.out$var.w.FE),
                           max(HLGM.out$var.w.RE),
                           max(BB.out$var.w.BB),
                           max(BB.out$var.w.FE),
                           max(BB.out$var.w.RE),
                           max(BB.out$var.w.J),
                           NA
        ),
        reliability = c(
          est.SSR,
          est.PSSR,
          median(est.aov),
          median(est.HLGM.latent),
          median(est.HLGM.delta),
          median(est.HLGM.MC),
          median(est.HLGM.FE),
          median(est.HLGM.RE),
          median(est.BB),
          median(est.BB.FE),
          median(est.BB.RE),
          median(est.BB.J),
          est.RIUR
        ),
        reliability_mean = c(
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
          mean(est.BB.J),
          NA
        ),
        reliability_min = c(
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
          min(est.BB.J),
          NA
        ),
        reliability_25p = c(
          NA,
          NA,
          stats::quantile(est.aov, 0.25),
          stats::quantile(est.HLGM.latent, 0.25),
          stats::quantile(est.HLGM.delta, 0.25),
          stats::quantile(est.HLGM.MC, 0.25),
          stats::quantile(est.HLGM.FE, 0.25),
          stats::quantile(est.HLGM.RE, 0.25),
          stats::quantile(est.BB, 0.25),
          stats::quantile(est.BB.FE, 0.25),
          stats::quantile(est.BB.RE, 0.25),
          stats::quantile(est.BB.J, 0.25),
          NA
        ),
        reliability_75p = c(
          NA,
          NA,
          stats::quantile(est.aov, 0.75),
          stats::quantile(est.HLGM.latent, 0.75),
          stats::quantile(est.HLGM.delta, 0.75),
          stats::quantile(est.HLGM.MC, 0.75),
          stats::quantile(est.HLGM.FE, 0.75),
          stats::quantile(est.HLGM.RE, 0.75),
          stats::quantile(est.BB, 0.75),
          stats::quantile(est.BB.FE, 0.75),
          stats::quantile(est.BB.RE, 0.75),
          stats::quantile(est.BB.J, 0.75),
          NA
        ),
        reliability_max = c(
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
          max(est.BB.J),
          NA
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
        BB.out = BB.out,
        RIUR.out = RIUR.out
      )
    }
  }

  if (data.type == 'continuous'){

    if (show.all == TRUE){message('Results are the same as when show.all = FALSE')}

    # anova
    message('calculating reliability based on one-way ANOVA method...')
    AOV.out <- suppressMessages(calcAOV(df = df, entity = entity, y = y, ctrPerf = ctrPerf))
    est.aov <- AOV.out$est.aov
    message('...done')

    # hierarchical linear regression model method
    message('calculating reliability based on hierarchial linear regression model...')
    HLM.out <- calcHLMRel(df = df, model = model, entity = entity, y = y, ctrPerf = ctrPerf)
    est.HLM <- HLM.out$est.HLM
    message('...done')

    rel.results <- data.frame(
      method = c('Permutation split-sample', 'ANOVA', 'Hierarchical linear regression'),
      between_var = c(NA,
                      AOV.out$var.b.aov,
                      median(HLM.out$var.b)),
      within_var = c(NA,
                     median(AOV.out$var.w.aov),
                     median(HLM.out$var.w)
      ),
      within_var_min = c(NA,
                         min(AOV.out$var.w.aov),
                         min(HLM.out$var.w)
      ),
      within_var_max = c(NA,
                         max(AOV.out$var.w.aov),
                         max(HLM.out$var.w)
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
        stats::quantile(est.aov, 0.25),
        stats::quantile(est.HLM, 0.25)
      ),
      reliability_75p = c(
        NA,
        stats::quantile(est.aov, 0.75),
        stats::quantile(est.HLM, 0.75)
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
