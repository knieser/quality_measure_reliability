#' Calculate reliability using split-sample method
#' @description
#' This function estimates reliability using the split-sample method.
#' @param df observation-level data; if null, will use the dataframe from the model object
#' @param model model; if null, will use an unadjusted model
#' @param entity data column containing the accountable entity identifier
#' @param y data column containing the outcome variable
#' @param data.type acceptable values are `binary` for 0/1 data and `continuous` for continuous data (default: `binary`)
#' @param ctrPerf parameters to control performance measure calculation
#' @param ctrRel parameters to control reliability estimation
#' @returns A list containing:
#' * `entity`: list of entities
#' * `n`: entity sample sizes
#' * `icc`: Spearman-Brown-adjusted intraclass correlation coefficients for each resample
#' * `icc.lb`: lower bound on confidence interval for Spearman-Brown-adjusted intraclass correlation coefficients for each resample
#' * `icc.ub`: upper bound on confidence interval for Spearman-Brown-adjusted intraclass correlation coefficients for each resample
#' * `est.SSR`: reliability estimate based on a single split
#' * `est.PSSR`: mean reliability estimate across resamples
#'
#' If a risk-adjustment model is included then, the outputted list will contain:
#' * `entity`: list of entities
#' * `n`: entity sample sizes
#' * `icc.oe`: Spearman-Brown-adjusted intraclass correlation coefficients for OE ratios for each resample
#' * `icc.oe.lb`: lower bound on confidence interval for Spearman-Brown-adjusted intraclass correlation coefficients for OE ratios for each resample
#' * `icc.oe.ub`: upper bound on confidence interval for Spearman-Brown-adjusted intraclass correlation coefficients for OE ratios for each resample
#' * `icc.pe`: Spearman-Brown-adjusted intraclass correlation coefficients for PE ratios for each resample
#' * `icc.pe.lb`: lower bound on confidence interval for Spearman-Brown-adjusted intraclass correlation coefficients for PE ratios for each resample
#' * `icc.pe.ub`: upper bound on confidence interval for Spearman-Brown-adjusted intraclass correlation coefficients for PE ratios for each resample
#' * `est.SSR.oe`: reliability estimate for OE ratio based on a single split
#' * `est.PSSR.oe`: mean reliability estimate for OE ratio across resamples
#' * `est.SSR.pe`: reliability estimate for PE ratio based on a single split
#' * `est.PSSR.pe`: mean reliability estimate for PE ratio across resamples
#'
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @references Nieser KJ, Harris AH. Comparing methods for assessing the reliability of health care quality measures. Statistics in Medicine. 2024 Oct 15;43(23):4575-94.
#' @examples
#' # Simulate data
#' df <- simulateData(n.entity = 50, n.obs = 100, mu = .2, r = .7)
#'
#' # Calculate reliability
#' out <- calcSSR(df = df, entity = 'entity', y = 'y')
#' out$est.PSSR
#'
#' # You can also plot the distribution of reliability estimates obtained from the permutation sampling.
#' hist(out$icc)
#' summary(out$icc)
#'
#' ## Example with risk-adjustment
#' # Simulate data
#' df1 <- simulateData(n.entity = 50, n.obs = 100, mu = .2, r = .6, beta1 = log(2))
#'
#' # Calculate reliability for both observed-to-expected and predicted-to-expected ratios.
#' out1 <- calcSSR(df = df1, model = 'y ~ x1 + (1 | entity)', entity = 'entity', y = 'y')
#' out1$est.PSSR.oe
#' out1$est.PSSR.pe
#'
#' # You can also plot the distribution of reliability estimates obtained from the permutation sampling.
#' hist(out1$icc.oe)
#' summary(out1$icc.oe)
#'
#'
#' @importFrom parallel makeCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar%
#' @importFrom psych ICC
#' @importFrom stats reshape
#' @export

calcSSR <- function(df = NULL, model = NULL, entity = 'entity', y = 'y', data.type = 'binary', ctrPerf = controlPerf(), ctrRel = controlRel()){
  if (is.null(df) & is.null(model)) stop ('Please provide either a dataframe or a model object')
  if (is.null(df)){df <- model@frame}

  cl <- match.call()
  n.resamples <- ctrRel$n.resamples
  n.cores     <- ctrRel$n.cores
  method      <- ctrRel$SSRmethod
  fn          <- ctrRel$fn

  data.out <- calcDataSummary(df, model, entity, y, data.type, ctrPerf)
  df <- data.out$df
  entities <- data.out$entities
  n  <- data.out$n
  n.entity = length(entities)

  cl <- parallel::makeCluster(n.cores)
  doParallel::registerDoParallel(cl)

  out <- foreach::foreach(s = 1:n.resamples, .combine = rbind, .packages = c('psych')) %dopar% {

    if (method=='permutation'){
      # randomly assign each record into either s=1 or s=2 for each entity
      df$s <- 1
      for (j in 1:n.entity){
        entity.df <- df[df$entity == entities[j], ]
        entity.df$s[sample(nrow(entity.df), nrow(entity.df)/2, replace = F)] <- 2
        df$s[df$entity == entities[j]] <- entity.df$s
        }
      df$s <- as.factor(df$s)

      if (data.type == 'binary' && !is.null(model)){
        # calculate performance by entity and split-half
        agg   <- aggregate(y ~ entity + s, data = df, sum)
        agg$obs <- agg$y
        agg$pred  <- aggregate(predict ~ entity + s, data = df, sum)$predict
        agg$exp   <- aggregate(expect ~ entity + s, data = df, sum)$expect
        agg$oe    <- agg$obs / agg$exp
        agg$pe    <- agg$pred / agg$exp
        entity.means.wide <- reshape(agg, idvar = "entity", timevar = "s", direction = "wide")

        # calculate ICCs using psych package
        ## OE
        aov.out.oe <- psych::ICC(entity.means.wide[,c('oe.1', 'oe.2')], lmer = F)
        aov.ICC.oe    = aov.out.oe$results$ICC
        aov.ICC.oe.lb = aov.out.oe$results$`lower bound`
        aov.ICC.oe.ub = aov.out.oe$results$`upper bound`
        icc.aov.oe    = c(aov.ICC.oe[4])
        icc.aov.oe.lb = c(aov.ICC.oe.lb[4])
        icc.aov.oe.ub = c(aov.ICC.oe.ub[4])

        ## PE
        aov.out.pe <- psych::ICC(entity.means.wide[,c('pe.1', 'pe.2')], lmer = F)
        aov.ICC.pe    = aov.out.pe$results$ICC
        aov.ICC.pe.lb = aov.out.pe$results$`lower bound`
        aov.ICC.pe.ub = aov.out.pe$results$`upper bound`
        icc.aov.pe    = c(aov.ICC.pe[4])
        icc.aov.pe.lb = c(aov.ICC.pe.lb[4])
        icc.aov.pe.ub = c(aov.ICC.pe.ub[4])

        icc.aov    = NA
        icc.aov.lb = NA
        icc.aov.ub = NA
        } else{
          # calculate performance by entity and split-half
          entity.means <- aggregate(y ~ entity + s, data = df, function(x) fn(x))
          entity.means.wide <- stats::reshape(entity.means, idvar = "entity", timevar = "s", direction = "wide")

          # calculate ICCs using psych package
          aov.out  <- psych::ICC(entity.means.wide[,-1], lmer = F)
          aov.ICC = aov.out$results$ICC
          aov.ICC.lb = aov.out$results$`lower bound`
          aov.ICC.ub = aov.out$results$`upper bound`

          icc.aov       = c(aov.ICC[4]) # Spearman-Brown correction
          icc.aov.lb    = c(aov.ICC.lb[4])
          icc.aov.ub    = c(aov.ICC.ub[4])
          icc.aov.oe    = NA
          icc.aov.oe.lb = NA
          icc.aov.oe.ub = NA
          icc.aov.pe    = NA
          icc.aov.pe.lb = NA
          icc.aov.pe.ub = NA
      }
  }

    if(method == 'bootstrap'){
      # resample data within each entity with replacement
      df.boots <- data.frame(matrix(ncol = (ncol(df) + 1), nrow = 0))
      names(df.boots) <- c(names(df), 's')

      for (j in 1:n.entity){
        entity.df <- df[df$entity == entities[j], ]
        entity.df.1 <- entity.df[sample(nrow(entity.df), nrow(entity.df), replace = T),]
        entity.df.1$s <- 1
        entity.df.2 <- entity.df[sample(nrow(entity.df), nrow(entity.df), replace = T),]
        entity.df.2$s <- 2
        entity.boot <- rbind(entity.df.1, entity.df.2)
        df.boots <- rbind(df.boots, entity.boot)
      }

      if (data.type == 'binary' && !is.null(model)){
        # calculate performance by entity and split-half
        agg   <- aggregate(y ~ entity + s, data = df.boots, sum)
        agg$obs <- agg$y
        agg$pred  <- aggregate(predict ~ entity + s, data = df.boots, sum)$predict
        agg$exp   <- aggregate(expect ~ entity + s, data = df.boots, sum)$expect
        agg$oe    <- agg$obs / agg$exp
        agg$pe    <- agg$pred / agg$exp
        entity.means.wide <- reshape(agg, idvar = "entity", timevar = "s", direction = "wide")

        # calculate ICCs using psych package
        ## OE
        aov.out.oe <- psych::ICC(entity.means.wide[,c('oe.1', 'oe.2')], lmer = F)
        aov.ICC.oe    = aov.out.oe$results$ICC
        aov.ICC.oe.lb = aov.out.oe$results$`lower bound`
        aov.ICC.oe.ub = aov.out.oe$results$`upper bound`
        icc.aov.oe    = c(aov.ICC.oe[4])
        icc.aov.oe.lb = c(aov.ICC.oe.lb[4])
        icc.aov.oe.ub = c(aov.ICC.oe.ub[4])

        ## PE
        aov.out.pe <- psych::ICC(entity.means.wide[,c('pe.1', 'pe.2')], lmer = F)
        aov.ICC.pe    = aov.out.pe$results$ICC
        aov.ICC.pe.lb = aov.out.pe$results$`lower bound`
        aov.ICC.pe.ub = aov.out.pe$results$`upper bound`
        icc.aov.pe    = c(aov.ICC.pe[4])
        icc.aov.pe.lb = c(aov.ICC.pe.lb[4])
        icc.aov.pe.ub = c(aov.ICC.pe.ub[4])

        icc.aov    = NA
        icc.aov.lb = NA
        icc.aov.ub = NA
      } else{
        entity.means <- aggregate(y ~ entity + s, data = df.boots, function(x) fn(x))
        entity.means.wide <- reshape(entity.means, idvar = "entity", timevar = "s", direction = "wide")

        # calculate ICCs using psych package
        aov.out  <- psych::ICC(entity.means.wide[,-1], lmer = F)
        aov.ICC = aov.out$results$ICC
        aov.ICC.lb = aov.out$results$`lower bound`
        aov.ICC.ub = aov.out$results$`upper bound`

        icc.aov = c(aov.ICC[1]) # no Spearman-Brown correction for bootstrap method
        icc.aov.lb = c(aov.ICC.lb[1])
        icc.aov.ub = c(aov.ICC.ub[1])

        icc.aov.oe    = NA
        icc.aov.oe.lb = NA
        icc.aov.oe.ub = NA
        icc.aov.pe    = NA
        icc.aov.pe.lb = NA
        icc.aov.pe.ub = NA
    }
  }

    list(icc.aov       = icc.aov,
         icc.aov.lb    = icc.aov.lb,
         icc.aov.ub    = icc.aov.ub,
         icc.aov.oe    = icc.aov.oe,
         icc.aov.oe.lb = icc.aov.oe.lb,
         icc.aov.oe.ub = icc.aov.oe.ub,
         icc.aov.pe    = icc.aov.pe,
         icc.aov.pe.lb = icc.aov.pe.lb,
         icc.aov.pe.ub = icc.aov.pe.ub
    )
  }

  parallel::stopCluster(cl)

  out <- as.data.frame(out)

  if (data.type == 'binary' && !is.null(model)){
    output = list(
      call = cl,
      entity = as.vector(entities),
      n = n,
      icc.oe = as.vector(unlist(out$icc.aov.oe)),
      icc.oe.lb = as.vector(unlist(out$icc.aov.oe.lb)),
      icc.oe.ub = as.vector(unlist(out$icc.aov.oe.ub)),
      icc.pe = as.vector(unlist(out$icc.aov.pe)),
      icc.pe.lb = as.vector(unlist(out$icc.aov.pe.lb)),
      icc.pe.ub = as.vector(unlist(out$icc.aov.pe.ub))
    )
    output$est.SSR.oe = output$icc.oe[1]
    output$est.PSSR.oe = mean(output$icc.oe)
    output$est.SSR.pe  <- output$icc.pe[1]
    output$est.PSSR.pe <- mean(output$icc.pe)
  } else {
    output = list(
      call = cl,
      entity = as.vector(entities),
      n = n,
      icc = as.vector(unlist(out$icc.aov)),
      icc.lb = as.vector(unlist(out$icc.aov.lb)),
      icc.ub = as.vector(unlist(out$icc.aov.ub))
     )
    output$est.SSR = output$icc[1]
    output$est.PSSR = mean(output$icc)
  }

  return(output)
}
