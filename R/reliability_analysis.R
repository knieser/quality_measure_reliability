#' Calculate reliability
#' @description
#' This function calculates reliability using several methods.
#' @param df observation-level data; if null, will use the dataframe from the model object
#' @param model model; if null, will use an unadjusted model
#' @param entity data column containing the accountable entity identifier
#' @param y data column containing the outcome variable
#' @param ctrPerf parameters to control performance measure calculation
#' @param ctrRel parameters to control reliability estimation
#' @returns Estimated risk-standardized measure performance by accountable entity
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @importFrom stats aggregate predict
#' @importFrom lme4 glmer
#' @export

reliability_analysis <- function(df, model, entity = 'entity', y = 'y', ctrPerf = controlPerf(), ctrRel = controlRel()){

  # clean data
  df = cleanData(df = df, entity = entity, y = y, ctrPerf = ctrPerf)

  # calculate reliability
  rel.results <- calcReliability(df = df, model = model, entity = entity, y = y, ctrPerf = ctrPerf, ctrRel = ctrRel)

  # make plot to show distribution of reliability estimates
  message('calculating HLGM reliability estimates for plot...')
  HLGM.out <- calcHLGMRel(df = df, model = model, entity = entity, y = y, ctrPerf = ctrPerf)
  message('...done')
  message('calculating Beta-binomial reliability estimates for plot...')
  BB.out <- calcBetaBin(df = df, model = model, entity = entity, y = y, ctrPerf = ctrPerf)
  message('...done')

  rel.plot.df <- data.frame(
    method = rep(c('MLM, latent scale',
                   'MLM, delta approx.',
                   'MLM, FE',
                   'MLM, RE',
                   'Beta-binomial',
                   'Beta-binomial, FE',
                   'Beta-binomial, RE',
                   'Beta-binomial, Jeffreys'), each = length(HLGM.out$n)),
    est = c(HLGM.out$est.HLGM.latent, HLGM.out$est.HLGM.delta, HLGM.out$est.HLGM.FE, HLGM.out$est.HLGM.RE,
            BB.out$est.BB, BB.out$est.BB.FE, BB.out$est.BB.RE, BB.out$est.BB.J)
  )

  fig.rel <- ggplot(data = rel.plot.df, aes(est, factor(method))) +
    geom_boxplot(outlier.shape = NA) +
    geom_point(alpha = 0.6, position = position_jitter(height = 0.1, width = 0)) +
    xlab('Entity-specific reliability estimate') +
    ylab('Method') +
    theme_classic() +
    theme(
      panel.grid.major = element_line(linewidth = 1),
      plot.title = element_text(size = 16, face ="bold"),
      axis.text = element_text(size = 16),
      axis.ticks.length = unit(.25,"cm"),
      axis.title = element_text(size = 18, face = "bold"),
      legend.position = 'bottom'
    )


  # split-sample plot to visualize split-sample reliability
  message('making example plot of split-sample reliability estimates')

  data.out <- calcDataSummary(df, model, entity, y, ctrPerf)
  marg.p <- data.out$marg.p
  df.SSR <- data.out$df

  entities = unique(df.SSR$entity)
  n.entity = length(entities)

  df.SSR$s <- 1
  for (j in 1:n.entity){
    entity.df <- df.SSR[df.SSR$entity == entities[j], ]
    entity.df$s[sample(nrow(entity.df), nrow(entity.df)/2, replace = F)] <- 2
    df.SSR$s[df.SSR$entity == entities[j]] <- entity.df$s
  }
  df.SSR$s <- as.factor(df.SSR$s)

  entity.means <- aggregate(y ~ entity + s, data = df.SSR, mean)
  entity.means.wide <- reshape(entity.means, idvar = "entity", timevar = "s", direction = "wide")

  agg       <- aggregate(y ~ entity + s, data = df.SSR, sum)
  agg$obs   <- agg$y
  agg$pred  <- aggregate(predict ~ entity + s, data = df.SSR, sum)$predict
  agg$exp   <- aggregate(expect ~ entity + s, data = df.SSR, sum)$expect
  agg$oe    <- agg$obs / agg$exp * marg.p
  agg$pe    <- agg$pred / agg$exp * marg.p
  entity.means.wide.adj <- reshape(agg, idvar = "entity", timevar = "s", direction = "wide")

  plot.df.SSR <- data.frame(
    Method = rep(c('OE-standardized', 'PE-standardized'), each = nrow(entity.means.wide.adj)),
    t1 = c(entity.means.wide.adj$oe.1, entity.means.wide.adj$pe.1),
    t2 = c(entity.means.wide.adj$oe.2, entity.means.wide.adj$pe.2)
  )
  max = max(c(plot.df.SSR$t1, plot.df.SSR$t2))
  fig.SSR <- ggplot2::ggplot(data = plot.df.SSR, aes(x = t1, y = t2, group = Method)) +
    geom_point(aes(color = Method, shape = Method), size = 3) +
    geom_abline(slope = 1, lty = 'dashed') +
    coord_cartesian(xlim = c(0, max), ylim = c(0, max)) +
    scale_color_manual(values = c('darkgrey', 'red')) +
    xlab('Split 1') +
    ylab('Split 2') +
    theme_classic() +
    theme(
      plot.title = element_text(size = 16, face ="bold"),
      axis.text = element_text(size = 16),
      axis.ticks.length = unit(.25,"cm"),
      axis.title = element_text(size = 18, face = "bold"),
      legend.position = 'top',
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 18, face = 'bold')
    )

  results = list(df = df,
                 df.SSR = df.SSR,
                 rel.results = rel.results,
                 HLGM.out = HLGM.out,
                 BB.out = BB.out,
                 fig.rel = fig.rel,
                 fig.SSR = fig.SSR)

  return(results)
}
