#' Calculate measure performance
#' @description
#' This function calculates measure performance by accountable entity.
#' @param df dataframe; if null, will use the dataframe in the model object
#' @param model model; if null, will use an unadjusted model (NOTE: currently, Beta-Binomial reliability estimates do not take risk-adjustment into account.)
#' @param entity data column containing the accountable entity identifier
#' @param y data column containing the outcome variable
#' @param ctrPerf parameters to control performance measure calculation
#' @returns Estimated risk-standardized measure performance by accountable entity
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @importFrom stats aggregate predict
#' @importFrom lme4 glmer
#' @export

profiling_analysis <- function(df, model = NULL, entity = 'entity', y = 'y', ctrPerf = controlPerf()){

  # clean data
  df = cleanData(df, entity = entity, y = y, ctrPerf = ctrPerf)

  # calculate measure performance
  message('calculating measure performance...')
  perf.out <- calcPerformance(df = df, model = model, entity = entity, y = y, ctrPerf = ctrPerf)
  df.perf <- perf.out$df
  fit <- perf.out$fit
  perf.results <- perf.out$perf.results
  marg.p = perf.out$marg.p
  rate.summary = rbind(summary(perf.results$p),
                       summary(perf.results$rs.oe),
                       summary(perf.results$rs.pe),
                       summary(perf.results$rs.direct)
                       )
  perf.summary = cbind(method = c('Unadjusted', 'OE_standardized', 'PE_standardized', 'Direct_standardized'),
                       rate.summary)
  perf.summary = as.data.frame(perf.summary)
  category.table = table(perf.results$category.oe, perf.results$category.pe)
  corr.oe.randint = cor(perf.results$intercept.OR, perf.results$oe)
  corr.pe.randint = cor(perf.results$intercept.OR, perf.results$pe)
  icc.oe.randint = psych::ICC(perf.results[,c('intercept.OR', 'oe')])
  icc.pe.randint = psych::ICC(perf.results[,c('intercept.OR', 'pe')])
  message('...done')

  message('making figures...')
  # plot of ORs comparing each facility with the average facility
  fig.rand.int <- ggplot(data = perf.results, aes(x = intercept.OR, y = entities)) +
    geom_point(aes(color = intercept.sig), size = 2) +
    geom_errorbar(aes(xmin = intercept.OR.lwr, xmax = intercept.OR.upr), width = 0.1, position = position_dodge(width = .7)) +
    scale_color_manual(values = c('black', 'red')) +
    scale_x_continuous(trans = 'log10') +
    geom_vline(xintercept = 1, lty = 2) +
    xlab('OR') +
    ylab('Entity') +
    theme_classic() +
    theme(
      plot.title = element_text(size = 16, face ="bold"),
      axis.text = element_text(size = 16),
      axis.text.y = element_blank(),
      axis.ticks.length.y = unit(0,'cm'),
      axis.ticks.length.x = unit(.25, 'cm'),
      axis.title = element_text(size = 18, face = "bold"),
      axis.text.x = element_text(angle = 55, vjust = 0.7),
      strip.text = element_text(size = 18, face = "bold"),
      legend.position = 'none'
    )

  # plot of standardization ratios against the entity random intercepts
  plot.df.corr <- data.frame(
    Method = rep(c('OE', 'PE'), each = nrow(perf.results)),
    rand.int = rep(perf.results$intercept.OR, 2),
    std.ratio = c(perf.results$oe, perf.results$pe)
  )

  fig.corr <- ggplot2::ggplot(data = plot.df.corr, aes(x = rand.int, y = std.ratio, group = Method)) +
    geom_point(aes(color = Method, shape = Method), size = 3) +
    stat_smooth(method = 'lm', formula = y ~ x, geom = 'smooth', se = F, aes(color = Method), lty = 'dashed') +
    scale_color_manual(values = c('darkgrey', 'red')) +
    xlab('Entity-specific random intercepts') +
    ylab('Standardization ratio') +
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

  # plot of measure performance across different risk-adjustment methods
  plot.df.p <- data.frame(
    method = rep(c('A. Unadjusted', 'B. OE-standardized', 'C. PE-standardized'), each = nrow(perf.results)),
    p = c(perf.results$p, perf.results$rs.oe, perf.results$rs.pe),
    lwr = c(perf.results$p.lwr, perf.results$rs.oe.lwr, perf.results$rs.pe.lwr),
    upr = c(perf.results$p.upr, perf.results$rs.oe.upr, perf.results$rs.pe.upr),
    rank = rep(perf.results$rank.p, 3),
    rank.oe = rep(perf.results$rank.oe, 3),
    rank.pe = rep(perf.results$rank.pe, 3)
  )

  fig.perf <- ggplot2::ggplot(data = plot.df.p, aes(x = rank, y = p, group = method)) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.1) +
    geom_hline(yintercept = marg.p, col = 'red', lty = 'dashed', size = 1.2, alpha = 0.7) +
    xlab('Entity rank') +
    ylab('Measure performance') +
    facet_wrap( ~ method, nrow = 1) +
    theme_classic() +
    theme(
      plot.title = element_text(size = 16, face ="bold"),
      axis.text = element_text(size = 16),
      axis.ticks.length = unit(.25,"cm"),
      axis.title = element_text(size = 18, face = "bold"),
      strip.text = element_text(size = 18, face = "bold"),
      legend.text = element_text(size = 16),
      legend.title = element_blank(),
      legend.position = 'bottom'
    )
  message('...done')

  results = list(df = df.perf,
                 model = model,
                 fit = fit,
                 marg.p = marg.p,
                 perf.results = perf.results,
                 perf.summary = perf.summary,
                 category.table = category.table,
                 corr.oe.randint = corr.oe.randint,
                 corr.pe.randint = corr.pe.randint,
                 icc.oe.randint = icc.oe.randint,
                 icc.pe.randint = icc.pe.randint,
                 fig.rand.int = fig.rand.int,
                 fig.corr = fig.corr,
                 fig.perf = fig.perf
                )

  return(results)
}
