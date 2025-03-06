#' Calculate measure performance and reliability
#' @description
#' This function calculates measure performance by accountable entity.
#' @param df dataframe; if null, will use the dataframe in the model object
#' @param model model; if null, will use an unadjusted model
#' @param entities list of accountable entities
#' @param entity variable to use as the accountable entity; default = "entity"
#' @param y variable to use as the outcome; default = "y"
#' @returns Estimated risk-standardized measure performance by accountable entity
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @importFrom stats aggregate predict
#' @importFrom lme4 glmer
#' @export

reliability_analysis <- function(df, model, entity, y, predictor.clean, ctrPerf, ctrRel, model.estimates.file, model.estimates.fig.file, prediction.fig.file, calibration.fig.file, rand.int.plot.file, corr.plot.file, comparison.rate.fig.file, rel.out.file, rel.dist.plot.file, SSR.plot.file){

  # clean data
  df = cleanData(df, entity = entity, y = y, ctrPerf = ctrPerf)

  # calculate measure performance
  message('calculating measure performance...')
  perf.out <- calcPerformance(df = df, model = model, entity = entity, y = y, ctrPerf = ctrPerf)
  df.perf <- perf.out$df
  fit <- perf.out$fit
  perf.results <- perf.out$perf.results
  marg.p = perf.out$marg.p
  message('...done')

  # model estimates
  results <- as.data.frame(summary(fit)$coef)
  results$exp.estimate <- exp(results$Estimate)
  results$lb <-  exp(results$Estimate - qnorm(0.975) * results$`Std. Error`)
  results$ub <-  exp(results$Estimate + qnorm(0.975) * results$`Std. Error`)

  model.results <- data.frame(
    predictor = row.names(results)[-1],
    est = results$exp.estimate[-1],
    lb = results$lb[-1],
    ub = results$ub[-1],
    p = results$`Pr(>|z|)`[-1]
  )
  model.results$sig = as.factor(ifelse(model.results$p < 0.05, 1, 0))
  write.csv(model.results, file = model.estimates.file)

  model.results$rank <- rank(model.results$est, ties.method = 'random')
  model.results$predictor.clean <- model.results$predictor
  predictor.original = model.results$predictor
  if (length(predictor.original) != length(predictor.clean)) stop('The length of predictor.clean does not match the number of predictors in the model.')
  for (k in 1:length(predictor.clean)){
	model.results$predictor.clean[model.results$predictor == predictor.original[k]] <- predictor.clean[k]
  }
  model.results$predictor.clean <- as.factor(model.results$predictor.clean)
  model.results$predictor.clean <- factor(model.results$predictor.clean, levels = model.results$predictor.clean[order(model.results$rank, decreasing = T)])

  # make plot of model results
  fig.estimates <- ggplot(data = model.results, aes(x = est, y = predictor.clean, group = sig)) +
    geom_point(aes(color = sig), size = 3) +
    geom_errorbar(aes(xmin = lb, xmax = ub, color = sig),
                  width = 0.5,
                  linetype = 1) +
    scale_color_manual(values = c('black', 'red')) +
    geom_vline(xintercept = 1, lty = 2) +
    scale_x_continuous(trans = 'log10') +
    scale_y_discrete(limits = rev) +
    labs(x = 'Adjusted OR', y = 'Predictor') +
    theme_classic() +
    theme(
      plot.title = element_text(size = 16, face ="bold"),
      axis.text = element_text(size = 16, color = 1),
      axis.text.y = element_text(hjust = 1),
      axis.ticks.length = unit(.25,"cm"),
      axis.title = element_text(size = 18, face = "bold"),
      strip.text = element_text(size = 18, face = "bold"),
      panel.grid.major.x = element_line(),
      panel.grid.minor.x = element_line(),
      legend.position = 'none'
    )
  ggsave(filename = model.estimates.fig.file, fig.estimates, width = 10, height = 10, units = 'in')

  # discrimination
  prediction_plot <- ggplot(data = df.perf, aes(x=predict, color = as.factor(y), fill = as.factor(y))) +
    geom_density(alpha = .3) +
    scale_color_manual('Observed outcome', values = c('black', 'red')) +
    scale_fill_manual('Observed outcome', values = c('black', 'red')) +
    xlab('Predicted probability') +
    ylab('Density') +
    theme_classic() +
    theme(
      axis.text = element_text(size = 16),
      axis.ticks.length = unit(.25, 'cm'),
      axis.title = element_text(size = 18, face = 'bold'),
      legend.position = 'top',
      legend.title = element_text(size = 18, face = 'bold'),
      legend.text = element_text(size = 18)
    )
  ggsave(filename = prediction.fig.file, prediction_plot, width = 10, height = 10, units = 'in')


  # calibration plot
  deciles = quantile(df.perf$predict, 1:10/10)
  df.perf$decile <- NA
  for (i in 10:1){
  df.perf$decile[df.perf$predict <= deciles[i]] <- i
  }

  calibration.plot.df <- data.frame(
    decile = 1:10,
    observed = aggregate(y ~ decile, data = df.perf, mean)$y,
    predicted = aggregate(predict ~ decile, data = df.perf, mean)$predict
  )
  calibration.fig <- ggplot(data = calibration.plot.df, aes(x = predicted, y = observed)) +
    geom_point(size = 3) +
    geom_line(lwd = 1) +
    geom_abline(slope = 1, intercept = 0, lty = 'dashed', lwd = 1) +
    xlab('Predicted probability') +
    ylab('Observed complication rate') +
    theme_classic() +
    theme(
      panel.grid.major = element_line(linewidth = 1),
      axis.text = element_text(size = 16),
      axis.ticks.length = unit(.25,"cm"),
      axis.title = element_text(size = 18, face = "bold"),
      strip.text = element_text(size = 18, face = "bold"),
      legend.text = element_text(size = 16),
      legend.title = element_blank(),
      legend.position = 'bottom'
    )
  ggsave(filename = calibration.fig.file, calibration.fig, width = 10, height = 10, units = 'in')


  # plot of ORs comparing each facility with the average facility
  fig.rand.int <- ggplot(data = perf.results, aes(x = intercept.OR, y = entities)) +
    geom_point(aes(color = intercept.sig), size = 2) +
    geom_errorbar(aes(xmin = intercept.OR.lwr, xmax = intercept.OR.upr), width = 0.1, position = position_dodge(width = .7)) +
    scale_color_manual(values = c('black', 'red')) +
    scale_x_continuous(trans = 'log10') +
    geom_vline(xintercept = 1, lty = 2) +
    xlab('OR') +
    ylab('Facility') +
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
  ggsave(filename = rand.int.plot.file, fig.rand.int, width = 8, height = 8, units = 'in')

  # plot of risk-standardized rates against the entity random intercepts
  plot.df.corr <- data.frame(
    Method = rep(c('OE-standardized', 'PE-standardized'), each = nrow(perf.results)),
    rand.int = rep(m.re.intercept$est.exp, 2),
    standardized.rates = c(perf.results$rs.oe, perf.results$rs.pe)
  )

  corrfig <- ggplot2::ggplot(data = plot.df.corr, aes(x = rand.int, y = standardized.rates, group = Method)) +
    geom_point(aes(color = Method, shape = Method), size = 3) +
    scale_color_manual(values = c('darkgrey', 'red')) +
    xlab('Entity-specific random intercepts') +
    ylab('Risk-standardized rates') +
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
  ggsave(filename = corr.plot.file, corrfig, width = 8, height = 8, units = 'in')

  # risk-adjustment method comparison plot
  plot.df.p <- data.frame(
    method = rep(c('A. Unadjusted', 'B. OE-standardized', 'C. PE-standardized', 'D. Direct standardized'), each = nrow(perf.results)),
    p = c(perf.results$p, perf.results$rs.oe, perf.results$rs.pe, perf.results$rs.direct),
    lwr = c(perf.results$p.lwr, perf.results$rs.oe.lwr, perf.results$rs.pe.lwr, perf.results$rs.direct.lwr),
    upr = c(perf.results$p.upr, perf.results$rs.oe.upr, perf.results$rs.pe.upr, perf.results$rs.direct.upr),
    rank = rep(perf.results$rank.p, 4),
    rank.oe = rep(perf.results$rank.oe, 4),
    rank.pe = rep(perf.results$rank.pe, 4)
  )

  fig <- ggplot2::ggplot(data = plot.df.p, aes(x = rank, y = p, group = method)) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.1) +
    geom_hline(yintercept = marg.p, col = 'red', lty = 'dashed', size = 1.2, alpha = 0.7) +
    xlab('Facility rank') +
    ylab('Complication rate') +
    facet_wrap( ~ method, nrow = 2) +
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
  ggsave(filename = comparison.rate.fig.file, fig, width = 10, height = 8, units = 'in')

  # calculate reliability
  rel.results <- calcReliability(df = df, model = model, entity = entity, y = y, ctrPerf = ctrPerf, ctrRel = ctrRel)
  write.csv(rel.results, rel.out.file, row.names=F)

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
    est = c(HLGM.out$est.HLGM.logistic, HLGM.out$est.HLGM.model, HLGM.out$est.HLGM.FE.model, HLGM.out$est.HLGM.RE.model,
            BB.out$est.BB, BB.out$est.BB.FE, BB.out$est.BB.RE, BB.out$est.BB.J)
  )

  rel.fig <- ggplot(data = rel.plot.df, aes(est, factor(method))) +
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
  ggsave(filename = rel.dist.plot.file, rel.fig, width = 10, height = 8, units = 'in')

  # split-sample plot to visualize split-sample reliability
  message('making example plot of split-sample reliability estimates')

  data.out <- calcDataSummary(df, model, entity, y, ctrPerf)
  df.SSR <- data.out$df

  entities = unique(df.SSR$entity)
  n.entity = length(entities)

  # randomly assign each record into either s=1 or s=2 for each entity
  df.SSR$s <- 1
  for (j in 1:n.entity){
    entity.df <- df.SSR[df.SSR$entity == entities[j], ]
    entity.df$s[sample(nrow(entity.df), nrow(entity.df)/2, replace = F)] <- 2
    df.SSR$s[df.SSR$entity == entities[j]] <- entity.df$s
  }
  df.SSR$s <- as.factor(df.SSR$s)

  # calculate performance by entity and split-half
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
  SSRfig <- ggplot2::ggplot(data = plot.df.SSR, aes(x = t1, y = t2, group = Method)) +
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
  SSRfig
  ggsave(filename = SSR.plot.file, SSRfig, width = 8, height = 8, units = 'in')
  message('...done')

  results = list(df = df,
                 df.perf = df.perf,
                 df.SSR = df.SSR,
                 fit = fit,
                 marg.p = marg.p,
                 perf.out = perf.out,
                 rel.results = rel.results,
                 HLGM.out = HLGM.out,
                 BB.out = BB.out)

  return(results)
}
