#' Calculate model performance
#' @description
#' This function calculates risk model performance.
#' @param df observation-level data; if null, will use the dataframe from the model object
#' @param model model; if null, will use an unadjusted model
#' @param entity data column containing the accountable entity identifier
#' @param y data column containing the outcome variable
#' @param predictor.clean optional list of formatted names of predictors in the model
#' @param ctrPerf parameters to control performance measure calculation
#' @returns Estimated risk-standardized measure performance by accountable entity
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @importFrom stats aggregate predict
#' @importFrom lme4 glmer
#' @export

model_performance <- function(df, model, entity, y, predictor.clean = NA, ctrPerf = controlPerf()){
  alpha = ctrPerf$alpha
  z = qnorm(1 - alpha/2)

  # clean data
  df = cleanData(df, entity = entity, y = y, ctrPerf = ctrPerf)

  data.out <- calcDataSummary(df, model, entity, y, ctrPerf)
  df = data.out$df
  model = data.out$model
  fit = data.out$fit
  marg.p <- data.out$marg.p

  coef.estimates <- as.data.frame(summary(fit)$coef)
  coef.estimates$exp.estimate <- exp(coef.estimates$Estimate)
  coef.estimates$lb <-  exp(coef.estimates$Estimate - z * coef.estimates$`Std. Error`)
  coef.estimates$ub <-  exp(coef.estimates$Estimate + z * coef.estimates$`Std. Error`)

  model.results <- data.frame(
    predictor = row.names(coef.estimates)[-1],
    est = coef.estimates$exp.estimate[-1],
    lb = coef.estimates$lb[-1],
    ub = coef.estimates$ub[-1],
    p = coef.estimates$`Pr(>|z|)`[-1]
  )
  model.results$sig = as.factor(ifelse(model.results$p < alpha, 1, 0))
  model.results$rank <- rank(model.results$est, ties.method = 'random')
  model.results$predictor.clean <- model.results$predictor
  predictor.original = model.results$predictor
  if (is.na(predictor.clean)){predictor.clean = model.results$predictor}
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

  # discrimination
  fig.prediction <- ggplot(data = df, aes(x=predict, color = as.factor(y), fill = as.factor(y))) +
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

  # calibration plot
  deciles = quantile(df$predict, 1:10/10)
  df$decile <- NA
  for (i in 10:1){
    df$decile[df$predict <= deciles[i]] <- i
  }

  calibration.df <- data.frame(
    decile = 1:10,
    observed = aggregate(y ~ decile, data = df, mean)$y,
    predicted = aggregate(predict ~ decile, data = df, mean)$predict
  )
  fig.calibration <- ggplot(data = calibration.df, aes(x = predicted, y = observed)) +
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

  # calculate c statistic using wilcox
  p1 = df$predict[df$y==1]
  p0 = df$predict[df$y==0]
  n1 = length(p1)
  n0 = length(p0)
  pairs = n0 * n1
  wilcox.out <- wilcox.test(predict ~ y, data = df)
  c.statistic = max(wilcox.out$statistic / pairs, 1 - wilcox.out$statistic / pairs)

  results = list(df = df,
                 model = model,
                 fit = fit,
                 marg.p = marg.p,
                 c.statistic = c.statistic,
                 model.results = model.results,
                 calibration.df = calibration.df,
                 fig.estimates = fig.estimates,
                 fig.prediction = fig.prediction,
                 fig.calibration = fig.calibration)

  return(results)
}
