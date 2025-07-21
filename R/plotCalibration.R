#' Plot calibration curve for risk-adjustment model
#' @description
#' This function creates a plot of the model calibration curve
#' @param model.performance results from model_performance()
#' @param quantiles number of quantiles to bin data; default is 10.
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @importFrom ggplot2 ggplot
#' @export

plotCalibration <- function(model.performance = model.performance, quantiles = 10){
  data.type = model.performance$data.type
  if (data.type != 'binary') stop ('This function only works for binary outcome data.')

  df = model.performance$df
  q = quantile(df$predict, 1:quantiles/quantiles)
  df$quantile <- NA
  for (i in quantiles:1){
    df$quantile[df$predict <= q[i]] <- i
  }

  calibration.df <- data.frame(
    quantile = 1:quantiles,
    observed = aggregate(y ~ quantile, data = df, mean)$y,
    predicted = aggregate(predict ~ quantile, data = df, mean)$predict
  )

  fig.calibration <- ggplot(data = calibration.df, aes(x = predicted, y = observed)) +
    geom_point(size = 3) +
    geom_line(lwd = 1) +
    geom_abline(slope = 1, intercept = 0, lty = 'dashed', lwd = 1) +
    xlab('Predicted probability') +
    ylab('Observed rate') +
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
  fig.calibration
}
