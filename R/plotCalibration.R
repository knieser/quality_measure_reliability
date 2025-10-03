#' Plot calibration curve for risk-adjustment model
#' @description
#' This function creates a plot of the model calibration curve
#' @details
#' This function only works for binary outcome data.
#'
#' @param model.performance results from `model_performance()`
#' @param quantiles number of quantiles to bin data; default is 10.
#' @returns A ggplot figure
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @examples
#' # Simulate data
#' df <- simulateData(n.entity = 100, n.obs = 80, mu = 0.2, r = 0.6, beta1 = log(1.6))
#'
#' # Calculate risk-adjustment model performance
#' model.perf <- model_performance(df = df, model = 'y ~ x1 + (1|entity)')
#'
#' # Calibration plots
#' plotCalibration(model.perf)
#' plotCalibration(model.perf, quantiles = 5)
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_abline xlab ylab theme_classic theme element_line element_text unit element_blank
#' @export

plotCalibration <- function(model.performance, quantiles = 10){
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

  fig <- ggplot2::ggplot(data = calibration.df, ggplot2::aes(x = .data$predicted, y = .data$observed)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_line(lwd = 1) +
    ggplot2::geom_abline(slope = 1, intercept = 0, lty = 'dashed', lwd = 1) +
    ggplot2::xlab('Predicted probability') +
    ggplot2::ylab('Observed rate') +
    ggplot2::theme_classic() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(linewidth = 1),
      axis.text = ggplot2::element_text(size = 16),
      axis.ticks.length = ggplot2::unit(.25,"cm"),
      axis.title = ggplot2::element_text(size = 18, face = "bold")
    )
  fig
}
