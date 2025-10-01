#' Plot densities of predicted values by outcome group
#' @description
#' This function creates a plot of the distributions of predicted values by outcome group
#' @param model.performance results from `model_performance()`
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @importFrom ggplot2 ggplot
#' @export

plotPredictedDistribution <- function(model.performance){
  data.type = model.performance$data.type
  if(data.type != 'binary') stop('This function only works for binary outcome data.')
  df = model.performance$df
  df$y <- as.factor(df$y)

  fig.prediction <- ggplot2::ggplot(data = df, ggplot2::aes(x = predict, color = .data$y, fill = .data$y)) +
    ggplot2::geom_density(alpha = .3) +
    ggplot2::scale_color_manual('Observed outcome', values = c('black', 'red')) +
    ggplot2::scale_fill_manual('Observed outcome', values = c('black', 'red')) +
    ggplot2::xlab('Predicted probability') +
    ggplot2::ylab('Density') +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 16),
      axis.ticks.length = ggplot2::unit(.25, 'cm'),
      axis.title = ggplot2::element_text(size = 18, face = 'bold'),
      legend.position = 'top',
      legend.title = ggplot2::element_text(size = 18, face = 'bold'),
      legend.text = ggplot2::element_text(size = 18)
    )
  fig.prediction
}
