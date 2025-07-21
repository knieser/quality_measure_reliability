#' Plot densities of predicted values by outcome group
#' @description
#' This function creates a plot of the distributions of predicted values by outcome group
#' @param model.performance results from model_performance()
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @importFrom ggplot2 ggplot
#' @export

plotPredictedDistribution <- function(model.performance = model.performance){
  data.type = model.performance$data.type
  if(data.type != 'binary') stop('This function only works for binary outcome data.')

  df = model.performance$df
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
  fig.prediction
}
