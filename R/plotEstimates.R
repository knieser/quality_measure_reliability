#' Plot estimates from regression model used for risk-adjustment
#' @description
#' This function creates a plot of model results
#' @param model.performance results from `model_performance()`
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @importFrom ggplot2 ggplot
#' @export


plotEstimates <- function(model.performance = model.performance){
  data.type = model.performance$data.type
  model.results = model.performance$model.results

  if (data.type == 'binary'){
    scale = 'log10'
    xlab = 'Adjusted OR'
    xline = 1
  }

  if (data.type == 'continuous'){
    scale = 'identity'
    xlab = 'Adjusted mean difference'
    xline = 0
  }
  fig <- ggplot2::ggplot(data = model.results, ggplot2::aes(x = est, y = predictor.clean, group = sig)) +
    ggplot2::geom_point(ggplot2::aes(color = sig), size = 3) +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = lb, xmax = ub, color = sig),
                  width = 0.5,
                  linetype = 1) +
    ggplot2::scale_color_manual(values = c('black', 'red')) +
    ggplot2::geom_vline(xintercept = xline, lty = 2) +
    ggplot2::scale_x_continuous(trans = scale) +
    ggplot2::scale_y_discrete(limits = rev) +
    ggplot2::labs(x = xlab, y = 'Predictor') +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 16, face ="bold"),
      axis.text = ggplot2::element_text(size = 16, color = 1),
      axis.text.y = ggplot2::element_text(hjust = 1),
      axis.ticks.length = ggplot2::unit(.25,"cm"),
      axis.title = ggplot2::element_text(size = 18, face = "bold"),
      strip.text = ggplot2::element_text(size = 18, face = "bold"),
      panel.grid.major.x = ggplot2::element_line(),
      panel.grid.minor.x = ggplot2::element_line(),
      legend.position = 'none'
    )
  fig
}
