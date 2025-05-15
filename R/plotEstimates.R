#' Plot estimates from regression model used for risk-adjustment
#' @description
#' This function creates a plot of model results
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @references None
#' @examples
#' # TBD
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
  fig <- ggplot2::ggplot(data = model.results, aes(x = est, y = predictor.clean, group = sig)) +
    geom_point(aes(color = sig), size = 3) +
    geom_errorbar(aes(xmin = lb, xmax = ub, color = sig),
                  width = 0.5,
                  linetype = 1) +
    scale_color_manual(values = c('black', 'red')) +
    geom_vline(xintercept = xline, lty = 2) +
    scale_x_continuous(trans = scale) +
    scale_y_discrete(limits = rev) +
    labs(x = xlab, y = 'Predictor') +
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
  fig
}
