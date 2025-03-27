#' Plot estimates from regression model used for risk-adjustment
#' @description
#' This function creates a plot of model results
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @references None
#' @examples
#' # TBD
#' @importFrom ggplot2 ggplot
#' @export


plotEstimates <- function(model.results = model.results){
fig <- ggplot2::ggplot(data = model.results, aes(x = est, y = predictor.clean, group = sig)) +
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
fig
}
