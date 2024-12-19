#' Plot measure performance across accountable entities
#' @description
#' This function creates a plot of measure performance across accountable entities, using the perf.results dataframe from calcPerformance() output.
#' @param df perf.results dataframe from calcPerformance() output
#' @param plot.y 'p' plots the unadjusted performance, 'oe' plots the observed-to-expected ratio based on the risk-adjustment model
#' @returns A ggplot figure
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @references None
#' @examples
#' # TBD
#' @importFrom ggplot2 ggplot
#' @export

plotPerformance <- function(df, plot.y = 'p'){
  if (plot.y == 'p') {
    fig <- ggplot2::ggplot(data = df, aes(x = rank, y = p)) +
      geom_point(color = 'Red') +
      geom_errorbar(aes(ymin = p.lwr, ymax = p.upr), width = 0.1) +
      scale_y_continuous(breaks = seq(0, 1, by = .1)) +
      xlab('Rank') +
      ylab('Measure performance') +
      theme_classic() +
      theme(
        plot.title = element_text(size = 16, face ="bold"),
        axis.text = element_text(size = 16),
        axis.ticks.length = unit(.25,"cm"),
        axis.title = element_text(size = 18, face = "bold"),
        strip.text = element_text(size = 18, face = "bold"),
        legend.position = 'none'
      )
  } else if (plot.y == 'oe') {
    fig <- ggplot(data = df, aes(x = rank.oe, y = oe)) +
      geom_point(color = 'Red') +
      geom_errorbar(aes(ymin = oe.boot.lwr, ymax = oe.boot.upr), width = 0.1) +
      xlab('Rank') +
      ylab('O/E ratio') +
      theme_classic() +
      theme(
        plot.title = element_text(size = 16, face ="bold"),
        axis.text = element_text(size = 16),
        axis.ticks.length = unit(.25,"cm"),
        axis.title = element_text(size = 18, face = "bold"),
        strip.text = element_text(size = 18, face = "bold"),
        legend.position = 'none'
      )
  }
return(fig)
}
