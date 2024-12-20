#' Plot measure performance across accountable entities
#' @description
#' This function creates a plot of measure performance across accountable entities, using the perf.results dataframe from calcPerformance() output.
#' @param df perf.results dataframe from calcPerformance() output
#' @param plot.y 'p' plots the unadjusted performance, 'oe' plots the observed-to-expected ratio, 'rs' plots the risk-standardized rate, and 'pe' plots the predicted-to-expected ratio'
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @references None
#' @examples
#' # TBD
#' @importFrom ggplot2 ggplot
#' @export

plotPerformance <- function(df, plot.y = 'p'){
  if (plot.y == 'p') {
    df$y = df$p
    df$rank = df$rank
    df$lwr = df$p.lwr
    df$upr = df$p.upr
    ylab = 'Measure performance'
  } else if (plot.y == 'oe') {
    df$y = df$oe
    df$rank = df$rank.oe
    df$lwr = df$oe.boot.lwr
    df$upr = df$oe.boot.upr
    ylab = 'O/E ratio'
  } else if (plot.y == 'pe') {
    df$y = df$pe
    df$rank = df$rank.pe
    df$lwr = df$pe.lwr
    df$upr = df$pe.upr
    ylab = 'P/E ratio'
  } else if (plot.y == 'rs') {
    df$y = df$rs
    df$rank = df$rank.rs
    df$lwr = df$rs.lwr
    df$upr = df$rs.upr
    ylab = 'Risk-standardized performance'
  }

  fig <- ggplot2::ggplot(data = df, aes(x = rank, y = y)) +
    geom_point(color = 'Red') +
    geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.1) +
    xlab('Rank') +
    ylab(ylab) +
    theme_classic() +
    theme(
      plot.title = element_text(size = 16, face ="bold"),
      axis.text = element_text(size = 16),
      axis.ticks.length = unit(.25,"cm"),
      axis.title = element_text(size = 18, face = "bold"),
      strip.text = element_text(size = 18, face = "bold"),
      legend.position = 'none'
    )
return(fig)
}
