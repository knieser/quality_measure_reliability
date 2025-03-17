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
    df$rank = df$rank.p
    df$lwr = df$p.lwr
    df$upr = df$p.upr
    ylab = 'Unadjusted rate'
  } else if (plot.y == 'oe') {
    df$y = df$rs.oe
    df$rank = df$rank.oe
    df$lwr = df$rs.oe.lwr
    df$upr = df$rs.oe.upr
    ylab = 'OE risk-standardized rate'
  } else if (plot.y == 'pe') {
    df$y = df$rs.pe
    df$rank = df$rank.pe
    df$lwr = df$rs.pe.lwr
    df$upr = df$rs.pe.lwr
    ylab = 'PE risk-standardized rate'
  }

  fig <- ggplot2::ggplot(data = df, aes(x = rank, y = y)) +
    geom_point(color = 'darkblue') +
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
