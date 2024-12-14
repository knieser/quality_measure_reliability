plotPerformance <- function(df, plot.y = 'p'){
  if (plot.y == 'p') {
    fig.performance <- ggplot(data = df, aes(x = rank, y = p)) +
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
    fig.performance <- ggplot(data = df, aes(x = rank.oe, y = oe)) +
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
return(fig.performance)
}
