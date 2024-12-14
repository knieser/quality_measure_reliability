plotN <- function(n){
  df <- data.frame(n)
  fig <- ggplot(data = df, aes(n)) +
    geom_histogram(binwidth = 100, color = 'white', fill = 'black') +
    scale_y_continuous(expand = c(0,0)) +
    xlab('Number of cases') +
    ylab('Providers') +
    ggtitle('Sample size distribution') +
    theme_classic() +
    theme(
      plot.title = element_text(size = 16, face ="bold"),
      axis.text.y = element_text(size = 16),
      axis.ticks.length = unit(.25,"cm"),
      axis.text.x = element_text(size = 16),
      axis.title = element_text(size = 18, face = "bold"),
      strip.text = element_text(size = 18, face = "bold"),
      legend.position = 'top',
      legend.title = element_blank(),
      legend.text = element_text(size = 18)
    )
  return(fig)
}