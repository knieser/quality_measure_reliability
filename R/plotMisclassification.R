plotMisclassification <- function(df){
  fig <- ggplot(data = df, aes(x = as.factor(orig.class), y = as.factor(boot.class))) +
    geom_jitter() +
    #scale_x_reverse() +
    #scale_y_reverse() +
    xlab('Observed class') +
    ylab('Resampled class') +
    coord_flip() +
    theme_classic() +
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 16),
          legend.position = 'none')
  fig
}
