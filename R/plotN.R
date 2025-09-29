#' Plot sample size distribution across accountable entities
#' @description
#' This function creates a histogram of entity sample sizes.
#' @param n vector of sample sizes
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @importFrom ggplot2 ggplot
#' @export

plotN <- function(n){
  df <- data.frame(n)
  fig <- ggplot2::ggplot(data = df, ggplot2::aes(n)) +
    ggplot2::geom_histogram(color = 'white', fill = 'black', binwidth = 5) +
    ggplot2::scale_y_continuous(expand = c(0,0)) +
    ggplot2::xlab('Number of observations') +
    ggplot2::ylab('Entities') +
    ggplot2::ggtitle('Sample size distribution') +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 16, face ="bold"),
      axis.text.y = ggplot2::element_text(size = 16),
      axis.ticks.length = ggplot2::unit(.25,"cm"),
      axis.text.x = ggplot2::element_text(size = 16),
      axis.title = ggplot2::element_text(size = 18, face = "bold"),
      strip.text = ggplot2::element_text(size = 18, face = "bold"),
      legend.position = 'top',
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 18)
    )
  return(fig)
}
