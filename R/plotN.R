#' Plot sample size distribution across accountable entities
#' @description
#' This function creates a histogram of entity sample sizes.
#' @param n vector of sample sizes
#' @returns A ggplot figure
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @references None
#' @examples
#' # TBD
#' @importFrom ggplot2 ggplot
#' @export

plotN <- function(n){
  df <- data.frame(n)
  fig <- ggplot2::ggplot(data = df, aes(n)) +
    geom_histogram(color = 'white', fill = 'black', binwidth = 5) +
    scale_y_continuous(expand = c(0,0)) +
    xlab('Number of observations') +
    ylab('Entities') +
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
