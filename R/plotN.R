#' Plot sample size distribution across accountable entities
#' @description
#' This function creates a histogram of entity sample sizes.
#' @param n vector of sample sizes
#' @param bin.width width of bins; default is `10`
#' @returns A ggplot figure
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @examples
#' # plot sample sizes from colonoscopy dataset
#' plotN(colonoscopy$n)
#'
#' # plot sample sizes from psychiatric readmissions dataset
#' plotN(psychreadmission$n, bin.width = 100)
#'
#' @importFrom ggplot2 ggplot
#' @export

plotN <- function(n, bin.width=10){
  if(!is.vector(n, mode = 'numeric')) stop('`n` should be a vector of numerical sample sizes.')
  df <- data.frame(n)
  fig <- ggplot2::ggplot(data = df, ggplot2::aes(n)) +
    ggplot2::geom_histogram(color = 'white', fill = 'black', binwidth = bin.width) +
    ggplot2::scale_y_continuous(expand = c(0,0)) +
    ggplot2::xlab('Number of observations') +
    ggplot2::ylab('Entities') +
    ggplot2::theme_classic() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(),
      panel.grid.minor = ggplot2::element_line(),
      axis.text.y = ggplot2::element_text(size = 16),
      axis.ticks.length = ggplot2::unit(.25,"cm"),
      axis.text.x = ggplot2::element_text(size = 16),
      axis.title = ggplot2::element_text(size = 18, face = "bold")
      )
  return(fig)
}
