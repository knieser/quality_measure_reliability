#' Plots misclassification visualization
#' @description
#' This function plots a visualization of misclassification probability
#' @param df dataframe; if null, will use the dataframe in the model object
#' @param model model; if null, will use an unadjusted model
#' @param entity variable to use as the accountable entity; default = "entity"
#' @param y variable to use as the outcome; default = "y"
#' @param ctrPerf parameters to control performance measure calculation
#' @returns Estimated measure performance by accountable entity
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @references None
#' @examples
#' # TBD
#' @importFrom ggplot2 ggplot
#' @export

plotMisclassification <- function(entities, orig.est, boot.est, num.groups, out.file){
  orig.ranks = rank(orig.est, ties.method = 'random')
  orig.breaks <- quantile(orig.ranks, probs = seq(0, 1, 1/num.groups))
  orig.quantiles = cut(orig.ranks, breaks = orig.breaks, labels = 1:num.groups, include.lowest = T)

  boot.ranks <- apply(boot.est, 2, rank, ties.method = 'random')
  boot.breaks <- apply(boot.ranks, 2, quantile, probs = seq(0, 1, 1/num.groups))
  boot.quantiles = matrix(data = NA, nrow = nrow(boot.ranks), ncol = ncol(boot.ranks))
  for (k in 1:ncol(boot.quantiles)){
    boot.quantiles[,k] <- cut(boot.ranks[,k], breaks = boot.breaks[,k], labels = 1:num.groups, include.lowest = T)
  }

  plot.df <- data.frame(
    entities = rep(entities, each = ncol(boot.est)),
    true.rank = rep(orig.quantiles, each = ncol(boot.est)),
    boot.rank = c(boot.quantiles)
  )
  fig <- ggplot2::ggplot(data = plot.df, aes(x = boot.rank, y = true.rank)) +
    geom_jitter() +
    xlab('Bootstrapped rank') +
    ylab('Estimated true rank') +
    theme_classic() +
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          legend.position = 'none')
  ggsave(filename = out.file, plot = fig, width = 8, height = 8)

  results = list(entities = entities,
                 orig.quantiles = orig.quantiles,
                 boot.quantiles = boot.quantiles,
                 plot.df = plot.df,
                 fig = fig)
  return(results)
}
