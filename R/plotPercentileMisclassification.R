#' Plot percentile misclassification probabilities
#' @description
#' This function plots percentile misclassification probabilities
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

plotPercentileMisclassification <- function(entities, orig.est, boot.est, out.file){
  orig.ranks = rank(orig.est, ties.method = 'random')
  orig.breaks <- quantile(orig.ranks, probs = seq(0, 1, 1/100))
  orig.quantiles = as.numeric(cut(orig.ranks, breaks = orig.breaks, labels = 1:100, include.lowest = T))

  boot.ranks <- apply(boot.est, 2, rank, ties.method = 'random')
  boot.breaks <- apply(boot.ranks, 2, quantile, probs = seq(0, 1, 1/100))
  boot.quantiles = matrix(data = NA, nrow = nrow(boot.ranks), ncol = ncol(boot.ranks))
  for (k in 1:ncol(boot.quantiles)){
    boot.quantiles[,k] <- cut(boot.ranks[,k], breaks = boot.breaks[,k], labels = 1:100, include.lowest = T)
  }

  quantile.diff = apply(boot.quantiles, 2, function(x) abs(x - orig.quantiles))
  diffs = seq(0, 100, 10)
  prob.diffs = matrix(data = NA, nrow = nrow(boot.ranks), ncol = length(diffs))
  for (d in 1:length(diffs)){
    delta = diffs[d]
    prob.diffs[,d] <- apply(quantile.diff, 1, function(x) mean(x < delta))
  }

  plot.df <- data.frame(
    entities = rep(entities, each = length(diffs)),
    d = rep(diffs, length(entities)),
    p = c(t(prob.diffs))
  )
  fig <- ggplot2::ggplot(data = plot.df, aes(x = d, y = p, group = entities)) +
    geom_point(aes(color = entities), size = 2) +
    geom_line(aes(color = entities)) +
    xlab('d (differences in percentiles)') +
    ylab('Pr(|observed rank - true rank| < d)') +
    theme_classic() +
    theme(axis.text = element_text(size = 16, face ="bold"),
          legend.position = 'none')
  ggsave(filename = out.file, plot = fig, width = 8, height = 8)

  results = list(entities = entities,
                 diffs = diffs,
                 prob.diffs = prob.diffs,
                 plot.df = plot.df,
                 fig = fig)
  return(results)
}
