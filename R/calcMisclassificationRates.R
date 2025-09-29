calcMisclassificationRates <- function(rs, rs.boot, d.steps){

  # calculate percentile misclassification rates
  orig.ranks = rank(rs, ties.method = 'random')
  orig.percentile.breaks <- stats::quantile(orig.ranks, probs = seq(0, 1, 1/100))
  orig.percentiles = as.numeric(cut(orig.ranks, breaks = orig.percentile.breaks, labels = 1:100, include.lowest = T))

  boot.ranks <- apply(rs.boot, 2, rank, ties.method = 'random')
  boot.percentile.breaks <- apply(boot.ranks, 2, stats::quantile, probs = seq(0, 1, 1/100))
  boot.percentiles = matrix(data = NA, nrow = nrow(boot.ranks), ncol = ncol(boot.ranks))
  for (k in 1:ncol(boot.percentiles)){
    boot.percentiles[,k] <- cut(boot.ranks[,k], breaks = boot.percentile.breaks[,k], labels = 1:100, include.lowest = T)
  }

  percentile.diff = apply(boot.percentiles, 2, function(x) abs(x - orig.percentiles))
  diffs = seq(0, 100, d.steps)
  prob.diffs = matrix(data = NA, nrow = nrow(boot.ranks), ncol = length(diffs))
  for (d in 1:length(diffs)){
    delta = diffs[d]
    prob.diffs[,d] <- apply(percentile.diff, 1, function(x) mean(x < delta))
  }

  return(prob.diffs)
}
