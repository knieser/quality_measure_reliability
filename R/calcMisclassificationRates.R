#' Calculate misclassification probabilities
#' @description
#' This function runs the misclassification functions
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
#' @export

calcMisclassificationRates <- function(df = NULL, model = NULL, entity = "entity", y = "y", ctrPerf = controlPerf(), ctrRel = controlRel(), output.dir, filename.add = NULL){
  if (is.null(df) & is.null(model)) stop ('Please provide either a dataframe or a model object')
  d.steps <- ctrRel$d.steps
  num.groups <- ctrRel$classification.quantiles
  method <- ctrRel$rs.method
  plot.file.type = '.png'
  percentile.misclassification.fig.file = paste0(output.dir, method, '_percentile_misclassification_fig', filename.add, plot.file.type)
  misclassification.fig.file = paste0(output.dir, method, '_misclassification_fig', filename.add, plot.file.type)

  # fit model and get estimates
  data.out <- calcDataSummary(df, model, entity, y, ctrPerf)
  df = data.out$df
  model = data.out$model
  fit = data.out$fit
  marg.p <- data.out$marg.p
  entities <- data.out$entities
  n       <- data.out$n
  obs     <- data.out$obs
  pred    <- data.out$pred
  exp     <- data.out$exp
  oe      <- obs / exp
  pe      <- pred / exp
  rs.oe   <- oe * marg.p
  rs.pe   <- pe * marg.p

  # obtain bootstrapped ranks
  output <- parametricBootstrap(df, model, entities, entity, y, ctrPerf)
  if (method == 'oe'){
    rs = rs.oe
    rs.boot = output$rs.oe.boot
  } else if (method == 'pe'){
    rs = rs.pe
    rs.boot = output$rs.pe.boot
  }

  # calculate percentile misclassification rates
  orig.ranks = rank(rs, ties.method = 'random')
  orig.percentile.breaks <- quantile(orig.ranks, probs = seq(0, 1, 1/100))
  orig.percentiles = as.numeric(cut(orig.ranks, breaks = orig.percentile.breaks, labels = 1:100, include.lowest = T))

  boot.ranks <- apply(rs.boot, 2, rank, ties.method = 'random')
  boot.percentile.breaks <- apply(boot.ranks, 2, quantile, probs = seq(0, 1, 1/100))
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

  percentile.plot.df <- data.frame(
    entities = rep(entities, each = length(diffs)),
    d = rep(diffs, length(entities)),
    p = c(t(prob.diffs))
  )
  percentile.fig <- ggplot2::ggplot(data = percentile.plot.df, aes(x = d, y = p, group = entities)) +
    geom_point(aes(color = entities), size = 2) +
    geom_line(aes(color = entities)) +
    xlab('d (differences in percentiles)') +
    ylab('Pr(|observed rank - true rank| < d)') +
    theme_classic() +
    theme(
      axis.title = element_text(size = 16, face = 'bold'),
      axis.text = element_text(size = 14, face = 'bold'),
      legend.position = 'none')
  ggsave(filename = percentile.misclassification.fig.file, plot = percentile.fig, width = 8, height = 8)

  # calculate misclassification by quantile
  orig.breaks <- quantile(orig.ranks, probs = seq(0, 1, 1/num.groups))
  orig.quantiles = cut(orig.ranks, breaks = orig.breaks, labels = 1:num.groups, include.lowest = T)

  boot.breaks <- apply(boot.ranks, 2, quantile, probs = seq(0, 1, 1/num.groups))
  boot.quantiles = matrix(data = NA, nrow = nrow(boot.ranks), ncol = ncol(boot.ranks))
  for (k in 1:ncol(boot.quantiles)){
    boot.quantiles[,k] <- cut(boot.ranks[,k], breaks = boot.breaks[,k], labels = 1:num.groups, include.lowest = T)
  }

  quantile.plot.df <- data.frame(
    entities = rep(entities, each = ncol(rs.boot)),
    true.rank = rep(orig.quantiles, each = ncol(rs.boot)),
    boot.rank = c(t(boot.quantiles))
  )
  quantile.fig <- ggplot2::ggplot(data = quantile.plot.df, aes(x = boot.rank, y = true.rank)) +
    geom_jitter() +
    xlab('Bootstrapped rank') +
    ylab('Estimated true rank') +
    theme_classic() +
    theme(axis.title = element_text(size = 16, face = 'bold'),
          axis.text = element_text(size = 14, face = 'bold'),
          legend.position = 'none')
  ggsave(filename = misclassification.fig.file, plot = quantile.fig, width = 8, height = 8)

  results = list(entities = entities,
                 orig.quantiles = orig.quantiles,
                 boot.quantiles = boot.quantiles,
                 quantile.plot.df = quantile.plot.df,
                 quantile.fig = quantile.fig,
                 diffs = diffs,
                 prob.diffs = prob.diffs,
                 percentile.plot.df = percentile.plot.df,
                 percentile.fig = percentile.fig)
}
