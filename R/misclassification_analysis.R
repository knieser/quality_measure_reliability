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

misclassification_analysis <- function(df = NULL, model = NULL, entity = "entity", y = "y", ctrPerf = controlPerf(), ctrRel = controlRel(), output.dir, filename.add = NULL){
  if (is.null(df) & is.null(model)) stop ('Please provide either a dataframe or a model object')
  d.steps <- ctrRel$d.steps
  plot.file.type = '.png'
  unadjusted.percentile.misclassification.fig.file = paste0(output.dir, 'fig_unadj_percentile_misclassification', filename.add, plot.file.type)
  oe.percentile.misclassification.fig.file = paste0(output.dir, 'fig_oe_percentile_misclassification', filename.add, plot.file.type)
  pe.percentile.misclassification.fig.file = paste0(output.dir, 'fig_pe_percentile_misclassification', filename.add, plot.file.type)

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
  p       <- obs / n
  oe      <- obs / exp
  pe      <- pred / exp
  rs.oe   <- oe * marg.p
  rs.pe   <- pe * marg.p

  # obtain bootstrapped ranks
  output <- parametricBootstrap(df, model, entities, entity, y, ctrPerf)
  p.boot = output$p.boot
  rs.oe.boot = output$rs.oe.boot
  rs.pe.boot = output$rs.pe.boot

  diffs = seq(0, 100, d.steps)
  p.diffs = calcMisclassificationRates(rs = p, rs.boot = p.boot, d.steps = d.steps)
  oe.diffs = calcMisclassificationRates(rs = rs.oe, rs.boot = rs.oe.boot, d.steps = d.steps)
  pe.diffs = calcMisclassificationRates(rs = rs.pe, rs.boot = rs.pe.boot, d.steps = d.steps)

  percentile.df <- data.frame(
    entities = rep(entities, each = length(diffs)),
    d = rep(diffs, length(entities)),
    p = c(t(p.diffs)),
    oe = c(t(oe.diffs)),
    pe = c(t(pe.diffs))
  )

  p.percentile.fig <- ggplot2::ggplot(data = percentile.df, aes(x = d, y = p, group = entities)) +
    geom_point(aes(color = entities), size = 2) +
    geom_line(aes(color = entities)) +
    xlab('d (differences in percentiles)') +
    ylab('Pr(|observed rank - true rank| < d)') +
    theme_classic() +
    theme(
      axis.title = element_text(size = 16, face = 'bold'),
      axis.text = element_text(size = 14, face = 'bold'),
      legend.position = 'none')
  ggsave(filename = unadjusted.percentile.misclassification.fig.file, plot = p.percentile.fig, width = 8, height = 8)

  oe.percentile.fig <- ggplot2::ggplot(data = percentile.df, aes(x = d, y = oe, group = entities)) +
    geom_point(aes(color = entities), size = 2) +
    geom_line(aes(color = entities)) +
    xlab('d (differences in percentiles)') +
    ylab('Pr(|observed rank - true rank| < d)') +
    theme_classic() +
    theme(
      axis.title = element_text(size = 16, face = 'bold'),
      axis.text = element_text(size = 14, face = 'bold'),
      legend.position = 'none')
  ggsave(filename = oe.percentile.misclassification.fig.file, plot = oe.percentile.fig, width = 8, height = 8)

  pe.percentile.fig <- ggplot2::ggplot(data = percentile.df, aes(x = d, y = pe, group = entities)) +
    geom_point(aes(color = entities), size = 2) +
    geom_line(aes(color = entities)) +
    xlab('d (differences in percentiles)') +
    ylab('Pr(|observed rank - true rank| < d)') +
    theme_classic() +
    theme(
      axis.title = element_text(size = 16, face = 'bold'),
      axis.text = element_text(size = 14, face = 'bold'),
      legend.position = 'none')
  ggsave(filename = pe.percentile.misclassification.fig.file, plot = pe.percentile.fig, width = 8, height = 8)

  results = list(entities = entities,
                 p.diffs = p.diffs,
                 oe.diffs = oe.diffs,
                 pe.diffs = pe.diffs,
                 percentile.df = percentile.df)
}
