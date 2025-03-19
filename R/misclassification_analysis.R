#' Calculate misclassification probabilities (in progress)
#' @description
#' This function runs the misclassification functions
#' @param df observation-level data; if null, will use the dataframe from the model object
#' @param model model; if null, will use an unadjusted model
#' @param entity data column containing the accountable entity identifier
#' @param y data column containing the outcome variable
#' @param ctrPerf parameters to control performance measure calculation
#' @param ctrRel parameters to control reliability estimation
#' @returns Estimated misclassification probabilities
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @references None
#' @examples
#' # TBD

misclassification_analysis <- function(df = NULL, model = NULL, entity = "entity", y = "y", ctrPerf = controlPerf(), ctrRel = controlRel()){
  if (is.null(df) & is.null(model)) stop ('Please provide either a dataframe or a model object')
  d.steps <- ctrRel$d.steps

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

  # make figures
  fig.p.percentile <- ggplot2::ggplot(data = percentile.df, aes(x = d, y = p, group = entities)) +
    geom_point(aes(color = entities), size = 2) +
    geom_line(aes(color = entities)) +
    xlab('d (differences in percentiles)') +
    ylab('Pr(|observed rank - true rank| < d)') +
    theme_classic() +
    theme(
      axis.title = element_text(size = 16, face = 'bold'),
      axis.text = element_text(size = 14, face = 'bold'),
      legend.position = 'none')

  fig.oe.percentile <- ggplot2::ggplot(data = percentile.df, aes(x = d, y = oe, group = entities)) +
    geom_point(aes(color = entities), size = 2) +
    geom_line(aes(color = entities)) +
    xlab('d (differences in percentiles)') +
    ylab('Pr(|observed rank - true rank| < d)') +
    theme_classic() +
    theme(
      axis.title = element_text(size = 16, face = 'bold'),
      axis.text = element_text(size = 14, face = 'bold'),
      legend.position = 'none')

  fig.pe.percentile <- ggplot2::ggplot(data = percentile.df, aes(x = d, y = pe, group = entities)) +
    geom_point(aes(color = entities), size = 2) +
    geom_line(aes(color = entities)) +
    xlab('d (differences in percentiles)') +
    ylab('Pr(|observed rank - true rank| < d)') +
    theme_classic() +
    theme(
      axis.title = element_text(size = 16, face = 'bold'),
      axis.text = element_text(size = 14, face = 'bold'),
      legend.position = 'none')

  results = list(entities = entities,
                 p.diffs = p.diffs,
                 oe.diffs = oe.diffs,
                 pe.diffs = pe.diffs,
                 percentile.df = percentile.df,
                 fig.p.percentile = fig.p.percentile,
                 fig.oe.percentile = fig.oe.percentile,
                 fig.pe.percentile = fig.pe.percentile)
}
