#' Example plot of values used to calcualte split-sample reliability.
#' @description
#' This function creates a plot of the measure performance for each entity split across two exclusive random samples of observations within each entity.
#' @param df observation-level data; if null, will use the dataframe from the model object
#' @param model model; if null, will use an unadjusted model
#' @param entity data column containing the accountable entity identifier
#' @param y data column containing the outcome variable
#' @param ctrPerf parameters to control performance measure calculation
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @references None
#' @examples
#' # TBD
#' @importFrom ggplot2 ggplot


plotSSR <- function(df, model = NULL, entity = 'entity', y = 'y', ctrPerf = controlPerf()){

  # clean data
  df = cleanData(df = df, entity = entity, y = y, ctrPerf = ctrPerf)

  # split-sample plot to visualize split-sample reliability
  message('making example plot of split-sample reliability estimates')

  data.out <- calcDataSummary(df, model, entity, y, ctrPerf)
  marg.p <- data.out$marg.p
  df.SSR <- data.out$df

  entities = unique(df.SSR$entity)
  n.entity = length(entities)

  df.SSR$s <- 1
  for (j in 1:n.entity){
    entity.df <- df.SSR[df.SSR$entity == entities[j], ]
    entity.df$s[sample(nrow(entity.df), nrow(entity.df)/2, replace = F)] <- 2
    df.SSR$s[df.SSR$entity == entities[j]] <- entity.df$s
  }
  df.SSR$s <- as.factor(df.SSR$s)

  entity.means <- aggregate(y ~ entity + s, data = df.SSR, mean)
  entity.means.wide <- reshape(entity.means, idvar = "entity", timevar = "s", direction = "wide")

  agg       <- aggregate(y ~ entity + s, data = df.SSR, sum)
  agg$obs   <- agg$y
  agg$pred  <- aggregate(predict ~ entity + s, data = df.SSR, sum)$predict
  agg$exp   <- aggregate(expect ~ entity + s, data = df.SSR, sum)$expect
  agg$oe    <- agg$obs / agg$exp * marg.p
  agg$pe    <- agg$pred / agg$exp * marg.p
  entity.means.wide.adj <- reshape(agg, idvar = "entity", timevar = "s", direction = "wide")

  plot.df.SSR <- data.frame(
    Method = rep(c('OE-standardized', 'PE-standardized'), each = nrow(entity.means.wide.adj)),
    t1 = c(entity.means.wide.adj$oe.1, entity.means.wide.adj$pe.1),
    t2 = c(entity.means.wide.adj$oe.2, entity.means.wide.adj$pe.2)
  )
  max = max(c(plot.df.SSR$t1, plot.df.SSR$t2))

  fig.SSR <- ggplot2::ggplot(data = plot.df.SSR, ggplot2::aes(x = t1, y = t2, group = Method)) +
    ggplot2::geom_point(ggplot2::aes(color = Method, shape = Method), size = 3) +
    ggplot2::geom_abline(slope = 1, lty = 'dashed') +
    ggplot2::coord_cartesian(xlim = c(0, max), ylim = c(0, max)) +
    ggplot2::scale_color_manual(values = c('darkgrey', 'red')) +
    ggplot2::xlab('Split 1') +
    ggplot2::ylab('Split 2') +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 16, face ="bold"),
      axis.text = ggplot2::element_text(size = 16),
      axis.ticks.length = ggplot2::unit(.25,"cm"),
      axis.title = ggplot2::element_text(size = 18, face = "bold"),
      legend.position = 'top',
      legend.text = ggplot2::element_text(size = 16),
      legend.title = ggplot2::element_text(size = 18, face = 'bold')
    )
fig.SSR
}
