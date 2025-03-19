
reliability_analysis <- function(df, model, entity = 'entity', y = 'y', ctrPerf = controlPerf(), ctrRel = controlRel()){

  # clean data
  df = cleanData(df = df, entity = entity, y = y, ctrPerf = ctrPerf)

  # calculate reliability
  rel.results <- calcReliability(df = df, model = model, entity = entity, y = y, ctrPerf = ctrPerf, ctrRel = ctrRel)
  HLGM.out <- rel.results$HLGM.out
  BB.out <- rel.results$BB.out

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
  fig.SSR <- ggplot2::ggplot(data = plot.df.SSR, aes(x = t1, y = t2, group = Method)) +
    geom_point(aes(color = Method, shape = Method), size = 3) +
    geom_abline(slope = 1, lty = 'dashed') +
    coord_cartesian(xlim = c(0, max), ylim = c(0, max)) +
    scale_color_manual(values = c('darkgrey', 'red')) +
    xlab('Split 1') +
    ylab('Split 2') +
    theme_classic() +
    theme(
      plot.title = element_text(size = 16, face ="bold"),
      axis.text = element_text(size = 16),
      axis.ticks.length = unit(.25,"cm"),
      axis.title = element_text(size = 18, face = "bold"),
      legend.position = 'top',
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 18, face = 'bold')
    )

  results = list(df = df,
                 df.SSR = df.SSR,
                 rel.results = rel.results,
                 HLGM.out = HLGM.out,
                 BB.out = BB.out,
                 fig.SSR = fig.SSR)

  return(results)
}
